/*
 * QEMU float support
 *
 * The code in this source file is derived from release 2a of the SoftFloat
 * IEC/IEEE Floating-point Arithmetic Package. Those parts of the code (and
 * some later contributions) are provided under that license, as detailed below.
 * It has subsequently been modified by contributors to the QEMU Project,
 * so some portions are provided under:
 *  the SoftFloat-2a license
 *  the BSD license
 *  GPL-v2-or-later
 *
 * Any future contributions to this file after December 1st 2014 will be
 * taken to be licensed under the Softfloat-2a license unless specifically
 * indicated otherwise.
 */

static void glue(return_nan,N)(glue(FloatParts,N) *a, float_status *s)
{
    switch (a->cls) {
    case float_class_snan:
        float_raise(float_flag_invalid, s);
        if (s->default_nan_mode) {
            glue(parts_default_nan,N)(a, s);
        } else {
            glue(parts_silence_nan,N)(a, s);
        }
        break;
    case float_class_qnan:
        if (s->default_nan_mode) {
            glue(parts_default_nan,N)(a, s);
        }
        break;
    default:
        g_assert_not_reached();
    }
}

static glue(FloatParts,N) *
glue(pick_nan,N)(glue(FloatParts,N) *a, glue(FloatParts,N) *b, float_status *s)
{
    int cmp;

    if (is_snan(a->cls) || is_snan(b->cls)) {
        float_raise(float_flag_invalid, s);
    }

    if (s->default_nan_mode) {
        glue(parts_default_nan,N)(a, s);
        return a;
    }

    cmp = glue(CMP,N)(a->frac, b->frac);
    if (cmp == 0) {
        cmp = a->sign < b->sign;
    }
    if (pickNaN(a->cls, b->cls, cmp > 0, s)) {
        a = b;
    }

    if (is_snan(a->cls)) {
        glue(parts_silence_nan,N)(a, s);
    }
    return a;
}

static glue(FloatParts,N) *
glue(pick_nan_muladd,N)(glue(FloatParts,N) *a, glue(FloatParts,N) *b,
                        glue(FloatParts,N) *c, float_status *s,
                        int ab_mask, int abc_mask)
{
    int which;

    if (unlikely(abc_mask & float_cmask_snan)) {
        float_raise(float_flag_invalid, s);
    }

    which = pickNaNMulAdd(a->cls, b->cls, c->cls,
                          ab_mask == float_cmask_infzero, s);

    if (s->default_nan_mode) {
        /*
         * Note that this check is after pickNaNMulAdd so that function
         * has an opportunity to set the Invalid flag for infzero.
         */
        which = 3;
    }

    switch (which) {
    case 0:
        break;
    case 1:
        a = b;
        break;
    case 2:
        a = c;
        break;
    case 3:
        glue(parts_default_nan,N)(a, s);
        return a;
    default:
        g_assert_not_reached();
    }
    if (is_snan(a->cls)) {
        glue(parts_silence_nan,N)(a, s);
    }
    return a;
}

/*
 * Canonicalize the FloatParts structure.  Determine the class,
 * unbias the exponent, and normalize the fraction.
 */
static void glue(parts_canonicalize,N)(glue(FloatParts,N) *p,
                                       float_status *status,
                                       const FloatFmt *fmt)
{
    if (p->exp == 0) {
        if (likely(glue(EQZ,N)(p->frac))) {
            p->cls = float_class_zero;
        } else if (status->flush_inputs_to_zero) {
            float_raise(float_flag_input_denormal, status);
            p->cls = float_class_zero;
            memset(p->frac, 0, N / 8);
        } else {
            int shift = glue(CLZ,N)(p->frac) - 1;
            p->cls = float_class_normal;
            p->exp = fmt->frac_shift - fmt->exp_bias - shift + 1;
            glue(SHL,N)(p->frac, p->frac, shift);
        }
    } else if (p->exp < fmt->exp_max || fmt->arm_althp) {
        p->cls = float_class_normal;
        p->exp -= fmt->exp_bias;
        glue(SHL,N)(p->frac, p->frac, fmt->frac_shift);
        p->frac[0] |= DECOMPOSED_IMPLICIT_BIT;
    } else if (likely(glue(EQZ,N)(p->frac))) {
        p->cls = float_class_inf;
    } else {
        glue(SHL,N)(p->frac, p->frac, fmt->frac_shift);
        p->cls = (parts_is_snan_frac(p->frac[0], status)
                  ? float_class_snan : float_class_qnan);
    }
}
