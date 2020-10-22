/*
 * Floating point arithmetic implementation
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

static glue(FloatParts,N) *
glue(parts_add_normal,N)(glue(FloatParts,N) *a, glue(FloatParts,N) *b,
                         float_status *s)
{
    int exp_diff = a->exp - b->exp;

    if (exp_diff > 0) {
        glue(SHRJAM,N)(b->frac, b->frac, exp_diff);
    } else if (exp_diff < 0) {
        glue(SHRJAM,N)(a->frac, a->frac, -exp_diff);
        a->exp = b->exp;
    }

    glue(ADD,N)(a->frac, a->frac, b->frac);
    if (a->frac[0] & DECOMPOSED_OVERFLOW_BIT) {
        glue(SHRJAM,N)(a->frac, a->frac, 1);
        a->exp += 1;
    }
    return a;
}

static glue(FloatParts,N) *
glue(parts_sub_normal,N)(glue(FloatParts,N) *a, glue(FloatParts,N) *b,
                         float_status *s)
{
    int exp_diff = a->exp - b->exp;
    int shift;

    if (exp_diff > 0) {
        glue(SHRJAM,N)(b->frac, b->frac, exp_diff);
    } else if (exp_diff < 0) {
        glue(SHRJAM,N)(a->frac, a->frac, -exp_diff);
        a->exp = b->exp;
    }

    glue(SUB,N)(a->frac, a->frac, b->frac);

    /* Overflow means that A was less than B. */
    if (a->frac[0] & DECOMPOSED_OVERFLOW_BIT) {
        glue(NEG,N)(a->frac, a->frac);
        a->sign ^= 1;
    }

    shift = glue(CLZ,N)(a->frac) - 1;

    if (unlikely(shift == N - 1)) {
        a->cls = float_class_zero;
        a->sign = s->float_rounding_mode == float_round_down;
    } else {
        glue(SHL,N)(a->frac, a->frac, shift);
        a->exp -= shift;
    }
    return a;
}
