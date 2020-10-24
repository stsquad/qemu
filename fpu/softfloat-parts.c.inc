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

#define PARTSN  glue(FloatParts,N)
#define PARTSW  glue(FloatParts,W)

static void __attribute__((unused))
glue(return_nan,N)(PARTSN *a, float_status *s)
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

static PARTSN *
glue(pick_nan,N)(PARTSN *a, PARTSN *b, float_status *s)
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

static PARTSN *
glue(pick_nan_muladd,N)(PARTSN *a, PARTSN *b, PARTSN *c,
                        float_status *s, int ab_mask, int abc_mask)
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
static void glue(parts_canonicalize,N)(PARTSN *p, float_status *status,
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

/*
 * Round and uncanonicalize a floating-point number by parts. There
 * are FRAC_SHIFT bits that may require rounding at the bottom of the
 * fraction; these bits will be removed. The exponent will be biased
 * by EXP_BIAS and must be bounded by [EXP_MAX-1, 0].
 */
static void glue(parts_round_uncanonicalize,N)(PARTSN *p, float_status *s,
                                               const FloatFmt *fmt)
{
    const int exp_max = fmt->exp_max;
    const int frac_shift = fmt->frac_shift;
    const uint64_t frac_lsb = 1ull << frac_shift;
    const uint64_t frac_lsbm1 = 1ull << (frac_shift - 1);
    const uint64_t round_mask = frac_lsb - 1;
    const uint64_t roundeven_mask = round_mask | frac_lsb;
    int flags = 0;

    switch (p->cls) {
    case float_class_normal:
        {
            uint64_t frac_lo, inc;
            bool overflow_norm;

            frac_lo = p->frac[N / 64 - 1];

            switch (s->float_rounding_mode) {
            case float_round_nearest_even:
                overflow_norm = false;
                inc = ((frac_lo & roundeven_mask) != frac_lsbm1
                       ? frac_lsbm1 : 0);
                break;
            case float_round_ties_away:
                overflow_norm = false;
                inc = frac_lsbm1;
                break;
            case float_round_to_zero:
                overflow_norm = true;
                inc = 0;
                break;
            case float_round_up:
                inc = p->sign ? 0 : round_mask;
                overflow_norm = p->sign;
                break;
            case float_round_down:
                inc = p->sign ? round_mask : 0;
                overflow_norm = !p->sign;
                break;
            case float_round_to_odd:
                overflow_norm = true;
                inc = frac_lo & frac_lsb ? 0 : round_mask;
                break;
            default:
                g_assert_not_reached();
            }

            int exp = p->exp + fmt->exp_bias;
            if (likely(exp > 0)) {
                if (frac_lo & round_mask) {
                    flags |= float_flag_inexact;
                    glue(ADDI,N)(p->frac, p->frac, inc);
                    if (p->frac[0] & DECOMPOSED_OVERFLOW_BIT) {
                        glue(SHR,N)(p->frac, p->frac, 1);
                        exp++;
                    }
                }
                glue(SHR,N)(p->frac, p->frac, frac_shift);

                if (fmt->arm_althp) {
                    /* ARM Alt HP eschews Inf and NaN for a wider exponent.  */
                    if (unlikely(exp > exp_max)) {
                        /* Overflow.  Return the maximum normal.  */
                        flags = float_flag_invalid;
                        exp = exp_max;
                        memset(p->frac, -1, N / 8);
                    }
                } else if (unlikely(exp >= exp_max)) {
                    flags |= float_flag_overflow | float_flag_inexact;
                    if (overflow_norm) {
                        exp = exp_max - 1;
                        memset(p->frac, -1, N / 8);
                    } else {
                        p->cls = float_class_inf;
                        goto do_inf;
                    }
                }
            } else if (s->flush_to_zero) {
                flags |= float_flag_output_denormal;
                p->cls = float_class_zero;
                goto do_zero;
            } else {
                bool is_tiny = s->tininess_before_rounding || exp < 0;

                if (!is_tiny) {
                    uint64_t tmp[N / 64];
                    glue(ADDI,N)(tmp, p->frac, inc);
                    is_tiny = !(tmp[0] & DECOMPOSED_OVERFLOW_BIT);
                }
                glue(SHRJAM,N)(p->frac, p->frac, 1 - exp);
                frac_lo = p->frac[N / 64 - 1];

                if (frac_lo & round_mask) {
                    /* Need to recompute round-to-even/round-to-odd. */
                    switch (s->float_rounding_mode) {
                    case float_round_nearest_even:
                        inc = ((frac_lo & roundeven_mask) != frac_lsbm1
                               ? frac_lsbm1 : 0);
                        break;
                    case float_round_to_odd:
                        inc = frac_lo & frac_lsb ? 0 : round_mask;
                        break;
                    default:
                        break;
                    }
                    flags |= float_flag_inexact;
                    glue(ADDI,N)(p->frac, p->frac, inc);
                }

                exp = (p->frac[0] & DECOMPOSED_IMPLICIT_BIT) != 0;
                glue(SHR,N)(p->frac, p->frac, frac_shift);

                if (is_tiny && (flags & float_flag_inexact)) {
                    flags |= float_flag_underflow;
                }
                if (exp == 0 && glue(EQZ,N)(p->frac)) {
                    p->cls = float_class_zero;
                }
            }
            p->exp = exp;
        }
        break;

    case float_class_zero:
    do_zero:
        p->exp = 0;
        memset(p->frac, 0, N / 8);
        break;

    case float_class_inf:
    do_inf:
        g_assert(!fmt->arm_althp);
        p->exp = exp_max;
        memset(p->frac, 0, N / 8);
        break;

    case float_class_qnan:
    case float_class_snan:
        g_assert(!fmt->arm_althp);
        p->exp = exp_max;
        glue(SHR,N)(p->frac, p->frac, frac_shift);
        break;

    default:
        g_assert_not_reached();
    }
    float_raise(flags, s);
}

/*
 * Returns the result of adding or subtracting the values of the
 * floating-point values `a' and `b'. The operation is performed
 * according to the IEC/IEEE Standard for Binary Floating-Point
 * Arithmetic.
 */

static PARTSN *
glue(parts_addsub,N)(PARTSN *a, PARTSN *b, float_status *s, bool subtract)
{
    bool b_sign = b->sign ^ subtract;
    int ab_mask = float_cmask(a->cls) | float_cmask(b->cls);

    if (a->sign != b_sign) {
        /* Subtraction */
        if (likely(ab_mask == float_cmask_normal)) {
            return glue(parts_sub_normal,N)(a, b, s);
        }

        if (ab_mask == float_cmask_zero) {
            a->sign = s->float_rounding_mode == float_round_down;
            return a;
        }

        if (unlikely(ab_mask == float_cmask_inf)) {
            float_raise(float_flag_invalid, s);
            glue(parts_default_nan,N)(a, s);
            return a;
        }

        if (unlikely(ab_mask & float_cmask_anynan)) {
            return glue(pick_nan,N)(a, b, s);
        }

        if (a->cls == float_class_zero || b->cls == float_class_inf) {
            b->sign = a->sign ^ 1;
            return b;
        }
        if (b->cls == float_class_zero || a->cls == float_class_inf) {
            return a;
        }
    } else {
        /* Addition */
        if (likely(ab_mask == float_cmask_normal)) {
            return glue(parts_add_normal,N)(a, b, s);
        }

        if (unlikely(ab_mask & float_cmask_anynan)) {
            return glue(pick_nan,N)(a, b, s);
        }

        if (a->cls == float_class_inf || b->cls == float_class_zero) {
            return a;
        }
        if (b->cls == float_class_inf || a->cls == float_class_zero) {
            b->sign = b_sign;
            return b;
        }
    }
    g_assert_not_reached();
}

/*
 * Returns the result of multiplying the floating-point values `a' and
 * `b'. The operation is performed according to the IEC/IEEE Standard
 * for Binary Floating-Point Arithmetic.
 */
static PARTSN *
glue(parts_mul,N)(PARTSN *a, PARTSN *b, float_status *s)
{
    int ab_mask = float_cmask(a->cls) | float_cmask(b->cls);
    bool sign = a->sign ^ b->sign;

    if (likely(ab_mask == float_cmask_normal)) {
        uint64_t tmp[N / 32];
        int shift;

        glue(MULW,N)(tmp, a->frac, b->frac);
        glue(TRUNCJAM,N)(a->frac, tmp);

        /*
         * N-bit multiplication produces an N*2-bit result, so e.g.
         * 63-bit inputs produce a 126-bit result, so some of the above
         * left-shift is expected in order to place the result at the
         * binary point.  Shift will be either 1 or 2.
         */
        shift = clz64(a->frac[0]) - 1;
        glue(SHL,N)(a->frac, a->frac, shift);

        a->exp += b->exp + 2 - shift;
        a->sign = sign;
        return a;
    }

    /* Inf * Zero == NaN */
    if (unlikely(ab_mask == float_cmask_infzero)) {
        float_raise(float_flag_invalid, s);
        glue(parts_default_nan,N)(a, s);
        return a;
    }

    if (unlikely(ab_mask & float_cmask_anynan)) {
        return glue(pick_nan,N)(a, b, s);
    }

    /* Multiply by 0 or Inf */
    if (a->cls == float_class_inf || a->cls == float_class_zero) {
        a->sign = sign;
        return a;
    }
    if (b->cls == float_class_inf || b->cls == float_class_zero) {
        b->sign = sign;
        return b;
    }
    g_assert_not_reached();
}

/*
 * Returns the result of multiplying the floating-point values `a' and
 * `b' then adding 'c', with no intermediate rounding step after the
 * multiplication. The operation is performed according to the
 * IEC/IEEE Standard for Binary Floating-Point Arithmetic 754-2008.
 * The flags argument allows the caller to select negation of the
 * addend, the intermediate product, or the final result. (The
 * difference between this and having the caller do a separate
 * negation is that negating externally will flip the sign bit on NaNs.)
 *
 * Requires A and C extracted into a double-sized structure to provide the
 * extra space for the widening multiply.
 */
static PARTSN *
glue(parts_muladd,N)(PARTSW *a, PARTSN *b, PARTSW *c,
                     int flags, float_status *s)
{
    int ab_mask, abc_mask, shift;

    ab_mask = float_cmask(a->cls) | float_cmask(b->cls);
    abc_mask = float_cmask(c->cls) | ab_mask;

    /*
     * It is implementation-defined whether the cases of (0,inf,qnan)
     * and (inf,0,qnan) raise InvalidOperation or not (and what QNaN
     * they return if they do), so we have to hand this information
     * off to the target-specific pick-a-NaN routine.
     */
    if (unlikely(abc_mask & float_cmask_anynan)) {
        return glue(pick_nan_muladd,N)((PARTSN *)a, b, (PARTSN *)c,
                                       s, ab_mask, abc_mask);
    }

    if (flags & float_muladd_negate_c) {
        c->sign ^= 1;
    }

    /* Compute the sign of the product into A. */
    a->sign ^= b->sign;
    if (flags & float_muladd_negate_product) {
        a->sign ^= 1;
    }

    if (unlikely(ab_mask != float_cmask_normal)) {
        if (ab_mask == float_cmask_infzero) {
            goto return_dnan;
        }

        if (ab_mask & float_cmask_inf) {
            if (c->cls == float_class_inf && a->sign != c->sign) {
                goto return_dnan;
            }
            a->cls = float_class_inf;
            goto return_a_sign;
        }

        if (ab_mask & float_cmask_zero) {
            if (c->cls == float_class_zero) {
                if (a->sign != c->sign) {
                    c->sign = s->float_rounding_mode == float_round_down;
                }
                a = c;
                goto return_a_sign;
            }
            a = c;
            goto return_a_sign_exp;
        }
        g_assert_not_reached();
    }

    if (unlikely(c->cls == float_class_inf)) {
        a = c;
        goto return_a_sign;
    }

    /*
     * Perform the multiplication step.
     */
    glue(MULW,N)(a->frac, a->frac, b->frac);

    /*
     * N-bit multiplication produces an N*2-bit result, so e.g. 63-bit inputs
     * produce a 126-bit result, so some of the above left-shift is expected
     * in order to place the result at the binary point.
     */
    shift = clz64(a->frac[0]) - 1;
    glue(SHL,W)(a->frac, a->frac, shift);
    a->exp += b->exp + 2 - shift;

    /* Perform the addition step.  */
    if (c->cls != float_class_zero) {
        /* Zero-extend C */
        memset(c->frac + N / 64, 0, N / 8);

        if (a->sign != c->sign) {
            glue(parts_sub_normal,W)(a, c, s);
        } else {
            glue(parts_add_normal,W)(a, c, s);
        }
    }

    glue(TRUNCJAM,N)(a->frac, a->frac);

 return_a_sign_exp:
    if (flags & float_muladd_halve_result) {
        a->exp -= 1;
    }
 return_a_sign:
    if (flags & float_muladd_negate_result) {
        a->sign ^= 1;
    }
    return (PARTSN *)a;

 return_dnan:
    float_raise(float_flag_invalid, s);
    glue(parts_default_nan,N)(b, s);
    return b;
}

#undef PARTSN
#undef PARTSW
