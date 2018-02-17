/*
 * ARM Shared AdvSIMD / SVE Operations
 *
 * Copyright (c) 2018 Linaro
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "cpu.h"
#include "exec/helper-proto.h"
#include "tcg/tcg-gvec-desc.h"
#include "fpu/softfloat.h"


/* Floating-point trigonometric starting value.
 * See the ARM ARM pseudocode function FPTrigSMul.
 */
static float16 float16_ftsmul(float16 op1, uint16_t op2, float_status *stat)
{
    float16 result = float16_mul(op1, op1, stat);
    if (!float16_is_any_nan(result)) {
        result = float16_set_sign(result, op2 & 1);
    }
    return result;
}

static float32 float32_ftsmul(float32 op1, uint32_t op2, float_status *stat)
{
    float32 result = float32_mul(op1, op1, stat);
    if (!float32_is_any_nan(result)) {
        result = float32_set_sign(result, op2 & 1);
    }
    return result;
}

static float64 float64_ftsmul(float64 op1, uint64_t op2, float_status *stat)
{
    float64 result = float64_mul(op1, op1, stat);
    if (!float64_is_any_nan(result)) {
        result = float64_set_sign(result, op2 & 1);
    }
    return result;
}

#define DO_3OP(NAME, FUNC, TYPE) \
void HELPER(NAME)(void *vd, void *vn, void *vm, void *stat, uint32_t desc) \
{                                                                          \
    intptr_t i, oprsz = simd_oprsz(desc);                                  \
    TYPE *d = vd, *n = vn, *m = vm;                                        \
    for (i = 0; i < oprsz / sizeof(TYPE); i++) {                           \
        d[i] = FUNC(n[i], m[i], stat);                                     \
    }                                                                      \
}

DO_3OP(gvec_fadd_h, float16_add, float16)
DO_3OP(gvec_fadd_s, float32_add, float32)
DO_3OP(gvec_fadd_d, float64_add, float64)

DO_3OP(gvec_fsub_h, float16_sub, float16)
DO_3OP(gvec_fsub_s, float32_sub, float32)
DO_3OP(gvec_fsub_d, float64_sub, float64)

DO_3OP(gvec_fmul_h, float16_mul, float16)
DO_3OP(gvec_fmul_s, float32_mul, float32)
DO_3OP(gvec_fmul_d, float64_mul, float64)

DO_3OP(gvec_ftsmul_h, float16_ftsmul, float16)
DO_3OP(gvec_ftsmul_s, float32_ftsmul, float32)
DO_3OP(gvec_ftsmul_d, float64_ftsmul, float64)

#ifdef TARGET_AARCH64

DO_3OP(gvec_recps_h, helper_recpsf_f16, float16)
DO_3OP(gvec_recps_s, helper_recpsf_f32, float32)
DO_3OP(gvec_recps_d, helper_recpsf_f64, float64)

DO_3OP(gvec_rsqrts_h, helper_rsqrtsf_f16, float16)
DO_3OP(gvec_rsqrts_s, helper_rsqrtsf_f32, float32)
DO_3OP(gvec_rsqrts_d, helper_rsqrtsf_f64, float64)

#endif
#undef DO_3OP
