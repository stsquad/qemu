/*
 *  Generic vector operation expansion
 *
 *  Copyright (c) 2017 Linaro
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
#include "qemu-common.h"
#include "cpu.h"
#include "exec/exec-all.h"
#include "tcg.h"
#include "tcg-op.h"
#include "tcg-op-gvec.h"
#include "trace-tcg.h"
#include "trace/mem.h"

#define REP8(x)    ((x) * 0x0101010101010101ull)
#define REP16(x)   ((x) * 0x0001000100010001ull)

#define MAX_UNROLL  4

static inline void check_size_align(uint32_t opsz, uint32_t clsz, uint32_t ofs)
{
    uint32_t align = clsz > 16 || opsz >= 16 ? 15 : 7;
    tcg_debug_assert(opsz > 0);
    tcg_debug_assert(opsz <= clsz);
    tcg_debug_assert((opsz & align) == 0);
    tcg_debug_assert((clsz & align) == 0);
    tcg_debug_assert((ofs & align) == 0);
}

static inline void check_overlap_3(uint32_t d, uint32_t a,
                                   uint32_t b, uint32_t s)
{
    tcg_debug_assert(d == a || d + s <= a || a + s <= d);
    tcg_debug_assert(d == b || d + s <= b || b + s <= d);
    tcg_debug_assert(a == b || a + s <= b || b + s <= a);
}

static inline bool check_size_impl(uint32_t opsz, uint32_t lnsz)
{
    uint32_t lnct = opsz / lnsz;
    return lnct >= 1 && lnct <= MAX_UNROLL;
}

static void expand_clr_v(uint32_t dofs, uint32_t clsz, uint32_t lnsz,
                         TCGType type, TCGOpcode opc_mv, TCGOpcode opc_st)
{
    TCGArg t0 = tcg_temp_new_internal(type, 0);
    TCGArg env = GET_TCGV_PTR(tcg_ctx.tcg_env);
    uint32_t i;

    tcg_gen_op2(&tcg_ctx, opc_mv, t0, 0);
    for (i = 0; i < clsz; i += lnsz) {
        tcg_gen_op3(&tcg_ctx, opc_st, t0, env, dofs + i);
    }
    tcg_temp_free_internal(t0);
}

static void expand_clr(uint32_t dofs, uint32_t clsz)
{
    if (clsz >= 32 && TCG_TARGET_HAS_v256) {
        uint32_t done = QEMU_ALIGN_DOWN(clsz, 32);
        expand_clr_v(dofs, done, 32, TCG_TYPE_V256,
                     INDEX_op_movi_v256, INDEX_op_st_v256);
        dofs += done;
        clsz -= done;
    }

    if (clsz >= 16 && TCG_TARGET_HAS_v128) {
        uint16_t done = QEMU_ALIGN_DOWN(clsz, 16);
        expand_clr_v(dofs, done, 16, TCG_TYPE_V128,
                     INDEX_op_movi_v128, INDEX_op_st_v128);
        dofs += done;
        clsz -= done;
    }

    if (TCG_TARGET_REG_BITS == 64) {
        expand_clr_v(dofs, clsz, 8, TCG_TYPE_I64,
                     INDEX_op_movi_i64, INDEX_op_st_i64);
    } else if (TCG_TARGET_HAS_v64) {
        expand_clr_v(dofs, clsz, 8, TCG_TYPE_V64,
                     INDEX_op_movi_v64, INDEX_op_st_v64);
    } else {
        expand_clr_v(dofs, clsz, 4, TCG_TYPE_I32,
                     INDEX_op_movi_i32, INDEX_op_st_i32);
    }
}

static TCGv_i32 make_desc(uint32_t opsz, uint32_t clsz)
{
    tcg_debug_assert(opsz >= 16 && opsz <= 255 * 16 && opsz % 16 == 0);
    tcg_debug_assert(clsz >= 16 && clsz <= 255 * 16 && clsz % 16 == 0);
    opsz /= 16;
    clsz /= 16;
    opsz -= 1;
    clsz -= 1;
    return tcg_const_i32(deposit32(opsz, 8, 8, clsz));
}

static void expand_3_o(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                       uint32_t opsz, uint32_t clsz,
                       void (*fno)(TCGv_ptr, TCGv_ptr, TCGv_ptr, TCGv_i32))
{
    TCGv_ptr d = tcg_temp_new_ptr();
    TCGv_ptr a = tcg_temp_new_ptr();
    TCGv_ptr b = tcg_temp_new_ptr();
    TCGv_i32 desc = make_desc(opsz, clsz);

    tcg_gen_addi_ptr(d, tcg_ctx.tcg_env, dofs);
    tcg_gen_addi_ptr(a, tcg_ctx.tcg_env, aofs);
    tcg_gen_addi_ptr(b, tcg_ctx.tcg_env, bofs);
    fno(d, a, b, desc);

    tcg_temp_free_ptr(d);
    tcg_temp_free_ptr(a);
    tcg_temp_free_ptr(b);
    tcg_temp_free_i32(desc);
}

static void expand_3x4(uint32_t dofs, uint32_t aofs,
                       uint32_t bofs, uint32_t opsz,
                       void (*fni)(TCGv_i32, TCGv_i32, TCGv_i32))
{
    TCGv_i32 t0 = tcg_temp_new_i32();
    uint32_t i;

    if (aofs == bofs) {
        for (i = 0; i < opsz; i += 4) {
            tcg_gen_ld_i32(t0, tcg_ctx.tcg_env, aofs + i);
            fni(t0, t0, t0);
            tcg_gen_st_i32(t0, tcg_ctx.tcg_env, dofs + i);
        }
    } else {
        TCGv_i32 t1 = tcg_temp_new_i32();
        for (i = 0; i < opsz; i += 4) {
            tcg_gen_ld_i32(t0, tcg_ctx.tcg_env, aofs + i);
            tcg_gen_ld_i32(t1, tcg_ctx.tcg_env, bofs + i);
            fni(t0, t0, t1);
            tcg_gen_st_i32(t0, tcg_ctx.tcg_env, dofs + i);
        }
        tcg_temp_free_i32(t1);
    }
    tcg_temp_free_i32(t0);
}

static void expand_3x8(uint32_t dofs, uint32_t aofs,
                       uint32_t bofs, uint32_t opsz,
                       void (*fni)(TCGv_i64, TCGv_i64, TCGv_i64))
{
    TCGv_i64 t0 = tcg_temp_new_i64();
    uint32_t i;

    if (aofs == bofs) {
        for (i = 0; i < opsz; i += 8) {
            tcg_gen_ld_i64(t0, tcg_ctx.tcg_env, aofs + i);
            fni(t0, t0, t0);
            tcg_gen_st_i64(t0, tcg_ctx.tcg_env, dofs + i);
        }
    } else {
        TCGv_i64 t1 = tcg_temp_new_i64();
        for (i = 0; i < opsz; i += 8) {
            tcg_gen_ld_i64(t0, tcg_ctx.tcg_env, aofs + i);
            tcg_gen_ld_i64(t1, tcg_ctx.tcg_env, bofs + i);
            fni(t0, t0, t1);
            tcg_gen_st_i64(t0, tcg_ctx.tcg_env, dofs + i);
        }
        tcg_temp_free_i64(t1);
    }
    tcg_temp_free_i64(t0);
}

/* FIXME: add CSE for constants and we can eliminate this.  */
static void expand_3x8p1(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                         uint32_t opsz, uint64_t data,
                         void (*fni)(TCGv_i64, TCGv_i64, TCGv_i64, TCGv_i64))
{
    TCGv_i64 t0 = tcg_temp_new_i64();
    TCGv_i64 t2 = tcg_const_i64(data);
    uint32_t i;

    if (aofs == bofs) {
        for (i = 0; i < opsz; i += 8) {
            tcg_gen_ld_i64(t0, tcg_ctx.tcg_env, aofs + i);
            fni(t0, t0, t0, t2);
            tcg_gen_st_i64(t0, tcg_ctx.tcg_env, dofs + i);
        }
    } else {
        TCGv_i64 t1 = tcg_temp_new_i64();
        for (i = 0; i < opsz; i += 8) {
            tcg_gen_ld_i64(t0, tcg_ctx.tcg_env, aofs + i);
            tcg_gen_ld_i64(t1, tcg_ctx.tcg_env, bofs + i);
            fni(t0, t0, t1, t2);
            tcg_gen_st_i64(t0, tcg_ctx.tcg_env, dofs + i);
        }
        tcg_temp_free_i64(t1);
    }
    tcg_temp_free_i64(t0);
    tcg_temp_free_i64(t2);
}

static void expand_3_v(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                       uint32_t opsz, uint32_t lnsz, TCGType type,
                       TCGOpcode opc_op, TCGOpcode opc_ld, TCGOpcode opc_st)
{
    TCGArg t0 = tcg_temp_new_internal(type, 0);
    TCGArg env = GET_TCGV_PTR(tcg_ctx.tcg_env);
    uint32_t i;

    if (aofs == bofs) {
        for (i = 0; i < opsz; i += lnsz) {
            tcg_gen_op3(&tcg_ctx, opc_ld, t0, env, aofs + i);
            tcg_gen_op3(&tcg_ctx, opc_op, t0, t0, t0);
            tcg_gen_op3(&tcg_ctx, opc_st, t0, env, dofs + i);
        }
    } else {
        TCGArg t1 = tcg_temp_new_internal(type, 0);
        for (i = 0; i < opsz; i += lnsz) {
            tcg_gen_op3(&tcg_ctx, opc_ld, t0, env, aofs + i);
            tcg_gen_op3(&tcg_ctx, opc_ld, t1, env, bofs + i);
            tcg_gen_op3(&tcg_ctx, opc_op, t0, t0, t1);
            tcg_gen_op3(&tcg_ctx, opc_st, t0, env, dofs + i);
        }
        tcg_temp_free_internal(t1);
    }
    tcg_temp_free_internal(t0);
}

void tcg_gen_gvec_3(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                    uint32_t opsz, uint32_t clsz, const GVecGen3 *g)
{
    check_size_align(opsz, clsz, dofs | aofs | bofs);
    check_overlap_3(dofs, aofs, bofs, clsz);

    if (opsz > MAX_UNROLL * 32 || clsz > MAX_UNROLL * 32) {
        goto do_ool;
    }

    /* Recall that ARM SVE allows vector sizes that are not a power of 2.
       Expand with successively smaller host vector sizes.  The intent is
       that e.g. opsz == 80 would be expanded with 2x32 + 1x16.  */
    /* ??? For clsz > opsz, the host may be able to use an op-sized
       operation, zeroing the balance of the register.  We can then
       use a cl-sized store to implement the clearing without an extra
       store operation.  This is true for aarch64 and x86_64 hosts.  */

    if (check_size_impl(opsz, 32) && tcg_op_supported(g->op_v256)) {
        uint32_t done = QEMU_ALIGN_DOWN(opsz, 32);
        expand_3_v(dofs, aofs, bofs, done, 32, TCG_TYPE_V256,
                   g->op_v256, INDEX_op_ld_v256, INDEX_op_st_v256);
        dofs += done;
        aofs += done;
        bofs += done;
        opsz -= done;
        clsz -= done;
    }

    if (check_size_impl(opsz, 16) && tcg_op_supported(g->op_v128)) {
        uint32_t done = QEMU_ALIGN_DOWN(opsz, 16);
        expand_3_v(dofs, aofs, bofs, done, 16, TCG_TYPE_V128,
                   g->op_v128, INDEX_op_ld_v128, INDEX_op_st_v128);
        dofs += done;
        aofs += done;
        bofs += done;
        opsz -= done;
        clsz -= done;
    }

    if (check_size_impl(opsz, 8)) {
        uint32_t done = QEMU_ALIGN_DOWN(opsz, 8);
        if (tcg_op_supported(g->op_v64)) {
            expand_3_v(dofs, aofs, bofs, done, 8, TCG_TYPE_V64,
                       g->op_v64, INDEX_op_ld_v64, INDEX_op_st_v64);
        } else if (g->fni8) {
            expand_3x8(dofs, aofs, bofs, done, g->fni8);
        } else if (g->fni8x) {
            expand_3x8p1(dofs, aofs, bofs, done, g->extra_value, g->fni8x);
        } else {
            done = 0;
        }
        dofs += done;
        aofs += done;
        bofs += done;
        opsz -= done;
        clsz -= done;
    }

    if (check_size_impl(opsz, 4)) {
        uint32_t done = QEMU_ALIGN_DOWN(opsz, 4);
        expand_3x4(dofs, aofs, bofs, done, g->fni4);
        dofs += done;
        aofs += done;
        bofs += done;
        opsz -= done;
        clsz -= done;
    }

    if (opsz == 0) {
        if (clsz != 0) {
            expand_clr(dofs, clsz);
        }
        return;
    }

 do_ool:
    expand_3_o(dofs, aofs, bofs, opsz, clsz, g->fno);
}

static void gen_addv_mask(TCGv_i64 d, TCGv_i64 a, TCGv_i64 b, TCGv_i64 m)
{
    TCGv_i64 t1 = tcg_temp_new_i64();
    TCGv_i64 t2 = tcg_temp_new_i64();
    TCGv_i64 t3 = tcg_temp_new_i64();

    tcg_gen_andc_i64(t1, a, m);
    tcg_gen_andc_i64(t2, b, m);
    tcg_gen_xor_i64(t3, a, b);
    tcg_gen_add_i64(d, t1, t2);
    tcg_gen_and_i64(t3, t3, m);
    tcg_gen_xor_i64(d, d, t3);

    tcg_temp_free_i64(t1);
    tcg_temp_free_i64(t2);
    tcg_temp_free_i64(t3);
}

void tcg_gen_gvec_add8(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                       uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .extra_value = REP8(0x80),
        .fni8x = gen_addv_mask,
        .op_v256 = INDEX_op_add8_v256,
        .op_v128 = INDEX_op_add8_v128,
        .op_v64 = INDEX_op_add8_v64,
        .fno = gen_helper_gvec_add8,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_gvec_add16(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                        uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .extra_value = REP16(0x8000),
        .fni8x = gen_addv_mask,
        .op_v256 = INDEX_op_add16_v256,
        .op_v128 = INDEX_op_add16_v128,
        .op_v64 = INDEX_op_add16_v64,
        .fno = gen_helper_gvec_add16,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_gvec_add32(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                        uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .fni4 = tcg_gen_add_i32,
        .op_v256 = INDEX_op_add32_v256,
        .op_v128 = INDEX_op_add32_v128,
        .op_v64 = INDEX_op_add32_v64,
        .fno = gen_helper_gvec_add32,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_gvec_add64(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                        uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .fni8 = tcg_gen_add_i64,
        .op_v256 = INDEX_op_add64_v256,
        .op_v128 = INDEX_op_add64_v128,
        .fno = gen_helper_gvec_add64,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_vec8_add8(TCGv_i64 d, TCGv_i64 a, TCGv_i64 b)
{
    TCGv_i64 m = tcg_const_i64(REP8(0x80));
    gen_addv_mask(d, a, b, m);
    tcg_temp_free_i64(m);
}

void tcg_gen_vec8_add16(TCGv_i64 d, TCGv_i64 a, TCGv_i64 b)
{
    TCGv_i64 m = tcg_const_i64(REP16(0x8000));
    gen_addv_mask(d, a, b, m);
    tcg_temp_free_i64(m);
}

void tcg_gen_vec8_add32(TCGv_i64 d, TCGv_i64 a, TCGv_i64 b)
{
    TCGv_i64 t1 = tcg_temp_new_i64();
    TCGv_i64 t2 = tcg_temp_new_i64();

    tcg_gen_andi_i64(t1, a, ~0xffffffffull);
    tcg_gen_add_i64(t2, a, b);
    tcg_gen_add_i64(t1, t1, b);
    tcg_gen_deposit_i64(d, t1, t2, 0, 32);

    tcg_temp_free_i64(t1);
    tcg_temp_free_i64(t2);
}

static void gen_subv_mask(TCGv_i64 d, TCGv_i64 a, TCGv_i64 b, TCGv_i64 m)
{
    TCGv_i64 t1 = tcg_temp_new_i64();
    TCGv_i64 t2 = tcg_temp_new_i64();
    TCGv_i64 t3 = tcg_temp_new_i64();

    tcg_gen_or_i64(t1, a, m);
    tcg_gen_andc_i64(t2, b, m);
    tcg_gen_eqv_i64(t3, a, b);
    tcg_gen_sub_i64(d, t1, t2);
    tcg_gen_and_i64(t3, t3, m);
    tcg_gen_xor_i64(d, d, t3);

    tcg_temp_free_i64(t1);
    tcg_temp_free_i64(t2);
    tcg_temp_free_i64(t3);
}

void tcg_gen_gvec_sub8(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                       uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .extra_value = REP8(0x80),
        .fni8x = gen_subv_mask,
        .op_v256 = INDEX_op_sub8_v256,
        .op_v128 = INDEX_op_sub8_v128,
        .op_v64 = INDEX_op_sub8_v64,
        .fno = gen_helper_gvec_sub8,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_gvec_sub16(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                        uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .extra_value = REP16(0x8000),
        .fni8x = gen_subv_mask,
        .op_v256 = INDEX_op_sub16_v256,
        .op_v128 = INDEX_op_sub16_v128,
        .op_v64 = INDEX_op_sub16_v64,
        .fno = gen_helper_gvec_sub16,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_gvec_sub32(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                        uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .fni4 = tcg_gen_sub_i32,
        .op_v256 = INDEX_op_sub32_v256,
        .op_v128 = INDEX_op_sub32_v128,
        .op_v64 = INDEX_op_sub32_v64,
        .fno = gen_helper_gvec_sub32,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_gvec_sub64(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                        uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .fni8 = tcg_gen_sub_i64,
        .op_v256 = INDEX_op_sub64_v256,
        .op_v128 = INDEX_op_sub64_v128,
        .fno = gen_helper_gvec_sub64,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_vec8_sub8(TCGv_i64 d, TCGv_i64 a, TCGv_i64 b)
{
    TCGv_i64 m = tcg_const_i64(REP8(0x80));
    gen_subv_mask(d, a, b, m);
    tcg_temp_free_i64(m);
}

void tcg_gen_vec8_sub16(TCGv_i64 d, TCGv_i64 a, TCGv_i64 b)
{
    TCGv_i64 m = tcg_const_i64(REP16(0x8000));
    gen_subv_mask(d, a, b, m);
    tcg_temp_free_i64(m);
}

void tcg_gen_vec8_sub32(TCGv_i64 d, TCGv_i64 a, TCGv_i64 b)
{
    TCGv_i64 t1 = tcg_temp_new_i64();
    TCGv_i64 t2 = tcg_temp_new_i64();

    tcg_gen_andi_i64(t1, b, ~0xffffffffull);
    tcg_gen_sub_i64(t2, a, b);
    tcg_gen_sub_i64(t1, a, t1);
    tcg_gen_deposit_i64(d, t1, t2, 0, 32);

    tcg_temp_free_i64(t1);
    tcg_temp_free_i64(t2);
}

void tcg_gen_gvec_and8(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                       uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .fni8 = tcg_gen_and_i64,
        .op_v256 = INDEX_op_and_v256,
        .op_v128 = INDEX_op_and_v128,
        .op_v64 = INDEX_op_and_v64,
        .fno = gen_helper_gvec_and8,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_gvec_or8(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                      uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .fni8 = tcg_gen_or_i64,
        .op_v256 = INDEX_op_or_v256,
        .op_v128 = INDEX_op_or_v128,
        .op_v64 = INDEX_op_or_v64,
        .fno = gen_helper_gvec_or8,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_gvec_xor8(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                       uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .fni8 = tcg_gen_xor_i64,
        .op_v256 = INDEX_op_xor_v256,
        .op_v128 = INDEX_op_xor_v128,
        .op_v64 = INDEX_op_xor_v64,
        .fno = gen_helper_gvec_xor8,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_gvec_andc8(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                        uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .fni8 = tcg_gen_andc_i64,
        .op_v256 = INDEX_op_andc_v256,
        .op_v128 = INDEX_op_andc_v128,
        .op_v64 = INDEX_op_andc_v64,
        .fno = gen_helper_gvec_andc8,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}

void tcg_gen_gvec_orc8(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                       uint32_t opsz, uint32_t clsz)
{
    static const GVecGen3 g = {
        .fni8 = tcg_gen_orc_i64,
        .op_v256 = INDEX_op_orc_v256,
        .op_v128 = INDEX_op_orc_v128,
        .op_v64 = INDEX_op_orc_v64,
        .fno = gen_helper_gvec_orc8,
    };
    tcg_gen_gvec_3(dofs, aofs, bofs, opsz, clsz, &g);
}
