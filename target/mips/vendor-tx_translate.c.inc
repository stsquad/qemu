/*
 *  Toshiba TX instruction translation routines.
 *
 *  Copyright (c) 2004-2005 Jocelyn Mayer
 *  Copyright (c) 2006 Marius Groeger (FPU operations)
 *  Copyright (c) 2006 Thiemo Seufer (MIPS32R2 support)
 *  Copyright (c) 2009 CodeSourcery (MIPS16 and microMIPS support)
 *  Copyright (c) 2012 Jia Liu & Dongxue Zhang (MIPS ASE DSP support)
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 *
 *     Overview of the TX79-specific instruction set
 *     =============================================
 *
 * Reference:
 *
 * The Toshiba TX System RISC TX79 Core Architecture manual,
 * https://wiki.qemu.org/File:C790.pdf
 *
 *     Three-Operand Multiply and Multiply-Add (4 instructions)
 *     --------------------------------------------------------
 * MADD    [rd,] rs, rt      Multiply/Add
 * MADDU   [rd,] rs, rt      Multiply/Add Unsigned
 * MULT    [rd,] rs, rt      Multiply (3-operand)
 * MULTU   [rd,] rs, rt      Multiply Unsigned (3-operand)
 *
 *     Multiply Instructions for Pipeline 1 (10 instructions)
 *     ------------------------------------------------------
 * MULT1   [rd,] rs, rt      Multiply Pipeline 1
 * MULTU1  [rd,] rs, rt      Multiply Unsigned Pipeline 1
 * DIV1    rs, rt            Divide Pipeline 1
 * DIVU1   rs, rt            Divide Unsigned Pipeline 1
 * MADD1   [rd,] rs, rt      Multiply-Add Pipeline 1
 * MADDU1  [rd,] rs, rt      Multiply-Add Unsigned Pipeline 1
 * MFHI1   rd                Move From HI1 Register
 * MFLO1   rd                Move From LO1 Register
 * MTHI1   rs                Move To HI1 Register
 * MTLO1   rs                Move To LO1 Register
 *
 */

#if defined(TARGET_MIPS64)

#include "vendor-tx-mmi_translate.c.inc"

/* Copy GPR to and from TX79 HI1/LO1 register. */
static void gen_HILO1_tx79(DisasContext *ctx, uint32_t opc, int reg)
{
    if (reg == 0 && (opc == OPC_MFHI1 || opc == OPC_MFLO1)) {
        /* Treat as NOP. */
        return;
    }

    switch (opc) {
    case OPC_MFHI1:
        tcg_gen_mov_tl(cpu_gpr[reg], cpu_HI[1]);
        break;
    case OPC_MFLO1:
        tcg_gen_mov_tl(cpu_gpr[reg], cpu_LO[1]);
        break;
    case OPC_MTHI1:
        if (reg != 0) {
            tcg_gen_mov_tl(cpu_HI[1], cpu_gpr[reg]);
        } else {
            tcg_gen_movi_tl(cpu_HI[1], 0);
        }
        break;
    case OPC_MTLO1:
        if (reg != 0) {
            tcg_gen_mov_tl(cpu_LO[1], cpu_gpr[reg]);
        } else {
            tcg_gen_movi_tl(cpu_LO[1], 0);
        }
        break;
    default:
        MIPS_INVAL("mfthilo1 TX79");
        generate_exception_end(ctx, EXCP_RI);
        break;
    }
}

static void gen_div1_tx79(DisasContext *ctx, uint32_t opc, int rs, int rt)
{
    TCGv t0, t1;

    t0 = tcg_temp_new();
    t1 = tcg_temp_new();

    gen_load_gpr(t0, rs);
    gen_load_gpr(t1, rt);

    switch (opc) {
    case OPC_DIV1:
        {
            TCGv t2 = tcg_temp_new();
            TCGv t3 = tcg_temp_new();
            tcg_gen_ext32s_tl(t0, t0);
            tcg_gen_ext32s_tl(t1, t1);
            tcg_gen_setcondi_tl(TCG_COND_EQ, t2, t0, INT_MIN);
            tcg_gen_setcondi_tl(TCG_COND_EQ, t3, t1, -1);
            tcg_gen_and_tl(t2, t2, t3);
            tcg_gen_setcondi_tl(TCG_COND_EQ, t3, t1, 0);
            tcg_gen_or_tl(t2, t2, t3);
            tcg_gen_movi_tl(t3, 0);
            tcg_gen_movcond_tl(TCG_COND_NE, t1, t2, t3, t2, t1);
            tcg_gen_div_tl(cpu_LO[1], t0, t1);
            tcg_gen_rem_tl(cpu_HI[1], t0, t1);
            tcg_gen_ext32s_tl(cpu_LO[1], cpu_LO[1]);
            tcg_gen_ext32s_tl(cpu_HI[1], cpu_HI[1]);
            tcg_temp_free(t3);
            tcg_temp_free(t2);
        }
        break;
    case OPC_DIVU1:
        {
            TCGv t2 = tcg_const_tl(0);
            TCGv t3 = tcg_const_tl(1);
            tcg_gen_ext32u_tl(t0, t0);
            tcg_gen_ext32u_tl(t1, t1);
            tcg_gen_movcond_tl(TCG_COND_EQ, t1, t1, t2, t3, t1);
            tcg_gen_divu_tl(cpu_LO[1], t0, t1);
            tcg_gen_remu_tl(cpu_HI[1], t0, t1);
            tcg_gen_ext32s_tl(cpu_LO[1], cpu_LO[1]);
            tcg_gen_ext32s_tl(cpu_HI[1], cpu_HI[1]);
            tcg_temp_free(t3);
            tcg_temp_free(t2);
        }
        break;
    default:
        MIPS_INVAL("div1 TX79");
        generate_exception_end(ctx, EXCP_RI);
        goto out;
    }
 out:
    tcg_temp_free(t0);
    tcg_temp_free(t1);
}
#endif

/*
 * These MULT[U] and MADD[U] instructions implemented in for example
 * the Toshiba/Sony R5900 and the Toshiba TX19, TX39 and TX79 core
 * architectures are special three-operand variants with the syntax
 *
 *     MULT[U][1] rd, rs, rt
 *
 * such that
 *
 *     (rd, LO, HI) <- rs * rt
 *
 * and
 *
 *     MADD[U][1] rd, rs, rt
 *
 * such that
 *
 *     (rd, LO, HI) <- (LO, HI) + rs * rt
 *
 * where the low-order 32-bits of the result is placed into both the
 * GPR rd and the special register LO. The high-order 32-bits of the
 * result is placed into the special register HI.
 *
 * If the GPR rd is omitted in assembly language, it is taken to be 0,
 * which is the zero register that always reads as 0.
 */
static void gen_mul_txx9(DisasContext *ctx, uint32_t opc,
                         int rd, int rs, int rt)
{
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();
    int acc = 0;

    gen_load_gpr(t0, rs);
    gen_load_gpr(t1, rt);

    switch (opc) {
    case OPC_MULT1:
        acc = 1;
        /* Fall through */
    case OPC_MULT:
        {
            TCGv_i32 t2 = tcg_temp_new_i32();
            TCGv_i32 t3 = tcg_temp_new_i32();
            tcg_gen_trunc_tl_i32(t2, t0);
            tcg_gen_trunc_tl_i32(t3, t1);
            tcg_gen_muls2_i32(t2, t3, t2, t3);
            if (rd) {
                tcg_gen_ext_i32_tl(cpu_gpr[rd], t2);
            }
            tcg_gen_ext_i32_tl(cpu_LO[acc], t2);
            tcg_gen_ext_i32_tl(cpu_HI[acc], t3);
            tcg_temp_free_i32(t2);
            tcg_temp_free_i32(t3);
        }
        break;
    case OPC_MULTU1:
        acc = 1;
        /* Fall through */
    case OPC_MULTU:
        {
            TCGv_i32 t2 = tcg_temp_new_i32();
            TCGv_i32 t3 = tcg_temp_new_i32();
            tcg_gen_trunc_tl_i32(t2, t0);
            tcg_gen_trunc_tl_i32(t3, t1);
            tcg_gen_mulu2_i32(t2, t3, t2, t3);
            if (rd) {
                tcg_gen_ext_i32_tl(cpu_gpr[rd], t2);
            }
            tcg_gen_ext_i32_tl(cpu_LO[acc], t2);
            tcg_gen_ext_i32_tl(cpu_HI[acc], t3);
            tcg_temp_free_i32(t2);
            tcg_temp_free_i32(t3);
        }
        break;
    case OPC_MADD1:
        acc = 1;
        /* Fall through */
    case OPC_MADD:
        {
            TCGv_i64 t2 = tcg_temp_new_i64();
            TCGv_i64 t3 = tcg_temp_new_i64();

            tcg_gen_ext_tl_i64(t2, t0);
            tcg_gen_ext_tl_i64(t3, t1);
            tcg_gen_mul_i64(t2, t2, t3);
            tcg_gen_concat_tl_i64(t3, cpu_LO[acc], cpu_HI[acc]);
            tcg_gen_add_i64(t2, t2, t3);
            tcg_temp_free_i64(t3);
            gen_move_low32(cpu_LO[acc], t2);
            gen_move_high32(cpu_HI[acc], t2);
            if (rd) {
                gen_move_low32(cpu_gpr[rd], t2);
            }
            tcg_temp_free_i64(t2);
        }
        break;
    case OPC_MADDU1:
        acc = 1;
        /* Fall through */
    case OPC_MADDU:
        {
            TCGv_i64 t2 = tcg_temp_new_i64();
            TCGv_i64 t3 = tcg_temp_new_i64();

            tcg_gen_ext32u_tl(t0, t0);
            tcg_gen_ext32u_tl(t1, t1);
            tcg_gen_extu_tl_i64(t2, t0);
            tcg_gen_extu_tl_i64(t3, t1);
            tcg_gen_mul_i64(t2, t2, t3);
            tcg_gen_concat_tl_i64(t3, cpu_LO[acc], cpu_HI[acc]);
            tcg_gen_add_i64(t2, t2, t3);
            tcg_temp_free_i64(t3);
            gen_move_low32(cpu_LO[acc], t2);
            gen_move_high32(cpu_HI[acc], t2);
            if (rd) {
                gen_move_low32(cpu_gpr[rd], t2);
            }
            tcg_temp_free_i64(t2);
        }
        break;
    default:
        MIPS_INVAL("mul/madd TXx9");
        generate_exception_end(ctx, EXCP_RI);
        goto out;
    }

 out:
    tcg_temp_free(t0);
    tcg_temp_free(t1);
}

static void decode_opc_special_tx79(CPUMIPSState *env, DisasContext *ctx)
{
    int rs = extract32(ctx->opcode, 21, 5);
    int rt = extract32(ctx->opcode, 16, 5);
    int rd = extract32(ctx->opcode, 11, 5);
    uint32_t op1 = MASK_SPECIAL(ctx->opcode);

    switch (op1) {
    case OPC_MOVN:         /* Conditional move */
    case OPC_MOVZ:
        gen_cond_move(ctx, op1, rd, rs, rt);
        break;
    case OPC_MFHI:          /* Move from HI/LO */
    case OPC_MFLO:
        gen_HILO(ctx, op1, 0, rd);
        break;
    case OPC_MTHI:
    case OPC_MTLO:          /* Move to HI/LO */
        gen_HILO(ctx, op1, 0, rs);
        break;
    case OPC_MULT:
    case OPC_MULTU:
        gen_mul_txx9(ctx, op1, rd, rs, rt);
        break;
    case OPC_DIV:
    case OPC_DIVU:
        gen_muldiv(ctx, op1, 0, rs, rt);
        break;
#if defined(TARGET_MIPS64)
    case OPC_DMULT:
    case OPC_DMULTU:
    case OPC_DDIV:
    case OPC_DDIVU:
        check_insn_opc_user_only(ctx, INSN_R5900);
        gen_muldiv(ctx, op1, 0, rs, rt);
        break;
#endif
    case OPC_JR:
        gen_compute_branch(ctx, op1, 4, rs, 0, 0, 4);
        break;
    default:            /* Invalid */
        MIPS_INVAL("special_tx79");
        generate_exception_end(ctx, EXCP_RI);
        break;
    }
}
