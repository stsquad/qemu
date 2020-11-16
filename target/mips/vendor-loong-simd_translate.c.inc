/*
 *  Loongson SIMD translation routines.
 *
 *  Copyright (c) 2004-2005 Jocelyn Mayer
 *  Copyright (c) 2006 Marius Groeger (FPU operations)
 *  Copyright (c) 2006 Thiemo Seufer (MIPS32R2 support)
 *  Copyright (c) 2009 CodeSourcery (MIPS16 and microMIPS support)
 *  Copyright (c) 2011 Richard Henderson <rth@twiddle.net>
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 */

enum {
    /* Loongson 2F */
    OPC_MULT_G_2F   = 0x10 | OPC_SPECIAL2,
    OPC_DMULT_G_2F  = 0x11 | OPC_SPECIAL2,
    OPC_MULTU_G_2F  = 0x12 | OPC_SPECIAL2,
    OPC_DMULTU_G_2F = 0x13 | OPC_SPECIAL2,
    OPC_DIV_G_2F    = 0x14 | OPC_SPECIAL2,
    OPC_DDIV_G_2F   = 0x15 | OPC_SPECIAL2,
    OPC_DIVU_G_2F   = 0x16 | OPC_SPECIAL2,
    OPC_DDIVU_G_2F  = 0x17 | OPC_SPECIAL2,
    OPC_MOD_G_2F    = 0x1c | OPC_SPECIAL2,
    OPC_DMOD_G_2F   = 0x1d | OPC_SPECIAL2,
    OPC_MODU_G_2F   = 0x1e | OPC_SPECIAL2,
    OPC_DMODU_G_2F  = 0x1f | OPC_SPECIAL2,
};

/* Godson integer instructions */
static void gen_loongson_integer(DisasContext *ctx, uint32_t opc,
                                 int rd, int rs, int rt)
{
    TCGv t0, t1;

    if (rd == 0) {
        /* Treat as NOP. */
        return;
    }

    switch (opc) {
    case OPC_MULT_G_2E:
    case OPC_MULT_G_2F:
    case OPC_MULTU_G_2E:
    case OPC_MULTU_G_2F:
#if defined(TARGET_MIPS64)
    case OPC_DMULT_G_2E:
    case OPC_DMULT_G_2F:
    case OPC_DMULTU_G_2E:
    case OPC_DMULTU_G_2F:
#endif
        t0 = tcg_temp_new();
        t1 = tcg_temp_new();
        break;
    default:
        t0 = tcg_temp_local_new();
        t1 = tcg_temp_local_new();
        break;
    }

    gen_load_gpr(t0, rs);
    gen_load_gpr(t1, rt);

    switch (opc) {
    case OPC_MULT_G_2E:
    case OPC_MULT_G_2F:
        tcg_gen_mul_tl(cpu_gpr[rd], t0, t1);
        tcg_gen_ext32s_tl(cpu_gpr[rd], cpu_gpr[rd]);
        break;
    case OPC_MULTU_G_2E:
    case OPC_MULTU_G_2F:
        tcg_gen_ext32u_tl(t0, t0);
        tcg_gen_ext32u_tl(t1, t1);
        tcg_gen_mul_tl(cpu_gpr[rd], t0, t1);
        tcg_gen_ext32s_tl(cpu_gpr[rd], cpu_gpr[rd]);
        break;
    case OPC_DIV_G_2E:
    case OPC_DIV_G_2F:
        {
            TCGLabel *l1 = gen_new_label();
            TCGLabel *l2 = gen_new_label();
            TCGLabel *l3 = gen_new_label();
            tcg_gen_ext32s_tl(t0, t0);
            tcg_gen_ext32s_tl(t1, t1);
            tcg_gen_brcondi_tl(TCG_COND_NE, t1, 0, l1);
            tcg_gen_movi_tl(cpu_gpr[rd], 0);
            tcg_gen_br(l3);
            gen_set_label(l1);
            tcg_gen_brcondi_tl(TCG_COND_NE, t0, INT_MIN, l2);
            tcg_gen_brcondi_tl(TCG_COND_NE, t1, -1, l2);
            tcg_gen_mov_tl(cpu_gpr[rd], t0);
            tcg_gen_br(l3);
            gen_set_label(l2);
            tcg_gen_div_tl(cpu_gpr[rd], t0, t1);
            tcg_gen_ext32s_tl(cpu_gpr[rd], cpu_gpr[rd]);
            gen_set_label(l3);
        }
        break;
    case OPC_DIVU_G_2E:
    case OPC_DIVU_G_2F:
        {
            TCGLabel *l1 = gen_new_label();
            TCGLabel *l2 = gen_new_label();
            tcg_gen_ext32u_tl(t0, t0);
            tcg_gen_ext32u_tl(t1, t1);
            tcg_gen_brcondi_tl(TCG_COND_NE, t1, 0, l1);
            tcg_gen_movi_tl(cpu_gpr[rd], 0);
            tcg_gen_br(l2);
            gen_set_label(l1);
            tcg_gen_divu_tl(cpu_gpr[rd], t0, t1);
            tcg_gen_ext32s_tl(cpu_gpr[rd], cpu_gpr[rd]);
            gen_set_label(l2);
        }
        break;
    case OPC_MOD_G_2E:
    case OPC_MOD_G_2F:
        {
            TCGLabel *l1 = gen_new_label();
            TCGLabel *l2 = gen_new_label();
            TCGLabel *l3 = gen_new_label();
            tcg_gen_ext32u_tl(t0, t0);
            tcg_gen_ext32u_tl(t1, t1);
            tcg_gen_brcondi_tl(TCG_COND_EQ, t1, 0, l1);
            tcg_gen_brcondi_tl(TCG_COND_NE, t0, INT_MIN, l2);
            tcg_gen_brcondi_tl(TCG_COND_NE, t1, -1, l2);
            gen_set_label(l1);
            tcg_gen_movi_tl(cpu_gpr[rd], 0);
            tcg_gen_br(l3);
            gen_set_label(l2);
            tcg_gen_rem_tl(cpu_gpr[rd], t0, t1);
            tcg_gen_ext32s_tl(cpu_gpr[rd], cpu_gpr[rd]);
            gen_set_label(l3);
        }
        break;
    case OPC_MODU_G_2E:
    case OPC_MODU_G_2F:
        {
            TCGLabel *l1 = gen_new_label();
            TCGLabel *l2 = gen_new_label();
            tcg_gen_ext32u_tl(t0, t0);
            tcg_gen_ext32u_tl(t1, t1);
            tcg_gen_brcondi_tl(TCG_COND_NE, t1, 0, l1);
            tcg_gen_movi_tl(cpu_gpr[rd], 0);
            tcg_gen_br(l2);
            gen_set_label(l1);
            tcg_gen_remu_tl(cpu_gpr[rd], t0, t1);
            tcg_gen_ext32s_tl(cpu_gpr[rd], cpu_gpr[rd]);
            gen_set_label(l2);
        }
        break;
#if defined(TARGET_MIPS64)
    case OPC_DMULT_G_2E:
    case OPC_DMULT_G_2F:
        tcg_gen_mul_tl(cpu_gpr[rd], t0, t1);
        break;
    case OPC_DMULTU_G_2E:
    case OPC_DMULTU_G_2F:
        tcg_gen_mul_tl(cpu_gpr[rd], t0, t1);
        break;
    case OPC_DDIV_G_2E:
    case OPC_DDIV_G_2F:
        {
            TCGLabel *l1 = gen_new_label();
            TCGLabel *l2 = gen_new_label();
            TCGLabel *l3 = gen_new_label();
            tcg_gen_brcondi_tl(TCG_COND_NE, t1, 0, l1);
            tcg_gen_movi_tl(cpu_gpr[rd], 0);
            tcg_gen_br(l3);
            gen_set_label(l1);
            tcg_gen_brcondi_tl(TCG_COND_NE, t0, -1LL << 63, l2);
            tcg_gen_brcondi_tl(TCG_COND_NE, t1, -1LL, l2);
            tcg_gen_mov_tl(cpu_gpr[rd], t0);
            tcg_gen_br(l3);
            gen_set_label(l2);
            tcg_gen_div_tl(cpu_gpr[rd], t0, t1);
            gen_set_label(l3);
        }
        break;
    case OPC_DDIVU_G_2E:
    case OPC_DDIVU_G_2F:
        {
            TCGLabel *l1 = gen_new_label();
            TCGLabel *l2 = gen_new_label();
            tcg_gen_brcondi_tl(TCG_COND_NE, t1, 0, l1);
            tcg_gen_movi_tl(cpu_gpr[rd], 0);
            tcg_gen_br(l2);
            gen_set_label(l1);
            tcg_gen_divu_tl(cpu_gpr[rd], t0, t1);
            gen_set_label(l2);
        }
        break;
    case OPC_DMOD_G_2E:
    case OPC_DMOD_G_2F:
        {
            TCGLabel *l1 = gen_new_label();
            TCGLabel *l2 = gen_new_label();
            TCGLabel *l3 = gen_new_label();
            tcg_gen_brcondi_tl(TCG_COND_EQ, t1, 0, l1);
            tcg_gen_brcondi_tl(TCG_COND_NE, t0, -1LL << 63, l2);
            tcg_gen_brcondi_tl(TCG_COND_NE, t1, -1LL, l2);
            gen_set_label(l1);
            tcg_gen_movi_tl(cpu_gpr[rd], 0);
            tcg_gen_br(l3);
            gen_set_label(l2);
            tcg_gen_rem_tl(cpu_gpr[rd], t0, t1);
            gen_set_label(l3);
        }
        break;
    case OPC_DMODU_G_2E:
    case OPC_DMODU_G_2F:
        {
            TCGLabel *l1 = gen_new_label();
            TCGLabel *l2 = gen_new_label();
            tcg_gen_brcondi_tl(TCG_COND_NE, t1, 0, l1);
            tcg_gen_movi_tl(cpu_gpr[rd], 0);
            tcg_gen_br(l2);
            gen_set_label(l1);
            tcg_gen_remu_tl(cpu_gpr[rd], t0, t1);
            gen_set_label(l2);
        }
        break;
#endif
    }

    tcg_temp_free(t0);
    tcg_temp_free(t1);
}

/* Loongson multimedia instructions */
static void gen_loongson_multimedia(DisasContext *ctx, int rd, int rs, int rt)
{
    uint32_t opc, shift_max;
    TCGv_i64 t0, t1;
    TCGCond cond;

    opc = MASK_LMMI(ctx->opcode);
    switch (opc) {
    case OPC_ADD_CP2:
    case OPC_SUB_CP2:
    case OPC_DADD_CP2:
    case OPC_DSUB_CP2:
        t0 = tcg_temp_local_new_i64();
        t1 = tcg_temp_local_new_i64();
        break;
    default:
        t0 = tcg_temp_new_i64();
        t1 = tcg_temp_new_i64();
        break;
    }

    check_cp1_enabled(ctx);
    gen_load_fpr64(ctx, t0, rs);
    gen_load_fpr64(ctx, t1, rt);

    switch (opc) {
    case OPC_PADDSH:
        gen_helper_paddsh(t0, t0, t1);
        break;
    case OPC_PADDUSH:
        gen_helper_paddush(t0, t0, t1);
        break;
    case OPC_PADDH:
        gen_helper_paddh(t0, t0, t1);
        break;
    case OPC_PADDW:
        gen_helper_paddw(t0, t0, t1);
        break;
    case OPC_PADDSB:
        gen_helper_paddsb(t0, t0, t1);
        break;
    case OPC_PADDUSB:
        gen_helper_paddusb(t0, t0, t1);
        break;
    case OPC_PADDB:
        gen_helper_paddb(t0, t0, t1);
        break;

    case OPC_PSUBSH:
        gen_helper_psubsh(t0, t0, t1);
        break;
    case OPC_PSUBUSH:
        gen_helper_psubush(t0, t0, t1);
        break;
    case OPC_PSUBH:
        gen_helper_psubh(t0, t0, t1);
        break;
    case OPC_PSUBW:
        gen_helper_psubw(t0, t0, t1);
        break;
    case OPC_PSUBSB:
        gen_helper_psubsb(t0, t0, t1);
        break;
    case OPC_PSUBUSB:
        gen_helper_psubusb(t0, t0, t1);
        break;
    case OPC_PSUBB:
        gen_helper_psubb(t0, t0, t1);
        break;

    case OPC_PSHUFH:
        gen_helper_pshufh(t0, t0, t1);
        break;
    case OPC_PACKSSWH:
        gen_helper_packsswh(t0, t0, t1);
        break;
    case OPC_PACKSSHB:
        gen_helper_packsshb(t0, t0, t1);
        break;
    case OPC_PACKUSHB:
        gen_helper_packushb(t0, t0, t1);
        break;

    case OPC_PUNPCKLHW:
        gen_helper_punpcklhw(t0, t0, t1);
        break;
    case OPC_PUNPCKHHW:
        gen_helper_punpckhhw(t0, t0, t1);
        break;
    case OPC_PUNPCKLBH:
        gen_helper_punpcklbh(t0, t0, t1);
        break;
    case OPC_PUNPCKHBH:
        gen_helper_punpckhbh(t0, t0, t1);
        break;
    case OPC_PUNPCKLWD:
        gen_helper_punpcklwd(t0, t0, t1);
        break;
    case OPC_PUNPCKHWD:
        gen_helper_punpckhwd(t0, t0, t1);
        break;

    case OPC_PAVGH:
        gen_helper_pavgh(t0, t0, t1);
        break;
    case OPC_PAVGB:
        gen_helper_pavgb(t0, t0, t1);
        break;
    case OPC_PMAXSH:
        gen_helper_pmaxsh(t0, t0, t1);
        break;
    case OPC_PMINSH:
        gen_helper_pminsh(t0, t0, t1);
        break;
    case OPC_PMAXUB:
        gen_helper_pmaxub(t0, t0, t1);
        break;
    case OPC_PMINUB:
        gen_helper_pminub(t0, t0, t1);
        break;

    case OPC_PCMPEQW:
        gen_helper_pcmpeqw(t0, t0, t1);
        break;
    case OPC_PCMPGTW:
        gen_helper_pcmpgtw(t0, t0, t1);
        break;
    case OPC_PCMPEQH:
        gen_helper_pcmpeqh(t0, t0, t1);
        break;
    case OPC_PCMPGTH:
        gen_helper_pcmpgth(t0, t0, t1);
        break;
    case OPC_PCMPEQB:
        gen_helper_pcmpeqb(t0, t0, t1);
        break;
    case OPC_PCMPGTB:
        gen_helper_pcmpgtb(t0, t0, t1);
        break;

    case OPC_PSLLW:
        gen_helper_psllw(t0, t0, t1);
        break;
    case OPC_PSLLH:
        gen_helper_psllh(t0, t0, t1);
        break;
    case OPC_PSRLW:
        gen_helper_psrlw(t0, t0, t1);
        break;
    case OPC_PSRLH:
        gen_helper_psrlh(t0, t0, t1);
        break;
    case OPC_PSRAW:
        gen_helper_psraw(t0, t0, t1);
        break;
    case OPC_PSRAH:
        gen_helper_psrah(t0, t0, t1);
        break;

    case OPC_PMULLH:
        gen_helper_pmullh(t0, t0, t1);
        break;
    case OPC_PMULHH:
        gen_helper_pmulhh(t0, t0, t1);
        break;
    case OPC_PMULHUH:
        gen_helper_pmulhuh(t0, t0, t1);
        break;
    case OPC_PMADDHW:
        gen_helper_pmaddhw(t0, t0, t1);
        break;

    case OPC_PASUBUB:
        gen_helper_pasubub(t0, t0, t1);
        break;
    case OPC_BIADD:
        gen_helper_biadd(t0, t0);
        break;
    case OPC_PMOVMSKB:
        gen_helper_pmovmskb(t0, t0);
        break;

    case OPC_PADDD:
        tcg_gen_add_i64(t0, t0, t1);
        break;
    case OPC_PSUBD:
        tcg_gen_sub_i64(t0, t0, t1);
        break;
    case OPC_XOR_CP2:
        tcg_gen_xor_i64(t0, t0, t1);
        break;
    case OPC_NOR_CP2:
        tcg_gen_nor_i64(t0, t0, t1);
        break;
    case OPC_AND_CP2:
        tcg_gen_and_i64(t0, t0, t1);
        break;
    case OPC_OR_CP2:
        tcg_gen_or_i64(t0, t0, t1);
        break;

    case OPC_PANDN:
        tcg_gen_andc_i64(t0, t1, t0);
        break;

    case OPC_PINSRH_0:
        tcg_gen_deposit_i64(t0, t0, t1, 0, 16);
        break;
    case OPC_PINSRH_1:
        tcg_gen_deposit_i64(t0, t0, t1, 16, 16);
        break;
    case OPC_PINSRH_2:
        tcg_gen_deposit_i64(t0, t0, t1, 32, 16);
        break;
    case OPC_PINSRH_3:
        tcg_gen_deposit_i64(t0, t0, t1, 48, 16);
        break;

    case OPC_PEXTRH:
        tcg_gen_andi_i64(t1, t1, 3);
        tcg_gen_shli_i64(t1, t1, 4);
        tcg_gen_shr_i64(t0, t0, t1);
        tcg_gen_ext16u_i64(t0, t0);
        break;

    case OPC_ADDU_CP2:
        tcg_gen_add_i64(t0, t0, t1);
        tcg_gen_ext32s_i64(t0, t0);
        break;
    case OPC_SUBU_CP2:
        tcg_gen_sub_i64(t0, t0, t1);
        tcg_gen_ext32s_i64(t0, t0);
        break;

    case OPC_SLL_CP2:
        shift_max = 32;
        goto do_shift;
    case OPC_SRL_CP2:
        shift_max = 32;
        goto do_shift;
    case OPC_SRA_CP2:
        shift_max = 32;
        goto do_shift;
    case OPC_DSLL_CP2:
        shift_max = 64;
        goto do_shift;
    case OPC_DSRL_CP2:
        shift_max = 64;
        goto do_shift;
    case OPC_DSRA_CP2:
        shift_max = 64;
        goto do_shift;
    do_shift:
        /* Make sure shift count isn't TCG undefined behaviour.  */
        tcg_gen_andi_i64(t1, t1, shift_max - 1);

        switch (opc) {
        case OPC_SLL_CP2:
        case OPC_DSLL_CP2:
            tcg_gen_shl_i64(t0, t0, t1);
            break;
        case OPC_SRA_CP2:
        case OPC_DSRA_CP2:
            /*
             * Since SRA is UndefinedResult without sign-extended inputs,
             * we can treat SRA and DSRA the same.
             */
            tcg_gen_sar_i64(t0, t0, t1);
            break;
        case OPC_SRL_CP2:
            /* We want to shift in zeros for SRL; zero-extend first.  */
            tcg_gen_ext32u_i64(t0, t0);
            /* FALLTHRU */
        case OPC_DSRL_CP2:
            tcg_gen_shr_i64(t0, t0, t1);
            break;
        }

        if (shift_max == 32) {
            tcg_gen_ext32s_i64(t0, t0);
        }

        /* Shifts larger than MAX produce zero.  */
        tcg_gen_setcondi_i64(TCG_COND_LTU, t1, t1, shift_max);
        tcg_gen_neg_i64(t1, t1);
        tcg_gen_and_i64(t0, t0, t1);
        break;

    case OPC_ADD_CP2:
    case OPC_DADD_CP2:
        {
            TCGv_i64 t2 = tcg_temp_new_i64();
            TCGLabel *lab = gen_new_label();

            tcg_gen_mov_i64(t2, t0);
            tcg_gen_add_i64(t0, t1, t2);
            if (opc == OPC_ADD_CP2) {
                tcg_gen_ext32s_i64(t0, t0);
            }
            tcg_gen_xor_i64(t1, t1, t2);
            tcg_gen_xor_i64(t2, t2, t0);
            tcg_gen_andc_i64(t1, t2, t1);
            tcg_temp_free_i64(t2);
            tcg_gen_brcondi_i64(TCG_COND_GE, t1, 0, lab);
            generate_exception(ctx, EXCP_OVERFLOW);
            gen_set_label(lab);
            break;
        }

    case OPC_SUB_CP2:
    case OPC_DSUB_CP2:
        {
            TCGv_i64 t2 = tcg_temp_new_i64();
            TCGLabel *lab = gen_new_label();

            tcg_gen_mov_i64(t2, t0);
            tcg_gen_sub_i64(t0, t1, t2);
            if (opc == OPC_SUB_CP2) {
                tcg_gen_ext32s_i64(t0, t0);
            }
            tcg_gen_xor_i64(t1, t1, t2);
            tcg_gen_xor_i64(t2, t2, t0);
            tcg_gen_and_i64(t1, t1, t2);
            tcg_temp_free_i64(t2);
            tcg_gen_brcondi_i64(TCG_COND_GE, t1, 0, lab);
            generate_exception(ctx, EXCP_OVERFLOW);
            gen_set_label(lab);
            break;
        }

    case OPC_PMULUW:
        tcg_gen_ext32u_i64(t0, t0);
        tcg_gen_ext32u_i64(t1, t1);
        tcg_gen_mul_i64(t0, t0, t1);
        break;

    case OPC_SEQU_CP2:
    case OPC_SEQ_CP2:
        cond = TCG_COND_EQ;
        goto do_cc_cond;
        break;
    case OPC_SLTU_CP2:
        cond = TCG_COND_LTU;
        goto do_cc_cond;
        break;
    case OPC_SLT_CP2:
        cond = TCG_COND_LT;
        goto do_cc_cond;
        break;
    case OPC_SLEU_CP2:
        cond = TCG_COND_LEU;
        goto do_cc_cond;
        break;
    case OPC_SLE_CP2:
        cond = TCG_COND_LE;
    do_cc_cond:
        {
            int cc = (ctx->opcode >> 8) & 0x7;
            TCGv_i64 t64 = tcg_temp_new_i64();
            TCGv_i32 t32 = tcg_temp_new_i32();

            tcg_gen_setcond_i64(cond, t64, t0, t1);
            tcg_gen_extrl_i64_i32(t32, t64);
            tcg_gen_deposit_i32(fpu_fcr31, fpu_fcr31, t32,
                                get_fp_bit(cc), 1);

            tcg_temp_free_i32(t32);
            tcg_temp_free_i64(t64);
        }
        goto no_rd;
        break;
    default:
        MIPS_INVAL("loongson_cp2");
        generate_exception_end(ctx, EXCP_RI);
        return;
    }

    gen_store_fpr64(ctx, t0, rd);

no_rd:
    tcg_temp_free_i64(t0);
    tcg_temp_free_i64(t1);
}
