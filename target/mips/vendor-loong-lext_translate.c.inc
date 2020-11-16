/*
 *  Loongson EXTensions translation routines.
 *
 *  Copyright (c) 2004-2005 Jocelyn Mayer
 *  Copyright (c) 2006 Marius Groeger (FPU operations)
 *  Copyright (c) 2006 Thiemo Seufer (MIPS32R2 support)
 *  Copyright (c) 2009 CodeSourcery (MIPS16 and microMIPS support)
 *  Copyright (c) 2012 Jia Liu & Dongxue Zhang (MIPS ASE DSP support)
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 */

/* Loongson EXT load/store quad word opcodes */
#define MASK_LOONGSON_GSLSQ(op)           (MASK_OP_MAJOR(op) | (op & 0x8020))
enum {
    OPC_GSLQ        = 0x0020 | OPC_LWC2,
    OPC_GSLQC1      = 0x8020 | OPC_LWC2,
    OPC_GSSHFL      = OPC_LWC2,
    OPC_GSSQ        = 0x0020 | OPC_SWC2,
    OPC_GSSQC1      = 0x8020 | OPC_SWC2,
    OPC_GSSHFS      = OPC_SWC2,
};

/* Loongson EXT shifted load/store opcodes */
#define MASK_LOONGSON_GSSHFLS(op)         (MASK_OP_MAJOR(op) | (op & 0xc03f))
enum {
    OPC_GSLWLC1     = 0x4 | OPC_GSSHFL,
    OPC_GSLWRC1     = 0x5 | OPC_GSSHFL,
    OPC_GSLDLC1     = 0x6 | OPC_GSSHFL,
    OPC_GSLDRC1     = 0x7 | OPC_GSSHFL,
    OPC_GSSWLC1     = 0x4 | OPC_GSSHFS,
    OPC_GSSWRC1     = 0x5 | OPC_GSSHFS,
    OPC_GSSDLC1     = 0x6 | OPC_GSSHFS,
    OPC_GSSDRC1     = 0x7 | OPC_GSSHFS,
};

/* Loongson EXT LDC2/SDC2 opcodes */
#define MASK_LOONGSON_LSDC2(op)           (MASK_OP_MAJOR(op) | (op & 0x7))

enum {
    OPC_GSLBX      = 0x0 | OPC_LDC2,
    OPC_GSLHX      = 0x1 | OPC_LDC2,
    OPC_GSLWX      = 0x2 | OPC_LDC2,
    OPC_GSLDX      = 0x3 | OPC_LDC2,
    OPC_GSLWXC1    = 0x6 | OPC_LDC2,
    OPC_GSLDXC1    = 0x7 | OPC_LDC2,
    OPC_GSSBX      = 0x0 | OPC_SDC2,
    OPC_GSSHX      = 0x1 | OPC_SDC2,
    OPC_GSSWX      = 0x2 | OPC_SDC2,
    OPC_GSSDX      = 0x3 | OPC_SDC2,
    OPC_GSSWXC1    = 0x6 | OPC_SDC2,
    OPC_GSSDXC1    = 0x7 | OPC_SDC2,
};

static void gen_loongson_lswc2(DisasContext *ctx, int rt,
                               int rs, int rd)
{
    TCGv t0, t1, t2;
    TCGv_i32 fp0;
#if defined(TARGET_MIPS64)
    int lsq_rt1 = ctx->opcode & 0x1f;
    int lsq_offset = sextract32(ctx->opcode, 6, 9) << 4;
#endif
    int shf_offset = sextract32(ctx->opcode, 6, 8);

    t0 = tcg_temp_new();

    switch (MASK_LOONGSON_GSLSQ(ctx->opcode)) {
#if defined(TARGET_MIPS64)
    case OPC_GSLQ:
        t1 = tcg_temp_new();
        gen_base_offset_addr(ctx, t0, rs, lsq_offset);
        tcg_gen_qemu_ld_tl(t1, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        gen_base_offset_addr(ctx, t0, rs, lsq_offset + 8);
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        gen_store_gpr(t1, rt);
        gen_store_gpr(t0, lsq_rt1);
        tcg_temp_free(t1);
        break;
    case OPC_GSLQC1:
        check_cp1_enabled(ctx);
        t1 = tcg_temp_new();
        gen_base_offset_addr(ctx, t0, rs, lsq_offset);
        tcg_gen_qemu_ld_tl(t1, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        gen_base_offset_addr(ctx, t0, rs, lsq_offset + 8);
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        gen_store_fpr64(ctx, t1, rt);
        gen_store_fpr64(ctx, t0, lsq_rt1);
        tcg_temp_free(t1);
        break;
    case OPC_GSSQ:
        t1 = tcg_temp_new();
        gen_base_offset_addr(ctx, t0, rs, lsq_offset);
        gen_load_gpr(t1, rt);
        tcg_gen_qemu_st_tl(t1, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        gen_base_offset_addr(ctx, t0, rs, lsq_offset + 8);
        gen_load_gpr(t1, lsq_rt1);
        tcg_gen_qemu_st_tl(t1, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        tcg_temp_free(t1);
        break;
    case OPC_GSSQC1:
        check_cp1_enabled(ctx);
        t1 = tcg_temp_new();
        gen_base_offset_addr(ctx, t0, rs, lsq_offset);
        gen_load_fpr64(ctx, t1, rt);
        tcg_gen_qemu_st_tl(t1, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        gen_base_offset_addr(ctx, t0, rs, lsq_offset + 8);
        gen_load_fpr64(ctx, t1, lsq_rt1);
        tcg_gen_qemu_st_tl(t1, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        tcg_temp_free(t1);
        break;
#endif
    case OPC_GSSHFL:
        switch (MASK_LOONGSON_GSSHFLS(ctx->opcode)) {
        case OPC_GSLWLC1:
            check_cp1_enabled(ctx);
            gen_base_offset_addr(ctx, t0, rs, shf_offset);
            t1 = tcg_temp_new();
            tcg_gen_qemu_ld_tl(t1, t0, ctx->mem_idx, MO_UB);
            tcg_gen_andi_tl(t1, t0, 3);
#ifndef TARGET_WORDS_BIGENDIAN
            tcg_gen_xori_tl(t1, t1, 3);
#endif
            tcg_gen_shli_tl(t1, t1, 3);
            tcg_gen_andi_tl(t0, t0, ~3);
            tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TEUL);
            tcg_gen_shl_tl(t0, t0, t1);
            t2 = tcg_const_tl(-1);
            tcg_gen_shl_tl(t2, t2, t1);
            fp0 = tcg_temp_new_i32();
            gen_load_fpr32(ctx, fp0, rt);
            tcg_gen_ext_i32_tl(t1, fp0);
            tcg_gen_andc_tl(t1, t1, t2);
            tcg_temp_free(t2);
            tcg_gen_or_tl(t0, t0, t1);
            tcg_temp_free(t1);
#if defined(TARGET_MIPS64)
            tcg_gen_extrl_i64_i32(fp0, t0);
#else
            tcg_gen_ext32s_tl(fp0, t0);
#endif
            gen_store_fpr32(ctx, fp0, rt);
            tcg_temp_free_i32(fp0);
            break;
        case OPC_GSLWRC1:
            check_cp1_enabled(ctx);
            gen_base_offset_addr(ctx, t0, rs, shf_offset);
            t1 = tcg_temp_new();
            tcg_gen_qemu_ld_tl(t1, t0, ctx->mem_idx, MO_UB);
            tcg_gen_andi_tl(t1, t0, 3);
#ifdef TARGET_WORDS_BIGENDIAN
            tcg_gen_xori_tl(t1, t1, 3);
#endif
            tcg_gen_shli_tl(t1, t1, 3);
            tcg_gen_andi_tl(t0, t0, ~3);
            tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TEUL);
            tcg_gen_shr_tl(t0, t0, t1);
            tcg_gen_xori_tl(t1, t1, 31);
            t2 = tcg_const_tl(0xfffffffeull);
            tcg_gen_shl_tl(t2, t2, t1);
            fp0 = tcg_temp_new_i32();
            gen_load_fpr32(ctx, fp0, rt);
            tcg_gen_ext_i32_tl(t1, fp0);
            tcg_gen_and_tl(t1, t1, t2);
            tcg_temp_free(t2);
            tcg_gen_or_tl(t0, t0, t1);
            tcg_temp_free(t1);
#if defined(TARGET_MIPS64)
            tcg_gen_extrl_i64_i32(fp0, t0);
#else
            tcg_gen_ext32s_tl(fp0, t0);
#endif
            gen_store_fpr32(ctx, fp0, rt);
            tcg_temp_free_i32(fp0);
            break;
#if defined(TARGET_MIPS64)
        case OPC_GSLDLC1:
            check_cp1_enabled(ctx);
            gen_base_offset_addr(ctx, t0, rs, shf_offset);
            t1 = tcg_temp_new();
            tcg_gen_qemu_ld_tl(t1, t0, ctx->mem_idx, MO_UB);
            tcg_gen_andi_tl(t1, t0, 7);
#ifndef TARGET_WORDS_BIGENDIAN
            tcg_gen_xori_tl(t1, t1, 7);
#endif
            tcg_gen_shli_tl(t1, t1, 3);
            tcg_gen_andi_tl(t0, t0, ~7);
            tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TEQ);
            tcg_gen_shl_tl(t0, t0, t1);
            t2 = tcg_const_tl(-1);
            tcg_gen_shl_tl(t2, t2, t1);
            gen_load_fpr64(ctx, t1, rt);
            tcg_gen_andc_tl(t1, t1, t2);
            tcg_temp_free(t2);
            tcg_gen_or_tl(t0, t0, t1);
            tcg_temp_free(t1);
            gen_store_fpr64(ctx, t0, rt);
            break;
        case OPC_GSLDRC1:
            check_cp1_enabled(ctx);
            gen_base_offset_addr(ctx, t0, rs, shf_offset);
            t1 = tcg_temp_new();
            tcg_gen_qemu_ld_tl(t1, t0, ctx->mem_idx, MO_UB);
            tcg_gen_andi_tl(t1, t0, 7);
#ifdef TARGET_WORDS_BIGENDIAN
            tcg_gen_xori_tl(t1, t1, 7);
#endif
            tcg_gen_shli_tl(t1, t1, 3);
            tcg_gen_andi_tl(t0, t0, ~7);
            tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TEQ);
            tcg_gen_shr_tl(t0, t0, t1);
            tcg_gen_xori_tl(t1, t1, 63);
            t2 = tcg_const_tl(0xfffffffffffffffeull);
            tcg_gen_shl_tl(t2, t2, t1);
            gen_load_fpr64(ctx, t1, rt);
            tcg_gen_and_tl(t1, t1, t2);
            tcg_temp_free(t2);
            tcg_gen_or_tl(t0, t0, t1);
            tcg_temp_free(t1);
            gen_store_fpr64(ctx, t0, rt);
            break;
#endif
        default:
            MIPS_INVAL("loongson_gsshfl");
            generate_exception_end(ctx, EXCP_RI);
            break;
        }
        break;
    case OPC_GSSHFS:
        switch (MASK_LOONGSON_GSSHFLS(ctx->opcode)) {
        case OPC_GSSWLC1:
            check_cp1_enabled(ctx);
            t1 = tcg_temp_new();
            gen_base_offset_addr(ctx, t0, rs, shf_offset);
            fp0 = tcg_temp_new_i32();
            gen_load_fpr32(ctx, fp0, rt);
            tcg_gen_ext_i32_tl(t1, fp0);
            gen_helper_0e2i(swl, t1, t0, ctx->mem_idx);
            tcg_temp_free_i32(fp0);
            tcg_temp_free(t1);
            break;
        case OPC_GSSWRC1:
            check_cp1_enabled(ctx);
            t1 = tcg_temp_new();
            gen_base_offset_addr(ctx, t0, rs, shf_offset);
            fp0 = tcg_temp_new_i32();
            gen_load_fpr32(ctx, fp0, rt);
            tcg_gen_ext_i32_tl(t1, fp0);
            gen_helper_0e2i(swr, t1, t0, ctx->mem_idx);
            tcg_temp_free_i32(fp0);
            tcg_temp_free(t1);
            break;
#if defined(TARGET_MIPS64)
        case OPC_GSSDLC1:
            check_cp1_enabled(ctx);
            t1 = tcg_temp_new();
            gen_base_offset_addr(ctx, t0, rs, shf_offset);
            gen_load_fpr64(ctx, t1, rt);
            gen_helper_0e2i(sdl, t1, t0, ctx->mem_idx);
            tcg_temp_free(t1);
            break;
        case OPC_GSSDRC1:
            check_cp1_enabled(ctx);
            t1 = tcg_temp_new();
            gen_base_offset_addr(ctx, t0, rs, shf_offset);
            gen_load_fpr64(ctx, t1, rt);
            gen_helper_0e2i(sdr, t1, t0, ctx->mem_idx);
            tcg_temp_free(t1);
            break;
#endif
        default:
            MIPS_INVAL("loongson_gsshfs");
            generate_exception_end(ctx, EXCP_RI);
            break;
        }
        break;
    default:
        MIPS_INVAL("loongson_gslsq");
        generate_exception_end(ctx, EXCP_RI);
        break;
    }
    tcg_temp_free(t0);
}

/* Loongson EXT LDC2/SDC2 */
static void gen_loongson_lsdc2(DisasContext *ctx, int rt,
                               int rs, int rd)
{
    int offset = sextract32(ctx->opcode, 3, 8);
    uint32_t opc = MASK_LOONGSON_LSDC2(ctx->opcode);
    TCGv t0, t1;
    TCGv_i32 fp0;

    /* Pre-conditions */
    switch (opc) {
    case OPC_GSLBX:
    case OPC_GSLHX:
    case OPC_GSLWX:
    case OPC_GSLDX:
        /* prefetch, implement as NOP */
        if (rt == 0) {
            return;
        }
        break;
    case OPC_GSSBX:
    case OPC_GSSHX:
    case OPC_GSSWX:
    case OPC_GSSDX:
        break;
    case OPC_GSLWXC1:
#if defined(TARGET_MIPS64)
    case OPC_GSLDXC1:
#endif
        check_cp1_enabled(ctx);
        /* prefetch, implement as NOP */
        if (rt == 0) {
            return;
        }
        break;
    case OPC_GSSWXC1:
#if defined(TARGET_MIPS64)
    case OPC_GSSDXC1:
#endif
        check_cp1_enabled(ctx);
        break;
    default:
        MIPS_INVAL("loongson_lsdc2");
        generate_exception_end(ctx, EXCP_RI);
        return;
        break;
    }

    t0 = tcg_temp_new();

    gen_base_offset_addr(ctx, t0, rs, offset);
    gen_op_addr_add(ctx, t0, cpu_gpr[rd], t0);

    switch (opc) {
    case OPC_GSLBX:
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_SB);
        gen_store_gpr(t0, rt);
        break;
    case OPC_GSLHX:
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TESW |
                           ctx->default_tcg_memop_mask);
        gen_store_gpr(t0, rt);
        break;
    case OPC_GSLWX:
        gen_base_offset_addr(ctx, t0, rs, offset);
        if (rd) {
            gen_op_addr_add(ctx, t0, cpu_gpr[rd], t0);
        }
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TESL |
                           ctx->default_tcg_memop_mask);
        gen_store_gpr(t0, rt);
        break;
#if defined(TARGET_MIPS64)
    case OPC_GSLDX:
        gen_base_offset_addr(ctx, t0, rs, offset);
        if (rd) {
            gen_op_addr_add(ctx, t0, cpu_gpr[rd], t0);
        }
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        gen_store_gpr(t0, rt);
        break;
#endif
    case OPC_GSLWXC1:
        check_cp1_enabled(ctx);
        gen_base_offset_addr(ctx, t0, rs, offset);
        if (rd) {
            gen_op_addr_add(ctx, t0, cpu_gpr[rd], t0);
        }
        fp0 = tcg_temp_new_i32();
        tcg_gen_qemu_ld_i32(fp0, t0, ctx->mem_idx, MO_TESL |
                            ctx->default_tcg_memop_mask);
        gen_store_fpr32(ctx, fp0, rt);
        tcg_temp_free_i32(fp0);
        break;
#if defined(TARGET_MIPS64)
    case OPC_GSLDXC1:
        check_cp1_enabled(ctx);
        gen_base_offset_addr(ctx, t0, rs, offset);
        if (rd) {
            gen_op_addr_add(ctx, t0, cpu_gpr[rd], t0);
        }
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        gen_store_fpr64(ctx, t0, rt);
        break;
#endif
    case OPC_GSSBX:
        t1 = tcg_temp_new();
        gen_load_gpr(t1, rt);
        tcg_gen_qemu_st_tl(t1, t0, ctx->mem_idx, MO_SB);
        tcg_temp_free(t1);
        break;
    case OPC_GSSHX:
        t1 = tcg_temp_new();
        gen_load_gpr(t1, rt);
        tcg_gen_qemu_st_tl(t1, t0, ctx->mem_idx, MO_TEUW |
                           ctx->default_tcg_memop_mask);
        tcg_temp_free(t1);
        break;
    case OPC_GSSWX:
        t1 = tcg_temp_new();
        gen_load_gpr(t1, rt);
        tcg_gen_qemu_st_tl(t1, t0, ctx->mem_idx, MO_TEUL |
                           ctx->default_tcg_memop_mask);
        tcg_temp_free(t1);
        break;
#if defined(TARGET_MIPS64)
    case OPC_GSSDX:
        t1 = tcg_temp_new();
        gen_load_gpr(t1, rt);
        tcg_gen_qemu_st_tl(t1, t0, ctx->mem_idx, MO_TEQ |
                           ctx->default_tcg_memop_mask);
        tcg_temp_free(t1);
        break;
#endif
    case OPC_GSSWXC1:
        fp0 = tcg_temp_new_i32();
        gen_load_fpr32(ctx, fp0, rt);
        tcg_gen_qemu_st_i32(fp0, t0, ctx->mem_idx, MO_TEUL |
                            ctx->default_tcg_memop_mask);
        tcg_temp_free_i32(fp0);
        break;
#if defined(TARGET_MIPS64)
    case OPC_GSSDXC1:
        t1 = tcg_temp_new();
        gen_load_fpr64(ctx, t1, rt);
        tcg_gen_qemu_st_i64(t1, t0, ctx->mem_idx, MO_TEQ |
                            ctx->default_tcg_memop_mask);
        tcg_temp_free(t1);
        break;
#endif
    default:
        break;
    }

    tcg_temp_free(t0);
}
