/*
 *  MIPS NEC Vr54xx translation routines.
 *
 *  Copyright (c) 2004-2005 Jocelyn Mayer
 *  Copyright (c) 2006 Marius Groeger (FPU operations)
 *  Copyright (c) 2006 Thiemo Seufer (MIPS32R2 support)
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 */

/* Multiplication variants of the vr54xx. */
#define MASK_MUL_VR54XX(op)         (MASK_SPECIAL(op) | (op & (0x1F << 6)))

enum {
    OPC_VR54XX_MULS    = (0x03 << 6) | OPC_MULT,
    OPC_VR54XX_MULSU   = (0x03 << 6) | OPC_MULTU,
    OPC_VR54XX_MACC    = (0x05 << 6) | OPC_MULT,
    OPC_VR54XX_MACCU   = (0x05 << 6) | OPC_MULTU,
    OPC_VR54XX_MSAC    = (0x07 << 6) | OPC_MULT,
    OPC_VR54XX_MSACU   = (0x07 << 6) | OPC_MULTU,
    OPC_VR54XX_MULHI   = (0x09 << 6) | OPC_MULT,
    OPC_VR54XX_MULHIU  = (0x09 << 6) | OPC_MULTU,
    OPC_VR54XX_MULSHI  = (0x0B << 6) | OPC_MULT,
    OPC_VR54XX_MULSHIU = (0x0B << 6) | OPC_MULTU,
    OPC_VR54XX_MACCHI  = (0x0D << 6) | OPC_MULT,
    OPC_VR54XX_MACCHIU = (0x0D << 6) | OPC_MULTU,
    OPC_VR54XX_MSACHI  = (0x0F << 6) | OPC_MULT,
    OPC_VR54XX_MSACHIU = (0x0F << 6) | OPC_MULTU,
};

static void gen_mul_vr54xx(DisasContext *ctx, uint32_t opc,
                           int rd, int rs, int rt)
{
    TCGv t0 = tcg_temp_new();
    TCGv t1 = tcg_temp_new();

    gen_load_gpr(t0, rs);
    gen_load_gpr(t1, rt);

    switch (opc) {
    case OPC_VR54XX_MULS:
        gen_helper_muls(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MULSU:
        gen_helper_mulsu(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MACC:
        gen_helper_macc(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MACCU:
        gen_helper_maccu(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MSAC:
        gen_helper_msac(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MSACU:
        gen_helper_msacu(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MULHI:
        gen_helper_mulhi(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MULHIU:
        gen_helper_mulhiu(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MULSHI:
        gen_helper_mulshi(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MULSHIU:
        gen_helper_mulshiu(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MACCHI:
        gen_helper_macchi(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MACCHIU:
        gen_helper_macchiu(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MSACHI:
        gen_helper_msachi(t0, cpu_env, t0, t1);
        break;
    case OPC_VR54XX_MSACHIU:
        gen_helper_msachiu(t0, cpu_env, t0, t1);
        break;
    default:
        MIPS_INVAL("mul vr54xx");
        generate_exception_end(ctx, EXCP_RI);
        goto out;
    }
    gen_store_gpr(t0, rd);

 out:
    tcg_temp_free(t0);
    tcg_temp_free(t1);
}
