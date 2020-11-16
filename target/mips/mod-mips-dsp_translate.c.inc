/*
 *  MIPS DSP Architecture Module translation routines
 *
 *  Copyright (c) 2004-2005 Jocelyn Mayer
 *  Copyright (c) 2006 Marius Groeger (FPU operations)
 *  Copyright (c) 2006 Thiemo Seufer (MIPS32R2 support)
 *  Copyright (c) 2009 CodeSourcery (MIPS16 and microMIPS support)
 *  Copyright (c) 2012 Jia Liu & Dongxue Zhang (MIPS ASE DSP support)
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 */

enum {
    /* MIPS DSP Load */
    OPC_LX_DSP         = 0x0A | OPC_SPECIAL3,
    /* MIPS DSP Arithmetic */
    OPC_ADDU_QB_DSP    = 0x10 | OPC_SPECIAL3,
    OPC_ADDU_OB_DSP    = 0x14 | OPC_SPECIAL3,
    OPC_ABSQ_S_PH_DSP  = 0x12 | OPC_SPECIAL3,
    OPC_ABSQ_S_QH_DSP  = 0x16 | OPC_SPECIAL3,
    /* OPC_ADDUH_QB_DSP is same as OPC_MULT_G_2E.  */
    /* OPC_ADDUH_QB_DSP   = 0x18 | OPC_SPECIAL3,  */
    OPC_CMPU_EQ_QB_DSP = 0x11 | OPC_SPECIAL3,
    OPC_CMPU_EQ_OB_DSP = 0x15 | OPC_SPECIAL3,
    /* MIPS DSP GPR-Based Shift Sub-class */
    OPC_SHLL_QB_DSP    = 0x13 | OPC_SPECIAL3,
    OPC_SHLL_OB_DSP    = 0x17 | OPC_SPECIAL3,
    /* MIPS DSP Multiply Sub-class insns */
    /* OPC_MUL_PH_DSP is same as OPC_ADDUH_QB_DSP.  */
    /* OPC_MUL_PH_DSP     = 0x18 | OPC_SPECIAL3,  */
    OPC_DPA_W_PH_DSP   = 0x30 | OPC_SPECIAL3,
    OPC_DPAQ_W_QH_DSP  = 0x34 | OPC_SPECIAL3,
    /* DSP Bit/Manipulation Sub-class */
    OPC_INSV_DSP       = 0x0C | OPC_SPECIAL3,
    OPC_DINSV_DSP      = 0x0D | OPC_SPECIAL3,
    /* MIPS DSP Append Sub-class */
    OPC_APPEND_DSP     = 0x31 | OPC_SPECIAL3,
    OPC_DAPPEND_DSP    = 0x35 | OPC_SPECIAL3,
    /* MIPS DSP Accumulator and DSPControl Access Sub-class */
    OPC_EXTR_W_DSP     = 0x38 | OPC_SPECIAL3,
    OPC_DEXTR_W_DSP    = 0x3C | OPC_SPECIAL3,
};

#define MASK_LX(op)                 (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
/* MIPS DSP Load */
enum {
    OPC_LBUX = (0x06 << 6) | OPC_LX_DSP,
    OPC_LHX  = (0x04 << 6) | OPC_LX_DSP,
    OPC_LWX  = (0x00 << 6) | OPC_LX_DSP,
    OPC_LDX = (0x08 << 6) | OPC_LX_DSP,
};

#define MASK_ADDU_QB(op)            (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Arithmetic Sub-class */
    OPC_ADDQ_PH        = (0x0A << 6) | OPC_ADDU_QB_DSP,
    OPC_ADDQ_S_PH      = (0x0E << 6) | OPC_ADDU_QB_DSP,
    OPC_ADDQ_S_W       = (0x16 << 6) | OPC_ADDU_QB_DSP,
    OPC_ADDU_QB        = (0x00 << 6) | OPC_ADDU_QB_DSP,
    OPC_ADDU_S_QB      = (0x04 << 6) | OPC_ADDU_QB_DSP,
    OPC_ADDU_PH        = (0x08 << 6) | OPC_ADDU_QB_DSP,
    OPC_ADDU_S_PH      = (0x0C << 6) | OPC_ADDU_QB_DSP,
    OPC_SUBQ_PH        = (0x0B << 6) | OPC_ADDU_QB_DSP,
    OPC_SUBQ_S_PH      = (0x0F << 6) | OPC_ADDU_QB_DSP,
    OPC_SUBQ_S_W       = (0x17 << 6) | OPC_ADDU_QB_DSP,
    OPC_SUBU_QB        = (0x01 << 6) | OPC_ADDU_QB_DSP,
    OPC_SUBU_S_QB      = (0x05 << 6) | OPC_ADDU_QB_DSP,
    OPC_SUBU_PH        = (0x09 << 6) | OPC_ADDU_QB_DSP,
    OPC_SUBU_S_PH      = (0x0D << 6) | OPC_ADDU_QB_DSP,
    OPC_ADDSC          = (0x10 << 6) | OPC_ADDU_QB_DSP,
    OPC_ADDWC          = (0x11 << 6) | OPC_ADDU_QB_DSP,
    OPC_MODSUB         = (0x12 << 6) | OPC_ADDU_QB_DSP,
    OPC_RADDU_W_QB     = (0x14 << 6) | OPC_ADDU_QB_DSP,
    /* MIPS DSP Multiply Sub-class insns */
    OPC_MULEU_S_PH_QBL = (0x06 << 6) | OPC_ADDU_QB_DSP,
    OPC_MULEU_S_PH_QBR = (0x07 << 6) | OPC_ADDU_QB_DSP,
    OPC_MULQ_RS_PH     = (0x1F << 6) | OPC_ADDU_QB_DSP,
    OPC_MULEQ_S_W_PHL  = (0x1C << 6) | OPC_ADDU_QB_DSP,
    OPC_MULEQ_S_W_PHR  = (0x1D << 6) | OPC_ADDU_QB_DSP,
    OPC_MULQ_S_PH      = (0x1E << 6) | OPC_ADDU_QB_DSP,
};

#define OPC_ADDUH_QB_DSP OPC_MULT_G_2E
#define MASK_ADDUH_QB(op)           (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Arithmetic Sub-class */
    OPC_ADDUH_QB   = (0x00 << 6) | OPC_ADDUH_QB_DSP,
    OPC_ADDUH_R_QB = (0x02 << 6) | OPC_ADDUH_QB_DSP,
    OPC_ADDQH_PH   = (0x08 << 6) | OPC_ADDUH_QB_DSP,
    OPC_ADDQH_R_PH = (0x0A << 6) | OPC_ADDUH_QB_DSP,
    OPC_ADDQH_W    = (0x10 << 6) | OPC_ADDUH_QB_DSP,
    OPC_ADDQH_R_W  = (0x12 << 6) | OPC_ADDUH_QB_DSP,
    OPC_SUBUH_QB   = (0x01 << 6) | OPC_ADDUH_QB_DSP,
    OPC_SUBUH_R_QB = (0x03 << 6) | OPC_ADDUH_QB_DSP,
    OPC_SUBQH_PH   = (0x09 << 6) | OPC_ADDUH_QB_DSP,
    OPC_SUBQH_R_PH = (0x0B << 6) | OPC_ADDUH_QB_DSP,
    OPC_SUBQH_W    = (0x11 << 6) | OPC_ADDUH_QB_DSP,
    OPC_SUBQH_R_W  = (0x13 << 6) | OPC_ADDUH_QB_DSP,
    /* MIPS DSP Multiply Sub-class insns */
    OPC_MUL_PH     = (0x0C << 6) | OPC_ADDUH_QB_DSP,
    OPC_MUL_S_PH   = (0x0E << 6) | OPC_ADDUH_QB_DSP,
    OPC_MULQ_S_W   = (0x16 << 6) | OPC_ADDUH_QB_DSP,
    OPC_MULQ_RS_W  = (0x17 << 6) | OPC_ADDUH_QB_DSP,
};

#define MASK_ABSQ_S_PH(op)          (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Arithmetic Sub-class */
    OPC_ABSQ_S_QB       = (0x01 << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_ABSQ_S_PH       = (0x09 << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_ABSQ_S_W        = (0x11 << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_PRECEQ_W_PHL    = (0x0C << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_PRECEQ_W_PHR    = (0x0D << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_PRECEQU_PH_QBL  = (0x04 << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_PRECEQU_PH_QBR  = (0x05 << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_PRECEQU_PH_QBLA = (0x06 << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_PRECEQU_PH_QBRA = (0x07 << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_PRECEU_PH_QBL   = (0x1C << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_PRECEU_PH_QBR   = (0x1D << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_PRECEU_PH_QBLA  = (0x1E << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_PRECEU_PH_QBRA  = (0x1F << 6) | OPC_ABSQ_S_PH_DSP,
    /* DSP Bit/Manipulation Sub-class */
    OPC_BITREV          = (0x1B << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_REPL_QB         = (0x02 << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_REPLV_QB        = (0x03 << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_REPL_PH         = (0x0A << 6) | OPC_ABSQ_S_PH_DSP,
    OPC_REPLV_PH        = (0x0B << 6) | OPC_ABSQ_S_PH_DSP,
};

#define MASK_CMPU_EQ_QB(op)         (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Arithmetic Sub-class */
    OPC_PRECR_QB_PH      = (0x0D << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_PRECRQ_QB_PH     = (0x0C << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_PRECR_SRA_PH_W   = (0x1E << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_PRECR_SRA_R_PH_W = (0x1F << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_PRECRQ_PH_W      = (0x14 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_PRECRQ_RS_PH_W   = (0x15 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_PRECRQU_S_QB_PH  = (0x0F << 6) | OPC_CMPU_EQ_QB_DSP,
    /* DSP Compare-Pick Sub-class */
    OPC_CMPU_EQ_QB       = (0x00 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMPU_LT_QB       = (0x01 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMPU_LE_QB       = (0x02 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMPGU_EQ_QB      = (0x04 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMPGU_LT_QB      = (0x05 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMPGU_LE_QB      = (0x06 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMPGDU_EQ_QB     = (0x18 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMPGDU_LT_QB     = (0x19 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMPGDU_LE_QB     = (0x1A << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMP_EQ_PH        = (0x08 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMP_LT_PH        = (0x09 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_CMP_LE_PH        = (0x0A << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_PICK_QB          = (0x03 << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_PICK_PH          = (0x0B << 6) | OPC_CMPU_EQ_QB_DSP,
    OPC_PACKRL_PH        = (0x0E << 6) | OPC_CMPU_EQ_QB_DSP,
};

#define MASK_SHLL_QB(op)            (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP GPR-Based Shift Sub-class */
    OPC_SHLL_QB    = (0x00 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHLLV_QB   = (0x02 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHLL_PH    = (0x08 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHLLV_PH   = (0x0A << 6) | OPC_SHLL_QB_DSP,
    OPC_SHLL_S_PH  = (0x0C << 6) | OPC_SHLL_QB_DSP,
    OPC_SHLLV_S_PH = (0x0E << 6) | OPC_SHLL_QB_DSP,
    OPC_SHLL_S_W   = (0x14 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHLLV_S_W  = (0x16 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRL_QB    = (0x01 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRLV_QB   = (0x03 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRL_PH    = (0x19 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRLV_PH   = (0x1B << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRA_QB    = (0x04 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRA_R_QB  = (0x05 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRAV_QB   = (0x06 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRAV_R_QB = (0x07 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRA_PH    = (0x09 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRAV_PH   = (0x0B << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRA_R_PH  = (0x0D << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRAV_R_PH = (0x0F << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRA_R_W   = (0x15 << 6) | OPC_SHLL_QB_DSP,
    OPC_SHRAV_R_W  = (0x17 << 6) | OPC_SHLL_QB_DSP,
};

#define MASK_DPA_W_PH(op)           (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Multiply Sub-class insns */
    OPC_DPAU_H_QBL    = (0x03 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPAU_H_QBR    = (0x07 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPSU_H_QBL    = (0x0B << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPSU_H_QBR    = (0x0F << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPA_W_PH      = (0x00 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPAX_W_PH     = (0x08 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPAQ_S_W_PH   = (0x04 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPAQX_S_W_PH  = (0x18 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPAQX_SA_W_PH = (0x1A << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPS_W_PH      = (0x01 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPSX_W_PH     = (0x09 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPSQ_S_W_PH   = (0x05 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPSQX_S_W_PH  = (0x19 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPSQX_SA_W_PH = (0x1B << 6) | OPC_DPA_W_PH_DSP,
    OPC_MULSAQ_S_W_PH = (0x06 << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPAQ_SA_L_W   = (0x0C << 6) | OPC_DPA_W_PH_DSP,
    OPC_DPSQ_SA_L_W   = (0x0D << 6) | OPC_DPA_W_PH_DSP,
    OPC_MAQ_S_W_PHL   = (0x14 << 6) | OPC_DPA_W_PH_DSP,
    OPC_MAQ_S_W_PHR   = (0x16 << 6) | OPC_DPA_W_PH_DSP,
    OPC_MAQ_SA_W_PHL  = (0x10 << 6) | OPC_DPA_W_PH_DSP,
    OPC_MAQ_SA_W_PHR  = (0x12 << 6) | OPC_DPA_W_PH_DSP,
    OPC_MULSA_W_PH    = (0x02 << 6) | OPC_DPA_W_PH_DSP,
};

#define MASK_INSV(op)               (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* DSP Bit/Manipulation Sub-class */
    OPC_INSV = (0x00 << 6) | OPC_INSV_DSP,
};

#define MASK_APPEND(op)             (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Append Sub-class */
    OPC_APPEND  = (0x00 << 6) | OPC_APPEND_DSP,
    OPC_PREPEND = (0x01 << 6) | OPC_APPEND_DSP,
    OPC_BALIGN  = (0x10 << 6) | OPC_APPEND_DSP,
};

#define MASK_EXTR_W(op)             (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Accumulator and DSPControl Access Sub-class */
    OPC_EXTR_W     = (0x00 << 6) | OPC_EXTR_W_DSP,
    OPC_EXTR_R_W   = (0x04 << 6) | OPC_EXTR_W_DSP,
    OPC_EXTR_RS_W  = (0x06 << 6) | OPC_EXTR_W_DSP,
    OPC_EXTR_S_H   = (0x0E << 6) | OPC_EXTR_W_DSP,
    OPC_EXTRV_S_H  = (0x0F << 6) | OPC_EXTR_W_DSP,
    OPC_EXTRV_W    = (0x01 << 6) | OPC_EXTR_W_DSP,
    OPC_EXTRV_R_W  = (0x05 << 6) | OPC_EXTR_W_DSP,
    OPC_EXTRV_RS_W = (0x07 << 6) | OPC_EXTR_W_DSP,
    OPC_EXTP       = (0x02 << 6) | OPC_EXTR_W_DSP,
    OPC_EXTPV      = (0x03 << 6) | OPC_EXTR_W_DSP,
    OPC_EXTPDP     = (0x0A << 6) | OPC_EXTR_W_DSP,
    OPC_EXTPDPV    = (0x0B << 6) | OPC_EXTR_W_DSP,
    OPC_SHILO      = (0x1A << 6) | OPC_EXTR_W_DSP,
    OPC_SHILOV     = (0x1B << 6) | OPC_EXTR_W_DSP,
    OPC_MTHLIP     = (0x1F << 6) | OPC_EXTR_W_DSP,
    OPC_WRDSP      = (0x13 << 6) | OPC_EXTR_W_DSP,
    OPC_RDDSP      = (0x12 << 6) | OPC_EXTR_W_DSP,
};

#define MASK_ABSQ_S_QH(op)          (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Arithmetic Sub-class */
    OPC_PRECEQ_L_PWL    = (0x14 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEQ_L_PWR    = (0x15 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEQ_PW_QHL   = (0x0C << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEQ_PW_QHR   = (0x0D << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEQ_PW_QHLA  = (0x0E << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEQ_PW_QHRA  = (0x0F << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEQU_QH_OBL  = (0x04 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEQU_QH_OBR  = (0x05 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEQU_QH_OBLA = (0x06 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEQU_QH_OBRA = (0x07 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEU_QH_OBL   = (0x1C << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEU_QH_OBR   = (0x1D << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEU_QH_OBLA  = (0x1E << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_PRECEU_QH_OBRA  = (0x1F << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_ABSQ_S_OB       = (0x01 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_ABSQ_S_PW       = (0x11 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_ABSQ_S_QH       = (0x09 << 6) | OPC_ABSQ_S_QH_DSP,
    /* DSP Bit/Manipulation Sub-class */
    OPC_REPL_OB         = (0x02 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_REPL_PW         = (0x12 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_REPL_QH         = (0x0A << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_REPLV_OB        = (0x03 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_REPLV_PW        = (0x13 << 6) | OPC_ABSQ_S_QH_DSP,
    OPC_REPLV_QH        = (0x0B << 6) | OPC_ABSQ_S_QH_DSP,
};

#define MASK_ADDU_OB(op)            (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Multiply Sub-class insns */
    OPC_MULEQ_S_PW_QHL = (0x1C << 6) | OPC_ADDU_OB_DSP,
    OPC_MULEQ_S_PW_QHR = (0x1D << 6) | OPC_ADDU_OB_DSP,
    OPC_MULEU_S_QH_OBL = (0x06 << 6) | OPC_ADDU_OB_DSP,
    OPC_MULEU_S_QH_OBR = (0x07 << 6) | OPC_ADDU_OB_DSP,
    OPC_MULQ_RS_QH     = (0x1F << 6) | OPC_ADDU_OB_DSP,
    /* MIPS DSP Arithmetic Sub-class */
    OPC_RADDU_L_OB     = (0x14 << 6) | OPC_ADDU_OB_DSP,
    OPC_SUBQ_PW        = (0x13 << 6) | OPC_ADDU_OB_DSP,
    OPC_SUBQ_S_PW      = (0x17 << 6) | OPC_ADDU_OB_DSP,
    OPC_SUBQ_QH        = (0x0B << 6) | OPC_ADDU_OB_DSP,
    OPC_SUBQ_S_QH      = (0x0F << 6) | OPC_ADDU_OB_DSP,
    OPC_SUBU_OB        = (0x01 << 6) | OPC_ADDU_OB_DSP,
    OPC_SUBU_S_OB      = (0x05 << 6) | OPC_ADDU_OB_DSP,
    OPC_SUBU_QH        = (0x09 << 6) | OPC_ADDU_OB_DSP,
    OPC_SUBU_S_QH      = (0x0D << 6) | OPC_ADDU_OB_DSP,
    OPC_SUBUH_OB       = (0x19 << 6) | OPC_ADDU_OB_DSP,
    OPC_SUBUH_R_OB     = (0x1B << 6) | OPC_ADDU_OB_DSP,
    OPC_ADDQ_PW        = (0x12 << 6) | OPC_ADDU_OB_DSP,
    OPC_ADDQ_S_PW      = (0x16 << 6) | OPC_ADDU_OB_DSP,
    OPC_ADDQ_QH        = (0x0A << 6) | OPC_ADDU_OB_DSP,
    OPC_ADDQ_S_QH      = (0x0E << 6) | OPC_ADDU_OB_DSP,
    OPC_ADDU_OB        = (0x00 << 6) | OPC_ADDU_OB_DSP,
    OPC_ADDU_S_OB      = (0x04 << 6) | OPC_ADDU_OB_DSP,
    OPC_ADDU_QH        = (0x08 << 6) | OPC_ADDU_OB_DSP,
    OPC_ADDU_S_QH      = (0x0C << 6) | OPC_ADDU_OB_DSP,
    OPC_ADDUH_OB       = (0x18 << 6) | OPC_ADDU_OB_DSP,
    OPC_ADDUH_R_OB     = (0x1A << 6) | OPC_ADDU_OB_DSP,
};

#define MASK_CMPU_EQ_OB(op)         (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* DSP Compare-Pick Sub-class */
    OPC_CMP_EQ_PW         = (0x10 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMP_LT_PW         = (0x11 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMP_LE_PW         = (0x12 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMP_EQ_QH         = (0x08 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMP_LT_QH         = (0x09 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMP_LE_QH         = (0x0A << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMPGDU_EQ_OB      = (0x18 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMPGDU_LT_OB      = (0x19 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMPGDU_LE_OB      = (0x1A << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMPGU_EQ_OB       = (0x04 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMPGU_LT_OB       = (0x05 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMPGU_LE_OB       = (0x06 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMPU_EQ_OB        = (0x00 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMPU_LT_OB        = (0x01 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_CMPU_LE_OB        = (0x02 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PACKRL_PW         = (0x0E << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PICK_OB           = (0x03 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PICK_PW           = (0x13 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PICK_QH           = (0x0B << 6) | OPC_CMPU_EQ_OB_DSP,
    /* MIPS DSP Arithmetic Sub-class */
    OPC_PRECR_OB_QH       = (0x0D << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PRECR_SRA_QH_PW   = (0x1E << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PRECR_SRA_R_QH_PW = (0x1F << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PRECRQ_OB_QH      = (0x0C << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PRECRQ_PW_L       = (0x1C << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PRECRQ_QH_PW      = (0x14 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PRECRQ_RS_QH_PW   = (0x15 << 6) | OPC_CMPU_EQ_OB_DSP,
    OPC_PRECRQU_S_OB_QH   = (0x0F << 6) | OPC_CMPU_EQ_OB_DSP,
};

#define MASK_DAPPEND(op)            (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* DSP Append Sub-class */
    OPC_DAPPEND  = (0x00 << 6) | OPC_DAPPEND_DSP,
    OPC_PREPENDD = (0x03 << 6) | OPC_DAPPEND_DSP,
    OPC_PREPENDW = (0x01 << 6) | OPC_DAPPEND_DSP,
    OPC_DBALIGN  = (0x10 << 6) | OPC_DAPPEND_DSP,
};

#define MASK_DEXTR_W(op)            (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Accumulator and DSPControl Access Sub-class */
    OPC_DMTHLIP     = (0x1F << 6) | OPC_DEXTR_W_DSP,
    OPC_DSHILO      = (0x1A << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTP       = (0x02 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTPDP     = (0x0A << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTPDPV    = (0x0B << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTPV      = (0x03 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTR_L     = (0x10 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTR_R_L   = (0x14 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTR_RS_L  = (0x16 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTR_W     = (0x00 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTR_R_W   = (0x04 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTR_RS_W  = (0x06 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTR_S_H   = (0x0E << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTRV_L    = (0x11 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTRV_R_L  = (0x15 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTRV_RS_L = (0x17 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTRV_S_H  = (0x0F << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTRV_W    = (0x01 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTRV_R_W  = (0x05 << 6) | OPC_DEXTR_W_DSP,
    OPC_DEXTRV_RS_W = (0x07 << 6) | OPC_DEXTR_W_DSP,
    OPC_DSHILOV     = (0x1B << 6) | OPC_DEXTR_W_DSP,
};

#define MASK_DINSV(op)              (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* DSP Bit/Manipulation Sub-class */
    OPC_DINSV = (0x00 << 6) | OPC_DINSV_DSP,
};

#define MASK_DPAQ_W_QH(op)          (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP Multiply Sub-class insns */
    OPC_DMADD         = (0x19 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DMADDU        = (0x1D << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DMSUB         = (0x1B << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DMSUBU        = (0x1F << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DPA_W_QH      = (0x00 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DPAQ_S_W_QH   = (0x04 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DPAQ_SA_L_PW  = (0x0C << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DPAU_H_OBL    = (0x03 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DPAU_H_OBR    = (0x07 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DPS_W_QH      = (0x01 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DPSQ_S_W_QH   = (0x05 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DPSQ_SA_L_PW  = (0x0D << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DPSU_H_OBL    = (0x0B << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_DPSU_H_OBR    = (0x0F << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MAQ_S_L_PWL   = (0x1C << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MAQ_S_L_PWR   = (0x1E << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MAQ_S_W_QHLL  = (0x14 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MAQ_SA_W_QHLL = (0x10 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MAQ_S_W_QHLR  = (0x15 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MAQ_SA_W_QHLR = (0x11 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MAQ_S_W_QHRL  = (0x16 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MAQ_SA_W_QHRL = (0x12 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MAQ_S_W_QHRR  = (0x17 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MAQ_SA_W_QHRR = (0x13 << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MULSAQ_S_L_PW = (0x0E << 6) | OPC_DPAQ_W_QH_DSP,
    OPC_MULSAQ_S_W_QH = (0x06 << 6) | OPC_DPAQ_W_QH_DSP,
};

#define MASK_SHLL_OB(op)            (MASK_SPECIAL3(op) | (op & (0x1F << 6)))
enum {
    /* MIPS DSP GPR-Based Shift Sub-class */
    OPC_SHLL_PW    = (0x10 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHLL_S_PW  = (0x14 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHLLV_OB   = (0x02 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHLLV_PW   = (0x12 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHLLV_S_PW = (0x16 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHLLV_QH   = (0x0A << 6) | OPC_SHLL_OB_DSP,
    OPC_SHLLV_S_QH = (0x0E << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRA_PW    = (0x11 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRA_R_PW  = (0x15 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRAV_OB   = (0x06 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRAV_R_OB = (0x07 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRAV_PW   = (0x13 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRAV_R_PW = (0x17 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRAV_QH   = (0x0B << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRAV_R_QH = (0x0F << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRLV_OB   = (0x03 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRLV_QH   = (0x1B << 6) | OPC_SHLL_OB_DSP,
    OPC_SHLL_OB    = (0x00 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHLL_QH    = (0x08 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHLL_S_QH  = (0x0C << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRA_OB    = (0x04 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRA_R_OB  = (0x05 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRA_QH    = (0x09 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRA_R_QH  = (0x0D << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRL_OB    = (0x01 << 6) | OPC_SHLL_OB_DSP,
    OPC_SHRL_QH    = (0x19 << 6) | OPC_SHLL_OB_DSP,
};

/*
 * Verify that the processor is running with DSP instructions enabled.
 * This is enabled by CP0 Status register MX(24) bit.
 */
static inline void check_dsp(DisasContext *ctx)
{
    if (unlikely(!(ctx->hflags & MIPS_HFLAG_DSP))) {
        if (ctx->insn_flags & ASE_DSP) {
            generate_exception_end(ctx, EXCP_DSPDIS);
        } else {
            generate_exception_end(ctx, EXCP_RI);
        }
    }
}

static inline void check_dsp_r2(DisasContext *ctx)
{
    if (unlikely(!(ctx->hflags & MIPS_HFLAG_DSP_R2))) {
        if (ctx->insn_flags & ASE_DSP) {
            generate_exception_end(ctx, EXCP_DSPDIS);
        } else {
            generate_exception_end(ctx, EXCP_RI);
        }
    }
}

static inline void check_dsp_r3(DisasContext *ctx)
{
    if (unlikely(!(ctx->hflags & MIPS_HFLAG_DSP_R3))) {
        if (ctx->insn_flags & ASE_DSP) {
            generate_exception_end(ctx, EXCP_DSPDIS);
        } else {
            generate_exception_end(ctx, EXCP_RI);
        }
    }
}

static void gen_mipsdsp_ld(DisasContext *ctx, uint32_t opc,
                           int rd, int base, int offset)
{
    TCGv t0;

    check_dsp(ctx);
    t0 = tcg_temp_new();

    if (base == 0) {
        gen_load_gpr(t0, offset);
    } else if (offset == 0) {
        gen_load_gpr(t0, base);
    } else {
        gen_op_addr_add(ctx, t0, cpu_gpr[base], cpu_gpr[offset]);
    }

    switch (opc) {
    case OPC_LBUX:
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_UB);
        gen_store_gpr(t0, rd);
        break;
    case OPC_LHX:
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TESW);
        gen_store_gpr(t0, rd);
        break;
    case OPC_LWX:
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TESL);
        gen_store_gpr(t0, rd);
        break;
#if defined(TARGET_MIPS64)
    case OPC_LDX:
        tcg_gen_qemu_ld_tl(t0, t0, ctx->mem_idx, MO_TEQ);
        gen_store_gpr(t0, rd);
        break;
#endif
    }
    tcg_temp_free(t0);
}

static void gen_mipsdsp_arith(DisasContext *ctx, uint32_t op1, uint32_t op2,
                              int ret, int v1, int v2)
{
    TCGv v1_t;
    TCGv v2_t;

    if (ret == 0) {
        /* Treat as NOP. */
        return;
    }

    v1_t = tcg_temp_new();
    v2_t = tcg_temp_new();

    gen_load_gpr(v1_t, v1);
    gen_load_gpr(v2_t, v2);

    switch (op1) {
    /* OPC_MULT_G_2E is equal OPC_ADDUH_QB_DSP */
    case OPC_MULT_G_2E:
        check_dsp_r2(ctx);
        switch (op2) {
        case OPC_ADDUH_QB:
            gen_helper_adduh_qb(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_ADDUH_R_QB:
            gen_helper_adduh_r_qb(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_ADDQH_PH:
            gen_helper_addqh_ph(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_ADDQH_R_PH:
            gen_helper_addqh_r_ph(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_ADDQH_W:
            gen_helper_addqh_w(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_ADDQH_R_W:
            gen_helper_addqh_r_w(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_SUBUH_QB:
            gen_helper_subuh_qb(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_SUBUH_R_QB:
            gen_helper_subuh_r_qb(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_SUBQH_PH:
            gen_helper_subqh_ph(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_SUBQH_R_PH:
            gen_helper_subqh_r_ph(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_SUBQH_W:
            gen_helper_subqh_w(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_SUBQH_R_W:
            gen_helper_subqh_r_w(cpu_gpr[ret], v1_t, v2_t);
            break;
        }
        break;
    case OPC_ABSQ_S_PH_DSP:
        switch (op2) {
        case OPC_ABSQ_S_QB:
            check_dsp_r2(ctx);
            gen_helper_absq_s_qb(cpu_gpr[ret], v2_t, cpu_env);
            break;
        case OPC_ABSQ_S_PH:
            check_dsp(ctx);
            gen_helper_absq_s_ph(cpu_gpr[ret], v2_t, cpu_env);
            break;
        case OPC_ABSQ_S_W:
            check_dsp(ctx);
            gen_helper_absq_s_w(cpu_gpr[ret], v2_t, cpu_env);
            break;
        case OPC_PRECEQ_W_PHL:
            check_dsp(ctx);
            tcg_gen_andi_tl(cpu_gpr[ret], v2_t, 0xFFFF0000);
            tcg_gen_ext32s_tl(cpu_gpr[ret], cpu_gpr[ret]);
            break;
        case OPC_PRECEQ_W_PHR:
            check_dsp(ctx);
            tcg_gen_andi_tl(cpu_gpr[ret], v2_t, 0x0000FFFF);
            tcg_gen_shli_tl(cpu_gpr[ret], cpu_gpr[ret], 16);
            tcg_gen_ext32s_tl(cpu_gpr[ret], cpu_gpr[ret]);
            break;
        case OPC_PRECEQU_PH_QBL:
            check_dsp(ctx);
            gen_helper_precequ_ph_qbl(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEQU_PH_QBR:
            check_dsp(ctx);
            gen_helper_precequ_ph_qbr(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEQU_PH_QBLA:
            check_dsp(ctx);
            gen_helper_precequ_ph_qbla(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEQU_PH_QBRA:
            check_dsp(ctx);
            gen_helper_precequ_ph_qbra(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEU_PH_QBL:
            check_dsp(ctx);
            gen_helper_preceu_ph_qbl(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEU_PH_QBR:
            check_dsp(ctx);
            gen_helper_preceu_ph_qbr(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEU_PH_QBLA:
            check_dsp(ctx);
            gen_helper_preceu_ph_qbla(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEU_PH_QBRA:
            check_dsp(ctx);
            gen_helper_preceu_ph_qbra(cpu_gpr[ret], v2_t);
            break;
        }
        break;
    case OPC_ADDU_QB_DSP:
        switch (op2) {
        case OPC_ADDQ_PH:
            check_dsp(ctx);
            gen_helper_addq_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDQ_S_PH:
            check_dsp(ctx);
            gen_helper_addq_s_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDQ_S_W:
            check_dsp(ctx);
            gen_helper_addq_s_w(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDU_QB:
            check_dsp(ctx);
            gen_helper_addu_qb(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDU_S_QB:
            check_dsp(ctx);
            gen_helper_addu_s_qb(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDU_PH:
            check_dsp_r2(ctx);
            gen_helper_addu_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDU_S_PH:
            check_dsp_r2(ctx);
            gen_helper_addu_s_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBQ_PH:
            check_dsp(ctx);
            gen_helper_subq_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBQ_S_PH:
            check_dsp(ctx);
            gen_helper_subq_s_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBQ_S_W:
            check_dsp(ctx);
            gen_helper_subq_s_w(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBU_QB:
            check_dsp(ctx);
            gen_helper_subu_qb(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBU_S_QB:
            check_dsp(ctx);
            gen_helper_subu_s_qb(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBU_PH:
            check_dsp_r2(ctx);
            gen_helper_subu_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBU_S_PH:
            check_dsp_r2(ctx);
            gen_helper_subu_s_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDSC:
            check_dsp(ctx);
            gen_helper_addsc(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDWC:
            check_dsp(ctx);
            gen_helper_addwc(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MODSUB:
            check_dsp(ctx);
            gen_helper_modsub(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_RADDU_W_QB:
            check_dsp(ctx);
            gen_helper_raddu_w_qb(cpu_gpr[ret], v1_t);
            break;
        }
        break;
    case OPC_CMPU_EQ_QB_DSP:
        switch (op2) {
        case OPC_PRECR_QB_PH:
            check_dsp_r2(ctx);
            gen_helper_precr_qb_ph(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_PRECRQ_QB_PH:
            check_dsp(ctx);
            gen_helper_precrq_qb_ph(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_PRECR_SRA_PH_W:
            check_dsp_r2(ctx);
            {
                TCGv_i32 sa_t = tcg_const_i32(v2);
                gen_helper_precr_sra_ph_w(cpu_gpr[ret], sa_t, v1_t,
                                          cpu_gpr[ret]);
                tcg_temp_free_i32(sa_t);
                break;
            }
        case OPC_PRECR_SRA_R_PH_W:
            check_dsp_r2(ctx);
            {
                TCGv_i32 sa_t = tcg_const_i32(v2);
                gen_helper_precr_sra_r_ph_w(cpu_gpr[ret], sa_t, v1_t,
                                            cpu_gpr[ret]);
                tcg_temp_free_i32(sa_t);
                break;
            }
        case OPC_PRECRQ_PH_W:
            check_dsp(ctx);
            gen_helper_precrq_ph_w(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_PRECRQ_RS_PH_W:
            check_dsp(ctx);
            gen_helper_precrq_rs_ph_w(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_PRECRQU_S_QB_PH:
            check_dsp(ctx);
            gen_helper_precrqu_s_qb_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        }
        break;
#ifdef TARGET_MIPS64
    case OPC_ABSQ_S_QH_DSP:
        switch (op2) {
        case OPC_PRECEQ_L_PWL:
            check_dsp(ctx);
            tcg_gen_andi_tl(cpu_gpr[ret], v2_t, 0xFFFFFFFF00000000ull);
            break;
        case OPC_PRECEQ_L_PWR:
            check_dsp(ctx);
            tcg_gen_shli_tl(cpu_gpr[ret], v2_t, 32);
            break;
        case OPC_PRECEQ_PW_QHL:
            check_dsp(ctx);
            gen_helper_preceq_pw_qhl(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEQ_PW_QHR:
            check_dsp(ctx);
            gen_helper_preceq_pw_qhr(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEQ_PW_QHLA:
            check_dsp(ctx);
            gen_helper_preceq_pw_qhla(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEQ_PW_QHRA:
            check_dsp(ctx);
            gen_helper_preceq_pw_qhra(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEQU_QH_OBL:
            check_dsp(ctx);
            gen_helper_precequ_qh_obl(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEQU_QH_OBR:
            check_dsp(ctx);
            gen_helper_precequ_qh_obr(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEQU_QH_OBLA:
            check_dsp(ctx);
            gen_helper_precequ_qh_obla(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEQU_QH_OBRA:
            check_dsp(ctx);
            gen_helper_precequ_qh_obra(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEU_QH_OBL:
            check_dsp(ctx);
            gen_helper_preceu_qh_obl(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEU_QH_OBR:
            check_dsp(ctx);
            gen_helper_preceu_qh_obr(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEU_QH_OBLA:
            check_dsp(ctx);
            gen_helper_preceu_qh_obla(cpu_gpr[ret], v2_t);
            break;
        case OPC_PRECEU_QH_OBRA:
            check_dsp(ctx);
            gen_helper_preceu_qh_obra(cpu_gpr[ret], v2_t);
            break;
        case OPC_ABSQ_S_OB:
            check_dsp_r2(ctx);
            gen_helper_absq_s_ob(cpu_gpr[ret], v2_t, cpu_env);
            break;
        case OPC_ABSQ_S_PW:
            check_dsp(ctx);
            gen_helper_absq_s_pw(cpu_gpr[ret], v2_t, cpu_env);
            break;
        case OPC_ABSQ_S_QH:
            check_dsp(ctx);
            gen_helper_absq_s_qh(cpu_gpr[ret], v2_t, cpu_env);
            break;
        }
        break;
    case OPC_ADDU_OB_DSP:
        switch (op2) {
        case OPC_RADDU_L_OB:
            check_dsp(ctx);
            gen_helper_raddu_l_ob(cpu_gpr[ret], v1_t);
            break;
        case OPC_SUBQ_PW:
            check_dsp(ctx);
            gen_helper_subq_pw(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBQ_S_PW:
            check_dsp(ctx);
            gen_helper_subq_s_pw(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBQ_QH:
            check_dsp(ctx);
            gen_helper_subq_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBQ_S_QH:
            check_dsp(ctx);
            gen_helper_subq_s_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBU_OB:
            check_dsp(ctx);
            gen_helper_subu_ob(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBU_S_OB:
            check_dsp(ctx);
            gen_helper_subu_s_ob(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBU_QH:
            check_dsp_r2(ctx);
            gen_helper_subu_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBU_S_QH:
            check_dsp_r2(ctx);
            gen_helper_subu_s_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_SUBUH_OB:
            check_dsp_r2(ctx);
            gen_helper_subuh_ob(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_SUBUH_R_OB:
            check_dsp_r2(ctx);
            gen_helper_subuh_r_ob(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_ADDQ_PW:
            check_dsp(ctx);
            gen_helper_addq_pw(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDQ_S_PW:
            check_dsp(ctx);
            gen_helper_addq_s_pw(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDQ_QH:
            check_dsp(ctx);
            gen_helper_addq_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDQ_S_QH:
            check_dsp(ctx);
            gen_helper_addq_s_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDU_OB:
            check_dsp(ctx);
            gen_helper_addu_ob(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDU_S_OB:
            check_dsp(ctx);
            gen_helper_addu_s_ob(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDU_QH:
            check_dsp_r2(ctx);
            gen_helper_addu_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDU_S_QH:
            check_dsp_r2(ctx);
            gen_helper_addu_s_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_ADDUH_OB:
            check_dsp_r2(ctx);
            gen_helper_adduh_ob(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_ADDUH_R_OB:
            check_dsp_r2(ctx);
            gen_helper_adduh_r_ob(cpu_gpr[ret], v1_t, v2_t);
            break;
        }
        break;
    case OPC_CMPU_EQ_OB_DSP:
        switch (op2) {
        case OPC_PRECR_OB_QH:
            check_dsp_r2(ctx);
            gen_helper_precr_ob_qh(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_PRECR_SRA_QH_PW:
            check_dsp_r2(ctx);
            {
                TCGv_i32 ret_t = tcg_const_i32(ret);
                gen_helper_precr_sra_qh_pw(v2_t, v1_t, v2_t, ret_t);
                tcg_temp_free_i32(ret_t);
                break;
            }
        case OPC_PRECR_SRA_R_QH_PW:
            check_dsp_r2(ctx);
            {
                TCGv_i32 sa_v = tcg_const_i32(ret);
                gen_helper_precr_sra_r_qh_pw(v2_t, v1_t, v2_t, sa_v);
                tcg_temp_free_i32(sa_v);
                break;
            }
        case OPC_PRECRQ_OB_QH:
            check_dsp(ctx);
            gen_helper_precrq_ob_qh(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_PRECRQ_PW_L:
            check_dsp(ctx);
            gen_helper_precrq_pw_l(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_PRECRQ_QH_PW:
            check_dsp(ctx);
            gen_helper_precrq_qh_pw(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_PRECRQ_RS_QH_PW:
            check_dsp(ctx);
            gen_helper_precrq_rs_qh_pw(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_PRECRQU_S_OB_QH:
            check_dsp(ctx);
            gen_helper_precrqu_s_ob_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        }
        break;
#endif
    }

    tcg_temp_free(v1_t);
    tcg_temp_free(v2_t);
}

static void gen_mipsdsp_shift(DisasContext *ctx, uint32_t opc,
                              int ret, int v1, int v2)
{
    uint32_t op2;
    TCGv t0;
    TCGv v1_t;
    TCGv v2_t;

    if (ret == 0) {
        /* Treat as NOP. */
        return;
    }

    t0 = tcg_temp_new();
    v1_t = tcg_temp_new();
    v2_t = tcg_temp_new();

    tcg_gen_movi_tl(t0, v1);
    gen_load_gpr(v1_t, v1);
    gen_load_gpr(v2_t, v2);

    switch (opc) {
    case OPC_SHLL_QB_DSP:
        {
            op2 = MASK_SHLL_QB(ctx->opcode);
            switch (op2) {
            case OPC_SHLL_QB:
                check_dsp(ctx);
                gen_helper_shll_qb(cpu_gpr[ret], t0, v2_t, cpu_env);
                break;
            case OPC_SHLLV_QB:
                check_dsp(ctx);
                gen_helper_shll_qb(cpu_gpr[ret], v1_t, v2_t, cpu_env);
                break;
            case OPC_SHLL_PH:
                check_dsp(ctx);
                gen_helper_shll_ph(cpu_gpr[ret], t0, v2_t, cpu_env);
                break;
            case OPC_SHLLV_PH:
                check_dsp(ctx);
                gen_helper_shll_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
                break;
            case OPC_SHLL_S_PH:
                check_dsp(ctx);
                gen_helper_shll_s_ph(cpu_gpr[ret], t0, v2_t, cpu_env);
                break;
            case OPC_SHLLV_S_PH:
                check_dsp(ctx);
                gen_helper_shll_s_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
                break;
            case OPC_SHLL_S_W:
                check_dsp(ctx);
                gen_helper_shll_s_w(cpu_gpr[ret], t0, v2_t, cpu_env);
                break;
            case OPC_SHLLV_S_W:
                check_dsp(ctx);
                gen_helper_shll_s_w(cpu_gpr[ret], v1_t, v2_t, cpu_env);
                break;
            case OPC_SHRL_QB:
                check_dsp(ctx);
                gen_helper_shrl_qb(cpu_gpr[ret], t0, v2_t);
                break;
            case OPC_SHRLV_QB:
                check_dsp(ctx);
                gen_helper_shrl_qb(cpu_gpr[ret], v1_t, v2_t);
                break;
            case OPC_SHRL_PH:
                check_dsp_r2(ctx);
                gen_helper_shrl_ph(cpu_gpr[ret], t0, v2_t);
                break;
            case OPC_SHRLV_PH:
                check_dsp_r2(ctx);
                gen_helper_shrl_ph(cpu_gpr[ret], v1_t, v2_t);
                break;
            case OPC_SHRA_QB:
                check_dsp_r2(ctx);
                gen_helper_shra_qb(cpu_gpr[ret], t0, v2_t);
                break;
            case OPC_SHRA_R_QB:
                check_dsp_r2(ctx);
                gen_helper_shra_r_qb(cpu_gpr[ret], t0, v2_t);
                break;
            case OPC_SHRAV_QB:
                check_dsp_r2(ctx);
                gen_helper_shra_qb(cpu_gpr[ret], v1_t, v2_t);
                break;
            case OPC_SHRAV_R_QB:
                check_dsp_r2(ctx);
                gen_helper_shra_r_qb(cpu_gpr[ret], v1_t, v2_t);
                break;
            case OPC_SHRA_PH:
                check_dsp(ctx);
                gen_helper_shra_ph(cpu_gpr[ret], t0, v2_t);
                break;
            case OPC_SHRA_R_PH:
                check_dsp(ctx);
                gen_helper_shra_r_ph(cpu_gpr[ret], t0, v2_t);
                break;
            case OPC_SHRAV_PH:
                check_dsp(ctx);
                gen_helper_shra_ph(cpu_gpr[ret], v1_t, v2_t);
                break;
            case OPC_SHRAV_R_PH:
                check_dsp(ctx);
                gen_helper_shra_r_ph(cpu_gpr[ret], v1_t, v2_t);
                break;
            case OPC_SHRA_R_W:
                check_dsp(ctx);
                gen_helper_shra_r_w(cpu_gpr[ret], t0, v2_t);
                break;
            case OPC_SHRAV_R_W:
                check_dsp(ctx);
                gen_helper_shra_r_w(cpu_gpr[ret], v1_t, v2_t);
                break;
            default:            /* Invalid */
                MIPS_INVAL("MASK SHLL.QB");
                generate_exception_end(ctx, EXCP_RI);
                break;
            }
            break;
        }
#ifdef TARGET_MIPS64
    case OPC_SHLL_OB_DSP:
        op2 = MASK_SHLL_OB(ctx->opcode);
        switch (op2) {
        case OPC_SHLL_PW:
            check_dsp(ctx);
            gen_helper_shll_pw(cpu_gpr[ret], v2_t, t0, cpu_env);
            break;
        case OPC_SHLLV_PW:
            check_dsp(ctx);
            gen_helper_shll_pw(cpu_gpr[ret], v2_t, v1_t, cpu_env);
            break;
        case OPC_SHLL_S_PW:
            check_dsp(ctx);
            gen_helper_shll_s_pw(cpu_gpr[ret], v2_t, t0, cpu_env);
            break;
        case OPC_SHLLV_S_PW:
            check_dsp(ctx);
            gen_helper_shll_s_pw(cpu_gpr[ret], v2_t, v1_t, cpu_env);
            break;
        case OPC_SHLL_OB:
            check_dsp(ctx);
            gen_helper_shll_ob(cpu_gpr[ret], v2_t, t0, cpu_env);
            break;
        case OPC_SHLLV_OB:
            check_dsp(ctx);
            gen_helper_shll_ob(cpu_gpr[ret], v2_t, v1_t, cpu_env);
            break;
        case OPC_SHLL_QH:
            check_dsp(ctx);
            gen_helper_shll_qh(cpu_gpr[ret], v2_t, t0, cpu_env);
            break;
        case OPC_SHLLV_QH:
            check_dsp(ctx);
            gen_helper_shll_qh(cpu_gpr[ret], v2_t, v1_t, cpu_env);
            break;
        case OPC_SHLL_S_QH:
            check_dsp(ctx);
            gen_helper_shll_s_qh(cpu_gpr[ret], v2_t, t0, cpu_env);
            break;
        case OPC_SHLLV_S_QH:
            check_dsp(ctx);
            gen_helper_shll_s_qh(cpu_gpr[ret], v2_t, v1_t, cpu_env);
            break;
        case OPC_SHRA_OB:
            check_dsp_r2(ctx);
            gen_helper_shra_ob(cpu_gpr[ret], v2_t, t0);
            break;
        case OPC_SHRAV_OB:
            check_dsp_r2(ctx);
            gen_helper_shra_ob(cpu_gpr[ret], v2_t, v1_t);
            break;
        case OPC_SHRA_R_OB:
            check_dsp_r2(ctx);
            gen_helper_shra_r_ob(cpu_gpr[ret], v2_t, t0);
            break;
        case OPC_SHRAV_R_OB:
            check_dsp_r2(ctx);
            gen_helper_shra_r_ob(cpu_gpr[ret], v2_t, v1_t);
            break;
        case OPC_SHRA_PW:
            check_dsp(ctx);
            gen_helper_shra_pw(cpu_gpr[ret], v2_t, t0);
            break;
        case OPC_SHRAV_PW:
            check_dsp(ctx);
            gen_helper_shra_pw(cpu_gpr[ret], v2_t, v1_t);
            break;
        case OPC_SHRA_R_PW:
            check_dsp(ctx);
            gen_helper_shra_r_pw(cpu_gpr[ret], v2_t, t0);
            break;
        case OPC_SHRAV_R_PW:
            check_dsp(ctx);
            gen_helper_shra_r_pw(cpu_gpr[ret], v2_t, v1_t);
            break;
        case OPC_SHRA_QH:
            check_dsp(ctx);
            gen_helper_shra_qh(cpu_gpr[ret], v2_t, t0);
            break;
        case OPC_SHRAV_QH:
            check_dsp(ctx);
            gen_helper_shra_qh(cpu_gpr[ret], v2_t, v1_t);
            break;
        case OPC_SHRA_R_QH:
            check_dsp(ctx);
            gen_helper_shra_r_qh(cpu_gpr[ret], v2_t, t0);
            break;
        case OPC_SHRAV_R_QH:
            check_dsp(ctx);
            gen_helper_shra_r_qh(cpu_gpr[ret], v2_t, v1_t);
            break;
        case OPC_SHRL_OB:
            check_dsp(ctx);
            gen_helper_shrl_ob(cpu_gpr[ret], v2_t, t0);
            break;
        case OPC_SHRLV_OB:
            check_dsp(ctx);
            gen_helper_shrl_ob(cpu_gpr[ret], v2_t, v1_t);
            break;
        case OPC_SHRL_QH:
            check_dsp_r2(ctx);
            gen_helper_shrl_qh(cpu_gpr[ret], v2_t, t0);
            break;
        case OPC_SHRLV_QH:
            check_dsp_r2(ctx);
            gen_helper_shrl_qh(cpu_gpr[ret], v2_t, v1_t);
            break;
        default:            /* Invalid */
            MIPS_INVAL("MASK SHLL.OB");
            generate_exception_end(ctx, EXCP_RI);
            break;
        }
        break;
#endif
    }

    tcg_temp_free(t0);
    tcg_temp_free(v1_t);
    tcg_temp_free(v2_t);
}

static void gen_mipsdsp_multiply(DisasContext *ctx, uint32_t op1, uint32_t op2,
                                 int ret, int v1, int v2, int check_ret)
{
    TCGv_i32 t0;
    TCGv v1_t;
    TCGv v2_t;

    if ((ret == 0) && (check_ret == 1)) {
        /* Treat as NOP. */
        return;
    }

    t0 = tcg_temp_new_i32();
    v1_t = tcg_temp_new();
    v2_t = tcg_temp_new();

    tcg_gen_movi_i32(t0, ret);
    gen_load_gpr(v1_t, v1);
    gen_load_gpr(v2_t, v2);

    switch (op1) {
    /*
     * OPC_MULT_G_2E, OPC_ADDUH_QB_DSP, OPC_MUL_PH_DSP have
     * the same mask and op1.
     */
    case OPC_MULT_G_2E:
        check_dsp_r2(ctx);
        switch (op2) {
        case  OPC_MUL_PH:
            gen_helper_mul_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case  OPC_MUL_S_PH:
            gen_helper_mul_s_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULQ_S_W:
            gen_helper_mulq_s_w(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULQ_RS_W:
            gen_helper_mulq_rs_w(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        }
        break;
    case OPC_DPA_W_PH_DSP:
        switch (op2) {
        case OPC_DPAU_H_QBL:
            check_dsp(ctx);
            gen_helper_dpau_h_qbl(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPAU_H_QBR:
            check_dsp(ctx);
            gen_helper_dpau_h_qbr(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPSU_H_QBL:
            check_dsp(ctx);
            gen_helper_dpsu_h_qbl(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPSU_H_QBR:
            check_dsp(ctx);
            gen_helper_dpsu_h_qbr(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPA_W_PH:
            check_dsp_r2(ctx);
            gen_helper_dpa_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPAX_W_PH:
            check_dsp_r2(ctx);
            gen_helper_dpax_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPAQ_S_W_PH:
            check_dsp(ctx);
            gen_helper_dpaq_s_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPAQX_S_W_PH:
            check_dsp_r2(ctx);
            gen_helper_dpaqx_s_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPAQX_SA_W_PH:
            check_dsp_r2(ctx);
            gen_helper_dpaqx_sa_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPS_W_PH:
            check_dsp_r2(ctx);
            gen_helper_dps_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPSX_W_PH:
            check_dsp_r2(ctx);
            gen_helper_dpsx_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPSQ_S_W_PH:
            check_dsp(ctx);
            gen_helper_dpsq_s_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPSQX_S_W_PH:
            check_dsp_r2(ctx);
            gen_helper_dpsqx_s_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPSQX_SA_W_PH:
            check_dsp_r2(ctx);
            gen_helper_dpsqx_sa_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_MULSAQ_S_W_PH:
            check_dsp(ctx);
            gen_helper_mulsaq_s_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPAQ_SA_L_W:
            check_dsp(ctx);
            gen_helper_dpaq_sa_l_w(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_DPSQ_SA_L_W:
            check_dsp(ctx);
            gen_helper_dpsq_sa_l_w(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_MAQ_S_W_PHL:
            check_dsp(ctx);
            gen_helper_maq_s_w_phl(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_MAQ_S_W_PHR:
            check_dsp(ctx);
            gen_helper_maq_s_w_phr(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_MAQ_SA_W_PHL:
            check_dsp(ctx);
            gen_helper_maq_sa_w_phl(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_MAQ_SA_W_PHR:
            check_dsp(ctx);
            gen_helper_maq_sa_w_phr(t0, v1_t, v2_t, cpu_env);
            break;
        case OPC_MULSA_W_PH:
            check_dsp_r2(ctx);
            gen_helper_mulsa_w_ph(t0, v1_t, v2_t, cpu_env);
            break;
        }
        break;
#ifdef TARGET_MIPS64
    case OPC_DPAQ_W_QH_DSP:
        {
            int ac = ret & 0x03;
            tcg_gen_movi_i32(t0, ac);

            switch (op2) {
            case OPC_DMADD:
                check_dsp(ctx);
                gen_helper_dmadd(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DMADDU:
                check_dsp(ctx);
                gen_helper_dmaddu(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DMSUB:
                check_dsp(ctx);
                gen_helper_dmsub(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DMSUBU:
                check_dsp(ctx);
                gen_helper_dmsubu(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DPA_W_QH:
                check_dsp_r2(ctx);
                gen_helper_dpa_w_qh(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DPAQ_S_W_QH:
                check_dsp(ctx);
                gen_helper_dpaq_s_w_qh(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DPAQ_SA_L_PW:
                check_dsp(ctx);
                gen_helper_dpaq_sa_l_pw(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DPAU_H_OBL:
                check_dsp(ctx);
                gen_helper_dpau_h_obl(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DPAU_H_OBR:
                check_dsp(ctx);
                gen_helper_dpau_h_obr(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DPS_W_QH:
                check_dsp_r2(ctx);
                gen_helper_dps_w_qh(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DPSQ_S_W_QH:
                check_dsp(ctx);
                gen_helper_dpsq_s_w_qh(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DPSQ_SA_L_PW:
                check_dsp(ctx);
                gen_helper_dpsq_sa_l_pw(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DPSU_H_OBL:
                check_dsp(ctx);
                gen_helper_dpsu_h_obl(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_DPSU_H_OBR:
                check_dsp(ctx);
                gen_helper_dpsu_h_obr(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MAQ_S_L_PWL:
                check_dsp(ctx);
                gen_helper_maq_s_l_pwl(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MAQ_S_L_PWR:
                check_dsp(ctx);
                gen_helper_maq_s_l_pwr(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MAQ_S_W_QHLL:
                check_dsp(ctx);
                gen_helper_maq_s_w_qhll(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MAQ_SA_W_QHLL:
                check_dsp(ctx);
                gen_helper_maq_sa_w_qhll(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MAQ_S_W_QHLR:
                check_dsp(ctx);
                gen_helper_maq_s_w_qhlr(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MAQ_SA_W_QHLR:
                check_dsp(ctx);
                gen_helper_maq_sa_w_qhlr(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MAQ_S_W_QHRL:
                check_dsp(ctx);
                gen_helper_maq_s_w_qhrl(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MAQ_SA_W_QHRL:
                check_dsp(ctx);
                gen_helper_maq_sa_w_qhrl(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MAQ_S_W_QHRR:
                check_dsp(ctx);
                gen_helper_maq_s_w_qhrr(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MAQ_SA_W_QHRR:
                check_dsp(ctx);
                gen_helper_maq_sa_w_qhrr(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MULSAQ_S_L_PW:
                check_dsp(ctx);
                gen_helper_mulsaq_s_l_pw(v1_t, v2_t, t0, cpu_env);
                break;
            case OPC_MULSAQ_S_W_QH:
                check_dsp(ctx);
                gen_helper_mulsaq_s_w_qh(v1_t, v2_t, t0, cpu_env);
                break;
            }
        }
        break;
#endif
    case OPC_ADDU_QB_DSP:
        switch (op2) {
        case OPC_MULEU_S_PH_QBL:
            check_dsp(ctx);
            gen_helper_muleu_s_ph_qbl(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULEU_S_PH_QBR:
            check_dsp(ctx);
            gen_helper_muleu_s_ph_qbr(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULQ_RS_PH:
            check_dsp(ctx);
            gen_helper_mulq_rs_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULEQ_S_W_PHL:
            check_dsp(ctx);
            gen_helper_muleq_s_w_phl(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULEQ_S_W_PHR:
            check_dsp(ctx);
            gen_helper_muleq_s_w_phr(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULQ_S_PH:
            check_dsp_r2(ctx);
            gen_helper_mulq_s_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        }
        break;
#ifdef TARGET_MIPS64
    case OPC_ADDU_OB_DSP:
        switch (op2) {
        case OPC_MULEQ_S_PW_QHL:
            check_dsp(ctx);
            gen_helper_muleq_s_pw_qhl(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULEQ_S_PW_QHR:
            check_dsp(ctx);
            gen_helper_muleq_s_pw_qhr(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULEU_S_QH_OBL:
            check_dsp(ctx);
            gen_helper_muleu_s_qh_obl(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULEU_S_QH_OBR:
            check_dsp(ctx);
            gen_helper_muleu_s_qh_obr(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_MULQ_RS_QH:
            check_dsp(ctx);
            gen_helper_mulq_rs_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        }
        break;
#endif
    }

    tcg_temp_free_i32(t0);
    tcg_temp_free(v1_t);
    tcg_temp_free(v2_t);
}

static void gen_mipsdsp_bitinsn(DisasContext *ctx, uint32_t op1, uint32_t op2,
                                int ret, int val)
{
    int16_t imm;
    TCGv t0;
    TCGv val_t;

    if (ret == 0) {
        /* Treat as NOP. */
        return;
    }

    t0 = tcg_temp_new();
    val_t = tcg_temp_new();
    gen_load_gpr(val_t, val);

    switch (op1) {
    case OPC_ABSQ_S_PH_DSP:
        switch (op2) {
        case OPC_BITREV:
            check_dsp(ctx);
            gen_helper_bitrev(cpu_gpr[ret], val_t);
            break;
        case OPC_REPL_QB:
            check_dsp(ctx);
            {
                target_long result;
                imm = (ctx->opcode >> 16) & 0xFF;
                result = (uint32_t)imm << 24 |
                         (uint32_t)imm << 16 |
                         (uint32_t)imm << 8  |
                         (uint32_t)imm;
                result = (int32_t)result;
                tcg_gen_movi_tl(cpu_gpr[ret], result);
            }
            break;
        case OPC_REPLV_QB:
            check_dsp(ctx);
            tcg_gen_ext8u_tl(cpu_gpr[ret], val_t);
            tcg_gen_shli_tl(t0, cpu_gpr[ret], 8);
            tcg_gen_or_tl(cpu_gpr[ret], cpu_gpr[ret], t0);
            tcg_gen_shli_tl(t0, cpu_gpr[ret], 16);
            tcg_gen_or_tl(cpu_gpr[ret], cpu_gpr[ret], t0);
            tcg_gen_ext32s_tl(cpu_gpr[ret], cpu_gpr[ret]);
            break;
        case OPC_REPL_PH:
            check_dsp(ctx);
            {
                imm = (ctx->opcode >> 16) & 0x03FF;
                imm = (int16_t)(imm << 6) >> 6;
                tcg_gen_movi_tl(cpu_gpr[ret], \
                                (target_long)((int32_t)imm << 16 | \
                                (uint16_t)imm));
            }
            break;
        case OPC_REPLV_PH:
            check_dsp(ctx);
            tcg_gen_ext16u_tl(cpu_gpr[ret], val_t);
            tcg_gen_shli_tl(t0, cpu_gpr[ret], 16);
            tcg_gen_or_tl(cpu_gpr[ret], cpu_gpr[ret], t0);
            tcg_gen_ext32s_tl(cpu_gpr[ret], cpu_gpr[ret]);
            break;
        }
        break;
#ifdef TARGET_MIPS64
    case OPC_ABSQ_S_QH_DSP:
        switch (op2) {
        case OPC_REPL_OB:
            check_dsp(ctx);
            {
                target_long temp;

                imm = (ctx->opcode >> 16) & 0xFF;
                temp = ((uint64_t)imm << 8) | (uint64_t)imm;
                temp = (temp << 16) | temp;
                temp = (temp << 32) | temp;
                tcg_gen_movi_tl(cpu_gpr[ret], temp);
                break;
            }
        case OPC_REPL_PW:
            check_dsp(ctx);
            {
                target_long temp;

                imm = (ctx->opcode >> 16) & 0x03FF;
                imm = (int16_t)(imm << 6) >> 6;
                temp = ((target_long)imm << 32) \
                       | ((target_long)imm & 0xFFFFFFFF);
                tcg_gen_movi_tl(cpu_gpr[ret], temp);
                break;
            }
        case OPC_REPL_QH:
            check_dsp(ctx);
            {
                target_long temp;

                imm = (ctx->opcode >> 16) & 0x03FF;
                imm = (int16_t)(imm << 6) >> 6;

                temp = ((uint64_t)(uint16_t)imm << 48) |
                       ((uint64_t)(uint16_t)imm << 32) |
                       ((uint64_t)(uint16_t)imm << 16) |
                       (uint64_t)(uint16_t)imm;
                tcg_gen_movi_tl(cpu_gpr[ret], temp);
                break;
            }
        case OPC_REPLV_OB:
            check_dsp(ctx);
            tcg_gen_ext8u_tl(cpu_gpr[ret], val_t);
            tcg_gen_shli_tl(t0, cpu_gpr[ret], 8);
            tcg_gen_or_tl(cpu_gpr[ret], cpu_gpr[ret], t0);
            tcg_gen_shli_tl(t0, cpu_gpr[ret], 16);
            tcg_gen_or_tl(cpu_gpr[ret], cpu_gpr[ret], t0);
            tcg_gen_shli_tl(t0, cpu_gpr[ret], 32);
            tcg_gen_or_tl(cpu_gpr[ret], cpu_gpr[ret], t0);
            break;
        case OPC_REPLV_PW:
            check_dsp(ctx);
            tcg_gen_ext32u_i64(cpu_gpr[ret], val_t);
            tcg_gen_shli_tl(t0, cpu_gpr[ret], 32);
            tcg_gen_or_tl(cpu_gpr[ret], cpu_gpr[ret], t0);
            break;
        case OPC_REPLV_QH:
            check_dsp(ctx);
            tcg_gen_ext16u_tl(cpu_gpr[ret], val_t);
            tcg_gen_shli_tl(t0, cpu_gpr[ret], 16);
            tcg_gen_or_tl(cpu_gpr[ret], cpu_gpr[ret], t0);
            tcg_gen_shli_tl(t0, cpu_gpr[ret], 32);
            tcg_gen_or_tl(cpu_gpr[ret], cpu_gpr[ret], t0);
            break;
        }
        break;
#endif
    }
    tcg_temp_free(t0);
    tcg_temp_free(val_t);
}

static void gen_mipsdsp_add_cmp_pick(DisasContext *ctx,
                                     uint32_t op1, uint32_t op2,
                                     int ret, int v1, int v2, int check_ret)
{
    TCGv t1;
    TCGv v1_t;
    TCGv v2_t;

    if ((ret == 0) && (check_ret == 1)) {
        /* Treat as NOP. */
        return;
    }

    t1 = tcg_temp_new();
    v1_t = tcg_temp_new();
    v2_t = tcg_temp_new();

    gen_load_gpr(v1_t, v1);
    gen_load_gpr(v2_t, v2);

    switch (op1) {
    case OPC_CMPU_EQ_QB_DSP:
        switch (op2) {
        case OPC_CMPU_EQ_QB:
            check_dsp(ctx);
            gen_helper_cmpu_eq_qb(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMPU_LT_QB:
            check_dsp(ctx);
            gen_helper_cmpu_lt_qb(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMPU_LE_QB:
            check_dsp(ctx);
            gen_helper_cmpu_le_qb(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMPGU_EQ_QB:
            check_dsp(ctx);
            gen_helper_cmpgu_eq_qb(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_CMPGU_LT_QB:
            check_dsp(ctx);
            gen_helper_cmpgu_lt_qb(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_CMPGU_LE_QB:
            check_dsp(ctx);
            gen_helper_cmpgu_le_qb(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_CMPGDU_EQ_QB:
            check_dsp_r2(ctx);
            gen_helper_cmpgu_eq_qb(t1, v1_t, v2_t);
            tcg_gen_mov_tl(cpu_gpr[ret], t1);
            tcg_gen_andi_tl(cpu_dspctrl, cpu_dspctrl, 0xF0FFFFFF);
            tcg_gen_shli_tl(t1, t1, 24);
            tcg_gen_or_tl(cpu_dspctrl, cpu_dspctrl, t1);
            break;
        case OPC_CMPGDU_LT_QB:
            check_dsp_r2(ctx);
            gen_helper_cmpgu_lt_qb(t1, v1_t, v2_t);
            tcg_gen_mov_tl(cpu_gpr[ret], t1);
            tcg_gen_andi_tl(cpu_dspctrl, cpu_dspctrl, 0xF0FFFFFF);
            tcg_gen_shli_tl(t1, t1, 24);
            tcg_gen_or_tl(cpu_dspctrl, cpu_dspctrl, t1);
            break;
        case OPC_CMPGDU_LE_QB:
            check_dsp_r2(ctx);
            gen_helper_cmpgu_le_qb(t1, v1_t, v2_t);
            tcg_gen_mov_tl(cpu_gpr[ret], t1);
            tcg_gen_andi_tl(cpu_dspctrl, cpu_dspctrl, 0xF0FFFFFF);
            tcg_gen_shli_tl(t1, t1, 24);
            tcg_gen_or_tl(cpu_dspctrl, cpu_dspctrl, t1);
            break;
        case OPC_CMP_EQ_PH:
            check_dsp(ctx);
            gen_helper_cmp_eq_ph(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMP_LT_PH:
            check_dsp(ctx);
            gen_helper_cmp_lt_ph(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMP_LE_PH:
            check_dsp(ctx);
            gen_helper_cmp_le_ph(v1_t, v2_t, cpu_env);
            break;
        case OPC_PICK_QB:
            check_dsp(ctx);
            gen_helper_pick_qb(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_PICK_PH:
            check_dsp(ctx);
            gen_helper_pick_ph(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_PACKRL_PH:
            check_dsp(ctx);
            gen_helper_packrl_ph(cpu_gpr[ret], v1_t, v2_t);
            break;
        }
        break;
#ifdef TARGET_MIPS64
    case OPC_CMPU_EQ_OB_DSP:
        switch (op2) {
        case OPC_CMP_EQ_PW:
            check_dsp(ctx);
            gen_helper_cmp_eq_pw(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMP_LT_PW:
            check_dsp(ctx);
            gen_helper_cmp_lt_pw(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMP_LE_PW:
            check_dsp(ctx);
            gen_helper_cmp_le_pw(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMP_EQ_QH:
            check_dsp(ctx);
            gen_helper_cmp_eq_qh(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMP_LT_QH:
            check_dsp(ctx);
            gen_helper_cmp_lt_qh(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMP_LE_QH:
            check_dsp(ctx);
            gen_helper_cmp_le_qh(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMPGDU_EQ_OB:
            check_dsp_r2(ctx);
            gen_helper_cmpgdu_eq_ob(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_CMPGDU_LT_OB:
            check_dsp_r2(ctx);
            gen_helper_cmpgdu_lt_ob(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_CMPGDU_LE_OB:
            check_dsp_r2(ctx);
            gen_helper_cmpgdu_le_ob(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_CMPGU_EQ_OB:
            check_dsp(ctx);
            gen_helper_cmpgu_eq_ob(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_CMPGU_LT_OB:
            check_dsp(ctx);
            gen_helper_cmpgu_lt_ob(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_CMPGU_LE_OB:
            check_dsp(ctx);
            gen_helper_cmpgu_le_ob(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_CMPU_EQ_OB:
            check_dsp(ctx);
            gen_helper_cmpu_eq_ob(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMPU_LT_OB:
            check_dsp(ctx);
            gen_helper_cmpu_lt_ob(v1_t, v2_t, cpu_env);
            break;
        case OPC_CMPU_LE_OB:
            check_dsp(ctx);
            gen_helper_cmpu_le_ob(v1_t, v2_t, cpu_env);
            break;
        case OPC_PACKRL_PW:
            check_dsp(ctx);
            gen_helper_packrl_pw(cpu_gpr[ret], v1_t, v2_t);
            break;
        case OPC_PICK_OB:
            check_dsp(ctx);
            gen_helper_pick_ob(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_PICK_PW:
            check_dsp(ctx);
            gen_helper_pick_pw(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        case OPC_PICK_QH:
            check_dsp(ctx);
            gen_helper_pick_qh(cpu_gpr[ret], v1_t, v2_t, cpu_env);
            break;
        }
        break;
#endif
    }

    tcg_temp_free(t1);
    tcg_temp_free(v1_t);
    tcg_temp_free(v2_t);
}

static void gen_mipsdsp_append(CPUMIPSState *env, DisasContext *ctx,
                               uint32_t op1, int rt, int rs, int sa)
{
    TCGv t0;

    check_dsp_r2(ctx);

    if (rt == 0) {
        /* Treat as NOP. */
        return;
    }

    t0 = tcg_temp_new();
    gen_load_gpr(t0, rs);

    switch (op1) {
    case OPC_APPEND_DSP:
        switch (MASK_APPEND(ctx->opcode)) {
        case OPC_APPEND:
            if (sa != 0) {
                tcg_gen_deposit_tl(cpu_gpr[rt], t0, cpu_gpr[rt], sa, 32 - sa);
            }
            tcg_gen_ext32s_tl(cpu_gpr[rt], cpu_gpr[rt]);
            break;
        case OPC_PREPEND:
            if (sa != 0) {
                tcg_gen_ext32u_tl(cpu_gpr[rt], cpu_gpr[rt]);
                tcg_gen_shri_tl(cpu_gpr[rt], cpu_gpr[rt], sa);
                tcg_gen_shli_tl(t0, t0, 32 - sa);
                tcg_gen_or_tl(cpu_gpr[rt], cpu_gpr[rt], t0);
            }
            tcg_gen_ext32s_tl(cpu_gpr[rt], cpu_gpr[rt]);
            break;
        case OPC_BALIGN:
            sa &= 3;
            if (sa != 0 && sa != 2) {
                tcg_gen_shli_tl(cpu_gpr[rt], cpu_gpr[rt], 8 * sa);
                tcg_gen_ext32u_tl(t0, t0);
                tcg_gen_shri_tl(t0, t0, 8 * (4 - sa));
                tcg_gen_or_tl(cpu_gpr[rt], cpu_gpr[rt], t0);
            }
            tcg_gen_ext32s_tl(cpu_gpr[rt], cpu_gpr[rt]);
            break;
        default:            /* Invalid */
            MIPS_INVAL("MASK APPEND");
            generate_exception_end(ctx, EXCP_RI);
            break;
        }
        break;
#ifdef TARGET_MIPS64
    case OPC_DAPPEND_DSP:
        switch (MASK_DAPPEND(ctx->opcode)) {
        case OPC_DAPPEND:
            if (sa != 0) {
                tcg_gen_deposit_tl(cpu_gpr[rt], t0, cpu_gpr[rt], sa, 64 - sa);
            }
            break;
        case OPC_PREPENDD:
            tcg_gen_shri_tl(cpu_gpr[rt], cpu_gpr[rt], 0x20 | sa);
            tcg_gen_shli_tl(t0, t0, 64 - (0x20 | sa));
            tcg_gen_or_tl(cpu_gpr[rt], t0, t0);
            break;
        case OPC_PREPENDW:
            if (sa != 0) {
                tcg_gen_shri_tl(cpu_gpr[rt], cpu_gpr[rt], sa);
                tcg_gen_shli_tl(t0, t0, 64 - sa);
                tcg_gen_or_tl(cpu_gpr[rt], cpu_gpr[rt], t0);
            }
            break;
        case OPC_DBALIGN:
            sa &= 7;
            if (sa != 0 && sa != 2 && sa != 4) {
                tcg_gen_shli_tl(cpu_gpr[rt], cpu_gpr[rt], 8 * sa);
                tcg_gen_shri_tl(t0, t0, 8 * (8 - sa));
                tcg_gen_or_tl(cpu_gpr[rt], cpu_gpr[rt], t0);
            }
            break;
        default:            /* Invalid */
            MIPS_INVAL("MASK DAPPEND");
            generate_exception_end(ctx, EXCP_RI);
            break;
        }
        break;
#endif
    }
    tcg_temp_free(t0);
}

static void gen_mipsdsp_accinsn(DisasContext *ctx, uint32_t op1, uint32_t op2,
                                int ret, int v1, int v2, int check_ret)

{
    TCGv t0;
    TCGv t1;
    TCGv v1_t;
    TCGv v2_t;
    int16_t imm;

    if ((ret == 0) && (check_ret == 1)) {
        /* Treat as NOP. */
        return;
    }

    t0 = tcg_temp_new();
    t1 = tcg_temp_new();
    v1_t = tcg_temp_new();
    v2_t = tcg_temp_new();

    gen_load_gpr(v1_t, v1);
    gen_load_gpr(v2_t, v2);

    switch (op1) {
    case OPC_EXTR_W_DSP:
        check_dsp(ctx);
        switch (op2) {
        case OPC_EXTR_W:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_extr_w(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_EXTR_R_W:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_extr_r_w(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_EXTR_RS_W:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_extr_rs_w(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_EXTR_S_H:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_extr_s_h(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_EXTRV_S_H:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_extr_s_h(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_EXTRV_W:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_extr_w(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_EXTRV_R_W:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_extr_r_w(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_EXTRV_RS_W:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_extr_rs_w(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_EXTP:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_extp(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_EXTPV:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_extp(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_EXTPDP:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_extpdp(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_EXTPDPV:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_extpdp(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_SHILO:
            imm = (ctx->opcode >> 20) & 0x3F;
            tcg_gen_movi_tl(t0, ret);
            tcg_gen_movi_tl(t1, imm);
            gen_helper_shilo(t0, t1, cpu_env);
            break;
        case OPC_SHILOV:
            tcg_gen_movi_tl(t0, ret);
            gen_helper_shilo(t0, v1_t, cpu_env);
            break;
        case OPC_MTHLIP:
            tcg_gen_movi_tl(t0, ret);
            gen_helper_mthlip(t0, v1_t, cpu_env);
            break;
        case OPC_WRDSP:
            imm = (ctx->opcode >> 11) & 0x3FF;
            tcg_gen_movi_tl(t0, imm);
            gen_helper_wrdsp(v1_t, t0, cpu_env);
            break;
        case OPC_RDDSP:
            imm = (ctx->opcode >> 16) & 0x03FF;
            tcg_gen_movi_tl(t0, imm);
            gen_helper_rddsp(cpu_gpr[ret], t0, cpu_env);
            break;
        }
        break;
#ifdef TARGET_MIPS64
    case OPC_DEXTR_W_DSP:
        check_dsp(ctx);
        switch (op2) {
        case OPC_DMTHLIP:
            tcg_gen_movi_tl(t0, ret);
            gen_helper_dmthlip(v1_t, t0, cpu_env);
            break;
        case OPC_DSHILO:
            {
                int shift = (ctx->opcode >> 19) & 0x7F;
                int ac = (ctx->opcode >> 11) & 0x03;
                tcg_gen_movi_tl(t0, shift);
                tcg_gen_movi_tl(t1, ac);
                gen_helper_dshilo(t0, t1, cpu_env);
                break;
            }
        case OPC_DSHILOV:
            {
                int ac = (ctx->opcode >> 11) & 0x03;
                tcg_gen_movi_tl(t0, ac);
                gen_helper_dshilo(v1_t, t0, cpu_env);
                break;
            }
        case OPC_DEXTP:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);

            gen_helper_dextp(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_DEXTPV:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_dextp(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_DEXTPDP:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_dextpdp(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_DEXTPDPV:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_dextpdp(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_DEXTR_L:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_dextr_l(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_DEXTR_R_L:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_dextr_r_l(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_DEXTR_RS_L:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_dextr_rs_l(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_DEXTR_W:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_dextr_w(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_DEXTR_R_W:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_dextr_r_w(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_DEXTR_RS_W:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_dextr_rs_w(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_DEXTR_S_H:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_dextr_s_h(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_DEXTRV_S_H:
            tcg_gen_movi_tl(t0, v2);
            tcg_gen_movi_tl(t1, v1);
            gen_helper_dextr_s_h(cpu_gpr[ret], t0, t1, cpu_env);
            break;
        case OPC_DEXTRV_L:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_dextr_l(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_DEXTRV_R_L:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_dextr_r_l(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_DEXTRV_RS_L:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_dextr_rs_l(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_DEXTRV_W:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_dextr_w(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_DEXTRV_R_W:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_dextr_r_w(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        case OPC_DEXTRV_RS_W:
            tcg_gen_movi_tl(t0, v2);
            gen_helper_dextr_rs_w(cpu_gpr[ret], t0, v1_t, cpu_env);
            break;
        }
        break;
#endif
    }

    tcg_temp_free(t0);
    tcg_temp_free(t1);
    tcg_temp_free(v1_t);
    tcg_temp_free(v2_t);
}
