/*
 *  Toshiba TX MultiMedia Instruction translation routines.
 *
 *  Copyright (c) 2004-2005 Jocelyn Mayer
 *  Copyright (c) 2006 Marius Groeger (FPU operations)
 *  Copyright (c) 2006 Thiemo Seufer (MIPS32R2 support)
 *  Copyright (c) 2009 CodeSourcery (MIPS16 and microMIPS support)
 *  Copyright (c) 2012 Jia Liu & Dongxue Zhang (MIPS ASE DSP support)
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 *
 * The R5900 and the C790 have 128-bit wide GPRs, where the upper 64 bits
 * are only used by the specific quadword (128-bit) LQ/SQ load/store
 * instructions and certain multimedia instructions (MMIs). These MMIs
 * configure the 128-bit data path as two 64-bit, four 32-bit, eight 16-bit
 * or sixteen 8-bit paths.
 *
 * Reference:
 *
 * The Toshiba TX System RISC TX79 Core Architecture manual,
 * https://wiki.qemu.org/File:C790.pdf
 *
 *     Arithmetic (19 instructions)
 *     ----------------------------
 * PADDB   rd, rs, rt        Parallel Add Byte
 * PSUBB   rd, rs, rt        Parallel Subtract Byte
 * PADDH   rd, rs, rt        Parallel Add Halfword
 * PSUBH   rd, rs, rt        Parallel Subtract Halfword
 * PADDW   rd, rs, rt        Parallel Add Word
 * PSUBW   rd, rs, rt        Parallel Subtract Word
 * PADSBH  rd, rs, rt        Parallel Add/Subtract Halfword
 * PADDSB  rd, rs, rt        Parallel Add with Signed Saturation Byte
 * PSUBSB  rd, rs, rt        Parallel Subtract with Signed Saturation Byte
 * PADDSH  rd, rs, rt        Parallel Add with Signed Saturation Halfword
 * PSUBSH  rd, rs, rt        Parallel Subtract with Signed Saturation Halfword
 * PADDSW  rd, rs, rt        Parallel Add with Signed Saturation Word
 * PSUBSW  rd, rs, rt        Parallel Subtract with Signed Saturation Word
 * PADDUB  rd, rs, rt        Parallel Add with Unsigned saturation Byte
 * PSUBUB  rd, rs, rt        Parallel Subtract with Unsigned saturation Byte
 * PADDUH  rd, rs, rt        Parallel Add with Unsigned saturation Halfword
 * PSUBUH  rd, rs, rt        Parallel Subtract with Unsigned saturation Halfword
 * PADDUW  rd, rs, rt        Parallel Add with Unsigned saturation Word
 * PSUBUW  rd, rs, rt        Parallel Subtract with Unsigned saturation Word
 *
 *     Min/Max (4 instructions)
 *     ------------------------
 * PMAXH   rd, rs, rt        Parallel Maximum Halfword
 * PMINH   rd, rs, rt        Parallel Minimum Halfword
 * PMAXW   rd, rs, rt        Parallel Maximum Word
 * PMINW   rd, rs, rt        Parallel Minimum Word
 *
 *     Absolute (2 instructions)
 *     -------------------------
 * PABSH   rd, rt            Parallel Absolute Halfword
 * PABSW   rd, rt            Parallel Absolute Word
 *
 *     Logical (4 instructions)
 *     ------------------------
 * PAND    rd, rs, rt        Parallel AND
 * POR     rd, rs, rt        Parallel OR
 * PXOR    rd, rs, rt        Parallel XOR
 * PNOR    rd, rs, rt        Parallel NOR
 *
 *     Shift (9 instructions)
 *     ----------------------
 * PSLLH   rd, rt, sa        Parallel Shift Left Logical Halfword
 * PSRLH   rd, rt, sa        Parallel Shift Right Logical Halfword
 * PSRAH   rd, rt, sa        Parallel Shift Right Arithmetic Halfword
 * PSLLW   rd, rt, sa        Parallel Shift Left Logical Word
 * PSRLW   rd, rt, sa        Parallel Shift Right Logical Word
 * PSRAW   rd, rt, sa        Parallel Shift Right Arithmetic Word
 * PSLLVW  rd, rt, rs        Parallel Shift Left Logical Variable Word
 * PSRLVW  rd, rt, rs        Parallel Shift Right Logical Variable Word
 * PSRAVW  rd, rt, rs        Parallel Shift Right Arithmetic Variable Word
 *
 *     Compare (6 instructions)
 *     ------------------------
 * PCGTB   rd, rs, rt        Parallel Compare for Greater Than Byte
 * PCEQB   rd, rs, rt        Parallel Compare for Equal Byte
 * PCGTH   rd, rs, rt        Parallel Compare for Greater Than Halfword
 * PCEQH   rd, rs, rt        Parallel Compare for Equal Halfword
 * PCGTW   rd, rs, rt        Parallel Compare for Greater Than Word
 * PCEQW   rd, rs, rt        Parallel Compare for Equal Word
 *
 *     LZC (1 instruction)
 *     -------------------
 * PLZCW   rd, rs            Parallel Leading Zero or One Count Word
 *
 *     Quadword Load and Store (2 instructions)
 *     ----------------------------------------
 * LQ      rt, offset(base)  Load Quadword
 * SQ      rt, offset(base)  Store Quadword
 *
 *     Multiply and Divide (19 instructions)
 *     -------------------------------------
 * PMULTW  rd, rs, rt        Parallel Multiply Word
 * PMULTUW rd, rs, rt        Parallel Multiply Unsigned Word
 * PDIVW   rs, rt            Parallel Divide Word
 * PDIVUW  rs, rt            Parallel Divide Unsigned Word
 * PMADDW  rd, rs, rt        Parallel Multiply-Add Word
 * PMADDUW rd, rs, rt        Parallel Multiply-Add Unsigned Word
 * PMSUBW  rd, rs, rt        Parallel Multiply-Subtract Word
 * PMULTH  rd, rs, rt        Parallel Multiply Halfword
 * PMADDH  rd, rs, rt        Parallel Multiply-Add Halfword
 * PMSUBH  rd, rs, rt        Parallel Multiply-Subtract Halfword
 * PHMADH  rd, rs, rt        Parallel Horizontal Multiply-Add Halfword
 * PHMSBH  rd, rs, rt        Parallel Horizontal Multiply-Subtract Halfword
 * PDIVBW  rs, rt            Parallel Divide Broadcast Word
 * PMFHI   rd                Parallel Move From HI Register
 * PMFLO   rd                Parallel Move From LO Register
 * PMTHI   rs                Parallel Move To HI Register
 * PMTLO   rs                Parallel Move To LO Register
 * PMFHL   rd                Parallel Move From HI/LO Register
 * PMTHL   rs                Parallel Move To HI/LO Register
 *
 *     Pack/Extend (11 instructions)
 *     -----------------------------
 * PPAC5   rd, rt            Parallel Pack to 5 bits
 * PPACB   rd, rs, rt        Parallel Pack to Byte
 * PPACH   rd, rs, rt        Parallel Pack to Halfword
 * PPACW   rd, rs, rt        Parallel Pack to Word
 * PEXT5   rd, rt            Parallel Extend Upper from 5 bits
 * PEXTUB  rd, rs, rt        Parallel Extend Upper from Byte
 * PEXTLB  rd, rs, rt        Parallel Extend Lower from Byte
 * PEXTUH  rd, rs, rt        Parallel Extend Upper from Halfword
 * PEXTLH  rd, rs, rt        Parallel Extend Lower from Halfword
 * PEXTUW  rd, rs, rt        Parallel Extend Upper from Word
 * PEXTLW  rd, rs, rt        Parallel Extend Lower from Word
 *
 *     Others (16 instructions)
 *     ------------------------
 * PCPYH   rd, rt            Parallel Copy Halfword
 * PCPYLD  rd, rs, rt        Parallel Copy Lower Doubleword
 * PCPYUD  rd, rs, rt        Parallel Copy Upper Doubleword
 * PREVH   rd, rt            Parallel Reverse Halfword
 * PINTH   rd, rs, rt        Parallel Interleave Halfword
 * PINTEH  rd, rs, rt        Parallel Interleave Even Halfword
 * PEXEH   rd, rt            Parallel Exchange Even Halfword
 * PEXCH   rd, rt            Parallel Exchange Center Halfword
 * PEXEW   rd, rt            Parallel Exchange Even Word
 * PEXCW   rd, rt            Parallel Exchange Center Word
 * QFSRV   rd, rs, rt        Quadword Funnel Shift Right Variable
 * MFSA    rd                Move from Shift Amount Register
 * MTSA    rs                Move to Shift Amount Register
 * MTSAB   rs, immediate     Move Byte Count to Shift Amount Register
 * MTSAH   rs, immediate     Move Halfword Count to Shift Amount Register
 * PROT3W  rd, rt            Parallel Rotate 3 Words
 *
 *     MMI (MultiMedia Instruction) encodings
 *     ======================================
 *
 * MMI instructions encoding table keys:
 *
 *     *   This code is reserved for future use. An attempt to execute it
 *         causes a Reserved Instruction exception.
 *     %   This code indicates an instruction class. The instruction word
 *         must be further decoded by examining additional tables that show
 *         the values for other instruction fields.
 *     #   This code is reserved for the unsupported instructions DMULT,
 *         DMULTU, DDIV, DDIVU, LL, LLD, SC, SCD, LWC2 and SWC2. An attempt
 *         to execute it causes a Reserved Instruction exception.
 *
 * MMI instructions encoded by opcode field (MMI, LQ, SQ):
 *
 *  31    26                                        0
 * +--------+----------------------------------------+
 * | opcode |                                        |
 * +--------+----------------------------------------+
 *
 *   opcode  bits 28..26
 *     bits |   0   |   1   |   2   |   3   |   4   |   5   |   6   |   7
 *   31..29 |  000  |  001  |  010  |  011  |  100  |  101  |  110  |  111
 *   -------+-------+-------+-------+-------+-------+-------+-------+-------
 *    0 000 |SPECIAL| REGIMM|   J   |  JAL  |  BEQ  |  BNE  |  BLEZ |  BGTZ
 *    1 001 |  ADDI | ADDIU |  SLTI | SLTIU |  ANDI |  ORI  |  XORI |  LUI
 *    2 010 |  COP0 |  COP1 |   *   |   *   |  BEQL |  BNEL | BLEZL | BGTZL
 *    3 011 | DADDI | DADDIU|  LDL  |  LDR  |  MMI% |   *   |   LQ  |   SQ
 *    4 100 |   LB  |   LH  |  LWL  |   LW  |  LBU  |  LHU  |  LWR  |  LWU
 *    5 101 |   SB  |   SH  |  SWL  |   SW  |  SDL  |  SDR  |  SWR  | CACHE
 *    6 110 |   #   |  LWC1 |   #   |  PREF |   #   |  LDC1 |   #   |   LD
 *    7 111 |   #   |  SWC1 |   #   |   *   |   #   |  SDC1 |   #   |   SD
 */

enum {
    MMI_OPC_CLASS_MMI = 0x1C << 26,    /* Same as OPC_SPECIAL2 */
    MMI_OPC_LQ        = 0x1E << 26,    /* Same as OPC_MSA */
    MMI_OPC_SQ        = 0x1F << 26,    /* Same as OPC_SPECIAL3 */
};

/*
 * MMI instructions with opcode field = MMI:
 *
 *  31    26                                 5      0
 * +--------+-------------------------------+--------+
 * |   MMI  |                               |function|
 * +--------+-------------------------------+--------+
 *
 * function  bits 2..0
 *     bits |   0   |   1   |   2   |   3   |   4   |   5   |   6   |   7
 *     5..3 |  000  |  001  |  010  |  011  |  100  |  101  |  110  |  111
 *   -------+-------+-------+-------+-------+-------+-------+-------+-------
 *    0 000 |  MADD | MADDU |   *   |   *   | PLZCW |   *   |   *   |   *
 *    1 001 | MMI0% | MMI2% |   *   |   *   |   *   |   *   |   *   |   *
 *    2 010 | MFHI1 | MTHI1 | MFLO1 | MTLO1 |   *   |   *   |   *   |   *
 *    3 011 | MULT1 | MULTU1|  DIV1 | DIVU1 |   *   |   *   |   *   |   *
 *    4 100 | MADD1 | MADDU1|   *   |   *   |   *   |   *   |   *   |   *
 *    5 101 | MMI1% | MMI3% |   *   |   *   |   *   |   *   |   *   |   *
 *    6 110 | PMFHL | PMTHL |   *   |   *   | PSLLH |   *   | PSRLH | PSRAH
 *    7 111 |   *   |   *   |   *   |   *   | PSLLW |   *   | PSRLW | PSRAW
 */

#define MASK_MMI(op) (MASK_OP_MAJOR(op) | ((op) & 0x3F))
enum {
    MMI_OPC_PLZCW      = 0x04 | MMI_OPC_CLASS_MMI,
    MMI_OPC_CLASS_MMI0 = 0x08 | MMI_OPC_CLASS_MMI,
    MMI_OPC_CLASS_MMI2 = 0x09 | MMI_OPC_CLASS_MMI,
    MMI_OPC_CLASS_MMI1 = 0x28 | MMI_OPC_CLASS_MMI,
    MMI_OPC_CLASS_MMI3 = 0x29 | MMI_OPC_CLASS_MMI,
    MMI_OPC_PMFHL      = 0x30 | MMI_OPC_CLASS_MMI,
    MMI_OPC_PMTHL      = 0x31 | MMI_OPC_CLASS_MMI,
    MMI_OPC_PSLLH      = 0x34 | MMI_OPC_CLASS_MMI,
    MMI_OPC_PSRLH      = 0x36 | MMI_OPC_CLASS_MMI,
    MMI_OPC_PSRAH      = 0x37 | MMI_OPC_CLASS_MMI,
    MMI_OPC_PSLLW      = 0x3C | MMI_OPC_CLASS_MMI,
    MMI_OPC_PSRLW      = 0x3E | MMI_OPC_CLASS_MMI,
    MMI_OPC_PSRAW      = 0x3F | MMI_OPC_CLASS_MMI,
};

/*
 * MMI instructions with opcode field = MMI and bits 5..0 = MMI0:
 *
 *  31    26                        10     6 5      0
 * +--------+----------------------+--------+--------+
 * |   MMI  |                      |function|  MMI0  |
 * +--------+----------------------+--------+--------+
 *
 * function  bits 7..6
 *     bits |   0   |   1   |   2   |   3
 *    10..8 |   00  |   01  |   10  |   11
 *   -------+-------+-------+-------+-------
 *    0 000 | PADDW | PSUBW | PCGTW | PMAXW
 *    1 001 | PADDH | PSUBH | PCGTH | PMAXH
 *    2 010 | PADDB | PSUBB | PCGTB |   *
 *    3 011 |   *   |   *   |   *   |   *
 *    4 100 | PADDSW| PSUBSW| PEXTLW| PPACW
 *    5 101 | PADDSH| PSUBSH| PEXTLH| PPACH
 *    6 110 | PADDSB| PSUBSB| PEXTLB| PPACB
 *    7 111 |   *   |   *   | PEXT5 | PPAC5
 */

#define MASK_MMI0(op) (MASK_OP_MAJOR(op) | ((op) & 0x7FF))
enum {
    MMI_OPC_0_PADDW  = (0x00 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PSUBW  = (0x01 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PCGTW  = (0x02 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PMAXW  = (0x03 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PADDH  = (0x04 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PSUBH  = (0x05 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PCGTH  = (0x06 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PMAXH  = (0x07 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PADDB  = (0x08 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PSUBB  = (0x09 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PCGTB  = (0x0A << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PADDSW = (0x10 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PSUBSW = (0x11 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PEXTLW = (0x12 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PPACW  = (0x13 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PADDSH = (0x14 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PSUBSH = (0x15 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PEXTLH = (0x16 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PPACH  = (0x17 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PADDSB = (0x18 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PSUBSB = (0x19 << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PEXTLB = (0x1A << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PPACB  = (0x1B << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PEXT5  = (0x1E << 6) | MMI_OPC_CLASS_MMI0,
    MMI_OPC_0_PPAC5  = (0x1F << 6) | MMI_OPC_CLASS_MMI0,
};

/*
 * MMI instructions with opcode field = MMI and bits 5..0 = MMI1:
 *
 *  31    26                        10     6 5      0
 * +--------+----------------------+--------+--------+
 * |   MMI  |                      |function|  MMI1  |
 * +--------+----------------------+--------+--------+
 *
 * function  bits 7..6
 *     bits |   0   |   1   |   2   |   3
 *    10..8 |   00  |   01  |   10  |   11
 *   -------+-------+-------+-------+-------
 *    0 000 |   *   | PABSW | PCEQW | PMINW
 *    1 001 | PADSBH| PABSH | PCEQH | PMINH
 *    2 010 |   *   |   *   | PCEQB |   *
 *    3 011 |   *   |   *   |   *   |   *
 *    4 100 | PADDUW| PSUBUW| PEXTUW|   *
 *    5 101 | PADDUH| PSUBUH| PEXTUH|   *
 *    6 110 | PADDUB| PSUBUB| PEXTUB| QFSRV
 *    7 111 |   *   |   *   |   *   |   *
 */

#define MASK_MMI1(op) (MASK_OP_MAJOR(op) | ((op) & 0x7FF))
enum {
    MMI_OPC_1_PABSW  = (0x01 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PCEQW  = (0x02 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PMINW  = (0x03 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PADSBH = (0x04 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PABSH  = (0x05 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PCEQH  = (0x06 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PMINH  = (0x07 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PCEQB  = (0x0A << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PADDUW = (0x10 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PSUBUW = (0x11 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PEXTUW = (0x12 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PADDUH = (0x14 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PSUBUH = (0x15 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PEXTUH = (0x16 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PADDUB = (0x18 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PSUBUB = (0x19 << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_PEXTUB = (0x1A << 6) | MMI_OPC_CLASS_MMI1,
    MMI_OPC_1_QFSRV  = (0x1B << 6) | MMI_OPC_CLASS_MMI1,
};

/*
 * MMI instructions with opcode field = MMI and bits 5..0 = MMI2:
 *
 *  31    26                        10     6 5      0
 * +--------+----------------------+--------+--------+
 * |   MMI  |                      |function|  MMI2  |
 * +--------+----------------------+--------+--------+
 *
 * function  bits 7..6
 *     bits |   0   |   1   |   2   |   3
 *    10..8 |   00  |   01  |   10  |   11
 *   -------+-------+-------+-------+-------
 *    0 000 | PMADDW|   *   | PSLLVW| PSRLVW
 *    1 001 | PMSUBW|   *   |   *   |   *
 *    2 010 | PMFHI | PMFLO | PINTH |   *
 *    3 011 | PMULTW| PDIVW | PCPYLD|   *
 *    4 100 | PMADDH| PHMADH|  PAND |  PXOR
 *    5 101 | PMSUBH| PHMSBH|   *   |   *
 *    6 110 |   *   |   *   | PEXEH | PREVH
 *    7 111 | PMULTH| PDIVBW| PEXEW | PROT3W
 */

#define MASK_MMI2(op) (MASK_OP_MAJOR(op) | ((op) & 0x7FF))
enum {
    MMI_OPC_2_PMADDW = (0x00 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PSLLVW = (0x02 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PSRLVW = (0x03 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PMSUBW = (0x04 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PMFHI  = (0x08 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PMFLO  = (0x09 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PINTH  = (0x0A << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PMULTW = (0x0C << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PDIVW  = (0x0D << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PCPYLD = (0x0E << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PMADDH = (0x10 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PHMADH = (0x11 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PAND   = (0x12 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PXOR   = (0x13 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PMSUBH = (0x14 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PHMSBH = (0x15 << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PEXEH  = (0x1A << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PREVH  = (0x1B << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PMULTH = (0x1C << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PDIVBW = (0x1D << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PEXEW  = (0x1E << 6) | MMI_OPC_CLASS_MMI2,
    MMI_OPC_2_PROT3W = (0x1F << 6) | MMI_OPC_CLASS_MMI2,
};

/*
 * MMI instructions with opcode field = MMI and bits 5..0 = MMI3:
 *
 *  31    26                        10     6 5      0
 * +--------+----------------------+--------+--------+
 * |   MMI  |                      |function|  MMI3  |
 * +--------+----------------------+--------+--------+
 *
 * function  bits 7..6
 *     bits |   0   |   1   |   2   |   3
 *    10..8 |   00  |   01  |   10  |   11
 *   -------+-------+-------+-------+-------
 *    0 000 |PMADDUW|   *   |   *   | PSRAVW
 *    1 001 |   *   |   *   |   *   |   *
 *    2 010 | PMTHI | PMTLO | PINTEH|   *
 *    3 011 |PMULTUW| PDIVUW| PCPYUD|   *
 *    4 100 |   *   |   *   |  POR  |  PNOR
 *    5 101 |   *   |   *   |   *   |   *
 *    6 110 |   *   |   *   | PEXCH | PCPYH
 *    7 111 |   *   |   *   | PEXCW |   *
 */

#define MASK_MMI3(op) (MASK_OP_MAJOR(op) | ((op) & 0x7FF))
enum {
    MMI_OPC_3_PMADDUW = (0x00 << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PSRAVW  = (0x03 << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PMTHI   = (0x08 << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PMTLO   = (0x09 << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PINTEH  = (0x0A << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PMULTUW = (0x0C << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PDIVUW  = (0x0D << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PCPYUD  = (0x0E << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_POR     = (0x12 << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PNOR    = (0x13 << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PEXCH   = (0x1A << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PCPYH   = (0x1B << 6) | MMI_OPC_CLASS_MMI3,
    MMI_OPC_3_PEXCW   = (0x1E << 6) | MMI_OPC_CLASS_MMI3,
};

#if defined(TARGET_MIPS64)
/* Upper halves of R5900's 128-bit registers: MMRs (multimedia registers) */
static TCGv_i64 cpu_mmr[32];

/*
 *
 *           MMI (MultiMedia Interface) ASE instructions
 *           ===========================================
 */

/*
 *          MMI instructions category: data communication
 *          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 *   PCPYH    PEXCH    PEXTLB   PINTH    PPACB    PEXT5    PREVH
 *   PCPYLD   PEXCW    PEXTLH   PINTEH   PPACH    PPAC5    PROT3W
 *   PCPYUD   PEXEH    PEXTLW            PPACW
 *            PEXEW    PEXTUB
 *                     PEXTUH
 *                     PEXTUW
 */

/*
 *  PCPYH rd, rt
 *
 *    Parallel Copy Halfword
 *
 *   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 *  +-----------+---------+---------+---------+---------+-----------+
 *  |    MMI    |0 0 0 0 0|   rt    |   rd    |  PCPYH  |    MMI3   |
 *  +-----------+---------+---------+---------+---------+-----------+
 */
static void gen_mmi_pcpyh(DisasContext *ctx)
{
    uint32_t pd, rt, rd;
    uint32_t opcode;

    opcode = ctx->opcode;

    pd = extract32(opcode, 21, 5);
    rt = extract32(opcode, 16, 5);
    rd = extract32(opcode, 11, 5);

    if (unlikely(pd != 0)) {
        generate_exception_end(ctx, EXCP_RI);
    } else if (rd == 0) {
        /* nop */
    } else if (rt == 0) {
        tcg_gen_movi_i64(cpu_gpr[rd], 0);
        tcg_gen_movi_i64(cpu_mmr[rd], 0);
    } else {
        TCGv_i64 t0 = tcg_temp_new();
        TCGv_i64 t1 = tcg_temp_new();
        uint64_t mask = (1ULL << 16) - 1;

        tcg_gen_andi_i64(t0, cpu_gpr[rt], mask);
        tcg_gen_movi_i64(t1, 0);
        tcg_gen_or_i64(t1, t0, t1);
        tcg_gen_shli_i64(t0, t0, 16);
        tcg_gen_or_i64(t1, t0, t1);
        tcg_gen_shli_i64(t0, t0, 16);
        tcg_gen_or_i64(t1, t0, t1);
        tcg_gen_shli_i64(t0, t0, 16);
        tcg_gen_or_i64(t1, t0, t1);

        tcg_gen_mov_i64(cpu_gpr[rd], t1);

        tcg_gen_andi_i64(t0, cpu_mmr[rt], mask);
        tcg_gen_movi_i64(t1, 0);
        tcg_gen_or_i64(t1, t0, t1);
        tcg_gen_shli_i64(t0, t0, 16);
        tcg_gen_or_i64(t1, t0, t1);
        tcg_gen_shli_i64(t0, t0, 16);
        tcg_gen_or_i64(t1, t0, t1);
        tcg_gen_shli_i64(t0, t0, 16);
        tcg_gen_or_i64(t1, t0, t1);

        tcg_gen_mov_i64(cpu_mmr[rd], t1);

        tcg_temp_free(t0);
        tcg_temp_free(t1);
    }
}

/*
 *  PCPYLD rd, rs, rt
 *
 *    Parallel Copy Lower Doubleword
 *
 *   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 *  +-----------+---------+---------+---------+---------+-----------+
 *  |    MMI    |   rs    |   rt    |   rd    | PCPYLD  |    MMI2   |
 *  +-----------+---------+---------+---------+---------+-----------+
 */
static void gen_mmi_pcpyld(DisasContext *ctx)
{
    uint32_t rs, rt, rd;
    uint32_t opcode;

    opcode = ctx->opcode;

    rs = extract32(opcode, 21, 5);
    rt = extract32(opcode, 16, 5);
    rd = extract32(opcode, 11, 5);

    if (rd == 0) {
        /* nop */
    } else {
        if (rs == 0) {
            tcg_gen_movi_i64(cpu_mmr[rd], 0);
        } else {
            tcg_gen_mov_i64(cpu_mmr[rd], cpu_gpr[rs]);
        }
        if (rt == 0) {
            tcg_gen_movi_i64(cpu_gpr[rd], 0);
        } else {
            if (rd != rt) {
                tcg_gen_mov_i64(cpu_gpr[rd], cpu_gpr[rt]);
            }
        }
    }
}

/*
 *  PCPYUD rd, rs, rt
 *
 *    Parallel Copy Upper Doubleword
 *
 *   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 *  +-----------+---------+---------+---------+---------+-----------+
 *  |    MMI    |   rs    |   rt    |   rd    | PCPYUD  |    MMI3   |
 *  +-----------+---------+---------+---------+---------+-----------+
 */
static void gen_mmi_pcpyud(DisasContext *ctx)
{
    uint32_t rs, rt, rd;
    uint32_t opcode;

    opcode = ctx->opcode;

    rs = extract32(opcode, 21, 5);
    rt = extract32(opcode, 16, 5);
    rd = extract32(opcode, 11, 5);

    if (rd == 0) {
        /* nop */
    } else {
        if (rs == 0) {
            tcg_gen_movi_i64(cpu_gpr[rd], 0);
        } else {
            tcg_gen_mov_i64(cpu_gpr[rd], cpu_mmr[rs]);
        }
        if (rt == 0) {
            tcg_gen_movi_i64(cpu_mmr[rd], 0);
        } else {
            if (rd != rt) {
                tcg_gen_mov_i64(cpu_mmr[rd], cpu_mmr[rt]);
            }
        }
    }
}

#endif
