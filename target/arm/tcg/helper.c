/*
 * ARM generic helpers.
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "cpu.h"
#include "internals.h"
#include "exec/gdbstub.h"
#include "exec/helper-proto.h"
#include "qemu/crc32c.h"
#include <zlib.h> /* For crc32 */
#include "arm_ldst.h"
#include "cpu-mmu.h"
#include "cpregs.h"
#include "tcg-cpu.h"

#ifdef TARGET_AARCH64
#include "cpu-sve.h"
#endif /* TARGET_AARCH64 */

static int vfp_gdb_get_reg(CPUARMState *env, GByteArray *buf, int reg)
{
    ARMCPU *cpu = env_archcpu(env);
    int nregs = cpu_isar_feature(aa32_simd_r32, cpu) ? 32 : 16;

    /* VFP data registers are always little-endian.  */
    if (reg < nregs) {
        return gdb_get_reg64(buf, *aa32_vfp_dreg(env, reg));
    }
    if (arm_feature(env, ARM_FEATURE_NEON)) {
        /* Aliases for Q regs.  */
        nregs += 16;
        if (reg < nregs) {
            uint64_t *q = aa32_vfp_qreg(env, reg - 32);
            return gdb_get_reg128(buf, q[0], q[1]);
        }
    }
    switch (reg - nregs) {
    case 0: return gdb_get_reg32(buf, env->vfp.xregs[ARM_VFP_FPSID]); break;
    case 1: return gdb_get_reg32(buf, vfp_get_fpscr(env)); break;
    case 2: return gdb_get_reg32(buf, env->vfp.xregs[ARM_VFP_FPEXC]); break;
    }
    return 0;
}

static int vfp_gdb_set_reg(CPUARMState *env, uint8_t *buf, int reg)
{
    ARMCPU *cpu = env_archcpu(env);
    int nregs = cpu_isar_feature(aa32_simd_r32, cpu) ? 32 : 16;

    if (reg < nregs) {
        *aa32_vfp_dreg(env, reg) = ldq_le_p(buf);
        return 8;
    }
    if (arm_feature(env, ARM_FEATURE_NEON)) {
        nregs += 16;
        if (reg < nregs) {
            uint64_t *q = aa32_vfp_qreg(env, reg - 32);
            q[0] = ldq_le_p(buf);
            q[1] = ldq_le_p(buf + 8);
            return 16;
        }
    }
    switch (reg - nregs) {
    case 0: env->vfp.xregs[ARM_VFP_FPSID] = ldl_p(buf); return 4;
    case 1: vfp_set_fpscr(env, ldl_p(buf)); return 4;
    case 2: env->vfp.xregs[ARM_VFP_FPEXC] = ldl_p(buf) & (1 << 30); return 4;
    }
    return 0;
}

static int aarch64_fpu_gdb_get_reg(CPUARMState *env, GByteArray *buf, int reg)
{
    switch (reg) {
    case 0 ... 31:
    {
        /* 128 bit FP register - quads are in LE order */
        uint64_t *q = aa64_vfp_qreg(env, reg);
        return gdb_get_reg128(buf, q[1], q[0]);
    }
    case 32:
        /* FPSR */
        return gdb_get_reg32(buf, vfp_get_fpsr(env));
    case 33:
        /* FPCR */
        return gdb_get_reg32(buf,vfp_get_fpcr(env));
    default:
        return 0;
    }
}

static int aarch64_fpu_gdb_set_reg(CPUARMState *env, uint8_t *buf, int reg)
{
    switch (reg) {
    case 0 ... 31:
        /* 128 bit FP register */
        {
            uint64_t *q = aa64_vfp_qreg(env, reg);
            q[0] = ldq_le_p(buf);
            q[1] = ldq_le_p(buf + 8);
            return 16;
        }
    case 32:
        /* FPSR */
        vfp_set_fpsr(env, ldl_p(buf));
        return 4;
    case 33:
        /* FPCR */
        vfp_set_fpcr(env, ldl_p(buf));
        return 4;
    default:
        return 0;
    }
}

/**
 * arm_get/set_gdb_*: get/set a gdb register
 * @env: the CPU state
 * @buf: a buffer to copy to/from
 * @reg: register number (offset from start of group)
 *
 * We return the number of bytes copied
 */

static int arm_gdb_get_sysreg(CPUARMState *env, GByteArray *buf, int reg)
{
    ARMCPU *cpu = env_archcpu(env);
    const ARMCPRegInfo *ri;
    uint32_t key;

    key = cpu->dyn_sysreg_xml.data.cpregs.keys[reg];
    ri = get_arm_cp_reginfo(cpu->cp_regs, key);
    if (ri) {
        if (cpreg_field_is_64bit(ri)) {
            return gdb_get_reg64(buf, (uint64_t)read_raw_cp_reg(env, ri));
        } else {
            return gdb_get_reg32(buf, (uint32_t)read_raw_cp_reg(env, ri));
        }
    }
    return 0;
}

static int arm_gdb_set_sysreg(CPUARMState *env, uint8_t *buf, int reg)
{
    return 0;
}

#ifdef TARGET_AARCH64
static int arm_gdb_get_svereg(CPUARMState *env, GByteArray *buf, int reg)
{
    ARMCPU *cpu = env_archcpu(env);

    switch (reg) {
    /* The first 32 registers are the zregs */
    case 0 ... 31:
    {
        int vq, len = 0;
        for (vq = 0; vq < cpu->sve_max_vq; vq++) {
            len += gdb_get_reg128(buf,
                                  env->vfp.zregs[reg].d[vq * 2 + 1],
                                  env->vfp.zregs[reg].d[vq * 2]);
        }
        return len;
    }
    case 32:
        return gdb_get_reg32(buf, vfp_get_fpsr(env));
    case 33:
        return gdb_get_reg32(buf, vfp_get_fpcr(env));
    /* then 16 predicates and the ffr */
    case 34 ... 50:
    {
        int preg = reg - 34;
        int vq, len = 0;
        for (vq = 0; vq < cpu->sve_max_vq; vq = vq + 4) {
            len += gdb_get_reg64(buf, env->vfp.pregs[preg].p[vq / 4]);
        }
        return len;
    }
    case 51:
    {
        /*
         * We report in Vector Granules (VG) which is 64bit in a Z reg
         * while the ZCR works in Vector Quads (VQ) which is 128bit chunks.
         */
        int vq = cpu_sve_get_zcr_len_for_el(env, arm_current_el(env)) + 1;
        return gdb_get_reg64(buf, vq * 2);
    }
    default:
        /* gdbstub asked for something out our range */
        qemu_log_mask(LOG_UNIMP, "%s: out of range register %d", __func__, reg);
        break;
    }

    return 0;
}

static int arm_gdb_set_svereg(CPUARMState *env, uint8_t *buf, int reg)
{
    ARMCPU *cpu = env_archcpu(env);

    /* The first 32 registers are the zregs */
    switch (reg) {
    /* The first 32 registers are the zregs */
    case 0 ... 31:
    {
        int vq, len = 0;
        uint64_t *p = (uint64_t *) buf;
        for (vq = 0; vq < cpu->sve_max_vq; vq++) {
            env->vfp.zregs[reg].d[vq * 2 + 1] = *p++;
            env->vfp.zregs[reg].d[vq * 2] = *p++;
            len += 16;
        }
        return len;
    }
    case 32:
        vfp_set_fpsr(env, *(uint32_t *)buf);
        return 4;
    case 33:
        vfp_set_fpcr(env, *(uint32_t *)buf);
        return 4;
    case 34 ... 50:
    {
        int preg = reg - 34;
        int vq, len = 0;
        uint64_t *p = (uint64_t *) buf;
        for (vq = 0; vq < cpu->sve_max_vq; vq = vq + 4) {
            env->vfp.pregs[preg].p[vq / 4] = *p++;
            len += 8;
        }
        return len;
    }
    case 51:
        /* cannot set vg via gdbstub */
        return 0;
    default:
        /* gdbstub asked for something out our range */
        break;
    }

    return 0;
}
#endif /* TARGET_AARCH64 */

void hw_watchpoint_update(ARMCPU *cpu, int n)
{
    CPUARMState *env = &cpu->env;
    vaddr len = 0;
    vaddr wvr = env->cp15.dbgwvr[n];
    uint64_t wcr = env->cp15.dbgwcr[n];
    int mask;
    int flags = BP_CPU | BP_STOP_BEFORE_ACCESS;

    if (env->cpu_watchpoint[n]) {
        cpu_watchpoint_remove_by_ref(CPU(cpu), env->cpu_watchpoint[n]);
        env->cpu_watchpoint[n] = NULL;
    }

    if (!extract64(wcr, 0, 1)) {
        /* E bit clear : watchpoint disabled */
        return;
    }

    switch (extract64(wcr, 3, 2)) {
    case 0:
        /* LSC 00 is reserved and must behave as if the wp is disabled */
        return;
    case 1:
        flags |= BP_MEM_READ;
        break;
    case 2:
        flags |= BP_MEM_WRITE;
        break;
    case 3:
        flags |= BP_MEM_ACCESS;
        break;
    }

    /* Attempts to use both MASK and BAS fields simultaneously are
     * CONSTRAINED UNPREDICTABLE; we opt to ignore BAS in this case,
     * thus generating a watchpoint for every byte in the masked region.
     */
    mask = extract64(wcr, 24, 4);
    if (mask == 1 || mask == 2) {
        /* Reserved values of MASK; we must act as if the mask value was
         * some non-reserved value, or as if the watchpoint were disabled.
         * We choose the latter.
         */
        return;
    } else if (mask) {
        /* Watchpoint covers an aligned area up to 2GB in size */
        len = 1ULL << mask;
        /* If masked bits in WVR are not zero it's CONSTRAINED UNPREDICTABLE
         * whether the watchpoint fires when the unmasked bits match; we opt
         * to generate the exceptions.
         */
        wvr &= ~(len - 1);
    } else {
        /* Watchpoint covers bytes defined by the byte address select bits */
        int bas = extract64(wcr, 5, 8);
        int basstart;

        if (extract64(wvr, 2, 1)) {
            /* Deprecated case of an only 4-aligned address. BAS[7:4] are
             * ignored, and BAS[3:0] define which bytes to watch.
             */
            bas &= 0xf;
        }

        if (bas == 0) {
            /* This must act as if the watchpoint is disabled */
            return;
        }

        /* The BAS bits are supposed to be programmed to indicate a contiguous
         * range of bytes. Otherwise it is CONSTRAINED UNPREDICTABLE whether
         * we fire for each byte in the word/doubleword addressed by the WVR.
         * We choose to ignore any non-zero bits after the first range of 1s.
         */
        basstart = ctz32(bas);
        len = cto32(bas >> basstart);
        wvr += basstart;
    }

    cpu_watchpoint_insert(CPU(cpu), wvr, len, flags,
                          &env->cpu_watchpoint[n]);
}

void hw_watchpoint_update_all(ARMCPU *cpu)
{
    int i;
    CPUARMState *env = &cpu->env;

    /* Completely clear out existing QEMU watchpoints and our array, to
     * avoid possible stale entries following migration load.
     */
    cpu_watchpoint_remove_all(CPU(cpu), BP_CPU);
    memset(env->cpu_watchpoint, 0, sizeof(env->cpu_watchpoint));

    for (i = 0; i < ARRAY_SIZE(cpu->env.cpu_watchpoint); i++) {
        hw_watchpoint_update(cpu, i);
    }
}

void hw_breakpoint_update(ARMCPU *cpu, int n)
{
    CPUARMState *env = &cpu->env;
    uint64_t bvr = env->cp15.dbgbvr[n];
    uint64_t bcr = env->cp15.dbgbcr[n];
    vaddr addr;
    int bt;
    int flags = BP_CPU;

    if (env->cpu_breakpoint[n]) {
        cpu_breakpoint_remove_by_ref(CPU(cpu), env->cpu_breakpoint[n]);
        env->cpu_breakpoint[n] = NULL;
    }

    if (!extract64(bcr, 0, 1)) {
        /* E bit clear : watchpoint disabled */
        return;
    }

    bt = extract64(bcr, 20, 4);

    switch (bt) {
    case 4: /* unlinked address mismatch (reserved if AArch64) */
    case 5: /* linked address mismatch (reserved if AArch64) */
        qemu_log_mask(LOG_UNIMP,
                      "arm: address mismatch breakpoint types not implemented\n");
        return;
    case 0: /* unlinked address match */
    case 1: /* linked address match */
    {
        /* Bits [63:49] are hardwired to the value of bit [48]; that is,
         * we behave as if the register was sign extended. Bits [1:0] are
         * RES0. The BAS field is used to allow setting breakpoints on 16
         * bit wide instructions; it is CONSTRAINED UNPREDICTABLE whether
         * a bp will fire if the addresses covered by the bp and the addresses
         * covered by the insn overlap but the insn doesn't start at the
         * start of the bp address range. We choose to require the insn and
         * the bp to have the same address. The constraints on writing to
         * BAS enforced in dbgbcr_write mean we have only four cases:
         *  0b0000  => no breakpoint
         *  0b0011  => breakpoint on addr
         *  0b1100  => breakpoint on addr + 2
         *  0b1111  => breakpoint on addr
         * See also figure D2-3 in the v8 ARM ARM (DDI0487A.c).
         */
        int bas = extract64(bcr, 5, 4);
        addr = sextract64(bvr, 0, 49) & ~3ULL;
        if (bas == 0) {
            return;
        }
        if (bas == 0xc) {
            addr += 2;
        }
        break;
    }
    case 2: /* unlinked context ID match */
    case 8: /* unlinked VMID match (reserved if no EL2) */
    case 10: /* unlinked context ID and VMID match (reserved if no EL2) */
        qemu_log_mask(LOG_UNIMP,
                      "arm: unlinked context breakpoint types not implemented\n");
        return;
    case 9: /* linked VMID match (reserved if no EL2) */
    case 11: /* linked context ID and VMID match (reserved if no EL2) */
    case 3: /* linked context ID match */
    default:
        /* We must generate no events for Linked context matches (unless
         * they are linked to by some other bp/wp, which is handled in
         * updates for the linking bp/wp). We choose to also generate no events
         * for reserved values.
         */
        return;
    }

    cpu_breakpoint_insert(CPU(cpu), addr, flags, &env->cpu_breakpoint[n]);
}

void hw_breakpoint_update_all(ARMCPU *cpu)
{
    int i;
    CPUARMState *env = &cpu->env;

    /* Completely clear out existing QEMU breakpoints and our array, to
     * avoid possible stale entries following migration load.
     */
    cpu_breakpoint_remove_all(CPU(cpu), BP_CPU);
    memset(env->cpu_breakpoint, 0, sizeof(env->cpu_breakpoint));

    for (i = 0; i < ARRAY_SIZE(cpu->env.cpu_breakpoint); i++) {
        hw_breakpoint_update(cpu, i);
    }
}

void arm_cpu_register_gdb_regs_for_features(ARMCPU *cpu)
{
    CPUState *cs = CPU(cpu);
    CPUARMState *env = &cpu->env;

    if (arm_feature(env, ARM_FEATURE_AARCH64)) {
        /*
         * The lower part of each SVE register aliases to the FPU
         * registers so we don't need to include both.
         */
#ifdef TARGET_AARCH64
        if (isar_feature_aa64_sve(&cpu->isar)) {
            gdb_register_coprocessor(cs, arm_gdb_get_svereg, arm_gdb_set_svereg,
                                     arm_gen_dynamic_svereg_xml(cs, cs->gdb_num_regs),
                                     "sve-registers.xml", 0);
        } else
#endif
        {
            gdb_register_coprocessor(cs, aarch64_fpu_gdb_get_reg,
                                     aarch64_fpu_gdb_set_reg,
                                     34, "aarch64-fpu.xml", 0);
        }
    } else if (arm_feature(env, ARM_FEATURE_NEON)) {
        gdb_register_coprocessor(cs, vfp_gdb_get_reg, vfp_gdb_set_reg,
                                 51, "arm-neon.xml", 0);
    } else if (cpu_isar_feature(aa32_simd_r32, cpu)) {
        gdb_register_coprocessor(cs, vfp_gdb_get_reg, vfp_gdb_set_reg,
                                 35, "arm-vfp3.xml", 0);
    } else if (cpu_isar_feature(aa32_vfp_simd, cpu)) {
        gdb_register_coprocessor(cs, vfp_gdb_get_reg, vfp_gdb_set_reg,
                                 19, "arm-vfp.xml", 0);
    }
    gdb_register_coprocessor(cs, arm_gdb_get_sysreg, arm_gdb_set_sysreg,
                             arm_gen_dynamic_sysreg_xml(cs, cs->gdb_num_regs),
                             "system-registers.xml", 0);

}

/* Sign/zero extend */
uint32_t HELPER(sxtb16)(uint32_t x)
{
    uint32_t res;
    res = (uint16_t)(int8_t)x;
    res |= (uint32_t)(int8_t)(x >> 16) << 16;
    return res;
}

uint32_t HELPER(uxtb16)(uint32_t x)
{
    uint32_t res;
    res = (uint16_t)(uint8_t)x;
    res |= (uint32_t)(uint8_t)(x >> 16) << 16;
    return res;
}

int32_t HELPER(sdiv)(int32_t num, int32_t den)
{
    if (den == 0)
      return 0;
    if (num == INT_MIN && den == -1)
      return INT_MIN;
    return num / den;
}

uint32_t HELPER(udiv)(uint32_t num, uint32_t den)
{
    if (den == 0)
      return 0;
    return num / den;
}

uint32_t HELPER(rbit)(uint32_t x)
{
    return revbit32(x);
}

#ifndef CONFIG_USER_ONLY

/*
 * Physical Interrupt Target EL Lookup Table
 *
 * [ From ARM ARM section G1.13.4 (Table G1-15) ]
 *
 * The below multi-dimensional table is used for looking up the target
 * exception level given numerous condition criteria.  Specifically, the
 * target EL is based on SCR and HCR routing controls as well as the
 * currently executing EL and secure state.
 *
 *    Dimensions:
 *    target_el_table[2][2][2][2][2][4]
 *                    |  |  |  |  |  +--- Current EL
 *                    |  |  |  |  +------ Non-secure(0)/Secure(1)
 *                    |  |  |  +--------- HCR mask override
 *                    |  |  +------------ SCR exec state control
 *                    |  +--------------- SCR mask override
 *                    +------------------ 32-bit(0)/64-bit(1) EL3
 *
 *    The table values are as such:
 *    0-3 = EL0-EL3
 *     -1 = Cannot occur
 *
 * The ARM ARM target EL table includes entries indicating that an "exception
 * is not taken".  The two cases where this is applicable are:
 *    1) An exception is taken from EL3 but the SCR does not have the exception
 *    routed to EL3.
 *    2) An exception is taken from EL2 but the HCR does not have the exception
 *    routed to EL2.
 * In these two cases, the below table contain a target of EL1.  This value is
 * returned as it is expected that the consumer of the table data will check
 * for "target EL >= current EL" to ensure the exception is not taken.
 *
 *            SCR     HCR
 *         64  EA     AMO                 From
 *        BIT IRQ     IMO      Non-secure         Secure
 *        EL3 FIQ  RW FMO   EL0 EL1 EL2 EL3   EL0 EL1 EL2 EL3
 */
static const int8_t target_el_table[2][2][2][2][2][4] = {
    {{{{/* 0   0   0   0 */{ 1,  1,  2, -1 },{ 3, -1, -1,  3 },},
       {/* 0   0   0   1 */{ 2,  2,  2, -1 },{ 3, -1, -1,  3 },},},
      {{/* 0   0   1   0 */{ 1,  1,  2, -1 },{ 3, -1, -1,  3 },},
       {/* 0   0   1   1 */{ 2,  2,  2, -1 },{ 3, -1, -1,  3 },},},},
     {{{/* 0   1   0   0 */{ 3,  3,  3, -1 },{ 3, -1, -1,  3 },},
       {/* 0   1   0   1 */{ 3,  3,  3, -1 },{ 3, -1, -1,  3 },},},
      {{/* 0   1   1   0 */{ 3,  3,  3, -1 },{ 3, -1, -1,  3 },},
       {/* 0   1   1   1 */{ 3,  3,  3, -1 },{ 3, -1, -1,  3 },},},},},
    {{{{/* 1   0   0   0 */{ 1,  1,  2, -1 },{ 1,  1, -1,  1 },},
       {/* 1   0   0   1 */{ 2,  2,  2, -1 },{ 2,  2, -1,  1 },},},
      {{/* 1   0   1   0 */{ 1,  1,  1, -1 },{ 1,  1,  1,  1 },},
       {/* 1   0   1   1 */{ 2,  2,  2, -1 },{ 2,  2,  2,  1 },},},},
     {{{/* 1   1   0   0 */{ 3,  3,  3, -1 },{ 3,  3, -1,  3 },},
       {/* 1   1   0   1 */{ 3,  3,  3, -1 },{ 3,  3, -1,  3 },},},
      {{/* 1   1   1   0 */{ 3,  3,  3, -1 },{ 3,  3,  3,  3 },},
       {/* 1   1   1   1 */{ 3,  3,  3, -1 },{ 3,  3,  3,  3 },},},},},
};

/*
 * Determine the target EL for physical exceptions
 */
uint32_t arm_phys_excp_target_el(CPUState *cs, uint32_t excp_idx,
                                 uint32_t cur_el, bool secure)
{
    CPUARMState *env = cs->env_ptr;
    bool rw;
    bool scr;
    bool hcr;
    int target_el;
    /* Is the highest EL AArch64? */
    bool is64 = arm_feature(env, ARM_FEATURE_AARCH64);
    uint64_t hcr_el2;

    if (arm_feature(env, ARM_FEATURE_EL3)) {
        rw = ((env->cp15.scr_el3 & SCR_RW) == SCR_RW);
    } else {
        /* Either EL2 is the highest EL (and so the EL2 register width
         * is given by is64); or there is no EL2 or EL3, in which case
         * the value of 'rw' does not affect the table lookup anyway.
         */
        rw = is64;
    }

    hcr_el2 = arm_hcr_el2_eff(env);
    switch (excp_idx) {
    case EXCP_IRQ:
        scr = ((env->cp15.scr_el3 & SCR_IRQ) == SCR_IRQ);
        hcr = hcr_el2 & HCR_IMO;
        break;
    case EXCP_FIQ:
        scr = ((env->cp15.scr_el3 & SCR_FIQ) == SCR_FIQ);
        hcr = hcr_el2 & HCR_FMO;
        break;
    default:
        scr = ((env->cp15.scr_el3 & SCR_EA) == SCR_EA);
        hcr = hcr_el2 & HCR_AMO;
        break;
    };

    /*
     * For these purposes, TGE and AMO/IMO/FMO both force the
     * interrupt to EL2.  Fold TGE into the bit extracted above.
     */
    hcr |= (hcr_el2 & HCR_TGE) != 0;

    /* Perform a table-lookup for the target EL given the current state */
    target_el = target_el_table[is64][scr][rw][hcr][secure][cur_el];

    assert(target_el > 0);

    return target_el;
}

#endif /* !CONFIG_USER_ONLY */

/* Returns true if the stage 1 translation regime is using LPAE format page
 * tables. Used when raising alignment exceptions, whose FSR changes depending
 * on whether the long or short descriptor format is in use. */
bool arm_s1_regime_using_lpae_format(CPUARMState *env, ARMMMUIdx mmu_idx)
{
    mmu_idx = stage_1_mmu_idx(mmu_idx);

    return regime_using_lpae_format(env, mmu_idx);
}

/* Note that signed overflow is undefined in C.  The following routines are
   careful to use unsigned types where modulo arithmetic is required.
   Failure to do so _will_ break on newer gcc.  */

/* Signed saturating arithmetic.  */

/* Perform 16-bit signed saturating addition.  */
static inline uint16_t add16_sat(uint16_t a, uint16_t b)
{
    uint16_t res;

    res = a + b;
    if (((res ^ a) & 0x8000) && !((a ^ b) & 0x8000)) {
        if (a & 0x8000)
            res = 0x8000;
        else
            res = 0x7fff;
    }
    return res;
}

/* Perform 8-bit signed saturating addition.  */
static inline uint8_t add8_sat(uint8_t a, uint8_t b)
{
    uint8_t res;

    res = a + b;
    if (((res ^ a) & 0x80) && !((a ^ b) & 0x80)) {
        if (a & 0x80)
            res = 0x80;
        else
            res = 0x7f;
    }
    return res;
}

/* Perform 16-bit signed saturating subtraction.  */
static inline uint16_t sub16_sat(uint16_t a, uint16_t b)
{
    uint16_t res;

    res = a - b;
    if (((res ^ a) & 0x8000) && ((a ^ b) & 0x8000)) {
        if (a & 0x8000)
            res = 0x8000;
        else
            res = 0x7fff;
    }
    return res;
}

/* Perform 8-bit signed saturating subtraction.  */
static inline uint8_t sub8_sat(uint8_t a, uint8_t b)
{
    uint8_t res;

    res = a - b;
    if (((res ^ a) & 0x80) && ((a ^ b) & 0x80)) {
        if (a & 0x80)
            res = 0x80;
        else
            res = 0x7f;
    }
    return res;
}

#define ADD16(a, b, n) RESULT(add16_sat(a, b), n, 16);
#define SUB16(a, b, n) RESULT(sub16_sat(a, b), n, 16);
#define ADD8(a, b, n)  RESULT(add8_sat(a, b), n, 8);
#define SUB8(a, b, n)  RESULT(sub8_sat(a, b), n, 8);
#define PFX q

#include "op_addsub.h"

/* Unsigned saturating arithmetic.  */
static inline uint16_t add16_usat(uint16_t a, uint16_t b)
{
    uint16_t res;
    res = a + b;
    if (res < a)
        res = 0xffff;
    return res;
}

static inline uint16_t sub16_usat(uint16_t a, uint16_t b)
{
    if (a > b)
        return a - b;
    else
        return 0;
}

static inline uint8_t add8_usat(uint8_t a, uint8_t b)
{
    uint8_t res;
    res = a + b;
    if (res < a)
        res = 0xff;
    return res;
}

static inline uint8_t sub8_usat(uint8_t a, uint8_t b)
{
    if (a > b)
        return a - b;
    else
        return 0;
}

#define ADD16(a, b, n) RESULT(add16_usat(a, b), n, 16);
#define SUB16(a, b, n) RESULT(sub16_usat(a, b), n, 16);
#define ADD8(a, b, n)  RESULT(add8_usat(a, b), n, 8);
#define SUB8(a, b, n)  RESULT(sub8_usat(a, b), n, 8);
#define PFX uq

#include "op_addsub.h"

/* Signed modulo arithmetic.  */
#define SARITH16(a, b, n, op) do { \
    int32_t sum; \
    sum = (int32_t)(int16_t)(a) op (int32_t)(int16_t)(b); \
    RESULT(sum, n, 16); \
    if (sum >= 0) \
        ge |= 3 << (n * 2); \
    } while(0)

#define SARITH8(a, b, n, op) do { \
    int32_t sum; \
    sum = (int32_t)(int8_t)(a) op (int32_t)(int8_t)(b); \
    RESULT(sum, n, 8); \
    if (sum >= 0) \
        ge |= 1 << n; \
    } while(0)


#define ADD16(a, b, n) SARITH16(a, b, n, +)
#define SUB16(a, b, n) SARITH16(a, b, n, -)
#define ADD8(a, b, n)  SARITH8(a, b, n, +)
#define SUB8(a, b, n)  SARITH8(a, b, n, -)
#define PFX s
#define ARITH_GE

#include "op_addsub.h"

/* Unsigned modulo arithmetic.  */
#define ADD16(a, b, n) do { \
    uint32_t sum; \
    sum = (uint32_t)(uint16_t)(a) + (uint32_t)(uint16_t)(b); \
    RESULT(sum, n, 16); \
    if ((sum >> 16) == 1) \
        ge |= 3 << (n * 2); \
    } while(0)

#define ADD8(a, b, n) do { \
    uint32_t sum; \
    sum = (uint32_t)(uint8_t)(a) + (uint32_t)(uint8_t)(b); \
    RESULT(sum, n, 8); \
    if ((sum >> 8) == 1) \
        ge |= 1 << n; \
    } while(0)

#define SUB16(a, b, n) do { \
    uint32_t sum; \
    sum = (uint32_t)(uint16_t)(a) - (uint32_t)(uint16_t)(b); \
    RESULT(sum, n, 16); \
    if ((sum >> 16) == 0) \
        ge |= 3 << (n * 2); \
    } while(0)

#define SUB8(a, b, n) do { \
    uint32_t sum; \
    sum = (uint32_t)(uint8_t)(a) - (uint32_t)(uint8_t)(b); \
    RESULT(sum, n, 8); \
    if ((sum >> 8) == 0) \
        ge |= 1 << n; \
    } while(0)

#define PFX u
#define ARITH_GE

#include "op_addsub.h"

/* Halved signed arithmetic.  */
#define ADD16(a, b, n) \
  RESULT(((int32_t)(int16_t)(a) + (int32_t)(int16_t)(b)) >> 1, n, 16)
#define SUB16(a, b, n) \
  RESULT(((int32_t)(int16_t)(a) - (int32_t)(int16_t)(b)) >> 1, n, 16)
#define ADD8(a, b, n) \
  RESULT(((int32_t)(int8_t)(a) + (int32_t)(int8_t)(b)) >> 1, n, 8)
#define SUB8(a, b, n) \
  RESULT(((int32_t)(int8_t)(a) - (int32_t)(int8_t)(b)) >> 1, n, 8)
#define PFX sh

#include "op_addsub.h"

/* Halved unsigned arithmetic.  */
#define ADD16(a, b, n) \
  RESULT(((uint32_t)(uint16_t)(a) + (uint32_t)(uint16_t)(b)) >> 1, n, 16)
#define SUB16(a, b, n) \
  RESULT(((uint32_t)(uint16_t)(a) - (uint32_t)(uint16_t)(b)) >> 1, n, 16)
#define ADD8(a, b, n) \
  RESULT(((uint32_t)(uint8_t)(a) + (uint32_t)(uint8_t)(b)) >> 1, n, 8)
#define SUB8(a, b, n) \
  RESULT(((uint32_t)(uint8_t)(a) - (uint32_t)(uint8_t)(b)) >> 1, n, 8)
#define PFX uh

#include "op_addsub.h"

static inline uint8_t do_usad(uint8_t a, uint8_t b)
{
    if (a > b)
        return a - b;
    else
        return b - a;
}

/* Unsigned sum of absolute byte differences.  */
uint32_t HELPER(usad8)(uint32_t a, uint32_t b)
{
    uint32_t sum;
    sum = do_usad(a, b);
    sum += do_usad(a >> 8, b >> 8);
    sum += do_usad(a >> 16, b >> 16);
    sum += do_usad(a >> 24, b >> 24);
    return sum;
}

/* For ARMv6 SEL instruction.  */
uint32_t HELPER(sel_flags)(uint32_t flags, uint32_t a, uint32_t b)
{
    uint32_t mask;

    mask = 0;
    if (flags & 1)
        mask |= 0xff;
    if (flags & 2)
        mask |= 0xff00;
    if (flags & 4)
        mask |= 0xff0000;
    if (flags & 8)
        mask |= 0xff000000;
    return (a & mask) | (b & ~mask);
}

/* CRC helpers.
 * The upper bytes of val (above the number specified by 'bytes') must have
 * been zeroed out by the caller.
 */
uint32_t HELPER(crc32)(uint32_t acc, uint32_t val, uint32_t bytes)
{
    uint8_t buf[4];

    stl_le_p(buf, val);

    /* zlib crc32 converts the accumulator and output to one's complement.  */
    return crc32(acc ^ 0xffffffff, buf, bytes) ^ 0xffffffff;
}

uint32_t HELPER(crc32c)(uint32_t acc, uint32_t val, uint32_t bytes)
{
    uint8_t buf[4];

    stl_le_p(buf, val);

    /* Linux crc32c converts the output to one's complement.  */
    return crc32c(acc, buf, bytes) ^ 0xffffffff;
}

#ifndef CONFIG_USER_ONLY
ARMMMUIdx arm_stage1_mmu_idx(CPUARMState *env)
{
    return stage_1_mmu_idx(arm_mmu_idx(env));
}
#endif

static CPUARMTBFlags rebuild_hflags_common(CPUARMState *env, int fp_el,
                                           ARMMMUIdx mmu_idx,
                                           CPUARMTBFlags flags)
{
    DP_TBFLAG_ANY(flags, FPEXC_EL, fp_el);
    DP_TBFLAG_ANY(flags, MMUIDX, arm_to_core_mmu_idx(mmu_idx));

    if (arm_singlestep_active(env)) {
        DP_TBFLAG_ANY(flags, SS_ACTIVE, 1);
    }
    return flags;
}

static CPUARMTBFlags rebuild_hflags_common_32(CPUARMState *env, int fp_el,
                                              ARMMMUIdx mmu_idx,
                                              CPUARMTBFlags flags)
{
    bool sctlr_b = arm_sctlr_b(env);

    if (sctlr_b) {
        DP_TBFLAG_A32(flags, SCTLR__B, 1);
    }
    if (arm_cpu_data_is_big_endian_a32(env, sctlr_b)) {
        DP_TBFLAG_ANY(flags, BE_DATA, 1);
    }
    DP_TBFLAG_A32(flags, NS, !access_secure_reg(env));

    return rebuild_hflags_common(env, fp_el, mmu_idx, flags);
}

static CPUARMTBFlags rebuild_hflags_m32(CPUARMState *env, int fp_el,
                                        ARMMMUIdx mmu_idx)
{
    CPUARMTBFlags flags = {};
    uint32_t ccr = env->v7m.ccr[env->v7m.secure];

    /* Without HaveMainExt, CCR.UNALIGN_TRP is RES1. */
    if (ccr & R_V7M_CCR_UNALIGN_TRP_MASK) {
        DP_TBFLAG_ANY(flags, ALIGN_MEM, 1);
    }

    if (arm_v7m_is_handler_mode(env)) {
        DP_TBFLAG_M32(flags, HANDLER, 1);
    }

    /*
     * v8M always applies stack limit checks unless CCR.STKOFHFNMIGN
     * is suppressing them because the requested execution priority
     * is less than 0.
     */
    if (arm_feature(env, ARM_FEATURE_V8) &&
        !((mmu_idx & ARM_MMU_IDX_M_NEGPRI) &&
          (ccr & R_V7M_CCR_STKOFHFNMIGN_MASK))) {
        DP_TBFLAG_M32(flags, STACKCHECK, 1);
    }

    return rebuild_hflags_common_32(env, fp_el, mmu_idx, flags);
}

static CPUARMTBFlags rebuild_hflags_aprofile(CPUARMState *env)
{
    CPUARMTBFlags flags = {};

    DP_TBFLAG_ANY(flags, DEBUG_TARGET_EL, arm_debug_target_el(env));
    return flags;
}

static CPUARMTBFlags rebuild_hflags_a32(CPUARMState *env, int fp_el,
                                        ARMMMUIdx mmu_idx)
{
    CPUARMTBFlags flags = rebuild_hflags_aprofile(env);
    int el = arm_current_el(env);

    if (arm_sctlr(env, el) & SCTLR_A) {
        DP_TBFLAG_ANY(flags, ALIGN_MEM, 1);
    }

    if (arm_el_is_aa64(env, 1)) {
        DP_TBFLAG_A32(flags, VFPEN, 1);
    }

    if (el < 2 && env->cp15.hstr_el2 &&
        (arm_hcr_el2_eff(env) & (HCR_E2H | HCR_TGE)) != (HCR_E2H | HCR_TGE)) {
        DP_TBFLAG_A32(flags, HSTR_ACTIVE, 1);
    }

    return rebuild_hflags_common_32(env, fp_el, mmu_idx, flags);
}

#ifdef TARGET_AARCH64

static CPUARMTBFlags rebuild_hflags_a64(CPUARMState *env, int el, int fp_el,
                                        ARMMMUIdx mmu_idx)
{
    CPUARMTBFlags flags = rebuild_hflags_aprofile(env);
    ARMMMUIdx stage1 = stage_1_mmu_idx(mmu_idx);
    uint64_t tcr = regime_tcr(env, mmu_idx)->raw_tcr;
    uint64_t sctlr;
    int tbii, tbid;

    DP_TBFLAG_ANY(flags, AARCH64_STATE, 1);

    /* Get control bits for tagged addresses.  */
    tbid = aa64_va_parameter_tbi(tcr, mmu_idx);
    tbii = tbid & ~aa64_va_parameter_tbid(tcr, mmu_idx);

    DP_TBFLAG_A64(flags, TBII, tbii);
    DP_TBFLAG_A64(flags, TBID, tbid);

    if (cpu_isar_feature(aa64_sve, env_archcpu(env))) {
        int sve_el = sve_exception_el(env, el);
        uint32_t zcr_len;

        /*
         * If SVE is disabled, but FP is enabled,
         * then the effective len is 0.
         */
        if (sve_el != 0 && fp_el == 0) {
            zcr_len = 0;
        } else {
            zcr_len = cpu_sve_get_zcr_len_for_el(env, el);
        }
        DP_TBFLAG_A64(flags, SVEEXC_EL, sve_el);
        DP_TBFLAG_A64(flags, ZCR_LEN, zcr_len);
    }

    sctlr = regime_sctlr(env, stage1);

    if (sctlr & SCTLR_A) {
        DP_TBFLAG_ANY(flags, ALIGN_MEM, 1);
    }

    if (arm_cpu_data_is_big_endian_a64(el, sctlr)) {
        DP_TBFLAG_ANY(flags, BE_DATA, 1);
    }

    if (cpu_isar_feature(aa64_pauth, env_archcpu(env))) {
        /*
         * In order to save space in flags, we record only whether
         * pauth is "inactive", meaning all insns are implemented as
         * a nop, or "active" when some action must be performed.
         * The decision of which action to take is left to a helper.
         */
        if (sctlr & (SCTLR_EnIA | SCTLR_EnIB | SCTLR_EnDA | SCTLR_EnDB)) {
            DP_TBFLAG_A64(flags, PAUTH_ACTIVE, 1);
        }
    }

    if (cpu_isar_feature(aa64_bti, env_archcpu(env))) {
        /* Note that SCTLR_EL[23].BT == SCTLR_BT1.  */
        if (sctlr & (el == 0 ? SCTLR_BT0 : SCTLR_BT1)) {
            DP_TBFLAG_A64(flags, BT, 1);
        }
    }

    /* Compute the condition for using AccType_UNPRIV for LDTR et al. */
    if (!(env->pstate & PSTATE_UAO)) {
        switch (mmu_idx) {
        case ARMMMUIdx_E10_1:
        case ARMMMUIdx_E10_1_PAN:
        case ARMMMUIdx_SE10_1:
        case ARMMMUIdx_SE10_1_PAN:
            /* TODO: ARMv8.3-NV */
            DP_TBFLAG_A64(flags, UNPRIV, 1);
            break;
        case ARMMMUIdx_E20_2:
        case ARMMMUIdx_E20_2_PAN:
        case ARMMMUIdx_SE20_2:
        case ARMMMUIdx_SE20_2_PAN:
            /*
             * Note that EL20_2 is gated by HCR_EL2.E2H == 1, but EL20_0 is
             * gated by HCR_EL2.<E2H,TGE> == '11', and so is LDTR.
             */
            if (env->cp15.hcr_el2 & HCR_TGE) {
                DP_TBFLAG_A64(flags, UNPRIV, 1);
            }
            break;
        default:
            break;
        }
    }

    if (cpu_isar_feature(aa64_mte, env_archcpu(env))) {
        /*
         * Set MTE_ACTIVE if any access may be Checked, and leave clear
         * if all accesses must be Unchecked:
         * 1) If no TBI, then there are no tags in the address to check,
         * 2) If Tag Check Override, then all accesses are Unchecked,
         * 3) If Tag Check Fail == 0, then Checked access have no effect,
         * 4) If no Allocation Tag Access, then all accesses are Unchecked.
         */
        if (allocation_tag_access_enabled(env, el, sctlr)) {
            DP_TBFLAG_A64(flags, ATA, 1);
            if (tbid
                && !(env->pstate & PSTATE_TCO)
                && (sctlr & (el == 0 ? SCTLR_TCF0 : SCTLR_TCF))) {
                DP_TBFLAG_A64(flags, MTE_ACTIVE, 1);
            }
        }
        /* And again for unprivileged accesses, if required.  */
        if (EX_TBFLAG_A64(flags, UNPRIV)
            && tbid
            && !(env->pstate & PSTATE_TCO)
            && (sctlr & SCTLR_TCF0)
            && allocation_tag_access_enabled(env, 0, sctlr)) {
            DP_TBFLAG_A64(flags, MTE0_ACTIVE, 1);
        }
        /* Cache TCMA as well as TBI. */
        DP_TBFLAG_A64(flags, TCMA, aa64_va_parameter_tcma(tcr, mmu_idx));
    }

    return rebuild_hflags_common(env, fp_el, mmu_idx, flags);
}

#else

QEMU_ERROR("this should have been optimized away!")
CPUARMTBFlags rebuild_hflags_a64(CPUARMState *env, int el, int fp_el,
                                 ARMMMUIdx mmu_idx);

#endif /* TARGET_AARCH64 */

static CPUARMTBFlags rebuild_hflags_internal(CPUARMState *env)
{
    int el = arm_current_el(env);
    int fp_el = fp_exception_el(env, el);
    ARMMMUIdx mmu_idx = arm_mmu_idx_el(env, el);

    if (is_a64(env)) {
        return rebuild_hflags_a64(env, el, fp_el, mmu_idx);
    } else if (arm_feature(env, ARM_FEATURE_M)) {
        return rebuild_hflags_m32(env, fp_el, mmu_idx);
    } else {
        return rebuild_hflags_a32(env, fp_el, mmu_idx);
    }
}

void arm_rebuild_hflags(CPUARMState *env)
{
    env->hflags = rebuild_hflags_internal(env);
}

/*
 * If we have triggered a EL state change we can't rely on the
 * translator having passed it to us, we need to recompute.
 */
void HELPER(rebuild_hflags_m32_newel)(CPUARMState *env)
{
    int el = arm_current_el(env);
    int fp_el = fp_exception_el(env, el);
    ARMMMUIdx mmu_idx = arm_mmu_idx_el(env, el);

    env->hflags = rebuild_hflags_m32(env, fp_el, mmu_idx);
}

void HELPER(rebuild_hflags_m32)(CPUARMState *env, int el)
{
    int fp_el = fp_exception_el(env, el);
    ARMMMUIdx mmu_idx = arm_mmu_idx_el(env, el);

    env->hflags = rebuild_hflags_m32(env, fp_el, mmu_idx);
}

/*
 * If we have triggered a EL state change we can't rely on the
 * translator having passed it to us, we need to recompute.
 */
void HELPER(rebuild_hflags_a32_newel)(CPUARMState *env)
{
    int el = arm_current_el(env);
    int fp_el = fp_exception_el(env, el);
    ARMMMUIdx mmu_idx = arm_mmu_idx_el(env, el);
    env->hflags = rebuild_hflags_a32(env, fp_el, mmu_idx);
}

void HELPER(rebuild_hflags_a32)(CPUARMState *env, int el)
{
    int fp_el = fp_exception_el(env, el);
    ARMMMUIdx mmu_idx = arm_mmu_idx_el(env, el);

    env->hflags = rebuild_hflags_a32(env, fp_el, mmu_idx);
}

#ifdef TARGET_AARCH64
void HELPER(rebuild_hflags_a64)(CPUARMState *env, int el)
{
    int fp_el = fp_exception_el(env, el);
    ARMMMUIdx mmu_idx = arm_mmu_idx_el(env, el);

    env->hflags = rebuild_hflags_a64(env, el, fp_el, mmu_idx);
}
#endif /* TARGET_AARCH64 */

static inline void assert_hflags_rebuild_correctly(CPUARMState *env)
{
#ifdef CONFIG_DEBUG_TCG
    CPUARMTBFlags c = env->hflags;
    CPUARMTBFlags r = rebuild_hflags_internal(env);

    if (unlikely(c.flags != r.flags || c.flags2 != r.flags2)) {
        fprintf(stderr, "TCG hflags mismatch "
                        "(current:(0x%08x,0x" TARGET_FMT_lx ")"
                        " rebuilt:(0x%08x,0x" TARGET_FMT_lx ")\n",
                c.flags, c.flags2, r.flags, r.flags2);
        abort();
    }
#endif
}

void cpu_get_tb_cpu_state(CPUARMState *env, target_ulong *pc,
                          target_ulong *cs_base, uint32_t *pflags)
{
    CPUARMTBFlags flags;

    assert_hflags_rebuild_correctly(env);
    flags = env->hflags;

    if (EX_TBFLAG_ANY(flags, AARCH64_STATE)) {
        *pc = env->pc;
        if (cpu_isar_feature(aa64_bti, env_archcpu(env))) {
            DP_TBFLAG_A64(flags, BTYPE, env->btype);
        }
    } else {
        *pc = env->regs[15];

        if (arm_feature(env, ARM_FEATURE_M)) {
            if (arm_feature(env, ARM_FEATURE_M_SECURITY) &&
                FIELD_EX32(env->v7m.fpccr[M_REG_S], V7M_FPCCR, S)
                != env->v7m.secure) {
                DP_TBFLAG_M32(flags, FPCCR_S_WRONG, 1);
            }

            if ((env->v7m.fpccr[env->v7m.secure] & R_V7M_FPCCR_ASPEN_MASK) &&
                (!(env->v7m.control[M_REG_S] & R_V7M_CONTROL_FPCA_MASK) ||
                 (env->v7m.secure &&
                  !(env->v7m.control[M_REG_S] & R_V7M_CONTROL_SFPA_MASK)))) {
                /*
                 * ASPEN is set, but FPCA/SFPA indicate that there is no
                 * active FP context; we must create a new FP context before
                 * executing any FP insn.
                 */
                DP_TBFLAG_M32(flags, NEW_FP_CTXT_NEEDED, 1);
            }

            bool is_secure = env->v7m.fpccr[M_REG_S] & R_V7M_FPCCR_S_MASK;
            if (env->v7m.fpccr[is_secure] & R_V7M_FPCCR_LSPACT_MASK) {
                DP_TBFLAG_M32(flags, LSPACT, 1);
            }
        } else {
            /*
             * Note that XSCALE_CPAR shares bits with VECSTRIDE.
             * Note that VECLEN+VECSTRIDE are RES0 for M-profile.
             */
            if (arm_feature(env, ARM_FEATURE_XSCALE)) {
                DP_TBFLAG_A32(flags, XSCALE_CPAR, env->cp15.c15_cpar);
            } else {
                DP_TBFLAG_A32(flags, VECLEN, env->vfp.vec_len);
                DP_TBFLAG_A32(flags, VECSTRIDE, env->vfp.vec_stride);
            }
            if (env->vfp.xregs[ARM_VFP_FPEXC] & (1 << 30)) {
                DP_TBFLAG_A32(flags, VFPEN, 1);
            }
        }

        DP_TBFLAG_AM32(flags, THUMB, env->thumb);
        DP_TBFLAG_AM32(flags, CONDEXEC, env->condexec_bits);
    }

    /*
     * The SS_ACTIVE and PSTATE_SS bits correspond to the state machine
     * states defined in the ARM ARM for software singlestep:
     *  SS_ACTIVE   PSTATE.SS   State
     *     0            x       Inactive (the TB flag for SS is always 0)
     *     1            0       Active-pending
     *     1            1       Active-not-pending
     * SS_ACTIVE is set in hflags; PSTATE__SS is computed every TB.
     */
    if (EX_TBFLAG_ANY(flags, SS_ACTIVE) && (env->pstate & PSTATE_SS)) {
        DP_TBFLAG_ANY(flags, PSTATE__SS, 1);
    }

    *pflags = flags.flags;
    *cs_base = flags.flags2;
}
