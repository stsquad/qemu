/*
 * ARM CP registers
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "trace.h"
#include "qemu/main-loop.h"
#include "exec/exec-all.h"
#include "hw/irq.h"
#include "qapi/error.h"
#include "qemu/guest-random.h"
#include "cpu-mmu.h"
#include "cpregs.h"

#define ARM_CPU_FREQ 1000000000 /* FIXME: 1 GHz, should be configurable */
#define PMCR_NUM_COUNTERS 4 /* QEMU IMPDEF choice */

static void *raw_ptr(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return (char *)env + ri->fieldoffset;
}

static void add_cpreg_to_list(gpointer key, gpointer opaque)
{
    ARMCPU *cpu = opaque;
    uint64_t regidx;
    const ARMCPRegInfo *ri;

    regidx = *(uint32_t *)key;
    ri = get_arm_cp_reginfo(cpu->cp_regs, regidx);

    if (!(ri->type & (ARM_CP_NO_RAW | ARM_CP_ALIAS))) {
        cpu->cpreg_indexes[cpu->cpreg_array_len] = cpreg_to_kvm_id(regidx);
        /* The value array need not be initialized at this point */
        cpu->cpreg_array_len++;
    }
}

static void count_cpreg(gpointer key, gpointer opaque)
{
    ARMCPU *cpu = opaque;
    uint64_t regidx;
    const ARMCPRegInfo *ri;

    regidx = *(uint32_t *)key;
    ri = get_arm_cp_reginfo(cpu->cp_regs, regidx);

    if (!(ri->type & (ARM_CP_NO_RAW | ARM_CP_ALIAS))) {
        cpu->cpreg_array_len++;
    }
}

static gint cpreg_key_compare(gconstpointer a, gconstpointer b)
{
    uint64_t aidx = cpreg_to_kvm_id(*(uint32_t *)a);
    uint64_t bidx = cpreg_to_kvm_id(*(uint32_t *)b);

    if (aidx > bidx) {
        return 1;
    }
    if (aidx < bidx) {
        return -1;
    }
    return 0;
}

void init_cpreg_list(ARMCPU *cpu)
{
    /*
     * Initialise the cpreg_tuples[] array based on the cp_regs hash.
     * Note that we require cpreg_tuples[] to be sorted by key ID.
     */
    GList *keys;
    int arraylen;

    keys = g_hash_table_get_keys(cpu->cp_regs);
    keys = g_list_sort(keys, cpreg_key_compare);

    cpu->cpreg_array_len = 0;

    g_list_foreach(keys, count_cpreg, cpu);

    arraylen = cpu->cpreg_array_len;
    cpu->cpreg_indexes = g_new(uint64_t, arraylen);
    cpu->cpreg_values = g_new(uint64_t, arraylen);
    cpu->cpreg_vmstate_indexes = g_new(uint64_t, arraylen);
    cpu->cpreg_vmstate_values = g_new(uint64_t, arraylen);
    cpu->cpreg_vmstate_array_len = cpu->cpreg_array_len;
    cpu->cpreg_array_len = 0;

    g_list_foreach(keys, add_cpreg_to_list, cpu);

    assert(cpu->cpreg_array_len == arraylen);

    g_list_free(keys);
}

/*
 * Some registers are not accessible from AArch32 EL3 if SCR.NS == 0.
 */
static CPAccessResult access_el3_aa32ns(CPUARMState *env,
                                        const ARMCPRegInfo *ri,
                                        bool isread)
{
    if (!is_a64(env) && arm_current_el(env) == 3 &&
        arm_is_secure_below_el3(env)) {
        return CP_ACCESS_TRAP_UNCATEGORIZED;
    }
    return CP_ACCESS_OK;
}

/*
 * Some secure-only AArch32 registers trap to EL3 if used from
 * Secure EL1 (but are just ordinary UNDEF in other non-EL3 contexts).
 * Note that an access from Secure EL1 can only happen if EL3 is AArch64.
 * We assume that the .access field is set to PL1_RW.
 */
static CPAccessResult access_trap_aa32s_el1(CPUARMState *env,
                                            const ARMCPRegInfo *ri,
                                            bool isread)
{
    if (arm_current_el(env) == 3) {
        return CP_ACCESS_OK;
    }
    if (arm_is_secure_below_el3(env)) {
        if (env->cp15.scr_el3 & SCR_EEL2) {
            return CP_ACCESS_TRAP_EL2;
        }
        return CP_ACCESS_TRAP_EL3;
    }
    /* This will be EL1 NS and EL2 NS, which just UNDEF */
    return CP_ACCESS_TRAP_UNCATEGORIZED;
}

static uint64_t arm_mdcr_el2_eff(CPUARMState *env)
{
    return arm_is_el2_enabled(env) ? env->cp15.mdcr_el2 : 0;
}

/*
 * Check for traps to "powerdown debug" registers, which are controlled
 * by MDCR.TDOSA
 */
static CPAccessResult access_tdosa(CPUARMState *env, const ARMCPRegInfo *ri,
                                   bool isread)
{
    int el = arm_current_el(env);
    uint64_t mdcr_el2 = arm_mdcr_el2_eff(env);
    bool mdcr_el2_tdosa = (mdcr_el2 & MDCR_TDOSA) || (mdcr_el2 & MDCR_TDE) ||
        (arm_hcr_el2_eff(env) & HCR_TGE);

    if (el < 2 && mdcr_el2_tdosa) {
        return CP_ACCESS_TRAP_EL2;
    }
    if (el < 3 && (env->cp15.mdcr_el3 & MDCR_TDOSA)) {
        return CP_ACCESS_TRAP_EL3;
    }
    return CP_ACCESS_OK;
}

/*
 * Check for traps to "debug ROM" registers, which are controlled
 * by MDCR_EL2.TDRA for EL2 but by the more general MDCR_EL3.TDA for EL3.
 */
static CPAccessResult access_tdra(CPUARMState *env, const ARMCPRegInfo *ri,
                                  bool isread)
{
    int el = arm_current_el(env);
    uint64_t mdcr_el2 = arm_mdcr_el2_eff(env);
    bool mdcr_el2_tdra = (mdcr_el2 & MDCR_TDRA) || (mdcr_el2 & MDCR_TDE) ||
        (arm_hcr_el2_eff(env) & HCR_TGE);

    if (el < 2 && mdcr_el2_tdra) {
        return CP_ACCESS_TRAP_EL2;
    }
    if (el < 3 && (env->cp15.mdcr_el3 & MDCR_TDA)) {
        return CP_ACCESS_TRAP_EL3;
    }
    return CP_ACCESS_OK;
}

/*
 * Check for traps to general debug registers, which are controlled
 * by MDCR_EL2.TDA for EL2 and MDCR_EL3.TDA for EL3.
 */
static CPAccessResult access_tda(CPUARMState *env, const ARMCPRegInfo *ri,
                                  bool isread)
{
    int el = arm_current_el(env);
    uint64_t mdcr_el2 = arm_mdcr_el2_eff(env);
    bool mdcr_el2_tda = (mdcr_el2 & MDCR_TDA) || (mdcr_el2 & MDCR_TDE) ||
        (arm_hcr_el2_eff(env) & HCR_TGE);

    if (el < 2 && mdcr_el2_tda) {
        return CP_ACCESS_TRAP_EL2;
    }
    if (el < 3 && (env->cp15.mdcr_el3 & MDCR_TDA)) {
        return CP_ACCESS_TRAP_EL3;
    }
    return CP_ACCESS_OK;
}

/*
 * Check for traps to performance monitor registers, which are controlled
 * by MDCR_EL2.TPM for EL2 and MDCR_EL3.TPM for EL3.
 */
static CPAccessResult access_tpm(CPUARMState *env, const ARMCPRegInfo *ri,
                                 bool isread)
{
    int el = arm_current_el(env);
    uint64_t mdcr_el2 = arm_mdcr_el2_eff(env);

    if (el < 2 && (mdcr_el2 & MDCR_TPM)) {
        return CP_ACCESS_TRAP_EL2;
    }
    if (el < 3 && (env->cp15.mdcr_el3 & MDCR_TPM)) {
        return CP_ACCESS_TRAP_EL3;
    }
    return CP_ACCESS_OK;
}

/* Check for traps from EL1 due to HCR_EL2.TVM and HCR_EL2.TRVM.  */
static CPAccessResult access_tvm_trvm(CPUARMState *env, const ARMCPRegInfo *ri,
                                      bool isread)
{
    if (arm_current_el(env) == 1) {
        uint64_t trap = isread ? HCR_TRVM : HCR_TVM;
        if (arm_hcr_el2_eff(env) & trap) {
            return CP_ACCESS_TRAP_EL2;
        }
    }
    return CP_ACCESS_OK;
}

/* Check for traps from EL1 due to HCR_EL2.TSW.  */
static CPAccessResult access_tsw(CPUARMState *env, const ARMCPRegInfo *ri,
                                 bool isread)
{
    if (arm_current_el(env) == 1 && (arm_hcr_el2_eff(env) & HCR_TSW)) {
        return CP_ACCESS_TRAP_EL2;
    }
    return CP_ACCESS_OK;
}

/* Check for traps from EL1 due to HCR_EL2.TACR.  */
static CPAccessResult access_tacr(CPUARMState *env, const ARMCPRegInfo *ri,
                                  bool isread)
{
    if (arm_current_el(env) == 1 && (arm_hcr_el2_eff(env) & HCR_TACR)) {
        return CP_ACCESS_TRAP_EL2;
    }
    return CP_ACCESS_OK;
}

/* Check for traps from EL1 due to HCR_EL2.TTLB. */
static CPAccessResult access_ttlb(CPUARMState *env, const ARMCPRegInfo *ri,
                                  bool isread)
{
    if (arm_current_el(env) == 1 && (arm_hcr_el2_eff(env) & HCR_TTLB)) {
        return CP_ACCESS_TRAP_EL2;
    }
    return CP_ACCESS_OK;
}

static void dacr_write(CPUARMState *env, const ARMCPRegInfo *ri, uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);

    raw_write(env, ri, value);
    tlb_flush(CPU(cpu)); /* Flush TLB as domain not tracked in TLB */
}

static void fcse_write(CPUARMState *env, const ARMCPRegInfo *ri, uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);

    if (raw_read(env, ri) != value) {
        /*
         * Unlike real hardware the qemu TLB uses virtual addresses,
         * not modified virtual addresses, so this causes a TLB flush.
         */
        tlb_flush(CPU(cpu));
        raw_write(env, ri, value);
    }
}

static void contextidr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);

    if (raw_read(env, ri) != value && !arm_feature(env, ARM_FEATURE_PMSA)
        && !extended_addresses_enabled(env)) {
        /*
         * For VMSA (when not using the LPAE long descriptor page table
         * format) this register includes the ASID, so do a TLB flush.
         * For PMSA it is purely a process ID and no action is needed.
         */
        tlb_flush(CPU(cpu));
    }
    raw_write(env, ri, value);
}

/* IS variants of TLB operations must affect all cores */
static void tlbiall_is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    CPUState *cs = env_cpu(env);

    tlb_flush_all_cpus_synced(cs);
}

static void tlbiasid_is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    CPUState *cs = env_cpu(env);

    tlb_flush_all_cpus_synced(cs);
}

static void tlbimva_is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    CPUState *cs = env_cpu(env);

    tlb_flush_page_all_cpus_synced(cs, value & TARGET_PAGE_MASK);
}

static void tlbimvaa_is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    CPUState *cs = env_cpu(env);

    tlb_flush_page_all_cpus_synced(cs, value & TARGET_PAGE_MASK);
}

/*
 * Non-IS variants of TLB operations are upgraded to
 * IS versions if we are at EL1 and HCR_EL2.FB is effectively set to
 * force broadcast of these operations.
 */
static bool tlb_force_broadcast(CPUARMState *env)
{
    return arm_current_el(env) == 1 && (arm_hcr_el2_eff(env) & HCR_FB);
}

static void tlbiall_write(CPUARMState *env, const ARMCPRegInfo *ri,
                          uint64_t value)
{
    /* Invalidate all (TLBIALL) */
    CPUState *cs = env_cpu(env);

    if (tlb_force_broadcast(env)) {
        tlb_flush_all_cpus_synced(cs);
    } else {
        tlb_flush(cs);
    }
}

static void tlbimva_write(CPUARMState *env, const ARMCPRegInfo *ri,
                          uint64_t value)
{
    /* Invalidate single TLB entry by MVA and ASID (TLBIMVA) */
    CPUState *cs = env_cpu(env);

    value &= TARGET_PAGE_MASK;
    if (tlb_force_broadcast(env)) {
        tlb_flush_page_all_cpus_synced(cs, value);
    } else {
        tlb_flush_page(cs, value);
    }
}

static void tlbiasid_write(CPUARMState *env, const ARMCPRegInfo *ri,
                           uint64_t value)
{
    /* Invalidate by ASID (TLBIASID) */
    CPUState *cs = env_cpu(env);

    if (tlb_force_broadcast(env)) {
        tlb_flush_all_cpus_synced(cs);
    } else {
        tlb_flush(cs);
    }
}

static void tlbimvaa_write(CPUARMState *env, const ARMCPRegInfo *ri,
                           uint64_t value)
{
    /* Invalidate single entry by MVA, all ASIDs (TLBIMVAA) */
    CPUState *cs = env_cpu(env);

    value &= TARGET_PAGE_MASK;
    if (tlb_force_broadcast(env)) {
        tlb_flush_page_all_cpus_synced(cs, value);
    } else {
        tlb_flush_page(cs, value);
    }
}

static void tlbiall_nsnh_write(CPUARMState *env, const ARMCPRegInfo *ri,
                               uint64_t value)
{
    CPUState *cs = env_cpu(env);

    tlb_flush_by_mmuidx(cs,
                        ARMMMUIdxBit_E10_1 |
                        ARMMMUIdxBit_E10_1_PAN |
                        ARMMMUIdxBit_E10_0);
}

static void tlbiall_nsnh_is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                  uint64_t value)
{
    CPUState *cs = env_cpu(env);

    tlb_flush_by_mmuidx_all_cpus_synced(cs,
                                        ARMMMUIdxBit_E10_1 |
                                        ARMMMUIdxBit_E10_1_PAN |
                                        ARMMMUIdxBit_E10_0);
}


static void tlbiall_hyp_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    CPUState *cs = env_cpu(env);

    tlb_flush_by_mmuidx(cs, ARMMMUIdxBit_E2);
}

static void tlbiall_hyp_is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                 uint64_t value)
{
    CPUState *cs = env_cpu(env);

    tlb_flush_by_mmuidx_all_cpus_synced(cs, ARMMMUIdxBit_E2);
}

static void tlbimva_hyp_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    CPUState *cs = env_cpu(env);
    uint64_t pageaddr = value & ~MAKE_64BIT_MASK(0, 12);

    tlb_flush_page_by_mmuidx(cs, pageaddr, ARMMMUIdxBit_E2);
}

static void tlbimva_hyp_is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                 uint64_t value)
{
    CPUState *cs = env_cpu(env);
    uint64_t pageaddr = value & ~MAKE_64BIT_MASK(0, 12);

    tlb_flush_page_by_mmuidx_all_cpus_synced(cs, pageaddr,
                                             ARMMMUIdxBit_E2);
}

static const ARMCPRegInfo cp_reginfo[] = {
    /*
     * Define the secure and non-secure FCSE identifier CP registers
     * separately because there is no secure bank in V8 (no _EL3).  This allows
     * the secure register to be properly reset and migrated. There is also no
     * v8 EL1 version of the register so the non-secure instance stands alone.
     */
    { .name = "FCSEIDR",
      .cp = 15, .opc1 = 0, .crn = 13, .crm = 0, .opc2 = 0,
      .access = PL1_RW, .secure = ARM_CP_SECSTATE_NS,
      .fieldoffset = offsetof(CPUARMState, cp15.fcseidr_ns),
      .resetvalue = 0, .writefn = fcse_write, .raw_writefn = raw_write, },
    { .name = "FCSEIDR_S",
      .cp = 15, .opc1 = 0, .crn = 13, .crm = 0, .opc2 = 0,
      .access = PL1_RW, .secure = ARM_CP_SECSTATE_S,
      .fieldoffset = offsetof(CPUARMState, cp15.fcseidr_s),
      .resetvalue = 0, .writefn = fcse_write, .raw_writefn = raw_write, },
    /*
     * Define the secure and non-secure context identifier CP registers
     * separately because there is no secure bank in V8 (no _EL3).  This allows
     * the secure register to be properly reset and migrated.  In the
     * non-secure case, the 32-bit register will have reset and migration
     * disabled during registration as it is handled by the 64-bit instance.
     */
    { .name = "CONTEXTIDR_EL1", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 0, .crn = 13, .crm = 0, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .secure = ARM_CP_SECSTATE_NS,
      .fieldoffset = offsetof(CPUARMState, cp15.contextidr_el[1]),
      .resetvalue = 0, .writefn = contextidr_write, .raw_writefn = raw_write, },
    { .name = "CONTEXTIDR_S", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 0, .crn = 13, .crm = 0, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .secure = ARM_CP_SECSTATE_S,
      .fieldoffset = offsetof(CPUARMState, cp15.contextidr_s),
      .resetvalue = 0, .writefn = contextidr_write, .raw_writefn = raw_write, },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo not_v8_cp_reginfo[] = {
    /*
     * NB: Some of these registers exist in v8 but with more precise
     * definitions that don't use CP_ANY wildcards (mostly in v8_cp_reginfo[]).
     */
    /* MMU Domain access control / MPU write buffer control */
    { .name = "DACR",
      .cp = 15, .opc1 = CP_ANY, .crn = 3, .crm = CP_ANY, .opc2 = CP_ANY,
      .access = PL1_RW, .accessfn = access_tvm_trvm, .resetvalue = 0,
      .writefn = dacr_write, .raw_writefn = raw_write,
      .bank_fieldoffsets = { offsetoflow32(CPUARMState, cp15.dacr_s),
                             offsetoflow32(CPUARMState, cp15.dacr_ns) } },
    /*
     * ARMv7 allocates a range of implementation defined TLB LOCKDOWN regs.
     * For v6 and v5, these mappings are overly broad.
     */
    { .name = "TLB_LOCKDOWN", .cp = 15, .crn = 10, .crm = 0,
      .opc1 = CP_ANY, .opc2 = CP_ANY, .access = PL1_RW, .type = ARM_CP_NOP },
    { .name = "TLB_LOCKDOWN", .cp = 15, .crn = 10, .crm = 1,
      .opc1 = CP_ANY, .opc2 = CP_ANY, .access = PL1_RW, .type = ARM_CP_NOP },
    { .name = "TLB_LOCKDOWN", .cp = 15, .crn = 10, .crm = 4,
      .opc1 = CP_ANY, .opc2 = CP_ANY, .access = PL1_RW, .type = ARM_CP_NOP },
    { .name = "TLB_LOCKDOWN", .cp = 15, .crn = 10, .crm = 8,
      .opc1 = CP_ANY, .opc2 = CP_ANY, .access = PL1_RW, .type = ARM_CP_NOP },
    /* Cache maintenance ops; some of this space may be overridden later. */
    { .name = "CACHEMAINT", .cp = 15, .crn = 7, .crm = CP_ANY,
      .opc1 = 0, .opc2 = CP_ANY, .access = PL1_W,
      .type = ARM_CP_NOP | ARM_CP_OVERRIDE },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo not_v6_cp_reginfo[] = {
    /*
     * Not all pre-v6 cores implemented this WFI, so this is slightly
     * over-broad.
     */
    { .name = "WFI_v5", .cp = 15, .crn = 7, .crm = 8, .opc1 = 0, .opc2 = 2,
      .access = PL1_W, .type = ARM_CP_WFI },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo not_v7_cp_reginfo[] = {
    /*
     * Standard v6 WFI (also used in some pre-v6 cores); not in v7 (which
     * is UNPREDICTABLE; we choose to NOP as most implementations do).
     */
    { .name = "WFI_v6", .cp = 15, .crn = 7, .crm = 0, .opc1 = 0, .opc2 = 4,
      .access = PL1_W, .type = ARM_CP_WFI },
    /*
     * L1 cache lockdown. Not architectural in v6 and earlier but in practice
     * implemented in 926, 946, 1026, 1136, 1176 and 11MPCore. StrongARM and
     * OMAPCP will override this space.
     */
    { .name = "DLOCKDOWN", .cp = 15, .crn = 9, .crm = 0, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW, .fieldoffset = offsetof(CPUARMState, cp15.c9_data),
      .resetvalue = 0 },
    { .name = "ILOCKDOWN", .cp = 15, .crn = 9, .crm = 0, .opc1 = 0, .opc2 = 1,
      .access = PL1_RW, .fieldoffset = offsetof(CPUARMState, cp15.c9_insn),
      .resetvalue = 0 },
    /* v6 doesn't have the cache ID registers but Linux reads them anyway */
    { .name = "DUMMY", .cp = 15, .crn = 0, .crm = 0, .opc1 = 1, .opc2 = CP_ANY,
      .access = PL1_R, .type = ARM_CP_CONST | ARM_CP_NO_RAW,
      .resetvalue = 0 },
    /*
     * We don't implement pre-v7 debug but most CPUs had at least a DBGDIDR;
     * implementing it as RAZ means the "debug architecture version" bits
     * will read as a reserved value, which should cause Linux to not try
     * to use the debug hardware.
     */
    { .name = "DBGDIDR", .cp = 14, .crn = 0, .crm = 0, .opc1 = 0, .opc2 = 0,
      .access = PL0_R, .type = ARM_CP_CONST, .resetvalue = 0 },
    /*
     * MMU TLB control. Note that the wildcarding means we cover not just
     * the unified TLB ops but also the dside/iside/inner-shareable variants.
     */
    { .name = "TLBIALL", .cp = 15, .crn = 8, .crm = CP_ANY,
      .opc1 = CP_ANY, .opc2 = 0, .access = PL1_W, .writefn = tlbiall_write,
      .type = ARM_CP_NO_RAW },
    { .name = "TLBIMVA", .cp = 15, .crn = 8, .crm = CP_ANY,
      .opc1 = CP_ANY, .opc2 = 1, .access = PL1_W, .writefn = tlbimva_write,
      .type = ARM_CP_NO_RAW },
    { .name = "TLBIASID", .cp = 15, .crn = 8, .crm = CP_ANY,
      .opc1 = CP_ANY, .opc2 = 2, .access = PL1_W, .writefn = tlbiasid_write,
      .type = ARM_CP_NO_RAW },
    { .name = "TLBIMVAA", .cp = 15, .crn = 8, .crm = CP_ANY,
      .opc1 = CP_ANY, .opc2 = 3, .access = PL1_W, .writefn = tlbimvaa_write,
      .type = ARM_CP_NO_RAW },
    { .name = "PRRR", .cp = 15, .crn = 10, .crm = 2,
      .opc1 = 0, .opc2 = 0, .access = PL1_RW, .type = ARM_CP_NOP },
    { .name = "NMRR", .cp = 15, .crn = 10, .crm = 2,
      .opc1 = 0, .opc2 = 1, .access = PL1_RW, .type = ARM_CP_NOP },
    REGINFO_SENTINEL
};

static void cpacr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                        uint64_t value)
{
    uint32_t mask = 0;

    /* In ARMv8 most bits of CPACR_EL1 are RES0. */
    if (!arm_feature(env, ARM_FEATURE_V8)) {
        /*
         * ARMv7 defines bits for unimplemented coprocessors as RAZ/WI.
         * ASEDIS [31] and D32DIS [30] are both UNK/SBZP without VFP.
         * TRCDIS [28] is RAZ/WI since we do not implement a trace macrocell.
         */
        if (cpu_isar_feature(aa32_vfp_simd, env_archcpu(env))) {
            /* VFP coprocessor: cp10 & cp11 [23:20] */
            mask |= (1 << 31) | (1 << 30) | (0xf << 20);

            if (!arm_feature(env, ARM_FEATURE_NEON)) {
                /* ASEDIS [31] bit is RAO/WI */
                value |= (1 << 31);
            }

            /*
             * VFPv3 and upwards with NEON implement 32 double precision
             * registers (D0-D31).
             */
            if (!cpu_isar_feature(aa32_simd_r32, env_archcpu(env))) {
                /* D32DIS [30] is RAO/WI if D16-31 are not implemented. */
                value |= (1 << 30);
            }
        }
        value &= mask;
    }

    /*
     * For A-profile AArch32 EL3 (but not M-profile secure mode), if NSACR.CP10
     * is 0 then CPACR.{CP11,CP10} ignore writes and read as 0b00.
     */
    if (arm_feature(env, ARM_FEATURE_EL3) && !arm_el_is_aa64(env, 3) &&
        !arm_is_secure(env) && !extract32(env->cp15.nsacr, 10, 1)) {
        value &= ~(0xf << 20);
        value |= env->cp15.cpacr_el1 & (0xf << 20);
    }

    env->cp15.cpacr_el1 = value;
}

static uint64_t cpacr_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    /*
     * For A-profile AArch32 EL3 (but not M-profile secure mode), if NSACR.CP10
     * is 0 then CPACR.{CP11,CP10} ignore writes and read as 0b00.
     */
    uint64_t value = env->cp15.cpacr_el1;

    if (arm_feature(env, ARM_FEATURE_EL3) && !arm_el_is_aa64(env, 3) &&
        !arm_is_secure(env) && !extract32(env->cp15.nsacr, 10, 1)) {
        value &= ~(0xf << 20);
    }
    return value;
}


static void cpacr_reset(CPUARMState *env, const ARMCPRegInfo *ri)
{
    /*
     * Call cpacr_write() so that we reset with the correct RAO bits set
     * for our CPU features.
     */
    cpacr_write(env, ri, 0);
}

static CPAccessResult cpacr_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                   bool isread)
{
    if (arm_feature(env, ARM_FEATURE_V8)) {
        /* Check if CPACR accesses are to be trapped to EL2 */
        if (arm_current_el(env) == 1 && arm_is_el2_enabled(env) &&
            (env->cp15.cptr_el[2] & CPTR_TCPAC)) {
            return CP_ACCESS_TRAP_EL2;
        /* Check if CPACR accesses are to be trapped to EL3 */
        } else if (arm_current_el(env) < 3 &&
                   (env->cp15.cptr_el[3] & CPTR_TCPAC)) {
            return CP_ACCESS_TRAP_EL3;
        }
    }

    return CP_ACCESS_OK;
}

static CPAccessResult cptr_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                  bool isread)
{
    /* Check if CPTR accesses are set to trap to EL3 */
    if (arm_current_el(env) == 2 && (env->cp15.cptr_el[3] & CPTR_TCPAC)) {
        return CP_ACCESS_TRAP_EL3;
    }

    return CP_ACCESS_OK;
}

static const ARMCPRegInfo v6_cp_reginfo[] = {
    /* prefetch by MVA in v6, NOP in v7 */
    { .name = "MVA_prefetch",
      .cp = 15, .crn = 7, .crm = 13, .opc1 = 0, .opc2 = 1,
      .access = PL1_W, .type = ARM_CP_NOP },
    /*
     * We need to break the TB after ISB to execute self-modifying code
     * correctly and also to take any pending interrupts immediately.
     * So use arm_cp_write_ignore() function instead of ARM_CP_NOP flag.
     */
    { .name = "ISB", .cp = 15, .crn = 7, .crm = 5, .opc1 = 0, .opc2 = 4,
      .access = PL0_W, .type = ARM_CP_NO_RAW, .writefn = arm_cp_write_ignore },
    { .name = "DSB", .cp = 15, .crn = 7, .crm = 10, .opc1 = 0, .opc2 = 4,
      .access = PL0_W, .type = ARM_CP_NOP },
    { .name = "DMB", .cp = 15, .crn = 7, .crm = 10, .opc1 = 0, .opc2 = 5,
      .access = PL0_W, .type = ARM_CP_NOP },
    { .name = "IFAR", .cp = 15, .crn = 6, .crm = 0, .opc1 = 0, .opc2 = 2,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .bank_fieldoffsets = { offsetof(CPUARMState, cp15.ifar_s),
                             offsetof(CPUARMState, cp15.ifar_ns) },
      .resetvalue = 0, },
    /*
     * Watchpoint Fault Address Register : should actually only be present
     * for 1136, 1176, 11MPCore.
     */
    { .name = "WFAR", .cp = 15, .crn = 6, .crm = 0, .opc1 = 0, .opc2 = 1,
      .access = PL1_RW, .type = ARM_CP_CONST, .resetvalue = 0, },
    { .name = "CPACR", .state = ARM_CP_STATE_BOTH, .opc0 = 3,
      .crn = 1, .crm = 0, .opc1 = 0, .opc2 = 2, .accessfn = cpacr_access,
      .access = PL1_RW, .fieldoffset = offsetof(CPUARMState, cp15.cpacr_el1),
      .resetfn = cpacr_reset, .writefn = cpacr_write, .readfn = cpacr_read },
    REGINFO_SENTINEL
};

/* Definitions for the PMU registers */
#define PMCRN_MASK  0xf800
#define PMCRN_SHIFT 11
#define PMCRLC  0x40
#define PMCRDP  0x20
#define PMCRX   0x10
#define PMCRD   0x8
#define PMCRC   0x4
#define PMCRP   0x2
#define PMCRE   0x1
/*
 * Mask of PMCR bits writeable by guest (not including WO bits like C, P,
 * which can be written as 1 to trigger behaviour but which stay RAZ).
 */
#define PMCR_WRITEABLE_MASK (PMCRLC | PMCRDP | PMCRX | PMCRD | PMCRE)

#define PMXEVTYPER_P          0x80000000
#define PMXEVTYPER_U          0x40000000
#define PMXEVTYPER_NSK        0x20000000
#define PMXEVTYPER_NSU        0x10000000
#define PMXEVTYPER_NSH        0x08000000
#define PMXEVTYPER_M          0x04000000
#define PMXEVTYPER_MT         0x02000000
#define PMXEVTYPER_EVTCOUNT   0x0000ffff
#define PMXEVTYPER_MASK       (PMXEVTYPER_P | PMXEVTYPER_U | PMXEVTYPER_NSK | \
                               PMXEVTYPER_NSU | PMXEVTYPER_NSH | \
                               PMXEVTYPER_M | PMXEVTYPER_MT | \
                               PMXEVTYPER_EVTCOUNT)

#define PMCCFILTR             0xf8000000
#define PMCCFILTR_M           PMXEVTYPER_M
#define PMCCFILTR_EL0         (PMCCFILTR | PMCCFILTR_M)

static inline uint32_t pmu_num_counters(CPUARMState *env)
{
    return (env->cp15.c9_pmcr & PMCRN_MASK) >> PMCRN_SHIFT;
}

/* Bits allowed to be set/cleared for PMCNTEN* and PMINTEN* */
static inline uint64_t pmu_counter_mask(CPUARMState *env)
{
  return (1 << 31) | ((1 << pmu_num_counters(env)) - 1);
}

typedef struct pm_event {
    uint16_t number; /* PMEVTYPER.evtCount is 16 bits wide */
    /* If the event is supported on this CPU (used to generate PMCEID[01]) */
    bool (*supported)(CPUARMState *);
    /*
     * Retrieve the current count of the underlying event. The programmed
     * counters hold a difference from the return value from this function
     */
    uint64_t (*get_count)(CPUARMState *);
    /*
     * Return how many nanoseconds it will take (at a minimum) for count events
     * to occur. A negative value indicates the counter will never overflow, or
     * that the counter has otherwise arranged for the overflow bit to be set
     * and the PMU interrupt to be raised on overflow.
     */
    int64_t (*ns_per_count)(uint64_t);
} pm_event;

static bool event_always_supported(CPUARMState *env)
{
    return true;
}

static uint64_t swinc_get_count(CPUARMState *env)
{
    /*
     * SW_INCR events are written directly to the pmevcntr's by writes to
     * PMSWINC, so there is no underlying count maintained by the PMU itself
     */
    return 0;
}

static int64_t swinc_ns_per(uint64_t ignored)
{
    return -1;
}

/*
 * Return the underlying cycle count for the PMU cycle counters. If we're in
 * usermode, simply return 0.
 */
static uint64_t cycles_get_count(CPUARMState *env)
{
#ifndef CONFIG_USER_ONLY
    return muldiv64(qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL),
                   ARM_CPU_FREQ, NANOSECONDS_PER_SECOND);
#else
    return cpu_get_host_ticks();
#endif
}

#ifndef CONFIG_USER_ONLY
static int64_t cycles_ns_per(uint64_t cycles)
{
    return (ARM_CPU_FREQ / NANOSECONDS_PER_SECOND) * cycles;
}

static bool instructions_supported(CPUARMState *env)
{
    return icount_enabled() == 1; /* Precise instruction counting */
}

static uint64_t instructions_get_count(CPUARMState *env)
{
    return (uint64_t)icount_get_raw();
}

static int64_t instructions_ns_per(uint64_t icount)
{
    return icount_to_ns((int64_t)icount);
}
#endif

static bool pmu_8_1_events_supported(CPUARMState *env)
{
    /* For events which are supported in any v8.1 PMU */
    return cpu_isar_feature(any_pmu_8_1, env_archcpu(env));
}

static bool pmu_8_4_events_supported(CPUARMState *env)
{
    /* For events which are supported in any v8.1 PMU */
    return cpu_isar_feature(any_pmu_8_4, env_archcpu(env));
}

static uint64_t zero_event_get_count(CPUARMState *env)
{
    /* For events which on QEMU never fire, so their count is always zero */
    return 0;
}

static int64_t zero_event_ns_per(uint64_t cycles)
{
    /* An event which never fires can never overflow */
    return -1;
}

static const pm_event pm_events[] = {
    { .number = 0x000, /* SW_INCR */
      .supported = event_always_supported,
      .get_count = swinc_get_count,
      .ns_per_count = swinc_ns_per,
    },
#ifndef CONFIG_USER_ONLY
    { .number = 0x008, /* INST_RETIRED, Instruction architecturally executed */
      .supported = instructions_supported,
      .get_count = instructions_get_count,
      .ns_per_count = instructions_ns_per,
    },
    { .number = 0x011, /* CPU_CYCLES, Cycle */
      .supported = event_always_supported,
      .get_count = cycles_get_count,
      .ns_per_count = cycles_ns_per,
    },
#endif
    { .number = 0x023, /* STALL_FRONTEND */
      .supported = pmu_8_1_events_supported,
      .get_count = zero_event_get_count,
      .ns_per_count = zero_event_ns_per,
    },
    { .number = 0x024, /* STALL_BACKEND */
      .supported = pmu_8_1_events_supported,
      .get_count = zero_event_get_count,
      .ns_per_count = zero_event_ns_per,
    },
    { .number = 0x03c, /* STALL */
      .supported = pmu_8_4_events_supported,
      .get_count = zero_event_get_count,
      .ns_per_count = zero_event_ns_per,
    },
};

/*
 * Note: Before increasing MAX_EVENT_ID beyond 0x3f into the 0x40xx range of
 * events (i.e. the statistical profiling extension), this implementation
 * should first be updated to something sparse instead of the current
 * supported_event_map[] array.
 */
#define MAX_EVENT_ID 0x3c
#define UNSUPPORTED_EVENT UINT16_MAX
static uint16_t supported_event_map[MAX_EVENT_ID + 1];

/*
 * Called upon CPU initialization to initialize PMCEID[01]_EL0 and build a map
 * of ARM event numbers to indices in our pm_events array.
 *
 * Note: Events in the 0x40XX range are not currently supported.
 */
void pmu_init(ARMCPU *cpu)
{
    unsigned int i;

    /*
     * Empty supported_event_map and cpu->pmceid[01] before adding supported
     * events to them
     */
    for (i = 0; i < ARRAY_SIZE(supported_event_map); i++) {
        supported_event_map[i] = UNSUPPORTED_EVENT;
    }
    cpu->pmceid0 = 0;
    cpu->pmceid1 = 0;

    for (i = 0; i < ARRAY_SIZE(pm_events); i++) {
        const pm_event *cnt = &pm_events[i];
        assert(cnt->number <= MAX_EVENT_ID);
        /* We do not currently support events in the 0x40xx range */
        assert(cnt->number <= 0x3f);

        if (cnt->supported(&cpu->env)) {
            supported_event_map[cnt->number] = i;
            uint64_t event_mask = 1ULL << (cnt->number & 0x1f);
            if (cnt->number & 0x20) {
                cpu->pmceid1 |= event_mask;
            } else {
                cpu->pmceid0 |= event_mask;
            }
        }
    }
}

/*
 * Check at runtime whether a PMU event is supported for the current machine
 */
static bool event_supported(uint16_t number)
{
    if (number > MAX_EVENT_ID) {
        return false;
    }
    return supported_event_map[number] != UNSUPPORTED_EVENT;
}

static CPAccessResult pmreg_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                   bool isread)
{
    /* Performance monitor registers user accessibility is controlled
     * by PMUSERENR. MDCR_EL2.TPM and MDCR_EL3.TPM allow configurable
     * trapping to EL2 or EL3 for other accesses.
     */
    int el = arm_current_el(env);
    uint64_t mdcr_el2 = arm_mdcr_el2_eff(env);

    if (el == 0 && !(env->cp15.c9_pmuserenr & 1)) {
        return CP_ACCESS_TRAP;
    }
    if (el < 2 && (mdcr_el2 & MDCR_TPM)) {
        return CP_ACCESS_TRAP_EL2;
    }
    if (el < 3 && (env->cp15.mdcr_el3 & MDCR_TPM)) {
        return CP_ACCESS_TRAP_EL3;
    }

    return CP_ACCESS_OK;
}

static CPAccessResult pmreg_access_xevcntr(CPUARMState *env,
                                           const ARMCPRegInfo *ri,
                                           bool isread)
{
    /* ER: event counter read trap control */
    if (arm_feature(env, ARM_FEATURE_V8)
        && arm_current_el(env) == 0
        && (env->cp15.c9_pmuserenr & (1 << 3)) != 0
        && isread) {
        return CP_ACCESS_OK;
    }

    return pmreg_access(env, ri, isread);
}

static CPAccessResult pmreg_access_swinc(CPUARMState *env,
                                         const ARMCPRegInfo *ri,
                                         bool isread)
{
    /* SW: software increment write trap control */
    if (arm_feature(env, ARM_FEATURE_V8)
        && arm_current_el(env) == 0
        && (env->cp15.c9_pmuserenr & (1 << 1)) != 0
        && !isread) {
        return CP_ACCESS_OK;
    }

    return pmreg_access(env, ri, isread);
}

static CPAccessResult pmreg_access_selr(CPUARMState *env,
                                        const ARMCPRegInfo *ri,
                                        bool isread)
{
    /* ER: event counter read trap control */
    if (arm_feature(env, ARM_FEATURE_V8)
        && arm_current_el(env) == 0
        && (env->cp15.c9_pmuserenr & (1 << 3)) != 0) {
        return CP_ACCESS_OK;
    }

    return pmreg_access(env, ri, isread);
}

static CPAccessResult pmreg_access_ccntr(CPUARMState *env,
                                         const ARMCPRegInfo *ri,
                                         bool isread)
{
    /* CR: cycle counter read trap control */
    if (arm_feature(env, ARM_FEATURE_V8)
        && arm_current_el(env) == 0
        && (env->cp15.c9_pmuserenr & (1 << 2)) != 0
        && isread) {
        return CP_ACCESS_OK;
    }

    return pmreg_access(env, ri, isread);
}

/* Returns true if the counter (pass 31 for PMCCNTR) should count events using
 * the current EL, security state, and register configuration.
 */
static bool pmu_counter_enabled(CPUARMState *env, uint8_t counter)
{
    uint64_t filter;
    bool e, p, u, nsk, nsu, nsh, m;
    bool enabled, prohibited, filtered;
    bool secure = arm_is_secure(env);
    int el = arm_current_el(env);
    uint64_t mdcr_el2 = arm_mdcr_el2_eff(env);
    uint8_t hpmn = mdcr_el2 & MDCR_HPMN;

    if (!arm_feature(env, ARM_FEATURE_PMU)) {
        return false;
    }

    if (!arm_feature(env, ARM_FEATURE_EL2) ||
            (counter < hpmn || counter == 31)) {
        e = env->cp15.c9_pmcr & PMCRE;
    } else {
        e = mdcr_el2 & MDCR_HPME;
    }
    enabled = e && (env->cp15.c9_pmcnten & (1 << counter));

    if (!secure) {
        if (el == 2 && (counter < hpmn || counter == 31)) {
            prohibited = mdcr_el2 & MDCR_HPMD;
        } else {
            prohibited = false;
        }
    } else {
        prohibited = arm_feature(env, ARM_FEATURE_EL3) &&
           !(env->cp15.mdcr_el3 & MDCR_SPME);
    }

    if (prohibited && counter == 31) {
        prohibited = env->cp15.c9_pmcr & PMCRDP;
    }

    if (counter == 31) {
        filter = env->cp15.pmccfiltr_el0;
    } else {
        filter = env->cp15.c14_pmevtyper[counter];
    }

    p   = filter & PMXEVTYPER_P;
    u   = filter & PMXEVTYPER_U;
    nsk = arm_feature(env, ARM_FEATURE_EL3) && (filter & PMXEVTYPER_NSK);
    nsu = arm_feature(env, ARM_FEATURE_EL3) && (filter & PMXEVTYPER_NSU);
    nsh = arm_feature(env, ARM_FEATURE_EL2) && (filter & PMXEVTYPER_NSH);
    m   = arm_el_is_aa64(env, 1) &&
              arm_feature(env, ARM_FEATURE_EL3) && (filter & PMXEVTYPER_M);

    if (el == 0) {
        filtered = secure ? u : u != nsu;
    } else if (el == 1) {
        filtered = secure ? p : p != nsk;
    } else if (el == 2) {
        filtered = !nsh;
    } else { /* EL3 */
        filtered = m != p;
    }

    if (counter != 31) {
        /*
         * If not checking PMCCNTR, ensure the counter is setup to an event we
         * support
         */
        uint16_t event = filter & PMXEVTYPER_EVTCOUNT;
        if (!event_supported(event)) {
            return false;
        }
    }

    return enabled && !prohibited && !filtered;
}

static void pmu_update_irq(CPUARMState *env)
{
    ARMCPU *cpu = env_archcpu(env);
    qemu_set_irq(cpu->pmu_interrupt, (env->cp15.c9_pmcr & PMCRE) &&
            (env->cp15.c9_pminten & env->cp15.c9_pmovsr));
}

/*
 * Ensure c15_ccnt is the guest-visible count so that operations such as
 * enabling/disabling the counter or filtering, modifying the count itself,
 * etc. can be done logically. This is essentially a no-op if the counter is
 * not enabled at the time of the call.
 */
static void pmccntr_op_start(CPUARMState *env)
{
    uint64_t cycles = cycles_get_count(env);

    if (pmu_counter_enabled(env, 31)) {
        uint64_t eff_cycles = cycles;
        if (env->cp15.c9_pmcr & PMCRD) {
            /* Increment once every 64 processor clock cycles */
            eff_cycles /= 64;
        }

        uint64_t new_pmccntr = eff_cycles - env->cp15.c15_ccnt_delta;

        uint64_t overflow_mask = env->cp15.c9_pmcr & PMCRLC ? \
                                 1ull << 63 : 1ull << 31;
        if (env->cp15.c15_ccnt & ~new_pmccntr & overflow_mask) {
            env->cp15.c9_pmovsr |= (1 << 31);
            pmu_update_irq(env);
        }

        env->cp15.c15_ccnt = new_pmccntr;
    }
    env->cp15.c15_ccnt_delta = cycles;
}

/*
 * If PMCCNTR is enabled, recalculate the delta between the clock and the
 * guest-visible count. A call to pmccntr_op_finish should follow every call to
 * pmccntr_op_start.
 */
static void pmccntr_op_finish(CPUARMState *env)
{
    if (pmu_counter_enabled(env, 31)) {
#ifndef CONFIG_USER_ONLY
        /* Calculate when the counter will next overflow */
        uint64_t remaining_cycles = -env->cp15.c15_ccnt;
        if (!(env->cp15.c9_pmcr & PMCRLC)) {
            remaining_cycles = (uint32_t)remaining_cycles;
        }
        int64_t overflow_in = cycles_ns_per(remaining_cycles);

        if (overflow_in > 0) {
            int64_t overflow_at = qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) +
                overflow_in;
            ARMCPU *cpu = env_archcpu(env);
            timer_mod_anticipate_ns(cpu->pmu_timer, overflow_at);
        }
#endif

        uint64_t prev_cycles = env->cp15.c15_ccnt_delta;
        if (env->cp15.c9_pmcr & PMCRD) {
            /* Increment once every 64 processor clock cycles */
            prev_cycles /= 64;
        }
        env->cp15.c15_ccnt_delta = prev_cycles - env->cp15.c15_ccnt;
    }
}

static void pmevcntr_op_start(CPUARMState *env, uint8_t counter)
{

    uint16_t event = env->cp15.c14_pmevtyper[counter] & PMXEVTYPER_EVTCOUNT;
    uint64_t count = 0;
    if (event_supported(event)) {
        uint16_t event_idx = supported_event_map[event];
        count = pm_events[event_idx].get_count(env);
    }

    if (pmu_counter_enabled(env, counter)) {
        uint32_t new_pmevcntr = count - env->cp15.c14_pmevcntr_delta[counter];

        if (env->cp15.c14_pmevcntr[counter] & ~new_pmevcntr & INT32_MIN) {
            env->cp15.c9_pmovsr |= (1 << counter);
            pmu_update_irq(env);
        }
        env->cp15.c14_pmevcntr[counter] = new_pmevcntr;
    }
    env->cp15.c14_pmevcntr_delta[counter] = count;
}

static void pmevcntr_op_finish(CPUARMState *env, uint8_t counter)
{
    if (pmu_counter_enabled(env, counter)) {
#ifndef CONFIG_USER_ONLY
        uint16_t event = env->cp15.c14_pmevtyper[counter] & PMXEVTYPER_EVTCOUNT;
        uint16_t event_idx = supported_event_map[event];
        uint64_t delta = UINT32_MAX -
            (uint32_t)env->cp15.c14_pmevcntr[counter] + 1;
        int64_t overflow_in = pm_events[event_idx].ns_per_count(delta);

        if (overflow_in > 0) {
            int64_t overflow_at = qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) +
                overflow_in;
            ARMCPU *cpu = env_archcpu(env);
            timer_mod_anticipate_ns(cpu->pmu_timer, overflow_at);
        }
#endif

        env->cp15.c14_pmevcntr_delta[counter] -=
            env->cp15.c14_pmevcntr[counter];
    }
}

void pmu_op_start(CPUARMState *env)
{
    unsigned int i;
    pmccntr_op_start(env);
    for (i = 0; i < pmu_num_counters(env); i++) {
        pmevcntr_op_start(env, i);
    }
}

void pmu_op_finish(CPUARMState *env)
{
    unsigned int i;
    pmccntr_op_finish(env);
    for (i = 0; i < pmu_num_counters(env); i++) {
        pmevcntr_op_finish(env, i);
    }
}

void pmu_pre_el_change(ARMCPU *cpu, void *ignored)
{
    pmu_op_start(&cpu->env);
}

void pmu_post_el_change(ARMCPU *cpu, void *ignored)
{
    pmu_op_finish(&cpu->env);
}

void arm_pmu_timer_cb(void *opaque)
{
    ARMCPU *cpu = opaque;

    /*
     * Update all the counter values based on the current underlying counts,
     * triggering interrupts to be raised, if necessary. pmu_op_finish() also
     * has the effect of setting the cpu->pmu_timer to the next earliest time a
     * counter may expire.
     */
    pmu_op_start(&cpu->env);
    pmu_op_finish(&cpu->env);
}

static void pmcr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                       uint64_t value)
{
    pmu_op_start(env);

    if (value & PMCRC) {
        /* The counter has been reset */
        env->cp15.c15_ccnt = 0;
    }

    if (value & PMCRP) {
        unsigned int i;
        for (i = 0; i < pmu_num_counters(env); i++) {
            env->cp15.c14_pmevcntr[i] = 0;
        }
    }

    env->cp15.c9_pmcr &= ~PMCR_WRITEABLE_MASK;
    env->cp15.c9_pmcr |= (value & PMCR_WRITEABLE_MASK);

    pmu_op_finish(env);
}

static void pmswinc_write(CPUARMState *env, const ARMCPRegInfo *ri,
                          uint64_t value)
{
    unsigned int i;
    for (i = 0; i < pmu_num_counters(env); i++) {
        /* Increment a counter's count iff: */
        if ((value & (1 << i)) && /* counter's bit is set */
                /* counter is enabled and not filtered */
                pmu_counter_enabled(env, i) &&
                /* counter is SW_INCR */
                (env->cp15.c14_pmevtyper[i] & PMXEVTYPER_EVTCOUNT) == 0x0) {
            pmevcntr_op_start(env, i);

            /*
             * Detect if this write causes an overflow since we can't predict
             * PMSWINC overflows like we can for other events
             */
            uint32_t new_pmswinc = env->cp15.c14_pmevcntr[i] + 1;

            if (env->cp15.c14_pmevcntr[i] & ~new_pmswinc & INT32_MIN) {
                env->cp15.c9_pmovsr |= (1 << i);
                pmu_update_irq(env);
            }

            env->cp15.c14_pmevcntr[i] = new_pmswinc;

            pmevcntr_op_finish(env, i);
        }
    }
}

static uint64_t pmccntr_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    uint64_t ret;
    pmccntr_op_start(env);
    ret = env->cp15.c15_ccnt;
    pmccntr_op_finish(env);
    return ret;
}

static void pmselr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
    /* The value of PMSELR.SEL affects the behavior of PMXEVTYPER and
     * PMXEVCNTR. We allow [0..31] to be written to PMSELR here; in the
     * meanwhile, we check PMSELR.SEL when PMXEVTYPER and PMXEVCNTR are
     * accessed.
     */
    env->cp15.c9_pmselr = value & 0x1f;
}

static void pmccntr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                        uint64_t value)
{
    pmccntr_op_start(env);
    env->cp15.c15_ccnt = value;
    pmccntr_op_finish(env);
}

static void pmccntr_write32(CPUARMState *env, const ARMCPRegInfo *ri,
                            uint64_t value)
{
    uint64_t cur_val = pmccntr_read(env, NULL);

    pmccntr_write(env, ri, deposit64(cur_val, 0, 32, value));
}

static void pmccfiltr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                            uint64_t value)
{
    pmccntr_op_start(env);
    env->cp15.pmccfiltr_el0 = value & PMCCFILTR_EL0;
    pmccntr_op_finish(env);
}

static void pmccfiltr_write_a32(CPUARMState *env, const ARMCPRegInfo *ri,
                            uint64_t value)
{
    pmccntr_op_start(env);
    /* M is not accessible from AArch32 */
    env->cp15.pmccfiltr_el0 = (env->cp15.pmccfiltr_el0 & PMCCFILTR_M) |
        (value & PMCCFILTR);
    pmccntr_op_finish(env);
}

static uint64_t pmccfiltr_read_a32(CPUARMState *env, const ARMCPRegInfo *ri)
{
    /* M is not visible in AArch32 */
    return env->cp15.pmccfiltr_el0 & PMCCFILTR;
}

static void pmcntenset_write(CPUARMState *env, const ARMCPRegInfo *ri,
                            uint64_t value)
{
    value &= pmu_counter_mask(env);
    env->cp15.c9_pmcnten |= value;
}

static void pmcntenclr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    value &= pmu_counter_mask(env);
    env->cp15.c9_pmcnten &= ~value;
}

static void pmovsr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
    value &= pmu_counter_mask(env);
    env->cp15.c9_pmovsr &= ~value;
    pmu_update_irq(env);
}

static void pmovsset_write(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
    value &= pmu_counter_mask(env);
    env->cp15.c9_pmovsr |= value;
    pmu_update_irq(env);
}

static void pmevtyper_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value, const uint8_t counter)
{
    if (counter == 31) {
        pmccfiltr_write(env, ri, value);
    } else if (counter < pmu_num_counters(env)) {
        pmevcntr_op_start(env, counter);

        /*
         * If this counter's event type is changing, store the current
         * underlying count for the new type in c14_pmevcntr_delta[counter] so
         * pmevcntr_op_finish has the correct baseline when it converts back to
         * a delta.
         */
        uint16_t old_event = env->cp15.c14_pmevtyper[counter] &
            PMXEVTYPER_EVTCOUNT;
        uint16_t new_event = value & PMXEVTYPER_EVTCOUNT;
        if (old_event != new_event) {
            uint64_t count = 0;
            if (event_supported(new_event)) {
                uint16_t event_idx = supported_event_map[new_event];
                count = pm_events[event_idx].get_count(env);
            }
            env->cp15.c14_pmevcntr_delta[counter] = count;
        }

        env->cp15.c14_pmevtyper[counter] = value & PMXEVTYPER_MASK;
        pmevcntr_op_finish(env, counter);
    }
    /* Attempts to access PMXEVTYPER are CONSTRAINED UNPREDICTABLE when
     * PMSELR value is equal to or greater than the number of implemented
     * counters, but not equal to 0x1f. We opt to behave as a RAZ/WI.
     */
}

static uint64_t pmevtyper_read(CPUARMState *env, const ARMCPRegInfo *ri,
                               const uint8_t counter)
{
    if (counter == 31) {
        return env->cp15.pmccfiltr_el0;
    } else if (counter < pmu_num_counters(env)) {
        return env->cp15.c14_pmevtyper[counter];
    } else {
      /*
       * We opt to behave as a RAZ/WI when attempts to access PMXEVTYPER
       * are CONSTRAINED UNPREDICTABLE. See comments in pmevtyper_write().
       */
        return 0;
    }
}

static void pmevtyper_writefn(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    uint8_t counter = ((ri->crm & 3) << 3) | (ri->opc2 & 7);
    pmevtyper_write(env, ri, value, counter);
}

static void pmevtyper_rawwrite(CPUARMState *env, const ARMCPRegInfo *ri,
                               uint64_t value)
{
    uint8_t counter = ((ri->crm & 3) << 3) | (ri->opc2 & 7);
    env->cp15.c14_pmevtyper[counter] = value;

    /*
     * pmevtyper_rawwrite is called between a pair of pmu_op_start and
     * pmu_op_finish calls when loading saved state for a migration. Because
     * we're potentially updating the type of event here, the value written to
     * c14_pmevcntr_delta by the preceeding pmu_op_start call may be for a
     * different counter type. Therefore, we need to set this value to the
     * current count for the counter type we're writing so that pmu_op_finish
     * has the correct count for its calculation.
     */
    uint16_t event = value & PMXEVTYPER_EVTCOUNT;
    if (event_supported(event)) {
        uint16_t event_idx = supported_event_map[event];
        env->cp15.c14_pmevcntr_delta[counter] =
            pm_events[event_idx].get_count(env);
    }
}

static uint64_t pmevtyper_readfn(CPUARMState *env, const ARMCPRegInfo *ri)
{
    uint8_t counter = ((ri->crm & 3) << 3) | (ri->opc2 & 7);
    return pmevtyper_read(env, ri, counter);
}

static void pmxevtyper_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    pmevtyper_write(env, ri, value, env->cp15.c9_pmselr & 31);
}

static uint64_t pmxevtyper_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return pmevtyper_read(env, ri, env->cp15.c9_pmselr & 31);
}

static void pmevcntr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value, uint8_t counter)
{
    if (counter < pmu_num_counters(env)) {
        pmevcntr_op_start(env, counter);
        env->cp15.c14_pmevcntr[counter] = value;
        pmevcntr_op_finish(env, counter);
    }
    /*
     * We opt to behave as a RAZ/WI when attempts to access PM[X]EVCNTR
     * are CONSTRAINED UNPREDICTABLE.
     */
}

static uint64_t pmevcntr_read(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint8_t counter)
{
    if (counter < pmu_num_counters(env)) {
        uint64_t ret;
        pmevcntr_op_start(env, counter);
        ret = env->cp15.c14_pmevcntr[counter];
        pmevcntr_op_finish(env, counter);
        return ret;
    } else {
      /* We opt to behave as a RAZ/WI when attempts to access PM[X]EVCNTR
       * are CONSTRAINED UNPREDICTABLE. */
        return 0;
    }
}

static void pmevcntr_writefn(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    uint8_t counter = ((ri->crm & 3) << 3) | (ri->opc2 & 7);
    pmevcntr_write(env, ri, value, counter);
}

static uint64_t pmevcntr_readfn(CPUARMState *env, const ARMCPRegInfo *ri)
{
    uint8_t counter = ((ri->crm & 3) << 3) | (ri->opc2 & 7);
    return pmevcntr_read(env, ri, counter);
}

static void pmevcntr_rawwrite(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    uint8_t counter = ((ri->crm & 3) << 3) | (ri->opc2 & 7);
    assert(counter < pmu_num_counters(env));
    env->cp15.c14_pmevcntr[counter] = value;
    pmevcntr_write(env, ri, value, counter);
}

static uint64_t pmevcntr_rawread(CPUARMState *env, const ARMCPRegInfo *ri)
{
    uint8_t counter = ((ri->crm & 3) << 3) | (ri->opc2 & 7);
    assert(counter < pmu_num_counters(env));
    return env->cp15.c14_pmevcntr[counter];
}

static void pmxevcntr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    pmevcntr_write(env, ri, value, env->cp15.c9_pmselr & 31);
}

static uint64_t pmxevcntr_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return pmevcntr_read(env, ri, env->cp15.c9_pmselr & 31);
}

static void pmuserenr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                            uint64_t value)
{
    if (arm_feature(env, ARM_FEATURE_V8)) {
        env->cp15.c9_pmuserenr = value & 0xf;
    } else {
        env->cp15.c9_pmuserenr = value & 1;
    }
}

static void pmintenset_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    /* We have no event counters so only the C bit can be changed */
    value &= pmu_counter_mask(env);
    env->cp15.c9_pminten |= value;
    pmu_update_irq(env);
}

static void pmintenclr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    value &= pmu_counter_mask(env);
    env->cp15.c9_pminten &= ~value;
    pmu_update_irq(env);
}

static void vbar_write(CPUARMState *env, const ARMCPRegInfo *ri,
                       uint64_t value)
{
    /* Note that even though the AArch64 view of this register has bits
     * [10:0] all RES0 we can only mask the bottom 5, to comply with the
     * architectural requirements for bits which are RES0 only in some
     * contexts. (ARMv8 would permit us to do no masking at all, but ARMv7
     * requires the bottom five bits to be RAZ/WI because they're UNK/SBZP.)
     */
    raw_write(env, ri, value & ~0x1FULL);
}

static void scr_write(CPUARMState *env, const ARMCPRegInfo *ri, uint64_t value)
{
    /* Begin with base v8.0 state.  */
    uint32_t valid_mask = 0x3fff;
    ARMCPU *cpu = env_archcpu(env);

    if (ri->state == ARM_CP_STATE_AA64) {
        if (arm_feature(env, ARM_FEATURE_AARCH64) &&
            !cpu_isar_feature(aa64_aa32_el1, cpu)) {
                value |= SCR_FW | SCR_AW;   /* these two bits are RES1.  */
        }
        valid_mask &= ~SCR_NET;

        if (cpu_isar_feature(aa64_lor, cpu)) {
            valid_mask |= SCR_TLOR;
        }
        if (cpu_isar_feature(aa64_pauth, cpu)) {
            valid_mask |= SCR_API | SCR_APK;
        }
        if (cpu_isar_feature(aa64_sel2, cpu)) {
            valid_mask |= SCR_EEL2;
        }
        if (cpu_isar_feature(aa64_mte, cpu)) {
            valid_mask |= SCR_ATA;
        }
    } else {
        valid_mask &= ~(SCR_RW | SCR_ST);
    }

    if (!arm_feature(env, ARM_FEATURE_EL2)) {
        valid_mask &= ~SCR_HCE;

        /* On ARMv7, SMD (or SCD as it is called in v7) is only
         * supported if EL2 exists. The bit is UNK/SBZP when
         * EL2 is unavailable. In QEMU ARMv7, we force it to always zero
         * when EL2 is unavailable.
         * On ARMv8, this bit is always available.
         */
        if (arm_feature(env, ARM_FEATURE_V7) &&
            !arm_feature(env, ARM_FEATURE_V8)) {
            valid_mask &= ~SCR_SMD;
        }
    }

    /* Clear all-context RES0 bits.  */
    value &= valid_mask;
    raw_write(env, ri, value);
}

static void scr_reset(CPUARMState *env, const ARMCPRegInfo *ri)
{
    /*
     * scr_write will set the RES1 bits on an AArch64-only CPU.
     * The reset value will be 0x30 on an AArch64-only CPU and 0 otherwise.
     */
    scr_write(env, ri, 0);
}

static CPAccessResult access_aa64_tid2(CPUARMState *env,
                                       const ARMCPRegInfo *ri,
                                       bool isread)
{
    if (arm_current_el(env) == 1 && (arm_hcr_el2_eff(env) & HCR_TID2)) {
        return CP_ACCESS_TRAP_EL2;
    }

    return CP_ACCESS_OK;
}

static uint64_t ccsidr_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    ARMCPU *cpu = env_archcpu(env);

    /* Acquire the CSSELR index from the bank corresponding to the CCSIDR
     * bank
     */
    uint32_t index = A32_BANKED_REG_GET(env, csselr,
                                        ri->secure & ARM_CP_SECSTATE_S);

    return cpu->ccsidr[index];
}

static void csselr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
    raw_write(env, ri, value & 0xf);
}

static uint64_t isr_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    CPUState *cs = env_cpu(env);
    bool el1 = arm_current_el(env) == 1;
    uint64_t hcr_el2 = el1 ? arm_hcr_el2_eff(env) : 0;
    uint64_t ret = 0;

    if (hcr_el2 & HCR_IMO) {
        if (cs->interrupt_request & CPU_INTERRUPT_VIRQ) {
            ret |= CPSR_I;
        }
    } else {
        if (cs->interrupt_request & CPU_INTERRUPT_HARD) {
            ret |= CPSR_I;
        }
    }

    if (hcr_el2 & HCR_FMO) {
        if (cs->interrupt_request & CPU_INTERRUPT_VFIQ) {
            ret |= CPSR_F;
        }
    } else {
        if (cs->interrupt_request & CPU_INTERRUPT_FIQ) {
            ret |= CPSR_F;
        }
    }

    /* External aborts are not possible in QEMU so A bit is always clear */
    return ret;
}

static CPAccessResult access_aa64_tid1(CPUARMState *env, const ARMCPRegInfo *ri,
                                       bool isread)
{
    if (arm_current_el(env) == 1 && (arm_hcr_el2_eff(env) & HCR_TID1)) {
        return CP_ACCESS_TRAP_EL2;
    }

    return CP_ACCESS_OK;
}

static CPAccessResult access_aa32_tid1(CPUARMState *env, const ARMCPRegInfo *ri,
                                       bool isread)
{
    if (arm_feature(env, ARM_FEATURE_V8)) {
        return access_aa64_tid1(env, ri, isread);
    }

    return CP_ACCESS_OK;
}

static const ARMCPRegInfo v7_cp_reginfo[] = {
    /* the old v6 WFI, UNPREDICTABLE in v7 but we choose to NOP */
    { .name = "NOP", .cp = 15, .crn = 7, .crm = 0, .opc1 = 0, .opc2 = 4,
      .access = PL1_W, .type = ARM_CP_NOP },
    /* Performance monitors are implementation defined in v7,
     * but with an ARM recommended set of registers, which we
     * follow.
     *
     * Performance registers fall into three categories:
     *  (a) always UNDEF in PL0, RW in PL1 (PMINTENSET, PMINTENCLR)
     *  (b) RO in PL0 (ie UNDEF on write), RW in PL1 (PMUSERENR)
     *  (c) UNDEF in PL0 if PMUSERENR.EN==0, otherwise accessible (all others)
     * For the cases controlled by PMUSERENR we must set .access to PL0_RW
     * or PL0_RO as appropriate and then check PMUSERENR in the helper fn.
     */
    { .name = "PMCNTENSET", .cp = 15, .crn = 9, .crm = 12, .opc1 = 0, .opc2 = 1,
      .access = PL0_RW, .type = ARM_CP_ALIAS,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.c9_pmcnten),
      .writefn = pmcntenset_write,
      .accessfn = pmreg_access,
      .raw_writefn = raw_write },
    { .name = "PMCNTENSET_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 12, .opc2 = 1,
      .access = PL0_RW, .accessfn = pmreg_access,
      .fieldoffset = offsetof(CPUARMState, cp15.c9_pmcnten), .resetvalue = 0,
      .writefn = pmcntenset_write, .raw_writefn = raw_write },
    { .name = "PMCNTENCLR", .cp = 15, .crn = 9, .crm = 12, .opc1 = 0, .opc2 = 2,
      .access = PL0_RW,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.c9_pmcnten),
      .accessfn = pmreg_access,
      .writefn = pmcntenclr_write,
      .type = ARM_CP_ALIAS },
    { .name = "PMCNTENCLR_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 12, .opc2 = 2,
      .access = PL0_RW, .accessfn = pmreg_access,
      .type = ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, cp15.c9_pmcnten),
      .writefn = pmcntenclr_write },
    { .name = "PMOVSR", .cp = 15, .crn = 9, .crm = 12, .opc1 = 0, .opc2 = 3,
      .access = PL0_RW, .type = ARM_CP_IO,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.c9_pmovsr),
      .accessfn = pmreg_access,
      .writefn = pmovsr_write,
      .raw_writefn = raw_write },
    { .name = "PMOVSCLR_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 12, .opc2 = 3,
      .access = PL0_RW, .accessfn = pmreg_access,
      .type = ARM_CP_ALIAS | ARM_CP_IO,
      .fieldoffset = offsetof(CPUARMState, cp15.c9_pmovsr),
      .writefn = pmovsr_write,
      .raw_writefn = raw_write },
    { .name = "PMSWINC", .cp = 15, .crn = 9, .crm = 12, .opc1 = 0, .opc2 = 4,
      .access = PL0_W, .accessfn = pmreg_access_swinc,
      .type = ARM_CP_NO_RAW | ARM_CP_IO,
      .writefn = pmswinc_write },
    { .name = "PMSWINC_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 12, .opc2 = 4,
      .access = PL0_W, .accessfn = pmreg_access_swinc,
      .type = ARM_CP_NO_RAW | ARM_CP_IO,
      .writefn = pmswinc_write },
    { .name = "PMSELR", .cp = 15, .crn = 9, .crm = 12, .opc1 = 0, .opc2 = 5,
      .access = PL0_RW, .type = ARM_CP_ALIAS,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.c9_pmselr),
      .accessfn = pmreg_access_selr, .writefn = pmselr_write,
      .raw_writefn = raw_write},
    { .name = "PMSELR_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 12, .opc2 = 5,
      .access = PL0_RW, .accessfn = pmreg_access_selr,
      .fieldoffset = offsetof(CPUARMState, cp15.c9_pmselr),
      .writefn = pmselr_write, .raw_writefn = raw_write, },
    { .name = "PMCCNTR", .cp = 15, .crn = 9, .crm = 13, .opc1 = 0, .opc2 = 0,
      .access = PL0_RW, .resetvalue = 0, .type = ARM_CP_ALIAS | ARM_CP_IO,
      .readfn = pmccntr_read, .writefn = pmccntr_write32,
      .accessfn = pmreg_access_ccntr },
    { .name = "PMCCNTR_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 13, .opc2 = 0,
      .access = PL0_RW, .accessfn = pmreg_access_ccntr,
      .type = ARM_CP_IO,
      .fieldoffset = offsetof(CPUARMState, cp15.c15_ccnt),
      .readfn = pmccntr_read, .writefn = pmccntr_write,
      .raw_readfn = raw_read, .raw_writefn = raw_write, },
    { .name = "PMCCFILTR", .cp = 15, .opc1 = 0, .crn = 14, .crm = 15, .opc2 = 7,
      .writefn = pmccfiltr_write_a32, .readfn = pmccfiltr_read_a32,
      .access = PL0_RW, .accessfn = pmreg_access,
      .type = ARM_CP_ALIAS | ARM_CP_IO,
      .resetvalue = 0, },
    { .name = "PMCCFILTR_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 15, .opc2 = 7,
      .writefn = pmccfiltr_write, .raw_writefn = raw_write,
      .access = PL0_RW, .accessfn = pmreg_access,
      .type = ARM_CP_IO,
      .fieldoffset = offsetof(CPUARMState, cp15.pmccfiltr_el0),
      .resetvalue = 0, },
    { .name = "PMXEVTYPER", .cp = 15, .crn = 9, .crm = 13, .opc1 = 0, .opc2 = 1,
      .access = PL0_RW, .type = ARM_CP_NO_RAW | ARM_CP_IO,
      .accessfn = pmreg_access,
      .writefn = pmxevtyper_write, .readfn = pmxevtyper_read },
    { .name = "PMXEVTYPER_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 13, .opc2 = 1,
      .access = PL0_RW, .type = ARM_CP_NO_RAW | ARM_CP_IO,
      .accessfn = pmreg_access,
      .writefn = pmxevtyper_write, .readfn = pmxevtyper_read },
    { .name = "PMXEVCNTR", .cp = 15, .crn = 9, .crm = 13, .opc1 = 0, .opc2 = 2,
      .access = PL0_RW, .type = ARM_CP_NO_RAW | ARM_CP_IO,
      .accessfn = pmreg_access_xevcntr,
      .writefn = pmxevcntr_write, .readfn = pmxevcntr_read },
    { .name = "PMXEVCNTR_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 13, .opc2 = 2,
      .access = PL0_RW, .type = ARM_CP_NO_RAW | ARM_CP_IO,
      .accessfn = pmreg_access_xevcntr,
      .writefn = pmxevcntr_write, .readfn = pmxevcntr_read },
    { .name = "PMUSERENR", .cp = 15, .crn = 9, .crm = 14, .opc1 = 0, .opc2 = 0,
      .access = PL0_R | PL1_RW, .accessfn = access_tpm,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.c9_pmuserenr),
      .resetvalue = 0,
      .writefn = pmuserenr_write, .raw_writefn = raw_write },
    { .name = "PMUSERENR_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 14, .opc2 = 0,
      .access = PL0_R | PL1_RW, .accessfn = access_tpm, .type = ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, cp15.c9_pmuserenr),
      .resetvalue = 0,
      .writefn = pmuserenr_write, .raw_writefn = raw_write },
    { .name = "PMINTENSET", .cp = 15, .crn = 9, .crm = 14, .opc1 = 0, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_tpm,
      .type = ARM_CP_ALIAS | ARM_CP_IO,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.c9_pminten),
      .resetvalue = 0,
      .writefn = pmintenset_write, .raw_writefn = raw_write },
    { .name = "PMINTENSET_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 9, .crm = 14, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_tpm,
      .type = ARM_CP_IO,
      .fieldoffset = offsetof(CPUARMState, cp15.c9_pminten),
      .writefn = pmintenset_write, .raw_writefn = raw_write,
      .resetvalue = 0x0 },
    { .name = "PMINTENCLR", .cp = 15, .crn = 9, .crm = 14, .opc1 = 0, .opc2 = 2,
      .access = PL1_RW, .accessfn = access_tpm,
      .type = ARM_CP_ALIAS | ARM_CP_IO | ARM_CP_NO_RAW,
      .fieldoffset = offsetof(CPUARMState, cp15.c9_pminten),
      .writefn = pmintenclr_write, },
    { .name = "PMINTENCLR_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 9, .crm = 14, .opc2 = 2,
      .access = PL1_RW, .accessfn = access_tpm,
      .type = ARM_CP_ALIAS | ARM_CP_IO | ARM_CP_NO_RAW,
      .fieldoffset = offsetof(CPUARMState, cp15.c9_pminten),
      .writefn = pmintenclr_write },
    { .name = "CCSIDR", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .crn = 0, .crm = 0, .opc1 = 1, .opc2 = 0,
      .access = PL1_R,
      .accessfn = access_aa64_tid2,
      .readfn = ccsidr_read, .type = ARM_CP_NO_RAW },
    { .name = "CSSELR", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .crn = 0, .crm = 0, .opc1 = 2, .opc2 = 0,
      .access = PL1_RW,
      .accessfn = access_aa64_tid2,
      .writefn = csselr_write, .resetvalue = 0,
      .bank_fieldoffsets = { offsetof(CPUARMState, cp15.csselr_s),
                             offsetof(CPUARMState, cp15.csselr_ns) } },
    /* Auxiliary ID register: this actually has an IMPDEF value but for now
     * just RAZ for all cores:
     */
    { .name = "AIDR", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 1, .crn = 0, .crm = 0, .opc2 = 7,
      .access = PL1_R, .type = ARM_CP_CONST,
      .accessfn = access_aa64_tid1,
      .resetvalue = 0 },
    /* Auxiliary fault status registers: these also are IMPDEF, and we
     * choose to RAZ/WI for all cores.
     */
    { .name = "AFSR0_EL1", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 0, .crn = 5, .crm = 1, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "AFSR1_EL1", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 0, .crn = 5, .crm = 1, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    /* MAIR can just read-as-written because we don't implement caches
     * and so don't need to care about memory attributes.
     */
    { .name = "MAIR_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 10, .crm = 2, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .fieldoffset = offsetof(CPUARMState, cp15.mair_el[1]),
      .resetvalue = 0 },
    { .name = "MAIR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 10, .crm = 2, .opc2 = 0,
      .access = PL3_RW, .fieldoffset = offsetof(CPUARMState, cp15.mair_el[3]),
      .resetvalue = 0 },
    /* For non-long-descriptor page tables these are PRRR and NMRR;
     * regardless they still act as reads-as-written for QEMU.
     */
     /* MAIR0/1 are defined separately from their 64-bit counterpart which
      * allows them to assign the correct fieldoffset based on the endianness
      * handled in the field definitions.
      */
    { .name = "MAIR0", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 0, .crn = 10, .crm = 2, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .bank_fieldoffsets = { offsetof(CPUARMState, cp15.mair0_s),
                             offsetof(CPUARMState, cp15.mair0_ns) },
      .resetfn = arm_cp_reset_ignore },
    { .name = "MAIR1", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 0, .crn = 10, .crm = 2, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .bank_fieldoffsets = { offsetof(CPUARMState, cp15.mair1_s),
                             offsetof(CPUARMState, cp15.mair1_ns) },
      .resetfn = arm_cp_reset_ignore },
    { .name = "ISR_EL1", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 0, .crn = 12, .crm = 1, .opc2 = 0,
      .type = ARM_CP_NO_RAW, .access = PL1_R, .readfn = isr_read },
    /* 32 bit ITLB invalidates */
    { .name = "ITLBIALL", .cp = 15, .opc1 = 0, .crn = 8, .crm = 5, .opc2 = 0,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbiall_write },
    { .name = "ITLBIMVA", .cp = 15, .opc1 = 0, .crn = 8, .crm = 5, .opc2 = 1,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbimva_write },
    { .name = "ITLBIASID", .cp = 15, .opc1 = 0, .crn = 8, .crm = 5, .opc2 = 2,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbiasid_write },
    /* 32 bit DTLB invalidates */
    { .name = "DTLBIALL", .cp = 15, .opc1 = 0, .crn = 8, .crm = 6, .opc2 = 0,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbiall_write },
    { .name = "DTLBIMVA", .cp = 15, .opc1 = 0, .crn = 8, .crm = 6, .opc2 = 1,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbimva_write },
    { .name = "DTLBIASID", .cp = 15, .opc1 = 0, .crn = 8, .crm = 6, .opc2 = 2,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbiasid_write },
    /* 32 bit TLB invalidates */
    { .name = "TLBIALL", .cp = 15, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 0,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbiall_write },
    { .name = "TLBIMVA", .cp = 15, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 1,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbimva_write },
    { .name = "TLBIASID", .cp = 15, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 2,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbiasid_write },
    { .name = "TLBIMVAA", .cp = 15, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 3,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbimvaa_write },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo v7mp_cp_reginfo[] = {
    /* 32 bit TLB invalidates, Inner Shareable */
    { .name = "TLBIALLIS", .cp = 15, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 0,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbiall_is_write },
    { .name = "TLBIMVAIS", .cp = 15, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 1,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbimva_is_write },
    { .name = "TLBIASIDIS", .cp = 15, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 2,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbiasid_is_write },
    { .name = "TLBIMVAAIS", .cp = 15, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 3,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbimvaa_is_write },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo pmovsset_cp_reginfo[] = {
    /* PMOVSSET is not implemented in v7 before v7ve */
    { .name = "PMOVSSET", .cp = 15, .opc1 = 0, .crn = 9, .crm = 14, .opc2 = 3,
      .access = PL0_RW, .accessfn = pmreg_access,
      .type = ARM_CP_ALIAS | ARM_CP_IO,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.c9_pmovsr),
      .writefn = pmovsset_write,
      .raw_writefn = raw_write },
    { .name = "PMOVSSET_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 14, .opc2 = 3,
      .access = PL0_RW, .accessfn = pmreg_access,
      .type = ARM_CP_ALIAS | ARM_CP_IO,
      .fieldoffset = offsetof(CPUARMState, cp15.c9_pmovsr),
      .writefn = pmovsset_write,
      .raw_writefn = raw_write },
    REGINFO_SENTINEL
};

static void teecr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                        uint64_t value)
{
    value &= 1;
    env->teecr = value;
}

static CPAccessResult teehbr_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                    bool isread)
{
    if (arm_current_el(env) == 0 && (env->teecr & 1)) {
        return CP_ACCESS_TRAP;
    }
    return CP_ACCESS_OK;
}

static const ARMCPRegInfo t2ee_cp_reginfo[] = {
    { .name = "TEECR", .cp = 14, .crn = 0, .crm = 0, .opc1 = 6, .opc2 = 0,
      .access = PL1_RW, .fieldoffset = offsetof(CPUARMState, teecr),
      .resetvalue = 0,
      .writefn = teecr_write },
    { .name = "TEEHBR", .cp = 14, .crn = 1, .crm = 0, .opc1 = 6, .opc2 = 0,
      .access = PL0_RW, .fieldoffset = offsetof(CPUARMState, teehbr),
      .accessfn = teehbr_access, .resetvalue = 0 },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo v6k_cp_reginfo[] = {
    { .name = "TPIDR_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .opc2 = 2, .crn = 13, .crm = 0,
      .access = PL0_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.tpidr_el[0]), .resetvalue = 0 },
    { .name = "TPIDRURW", .cp = 15, .crn = 13, .crm = 0, .opc1 = 0, .opc2 = 2,
      .access = PL0_RW,
      .bank_fieldoffsets = { offsetoflow32(CPUARMState, cp15.tpidrurw_s),
                             offsetoflow32(CPUARMState, cp15.tpidrurw_ns) },
      .resetfn = arm_cp_reset_ignore },
    { .name = "TPIDRRO_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .opc2 = 3, .crn = 13, .crm = 0,
      .access = PL0_R|PL1_W,
      .fieldoffset = offsetof(CPUARMState, cp15.tpidrro_el[0]),
      .resetvalue = 0},
    { .name = "TPIDRURO", .cp = 15, .crn = 13, .crm = 0, .opc1 = 0, .opc2 = 3,
      .access = PL0_R|PL1_W,
      .bank_fieldoffsets = { offsetoflow32(CPUARMState, cp15.tpidruro_s),
                             offsetoflow32(CPUARMState, cp15.tpidruro_ns) },
      .resetfn = arm_cp_reset_ignore },
    { .name = "TPIDR_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .opc2 = 4, .crn = 13, .crm = 0,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.tpidr_el[1]), .resetvalue = 0 },
    { .name = "TPIDRPRW", .opc1 = 0, .cp = 15, .crn = 13, .crm = 0, .opc2 = 4,
      .access = PL1_RW,
      .bank_fieldoffsets = { offsetoflow32(CPUARMState, cp15.tpidrprw_s),
                             offsetoflow32(CPUARMState, cp15.tpidrprw_ns) },
      .resetvalue = 0 },
    REGINFO_SENTINEL
};

#ifndef CONFIG_USER_ONLY

static CPAccessResult gt_cntfrq_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                       bool isread)
{
    /*
     * CNTFRQ: not visible from PL0 if both PL0PCTEN and PL0VCTEN are zero.
     * Writable only at the highest implemented exception level.
     */
    int el = arm_current_el(env);
    uint64_t hcr;
    uint32_t cntkctl;

    switch (el) {
    case 0:
        hcr = arm_hcr_el2_eff(env);
        if ((hcr & (HCR_E2H | HCR_TGE)) == (HCR_E2H | HCR_TGE)) {
            cntkctl = env->cp15.cnthctl_el2;
        } else {
            cntkctl = env->cp15.c14_cntkctl;
        }
        if (!extract32(cntkctl, 0, 2)) {
            return CP_ACCESS_TRAP;
        }
        break;
    case 1:
        if (!isread && ri->state == ARM_CP_STATE_AA32 &&
            arm_is_secure_below_el3(env)) {
            /* Accesses from 32-bit Secure EL1 UNDEF (*not* trap to EL3!) */
            return CP_ACCESS_TRAP_UNCATEGORIZED;
        }
        break;
    case 2:
    case 3:
        break;
    }

    if (!isread && el < arm_highest_el(env)) {
        return CP_ACCESS_TRAP_UNCATEGORIZED;
    }

    return CP_ACCESS_OK;
}

static CPAccessResult gt_counter_access(CPUARMState *env, int timeridx,
                                        bool isread)
{
    unsigned int cur_el = arm_current_el(env);
    bool has_el2 = arm_is_el2_enabled(env);
    uint64_t hcr = arm_hcr_el2_eff(env);

    switch (cur_el) {
    case 0:
        /* If HCR_EL2.<E2H,TGE> == '11': check CNTHCTL_EL2.EL0[PV]CTEN. */
        if ((hcr & (HCR_E2H | HCR_TGE)) == (HCR_E2H | HCR_TGE)) {
            return (extract32(env->cp15.cnthctl_el2, timeridx, 1)
                    ? CP_ACCESS_OK : CP_ACCESS_TRAP_EL2);
        }

        /* CNT[PV]CT: not visible from PL0 if EL0[PV]CTEN is zero */
        if (!extract32(env->cp15.c14_cntkctl, timeridx, 1)) {
            return CP_ACCESS_TRAP;
        }

        /* If HCR_EL2.<E2H,TGE> == '10': check CNTHCTL_EL2.EL1PCTEN. */
        if (hcr & HCR_E2H) {
            if (timeridx == GTIMER_PHYS &&
                !extract32(env->cp15.cnthctl_el2, 10, 1)) {
                return CP_ACCESS_TRAP_EL2;
            }
        } else {
            /* If HCR_EL2.<E2H> == 0: check CNTHCTL_EL2.EL1PCEN. */
            if (has_el2 && timeridx == GTIMER_PHYS &&
                !extract32(env->cp15.cnthctl_el2, 1, 1)) {
                return CP_ACCESS_TRAP_EL2;
            }
        }
        break;

    case 1:
        /* Check CNTHCTL_EL2.EL1PCTEN, which changes location based on E2H. */
        if (has_el2 && timeridx == GTIMER_PHYS &&
            (hcr & HCR_E2H
             ? !extract32(env->cp15.cnthctl_el2, 10, 1)
             : !extract32(env->cp15.cnthctl_el2, 0, 1))) {
            return CP_ACCESS_TRAP_EL2;
        }
        break;
    }
    return CP_ACCESS_OK;
}

static CPAccessResult gt_timer_access(CPUARMState *env, int timeridx,
                                      bool isread)
{
    unsigned int cur_el = arm_current_el(env);
    bool has_el2 = arm_is_el2_enabled(env);
    uint64_t hcr = arm_hcr_el2_eff(env);

    switch (cur_el) {
    case 0:
        if ((hcr & (HCR_E2H | HCR_TGE)) == (HCR_E2H | HCR_TGE)) {
            /* If HCR_EL2.<E2H,TGE> == '11': check CNTHCTL_EL2.EL0[PV]TEN. */
            return (extract32(env->cp15.cnthctl_el2, 9 - timeridx, 1)
                    ? CP_ACCESS_OK : CP_ACCESS_TRAP_EL2);
        }

        /*
         * CNT[PV]_CVAL, CNT[PV]_CTL, CNT[PV]_TVAL: not visible from
         * EL0 if EL0[PV]TEN is zero.
         */
        if (!extract32(env->cp15.c14_cntkctl, 9 - timeridx, 1)) {
            return CP_ACCESS_TRAP;
        }
        /* fall through */

    case 1:
        if (has_el2 && timeridx == GTIMER_PHYS) {
            if (hcr & HCR_E2H) {
                /* If HCR_EL2.<E2H,TGE> == '10': check CNTHCTL_EL2.EL1PTEN. */
                if (!extract32(env->cp15.cnthctl_el2, 11, 1)) {
                    return CP_ACCESS_TRAP_EL2;
                }
            } else {
                /* If HCR_EL2.<E2H> == 0: check CNTHCTL_EL2.EL1PCEN. */
                if (!extract32(env->cp15.cnthctl_el2, 1, 1)) {
                    return CP_ACCESS_TRAP_EL2;
                }
            }
        }
        break;
    }
    return CP_ACCESS_OK;
}

static CPAccessResult gt_pct_access(CPUARMState *env,
                                    const ARMCPRegInfo *ri,
                                    bool isread)
{
    return gt_counter_access(env, GTIMER_PHYS, isread);
}

static CPAccessResult gt_vct_access(CPUARMState *env,
                                    const ARMCPRegInfo *ri,
                                    bool isread)
{
    return gt_counter_access(env, GTIMER_VIRT, isread);
}

static CPAccessResult gt_ptimer_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                       bool isread)
{
    return gt_timer_access(env, GTIMER_PHYS, isread);
}

static CPAccessResult gt_vtimer_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                       bool isread)
{
    return gt_timer_access(env, GTIMER_VIRT, isread);
}

static CPAccessResult gt_stimer_access(CPUARMState *env,
                                       const ARMCPRegInfo *ri,
                                       bool isread)
{
    /*
     * The AArch64 register view of the secure physical timer is
     * always accessible from EL3, and configurably accessible from
     * Secure EL1.
     */
    switch (arm_current_el(env)) {
    case 1:
        if (!arm_is_secure(env)) {
            return CP_ACCESS_TRAP;
        }
        if (!(env->cp15.scr_el3 & SCR_ST)) {
            return CP_ACCESS_TRAP_EL3;
        }
        return CP_ACCESS_OK;
    case 0:
    case 2:
        return CP_ACCESS_TRAP;
    case 3:
        return CP_ACCESS_OK;
    default:
        g_assert_not_reached();
    }
}

static uint64_t gt_get_countervalue(CPUARMState *env)
{
    ARMCPU *cpu = env_archcpu(env);

    return qemu_clock_get_ns(QEMU_CLOCK_VIRTUAL) / gt_cntfrq_period_ns(cpu);
}

static void gt_recalc_timer(ARMCPU *cpu, int timeridx)
{
    ARMGenericTimer *gt = &cpu->env.cp15.c14_timer[timeridx];

    if (gt->ctl & 1) {
        /*
         * Timer enabled: calculate and set current ISTATUS, irq, and
         * reset timer to when ISTATUS next has to change
         */
        uint64_t offset = timeridx == GTIMER_VIRT ?
                                      cpu->env.cp15.cntvoff_el2 : 0;
        uint64_t count = gt_get_countervalue(&cpu->env);
        /* Note that this must be unsigned 64 bit arithmetic: */
        int istatus = count - offset >= gt->cval;
        uint64_t nexttick;
        int irqstate;

        gt->ctl = deposit32(gt->ctl, 2, 1, istatus);

        irqstate = (istatus && !(gt->ctl & 2));
        qemu_set_irq(cpu->gt_timer_outputs[timeridx], irqstate);

        if (istatus) {
            /* Next transition is when count rolls back over to zero */
            nexttick = UINT64_MAX;
        } else {
            /* Next transition is when we hit cval */
            nexttick = gt->cval + offset;
        }
        /*
         * Note that the desired next expiry time might be beyond the
         * signed-64-bit range of a QEMUTimer -- in this case we just
         * set the timer for as far in the future as possible. When the
         * timer expires we will reset the timer for any remaining period.
         */
        if (nexttick > INT64_MAX / gt_cntfrq_period_ns(cpu)) {
            timer_mod_ns(cpu->gt_timer[timeridx], INT64_MAX);
        } else {
            timer_mod(cpu->gt_timer[timeridx], nexttick);
        }
        trace_arm_gt_recalc(timeridx, irqstate, nexttick);
    } else {
        /* Timer disabled: ISTATUS and timer output always clear */
        gt->ctl &= ~4;
        qemu_set_irq(cpu->gt_timer_outputs[timeridx], 0);
        timer_del(cpu->gt_timer[timeridx]);
        trace_arm_gt_recalc_disabled(timeridx);
    }
}

static void gt_timer_reset(CPUARMState *env, const ARMCPRegInfo *ri,
                           int timeridx)
{
    ARMCPU *cpu = env_archcpu(env);

    timer_del(cpu->gt_timer[timeridx]);
}

static uint64_t gt_cnt_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return gt_get_countervalue(env);
}

static uint64_t gt_virt_cnt_offset(CPUARMState *env)
{
    uint64_t hcr;

    switch (arm_current_el(env)) {
    case 2:
        hcr = arm_hcr_el2_eff(env);
        if (hcr & HCR_E2H) {
            return 0;
        }
        break;
    case 0:
        hcr = arm_hcr_el2_eff(env);
        if ((hcr & (HCR_E2H | HCR_TGE)) == (HCR_E2H | HCR_TGE)) {
            return 0;
        }
        break;
    }

    return env->cp15.cntvoff_el2;
}

static uint64_t gt_virt_cnt_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return gt_get_countervalue(env) - gt_virt_cnt_offset(env);
}

static void gt_cval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                          int timeridx,
                          uint64_t value)
{
    trace_arm_gt_cval_write(timeridx, value);
    env->cp15.c14_timer[timeridx].cval = value;
    gt_recalc_timer(env_archcpu(env), timeridx);
}

static uint64_t gt_tval_read(CPUARMState *env, const ARMCPRegInfo *ri,
                             int timeridx)
{
    uint64_t offset = 0;

    switch (timeridx) {
    case GTIMER_VIRT:
    case GTIMER_HYPVIRT:
        offset = gt_virt_cnt_offset(env);
        break;
    }

    return (uint32_t)(env->cp15.c14_timer[timeridx].cval -
                      (gt_get_countervalue(env) - offset));
}

static void gt_tval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                          int timeridx,
                          uint64_t value)
{
    uint64_t offset = 0;

    switch (timeridx) {
    case GTIMER_VIRT:
    case GTIMER_HYPVIRT:
        offset = gt_virt_cnt_offset(env);
        break;
    }

    trace_arm_gt_tval_write(timeridx, value);
    env->cp15.c14_timer[timeridx].cval = gt_get_countervalue(env) - offset +
                                         sextract64(value, 0, 32);
    gt_recalc_timer(env_archcpu(env), timeridx);
}

static void gt_ctl_write(CPUARMState *env, const ARMCPRegInfo *ri,
                         int timeridx,
                         uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    uint32_t oldval = env->cp15.c14_timer[timeridx].ctl;

    trace_arm_gt_ctl_write(timeridx, value);
    env->cp15.c14_timer[timeridx].ctl = deposit64(oldval, 0, 2, value);
    if ((oldval ^ value) & 1) {
        /* Enable toggled */
        gt_recalc_timer(cpu, timeridx);
    } else if ((oldval ^ value) & 2) {
        /*
         * IMASK toggled: don't need to recalculate,
         * just set the interrupt line based on ISTATUS
         */
        int irqstate = (oldval & 4) && !(value & 2);

        trace_arm_gt_imask_toggle(timeridx, irqstate);
        qemu_set_irq(cpu->gt_timer_outputs[timeridx], irqstate);
    }
}

static void gt_phys_timer_reset(CPUARMState *env, const ARMCPRegInfo *ri)
{
    gt_timer_reset(env, ri, GTIMER_PHYS);
}

static void gt_phys_cval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                               uint64_t value)
{
    gt_cval_write(env, ri, GTIMER_PHYS, value);
}

static uint64_t gt_phys_tval_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return gt_tval_read(env, ri, GTIMER_PHYS);
}

static void gt_phys_tval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                               uint64_t value)
{
    gt_tval_write(env, ri, GTIMER_PHYS, value);
}

static void gt_phys_ctl_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    gt_ctl_write(env, ri, GTIMER_PHYS, value);
}

static int gt_phys_redir_timeridx(CPUARMState *env)
{
    switch (arm_mmu_idx(env)) {
    case ARMMMUIdx_E20_0:
    case ARMMMUIdx_E20_2:
    case ARMMMUIdx_E20_2_PAN:
    case ARMMMUIdx_SE20_0:
    case ARMMMUIdx_SE20_2:
    case ARMMMUIdx_SE20_2_PAN:
        return GTIMER_HYP;
    default:
        return GTIMER_PHYS;
    }
}

static int gt_virt_redir_timeridx(CPUARMState *env)
{
    switch (arm_mmu_idx(env)) {
    case ARMMMUIdx_E20_0:
    case ARMMMUIdx_E20_2:
    case ARMMMUIdx_E20_2_PAN:
    case ARMMMUIdx_SE20_0:
    case ARMMMUIdx_SE20_2:
    case ARMMMUIdx_SE20_2_PAN:
        return GTIMER_HYPVIRT;
    default:
        return GTIMER_VIRT;
    }
}

static uint64_t gt_phys_redir_cval_read(CPUARMState *env,
                                        const ARMCPRegInfo *ri)
{
    int timeridx = gt_phys_redir_timeridx(env);
    return env->cp15.c14_timer[timeridx].cval;
}

static void gt_phys_redir_cval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                     uint64_t value)
{
    int timeridx = gt_phys_redir_timeridx(env);
    gt_cval_write(env, ri, timeridx, value);
}

static uint64_t gt_phys_redir_tval_read(CPUARMState *env,
                                        const ARMCPRegInfo *ri)
{
    int timeridx = gt_phys_redir_timeridx(env);
    return gt_tval_read(env, ri, timeridx);
}

static void gt_phys_redir_tval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                     uint64_t value)
{
    int timeridx = gt_phys_redir_timeridx(env);
    gt_tval_write(env, ri, timeridx, value);
}

static uint64_t gt_phys_redir_ctl_read(CPUARMState *env,
                                       const ARMCPRegInfo *ri)
{
    int timeridx = gt_phys_redir_timeridx(env);
    return env->cp15.c14_timer[timeridx].ctl;
}

static void gt_phys_redir_ctl_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                    uint64_t value)
{
    int timeridx = gt_phys_redir_timeridx(env);
    gt_ctl_write(env, ri, timeridx, value);
}

static void gt_virt_timer_reset(CPUARMState *env, const ARMCPRegInfo *ri)
{
    gt_timer_reset(env, ri, GTIMER_VIRT);
}

static void gt_virt_cval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                               uint64_t value)
{
    gt_cval_write(env, ri, GTIMER_VIRT, value);
}

static uint64_t gt_virt_tval_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return gt_tval_read(env, ri, GTIMER_VIRT);
}

static void gt_virt_tval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                               uint64_t value)
{
    gt_tval_write(env, ri, GTIMER_VIRT, value);
}

static void gt_virt_ctl_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    gt_ctl_write(env, ri, GTIMER_VIRT, value);
}

static void gt_cntvoff_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);

    trace_arm_gt_cntvoff_write(value);
    raw_write(env, ri, value);
    gt_recalc_timer(cpu, GTIMER_VIRT);
}

static uint64_t gt_virt_redir_cval_read(CPUARMState *env,
                                        const ARMCPRegInfo *ri)
{
    int timeridx = gt_virt_redir_timeridx(env);
    return env->cp15.c14_timer[timeridx].cval;
}

static void gt_virt_redir_cval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                     uint64_t value)
{
    int timeridx = gt_virt_redir_timeridx(env);
    gt_cval_write(env, ri, timeridx, value);
}

static uint64_t gt_virt_redir_tval_read(CPUARMState *env,
                                        const ARMCPRegInfo *ri)
{
    int timeridx = gt_virt_redir_timeridx(env);
    return gt_tval_read(env, ri, timeridx);
}

static void gt_virt_redir_tval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                     uint64_t value)
{
    int timeridx = gt_virt_redir_timeridx(env);
    gt_tval_write(env, ri, timeridx, value);
}

static uint64_t gt_virt_redir_ctl_read(CPUARMState *env,
                                       const ARMCPRegInfo *ri)
{
    int timeridx = gt_virt_redir_timeridx(env);
    return env->cp15.c14_timer[timeridx].ctl;
}

static void gt_virt_redir_ctl_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                    uint64_t value)
{
    int timeridx = gt_virt_redir_timeridx(env);
    gt_ctl_write(env, ri, timeridx, value);
}

static void gt_hyp_timer_reset(CPUARMState *env, const ARMCPRegInfo *ri)
{
    gt_timer_reset(env, ri, GTIMER_HYP);
}

static void gt_hyp_cval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    gt_cval_write(env, ri, GTIMER_HYP, value);
}

static uint64_t gt_hyp_tval_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return gt_tval_read(env, ri, GTIMER_HYP);
}

static void gt_hyp_tval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    gt_tval_write(env, ri, GTIMER_HYP, value);
}

static void gt_hyp_ctl_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    gt_ctl_write(env, ri, GTIMER_HYP, value);
}

static void gt_sec_timer_reset(CPUARMState *env, const ARMCPRegInfo *ri)
{
    gt_timer_reset(env, ri, GTIMER_SEC);
}

static void gt_sec_cval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    gt_cval_write(env, ri, GTIMER_SEC, value);
}

static uint64_t gt_sec_tval_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return gt_tval_read(env, ri, GTIMER_SEC);
}

static void gt_sec_tval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    gt_tval_write(env, ri, GTIMER_SEC, value);
}

static void gt_sec_ctl_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    gt_ctl_write(env, ri, GTIMER_SEC, value);
}

static void gt_hv_timer_reset(CPUARMState *env, const ARMCPRegInfo *ri)
{
    gt_timer_reset(env, ri, GTIMER_HYPVIRT);
}

static void gt_hv_cval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    gt_cval_write(env, ri, GTIMER_HYPVIRT, value);
}

static uint64_t gt_hv_tval_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return gt_tval_read(env, ri, GTIMER_HYPVIRT);
}

static void gt_hv_tval_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    gt_tval_write(env, ri, GTIMER_HYPVIRT, value);
}

static void gt_hv_ctl_write(CPUARMState *env, const ARMCPRegInfo *ri,
                            uint64_t value)
{
    gt_ctl_write(env, ri, GTIMER_HYPVIRT, value);
}

void arm_gt_ptimer_cb(void *opaque)
{
    ARMCPU *cpu = opaque;

    gt_recalc_timer(cpu, GTIMER_PHYS);
}

void arm_gt_vtimer_cb(void *opaque)
{
    ARMCPU *cpu = opaque;

    gt_recalc_timer(cpu, GTIMER_VIRT);
}

void arm_gt_htimer_cb(void *opaque)
{
    ARMCPU *cpu = opaque;

    gt_recalc_timer(cpu, GTIMER_HYP);
}

void arm_gt_stimer_cb(void *opaque)
{
    ARMCPU *cpu = opaque;

    gt_recalc_timer(cpu, GTIMER_SEC);
}

void arm_gt_hvtimer_cb(void *opaque)
{
    ARMCPU *cpu = opaque;

    gt_recalc_timer(cpu, GTIMER_HYPVIRT);
}

static void arm_gt_cntfrq_reset(CPUARMState *env, const ARMCPRegInfo *opaque)
{
    ARMCPU *cpu = env_archcpu(env);

    cpu->env.cp15.c14_cntfrq = cpu->gt_cntfrq_hz;
}

static const ARMCPRegInfo generic_timer_cp_reginfo[] = {
    /*
     * Note that CNTFRQ is purely reads-as-written for the benefit
     * of software; writing it doesn't actually change the timer frequency.
     * Our reset value matches the fixed frequency we implement the timer at.
     */
    { .name = "CNTFRQ", .cp = 15, .crn = 14, .crm = 0, .opc1 = 0, .opc2 = 0,
      .type = ARM_CP_ALIAS,
      .access = PL1_RW | PL0_R, .accessfn = gt_cntfrq_access,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.c14_cntfrq),
    },
    { .name = "CNTFRQ_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 0, .opc2 = 0,
      .access = PL1_RW | PL0_R, .accessfn = gt_cntfrq_access,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_cntfrq),
      .resetfn = arm_gt_cntfrq_reset,
    },
    /* overall control: mostly access permissions */
    { .name = "CNTKCTL", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 0, .crn = 14, .crm = 1, .opc2 = 0,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_cntkctl),
      .resetvalue = 0,
    },
    /* per-timer control */
    { .name = "CNTP_CTL", .cp = 15, .crn = 14, .crm = 2, .opc1 = 0, .opc2 = 1,
      .secure = ARM_CP_SECSTATE_NS,
      .type = ARM_CP_IO | ARM_CP_ALIAS, .access = PL0_RW,
      .accessfn = gt_ptimer_access,
      .fieldoffset = offsetoflow32(CPUARMState,
                                   cp15.c14_timer[GTIMER_PHYS].ctl),
      .readfn = gt_phys_redir_ctl_read, .raw_readfn = raw_read,
      .writefn = gt_phys_redir_ctl_write, .raw_writefn = raw_write,
    },
    { .name = "CNTP_CTL_S",
      .cp = 15, .crn = 14, .crm = 2, .opc1 = 0, .opc2 = 1,
      .secure = ARM_CP_SECSTATE_S,
      .type = ARM_CP_IO | ARM_CP_ALIAS, .access = PL0_RW,
      .accessfn = gt_ptimer_access,
      .fieldoffset = offsetoflow32(CPUARMState,
                                   cp15.c14_timer[GTIMER_SEC].ctl),
      .writefn = gt_sec_ctl_write, .raw_writefn = raw_write,
    },
    { .name = "CNTP_CTL_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 2, .opc2 = 1,
      .type = ARM_CP_IO, .access = PL0_RW,
      .accessfn = gt_ptimer_access,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_PHYS].ctl),
      .resetvalue = 0,
      .readfn = gt_phys_redir_ctl_read, .raw_readfn = raw_read,
      .writefn = gt_phys_redir_ctl_write, .raw_writefn = raw_write,
    },
    { .name = "CNTV_CTL", .cp = 15, .crn = 14, .crm = 3, .opc1 = 0, .opc2 = 1,
      .type = ARM_CP_IO | ARM_CP_ALIAS, .access = PL0_RW,
      .accessfn = gt_vtimer_access,
      .fieldoffset = offsetoflow32(CPUARMState,
                                   cp15.c14_timer[GTIMER_VIRT].ctl),
      .readfn = gt_virt_redir_ctl_read, .raw_readfn = raw_read,
      .writefn = gt_virt_redir_ctl_write, .raw_writefn = raw_write,
    },
    { .name = "CNTV_CTL_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 3, .opc2 = 1,
      .type = ARM_CP_IO, .access = PL0_RW,
      .accessfn = gt_vtimer_access,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_VIRT].ctl),
      .resetvalue = 0,
      .readfn = gt_virt_redir_ctl_read, .raw_readfn = raw_read,
      .writefn = gt_virt_redir_ctl_write, .raw_writefn = raw_write,
    },
    /* TimerValue views: a 32 bit downcounting view of the underlying state */
    { .name = "CNTP_TVAL", .cp = 15, .crn = 14, .crm = 2, .opc1 = 0, .opc2 = 0,
      .secure = ARM_CP_SECSTATE_NS,
      .type = ARM_CP_NO_RAW | ARM_CP_IO, .access = PL0_RW,
      .accessfn = gt_ptimer_access,
      .readfn = gt_phys_redir_tval_read, .writefn = gt_phys_redir_tval_write,
    },
    { .name = "CNTP_TVAL_S",
      .cp = 15, .crn = 14, .crm = 2, .opc1 = 0, .opc2 = 0,
      .secure = ARM_CP_SECSTATE_S,
      .type = ARM_CP_NO_RAW | ARM_CP_IO, .access = PL0_RW,
      .accessfn = gt_ptimer_access,
      .readfn = gt_sec_tval_read, .writefn = gt_sec_tval_write,
    },
    { .name = "CNTP_TVAL_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 2, .opc2 = 0,
      .type = ARM_CP_NO_RAW | ARM_CP_IO, .access = PL0_RW,
      .accessfn = gt_ptimer_access, .resetfn = gt_phys_timer_reset,
      .readfn = gt_phys_redir_tval_read, .writefn = gt_phys_redir_tval_write,
    },
    { .name = "CNTV_TVAL", .cp = 15, .crn = 14, .crm = 3, .opc1 = 0, .opc2 = 0,
      .type = ARM_CP_NO_RAW | ARM_CP_IO, .access = PL0_RW,
      .accessfn = gt_vtimer_access,
      .readfn = gt_virt_redir_tval_read, .writefn = gt_virt_redir_tval_write,
    },
    { .name = "CNTV_TVAL_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 3, .opc2 = 0,
      .type = ARM_CP_NO_RAW | ARM_CP_IO, .access = PL0_RW,
      .accessfn = gt_vtimer_access, .resetfn = gt_virt_timer_reset,
      .readfn = gt_virt_redir_tval_read, .writefn = gt_virt_redir_tval_write,
    },
    /* The counter itself */
    { .name = "CNTPCT", .cp = 15, .crm = 14, .opc1 = 0,
      .access = PL0_R, .type = ARM_CP_64BIT | ARM_CP_NO_RAW | ARM_CP_IO,
      .accessfn = gt_pct_access,
      .readfn = gt_cnt_read, .resetfn = arm_cp_reset_ignore,
    },
    { .name = "CNTPCT_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 0, .opc2 = 1,
      .access = PL0_R, .type = ARM_CP_NO_RAW | ARM_CP_IO,
      .accessfn = gt_pct_access, .readfn = gt_cnt_read,
    },
    { .name = "CNTVCT", .cp = 15, .crm = 14, .opc1 = 1,
      .access = PL0_R, .type = ARM_CP_64BIT | ARM_CP_NO_RAW | ARM_CP_IO,
      .accessfn = gt_vct_access,
      .readfn = gt_virt_cnt_read, .resetfn = arm_cp_reset_ignore,
    },
    { .name = "CNTVCT_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 0, .opc2 = 2,
      .access = PL0_R, .type = ARM_CP_NO_RAW | ARM_CP_IO,
      .accessfn = gt_vct_access, .readfn = gt_virt_cnt_read,
    },
    /* Comparison value, indicating when the timer goes off */
    { .name = "CNTP_CVAL", .cp = 15, .crm = 14, .opc1 = 2,
      .secure = ARM_CP_SECSTATE_NS,
      .access = PL0_RW,
      .type = ARM_CP_64BIT | ARM_CP_IO | ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_PHYS].cval),
      .accessfn = gt_ptimer_access,
      .readfn = gt_phys_redir_cval_read, .raw_readfn = raw_read,
      .writefn = gt_phys_redir_cval_write, .raw_writefn = raw_write,
    },
    { .name = "CNTP_CVAL_S", .cp = 15, .crm = 14, .opc1 = 2,
      .secure = ARM_CP_SECSTATE_S,
      .access = PL0_RW,
      .type = ARM_CP_64BIT | ARM_CP_IO | ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_SEC].cval),
      .accessfn = gt_ptimer_access,
      .writefn = gt_sec_cval_write, .raw_writefn = raw_write,
    },
    { .name = "CNTP_CVAL_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 2, .opc2 = 2,
      .access = PL0_RW,
      .type = ARM_CP_IO,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_PHYS].cval),
      .resetvalue = 0, .accessfn = gt_ptimer_access,
      .readfn = gt_phys_redir_cval_read, .raw_readfn = raw_read,
      .writefn = gt_phys_redir_cval_write, .raw_writefn = raw_write,
    },
    { .name = "CNTV_CVAL", .cp = 15, .crm = 14, .opc1 = 3,
      .access = PL0_RW,
      .type = ARM_CP_64BIT | ARM_CP_IO | ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_VIRT].cval),
      .accessfn = gt_vtimer_access,
      .readfn = gt_virt_redir_cval_read, .raw_readfn = raw_read,
      .writefn = gt_virt_redir_cval_write, .raw_writefn = raw_write,
    },
    { .name = "CNTV_CVAL_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 3, .opc2 = 2,
      .access = PL0_RW,
      .type = ARM_CP_IO,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_VIRT].cval),
      .resetvalue = 0, .accessfn = gt_vtimer_access,
      .readfn = gt_virt_redir_cval_read, .raw_readfn = raw_read,
      .writefn = gt_virt_redir_cval_write, .raw_writefn = raw_write,
    },
    /*
     * Secure timer -- this is actually restricted to only EL3
     * and configurably Secure-EL1 via the accessfn.
     */
    { .name = "CNTPS_TVAL_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 7, .crn = 14, .crm = 2, .opc2 = 0,
      .type = ARM_CP_NO_RAW | ARM_CP_IO, .access = PL1_RW,
      .accessfn = gt_stimer_access,
      .readfn = gt_sec_tval_read,
      .writefn = gt_sec_tval_write,
      .resetfn = gt_sec_timer_reset,
    },
    { .name = "CNTPS_CTL_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 7, .crn = 14, .crm = 2, .opc2 = 1,
      .type = ARM_CP_IO, .access = PL1_RW,
      .accessfn = gt_stimer_access,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_SEC].ctl),
      .resetvalue = 0,
      .writefn = gt_sec_ctl_write, .raw_writefn = raw_write,
    },
    { .name = "CNTPS_CVAL_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 7, .crn = 14, .crm = 2, .opc2 = 2,
      .type = ARM_CP_IO, .access = PL1_RW,
      .accessfn = gt_stimer_access,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_SEC].cval),
      .writefn = gt_sec_cval_write, .raw_writefn = raw_write,
    },
    REGINFO_SENTINEL
};

static CPAccessResult e2h_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                 bool isread)
{
    if (!(arm_hcr_el2_eff(env) & HCR_E2H)) {
        return CP_ACCESS_TRAP;
    }
    return CP_ACCESS_OK;
}

#else

/*
 * In user-mode most of the generic timer registers are inaccessible
 * however modern kernels (4.12+) allow access to cntvct_el0
 */

static uint64_t gt_virt_cnt_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    ARMCPU *cpu = env_archcpu(env);

    /*
     * Currently we have no support for QEMUTimer in linux-user so we
     * can't call gt_get_countervalue(env), instead we directly
     * call the lower level functions.
     */
    return cpu_get_clock() / gt_cntfrq_period_ns(cpu);
}

static const ARMCPRegInfo generic_timer_cp_reginfo[] = {
    { .name = "CNTFRQ_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 0, .opc2 = 0,
      .type = ARM_CP_CONST, .access = PL0_R /* no PL1_RW in linux-user */,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_cntfrq),
      .resetvalue = NANOSECONDS_PER_SECOND / GTIMER_SCALE,
    },
    { .name = "CNTVCT_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 0, .opc2 = 2,
      .access = PL0_R, .type = ARM_CP_NO_RAW | ARM_CP_IO,
      .readfn = gt_virt_cnt_read,
    },
    REGINFO_SENTINEL
};

#endif

static void par_write(CPUARMState *env, const ARMCPRegInfo *ri, uint64_t value)
{
    if (arm_feature(env, ARM_FEATURE_LPAE)) {
        raw_write(env, ri, value);
    } else if (arm_feature(env, ARM_FEATURE_V7)) {
        raw_write(env, ri, value & 0xfffff6ff);
    } else {
        raw_write(env, ri, value & 0xfffff1ff);
    }
}

#ifndef CONFIG_USER_ONLY
/* get_phys_addr() isn't present for user-mode-only targets */

static CPAccessResult ats_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                 bool isread)
{
    if (ri->opc2 & 4) {
        /*
         * The ATS12NSO* operations must trap to EL3 or EL2 if executed in
         * Secure EL1 (which can only happen if EL3 is AArch64).
         * They are simply UNDEF if executed from NS EL1.
         * They function normally from EL2 or EL3.
         */
        if (arm_current_el(env) == 1) {
            if (arm_is_secure_below_el3(env)) {
                if (env->cp15.scr_el3 & SCR_EEL2) {
                    return CP_ACCESS_TRAP_UNCATEGORIZED_EL2;
                }
                return CP_ACCESS_TRAP_UNCATEGORIZED_EL3;
            }
            return CP_ACCESS_TRAP_UNCATEGORIZED;
        }
    }
    return CP_ACCESS_OK;
}

static uint64_t do_ats_write(CPUARMState *env, uint64_t value,
                             MMUAccessType access_type, ARMMMUIdx mmu_idx)
{
    hwaddr phys_addr;
    target_ulong page_size;
    int prot;
    bool ret;
    uint64_t par64;
    bool format64 = false;
    MemTxAttrs attrs = {};
    ARMMMUFaultInfo fi = {};
    ARMCacheAttrs cacheattrs = {};

    ret = get_phys_addr(env, value, access_type, mmu_idx, &phys_addr, &attrs,
                        &prot, &page_size, &fi, &cacheattrs);

    if (ret) {
        /*
         * Some kinds of translation fault must cause exceptions rather
         * than being reported in the PAR.
         */
        int current_el = arm_current_el(env);
        int target_el;
        uint32_t syn, fsr, fsc;
        bool take_exc = false;

        if (fi.s1ptw && current_el == 1
            && arm_mmu_idx_is_stage1_of_2(mmu_idx)) {
            /*
             * Synchronous stage 2 fault on an access made as part of the
             * translation table walk for AT S1E0* or AT S1E1* insn
             * executed from NS EL1. If this is a synchronous external abort
             * and SCR_EL3.EA == 1, then we take a synchronous external abort
             * to EL3. Otherwise the fault is taken as an exception to EL2,
             * and HPFAR_EL2 holds the faulting IPA.
             */
            if (fi.type == ARMFault_SyncExternalOnWalk &&
                (env->cp15.scr_el3 & SCR_EA)) {
                target_el = 3;
            } else {
                env->cp15.hpfar_el2 = extract64(fi.s2addr, 12, 47) << 4;
                if (arm_is_secure_below_el3(env) && fi.s1ns) {
                    env->cp15.hpfar_el2 |= HPFAR_NS;
                }
                target_el = 2;
            }
            take_exc = true;
        } else if (fi.type == ARMFault_SyncExternalOnWalk) {
            /*
             * Synchronous external aborts during a translation table walk
             * are taken as Data Abort exceptions.
             */
            if (fi.stage2) {
                if (current_el == 3) {
                    target_el = 3;
                } else {
                    target_el = 2;
                }
            } else {
                target_el = exception_target_el(env);
            }
            take_exc = true;
        }

        if (take_exc) {
            /* Construct FSR and FSC using same logic as arm_deliver_fault() */
            if (target_el == 2 || arm_el_is_aa64(env, target_el) ||
                arm_s1_regime_using_lpae_format(env, mmu_idx)) {
                fsr = arm_fi_to_lfsc(&fi);
                fsc = extract32(fsr, 0, 6);
            } else {
                fsr = arm_fi_to_sfsc(&fi);
                fsc = 0x3f;
            }
            /*
             * Report exception with ESR indicating a fault due to a
             * translation table walk for a cache maintenance instruction.
             */
            syn = syn_data_abort_no_iss(current_el == target_el, 0,
                                        fi.ea, 1, fi.s1ptw, 1, fsc);
            env->exception.vaddress = value;
            env->exception.fsr = fsr;
            raise_exception(env, EXCP_DATA_ABORT, syn, target_el);
        }
    }

    if (is_a64(env)) {
        format64 = true;
    } else if (arm_feature(env, ARM_FEATURE_LPAE)) {
        /*
         * ATS1Cxx:
         * * TTBCR.EAE determines whether the result is returned using the
         *   32-bit or the 64-bit PAR format
         * * Instructions executed in Hyp mode always use the 64bit format
         *
         * ATS1S2NSOxx uses the 64bit format if any of the following is true:
         * * The Non-secure TTBCR.EAE bit is set to 1
         * * The implementation includes EL2, and the value of HCR.VM is 1
         *
         * (Note that HCR.DC makes HCR.VM behave as if it is 1.)
         *
         * ATS1Hx always uses the 64bit format.
         */
        format64 = arm_s1_regime_using_lpae_format(env, mmu_idx);

        if (arm_feature(env, ARM_FEATURE_EL2)) {
            if (mmu_idx == ARMMMUIdx_E10_0 ||
                mmu_idx == ARMMMUIdx_E10_1 ||
                mmu_idx == ARMMMUIdx_E10_1_PAN) {
                format64 |= env->cp15.hcr_el2 & (HCR_VM | HCR_DC);
            } else {
                format64 |= arm_current_el(env) == 2;
            }
        }
    }

    if (format64) {
        /* Create a 64-bit PAR */
        par64 = (1 << 11); /* LPAE bit always set */
        if (!ret) {
            par64 |= phys_addr & ~0xfffULL;
            if (!attrs.secure) {
                par64 |= (1 << 9); /* NS */
            }
            par64 |= (uint64_t)cacheattrs.attrs << 56; /* ATTR */
            par64 |= cacheattrs.shareability << 7; /* SH */
        } else {
            uint32_t fsr = arm_fi_to_lfsc(&fi);

            par64 |= 1; /* F */
            par64 |= (fsr & 0x3f) << 1; /* FS */
            if (fi.stage2) {
                par64 |= (1 << 9); /* S */
            }
            if (fi.s1ptw) {
                par64 |= (1 << 8); /* PTW */
            }
        }
    } else {
        /*
         * fsr is a DFSR/IFSR value for the short descriptor
         * translation table format (with WnR always clear).
         * Convert it to a 32-bit PAR.
         */
        if (!ret) {
            /* We do not set any attribute bits in the PAR */
            if (page_size == (1 << 24)
                && arm_feature(env, ARM_FEATURE_V7)) {
                par64 = (phys_addr & 0xff000000) | (1 << 1);
            } else {
                par64 = phys_addr & 0xfffff000;
            }
            if (!attrs.secure) {
                par64 |= (1 << 9); /* NS */
            }
        } else {
            uint32_t fsr = arm_fi_to_sfsc(&fi);

            par64 = ((fsr & (1 << 10)) >> 5) | ((fsr & (1 << 12)) >> 6) |
                    ((fsr & 0xf) << 1) | 1;
        }
    }
    return par64;
}

static void ats_write(CPUARMState *env, const ARMCPRegInfo *ri, uint64_t value)
{
    MMUAccessType access_type = ri->opc2 & 1 ? MMU_DATA_STORE : MMU_DATA_LOAD;
    uint64_t par64;
    ARMMMUIdx mmu_idx;
    int el = arm_current_el(env);
    bool secure = arm_is_secure_below_el3(env);

    switch (ri->opc2 & 6) {
    case 0:
        /* stage 1 current state PL1: ATS1CPR, ATS1CPW, ATS1CPRP, ATS1CPWP */
        switch (el) {
        case 3:
            mmu_idx = ARMMMUIdx_SE3;
            break;
        case 2:
            g_assert(!secure);  /* ARMv8.4-SecEL2 is 64-bit only */
            /* fall through */
        case 1:
            if (ri->crm == 9 && (env->uncached_cpsr & CPSR_PAN)) {
                mmu_idx = (secure ? ARMMMUIdx_Stage1_SE1_PAN
                           : ARMMMUIdx_Stage1_E1_PAN);
            } else {
                mmu_idx = secure ? ARMMMUIdx_Stage1_SE1 : ARMMMUIdx_Stage1_E1;
            }
            break;
        default:
            g_assert_not_reached();
        }
        break;
    case 2:
        /* stage 1 current state PL0: ATS1CUR, ATS1CUW */
        switch (el) {
        case 3:
            mmu_idx = ARMMMUIdx_SE10_0;
            break;
        case 2:
            g_assert(!secure);  /* ARMv8.4-SecEL2 is 64-bit only */
            mmu_idx = ARMMMUIdx_Stage1_E0;
            break;
        case 1:
            mmu_idx = secure ? ARMMMUIdx_Stage1_SE0 : ARMMMUIdx_Stage1_E0;
            break;
        default:
            g_assert_not_reached();
        }
        break;
    case 4:
        /* stage 1+2 NonSecure PL1: ATS12NSOPR, ATS12NSOPW */
        mmu_idx = ARMMMUIdx_E10_1;
        break;
    case 6:
        /* stage 1+2 NonSecure PL0: ATS12NSOUR, ATS12NSOUW */
        mmu_idx = ARMMMUIdx_E10_0;
        break;
    default:
        g_assert_not_reached();
    }

    par64 = do_ats_write(env, value, access_type, mmu_idx);

    A32_BANKED_CURRENT_REG_SET(env, par, par64);
}

static void ats1h_write(CPUARMState *env, const ARMCPRegInfo *ri,
                        uint64_t value)
{
    MMUAccessType access_type = ri->opc2 & 1 ? MMU_DATA_STORE : MMU_DATA_LOAD;
    uint64_t par64;

    par64 = do_ats_write(env, value, access_type, ARMMMUIdx_E2);

    A32_BANKED_CURRENT_REG_SET(env, par, par64);
}

static CPAccessResult at_s1e2_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                     bool isread)
{
    if (arm_current_el(env) == 3 &&
        !(env->cp15.scr_el3 & (SCR_NS | SCR_EEL2))) {
        return CP_ACCESS_TRAP;
    }
    return CP_ACCESS_OK;
}

static void ats_write64(CPUARMState *env, const ARMCPRegInfo *ri,
                        uint64_t value)
{
    MMUAccessType access_type = ri->opc2 & 1 ? MMU_DATA_STORE : MMU_DATA_LOAD;
    ARMMMUIdx mmu_idx;
    int secure = arm_is_secure_below_el3(env);

    switch (ri->opc2 & 6) {
    case 0:
        switch (ri->opc1) {
        case 0: /* AT S1E1R, AT S1E1W, AT S1E1RP, AT S1E1WP */
            if (ri->crm == 9 && (env->pstate & PSTATE_PAN)) {
                mmu_idx = (secure ? ARMMMUIdx_Stage1_SE1_PAN
                           : ARMMMUIdx_Stage1_E1_PAN);
            } else {
                mmu_idx = secure ? ARMMMUIdx_Stage1_SE1 : ARMMMUIdx_Stage1_E1;
            }
            break;
        case 4: /* AT S1E2R, AT S1E2W */
            mmu_idx = secure ? ARMMMUIdx_SE2 : ARMMMUIdx_E2;
            break;
        case 6: /* AT S1E3R, AT S1E3W */
            mmu_idx = ARMMMUIdx_SE3;
            break;
        default:
            g_assert_not_reached();
        }
        break;
    case 2: /* AT S1E0R, AT S1E0W */
        mmu_idx = secure ? ARMMMUIdx_Stage1_SE0 : ARMMMUIdx_Stage1_E0;
        break;
    case 4: /* AT S12E1R, AT S12E1W */
        mmu_idx = secure ? ARMMMUIdx_SE10_1 : ARMMMUIdx_E10_1;
        break;
    case 6: /* AT S12E0R, AT S12E0W */
        mmu_idx = secure ? ARMMMUIdx_SE10_0 : ARMMMUIdx_E10_0;
        break;
    default:
        g_assert_not_reached();
    }

    env->cp15.par_el[1] = do_ats_write(env, value, access_type, mmu_idx);
}
#endif

static const ARMCPRegInfo vapa_cp_reginfo[] = {
    { .name = "PAR", .cp = 15, .crn = 7, .crm = 4, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW, .resetvalue = 0,
      .bank_fieldoffsets = { offsetoflow32(CPUARMState, cp15.par_s),
                             offsetoflow32(CPUARMState, cp15.par_ns) },
      .writefn = par_write },
#ifndef CONFIG_USER_ONLY
    /* This underdecoding is safe because the reginfo is NO_RAW. */
    { .name = "ATS", .cp = 15, .crn = 7, .crm = 8, .opc1 = 0, .opc2 = CP_ANY,
      .access = PL1_W, .accessfn = ats_access,
      .writefn = ats_write, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC },
#endif
    REGINFO_SENTINEL
};

/* Return basic MPU access permission bits.  */
static uint32_t simple_mpu_ap_bits(uint32_t val)
{
    uint32_t ret;
    uint32_t mask;
    int i;
    ret = 0;
    mask = 3;
    for (i = 0; i < 16; i += 2) {
        ret |= (val >> i) & mask;
        mask <<= 2;
    }
    return ret;
}

/* Pad basic MPU access permission bits to extended format.  */
static uint32_t extended_mpu_ap_bits(uint32_t val)
{
    uint32_t ret;
    uint32_t mask;
    int i;
    ret = 0;
    mask = 3;
    for (i = 0; i < 16; i += 2) {
        ret |= (val & mask) << i;
        mask <<= 2;
    }
    return ret;
}

static void pmsav5_data_ap_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                 uint64_t value)
{
    env->cp15.pmsav5_data_ap = extended_mpu_ap_bits(value);
}

static uint64_t pmsav5_data_ap_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return simple_mpu_ap_bits(env->cp15.pmsav5_data_ap);
}

static void pmsav5_insn_ap_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                 uint64_t value)
{
    env->cp15.pmsav5_insn_ap = extended_mpu_ap_bits(value);
}

static uint64_t pmsav5_insn_ap_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return simple_mpu_ap_bits(env->cp15.pmsav5_insn_ap);
}

static uint64_t pmsav7_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    uint32_t *u32p = *(uint32_t **)raw_ptr(env, ri);

    if (!u32p) {
        return 0;
    }

    u32p += env->pmsav7.rnr[M_REG_NS];
    return *u32p;
}

static void pmsav7_write(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    uint32_t *u32p = *(uint32_t **)raw_ptr(env, ri);

    if (!u32p) {
        return;
    }

    u32p += env->pmsav7.rnr[M_REG_NS];
    tlb_flush(CPU(cpu)); /* Mappings may have changed - purge! */
    *u32p = value;
}

static void pmsav7_rgnr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    uint32_t nrgs = cpu->pmsav7_dregion;

    if (value >= nrgs) {
        qemu_log_mask(LOG_GUEST_ERROR,
                      "PMSAv7 RGNR write >= # supported regions, %" PRIu32
                      " > %" PRIu32 "\n", (uint32_t)value, nrgs);
        return;
    }

    raw_write(env, ri, value);
}

static const ARMCPRegInfo pmsav7_cp_reginfo[] = {
    /*
     * Reset for all these registers is handled in arm_cpu_reset(),
     * because the PMSAv7 is also used by M-profile CPUs, which do
     * not register cpregs but still need the state to be reset.
     */
    { .name = "DRBAR", .cp = 15, .crn = 6, .opc1 = 0, .crm = 1, .opc2 = 0,
      .access = PL1_RW, .type = ARM_CP_NO_RAW,
      .fieldoffset = offsetof(CPUARMState, pmsav7.drbar),
      .readfn = pmsav7_read, .writefn = pmsav7_write,
      .resetfn = arm_cp_reset_ignore },
    { .name = "DRSR", .cp = 15, .crn = 6, .opc1 = 0, .crm = 1, .opc2 = 2,
      .access = PL1_RW, .type = ARM_CP_NO_RAW,
      .fieldoffset = offsetof(CPUARMState, pmsav7.drsr),
      .readfn = pmsav7_read, .writefn = pmsav7_write,
      .resetfn = arm_cp_reset_ignore },
    { .name = "DRACR", .cp = 15, .crn = 6, .opc1 = 0, .crm = 1, .opc2 = 4,
      .access = PL1_RW, .type = ARM_CP_NO_RAW,
      .fieldoffset = offsetof(CPUARMState, pmsav7.dracr),
      .readfn = pmsav7_read, .writefn = pmsav7_write,
      .resetfn = arm_cp_reset_ignore },
    { .name = "RGNR", .cp = 15, .crn = 6, .opc1 = 0, .crm = 2, .opc2 = 0,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, pmsav7.rnr[M_REG_NS]),
      .writefn = pmsav7_rgnr_write,
      .resetfn = arm_cp_reset_ignore },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo pmsav5_cp_reginfo[] = {
    { .name = "DATA_AP", .cp = 15, .crn = 5, .crm = 0, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW, .type = ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, cp15.pmsav5_data_ap),
      .readfn = pmsav5_data_ap_read, .writefn = pmsav5_data_ap_write, },
    { .name = "INSN_AP", .cp = 15, .crn = 5, .crm = 0, .opc1 = 0, .opc2 = 1,
      .access = PL1_RW, .type = ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, cp15.pmsav5_insn_ap),
      .readfn = pmsav5_insn_ap_read, .writefn = pmsav5_insn_ap_write, },
    { .name = "DATA_EXT_AP", .cp = 15, .crn = 5, .crm = 0, .opc1 = 0, .opc2 = 2,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.pmsav5_data_ap),
      .resetvalue = 0, },
    { .name = "INSN_EXT_AP", .cp = 15, .crn = 5, .crm = 0, .opc1 = 0, .opc2 = 3,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.pmsav5_insn_ap),
      .resetvalue = 0, },
    { .name = "DCACHE_CFG", .cp = 15, .crn = 2, .crm = 0, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.c2_data), .resetvalue = 0, },
    { .name = "ICACHE_CFG", .cp = 15, .crn = 2, .crm = 0, .opc1 = 0, .opc2 = 1,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.c2_insn), .resetvalue = 0, },
    /* Protection region base and size registers */
    { .name = "946_PRBS0", .cp = 15, .crn = 6, .crm = 0, .opc1 = 0,
      .opc2 = CP_ANY, .access = PL1_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.c6_region[0]) },
    { .name = "946_PRBS1", .cp = 15, .crn = 6, .crm = 1, .opc1 = 0,
      .opc2 = CP_ANY, .access = PL1_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.c6_region[1]) },
    { .name = "946_PRBS2", .cp = 15, .crn = 6, .crm = 2, .opc1 = 0,
      .opc2 = CP_ANY, .access = PL1_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.c6_region[2]) },
    { .name = "946_PRBS3", .cp = 15, .crn = 6, .crm = 3, .opc1 = 0,
      .opc2 = CP_ANY, .access = PL1_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.c6_region[3]) },
    { .name = "946_PRBS4", .cp = 15, .crn = 6, .crm = 4, .opc1 = 0,
      .opc2 = CP_ANY, .access = PL1_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.c6_region[4]) },
    { .name = "946_PRBS5", .cp = 15, .crn = 6, .crm = 5, .opc1 = 0,
      .opc2 = CP_ANY, .access = PL1_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.c6_region[5]) },
    { .name = "946_PRBS6", .cp = 15, .crn = 6, .crm = 6, .opc1 = 0,
      .opc2 = CP_ANY, .access = PL1_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.c6_region[6]) },
    { .name = "946_PRBS7", .cp = 15, .crn = 6, .crm = 7, .opc1 = 0,
      .opc2 = CP_ANY, .access = PL1_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.c6_region[7]) },
    REGINFO_SENTINEL
};

static void vmsa_ttbcr_raw_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                 uint64_t value)
{
    TCR *tcr = raw_ptr(env, ri);
    int maskshift = extract32(value, 0, 3);

    if (!arm_feature(env, ARM_FEATURE_V8)) {
        if (arm_feature(env, ARM_FEATURE_LPAE) && (value & TTBCR_EAE)) {
            /*
             * Pre ARMv8 bits [21:19], [15:14] and [6:3] are UNK/SBZP when
             * using Long-desciptor translation table format
             */
            value &= ~((7 << 19) | (3 << 14) | (0xf << 3));
        } else if (arm_feature(env, ARM_FEATURE_EL3)) {
            /*
             * In an implementation that includes the Security Extensions
             * TTBCR has additional fields PD0 [4] and PD1 [5] for
             * Short-descriptor translation table format.
             */
            value &= TTBCR_PD1 | TTBCR_PD0 | TTBCR_N;
        } else {
            value &= TTBCR_N;
        }
    }

    /*
     * Update the masks corresponding to the TCR bank being written
     * Note that we always calculate mask and base_mask, but
     * they are only used for short-descriptor tables (ie if EAE is 0);
     * for long-descriptor tables the TCR fields are used differently
     * and the mask and base_mask values are meaningless.
     */
    tcr->raw_tcr = value;
    tcr->mask = ~(((uint32_t)0xffffffffu) >> maskshift);
    tcr->base_mask = ~((uint32_t)0x3fffu >> maskshift);
}

static void vmsa_ttbcr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                             uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    TCR *tcr = raw_ptr(env, ri);

    if (arm_feature(env, ARM_FEATURE_LPAE)) {
        /*
         * With LPAE the TTBCR could result in a change of ASID
         * via the TTBCR.A1 bit, so do a TLB flush.
         */
        tlb_flush(CPU(cpu));
    }
    /* Preserve the high half of TCR_EL1, set via TTBCR2.  */
    value = deposit64(tcr->raw_tcr, 0, 32, value);
    vmsa_ttbcr_raw_write(env, ri, value);
}

static void vmsa_ttbcr_reset(CPUARMState *env, const ARMCPRegInfo *ri)
{
    TCR *tcr = raw_ptr(env, ri);

    /*
     * Reset both the TCR as well as the masks corresponding to the bank of
     * the TCR being reset.
     */
    tcr->raw_tcr = 0;
    tcr->mask = 0;
    tcr->base_mask = 0xffffc000u;
}

static void vmsa_tcr_el12_write(CPUARMState *env, const ARMCPRegInfo *ri,
                               uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    TCR *tcr = raw_ptr(env, ri);

    /* For AArch64 the A1 bit could result in a change of ASID, so TLB flush. */
    tlb_flush(CPU(cpu));
    tcr->raw_tcr = value;
}

static void vmsa_ttbr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                            uint64_t value)
{
    /* If the ASID changes (with a 64-bit write), we must flush the TLB.  */
    if (cpreg_field_is_64bit(ri) &&
        extract64(raw_read(env, ri) ^ value, 48, 16) != 0) {
        ARMCPU *cpu = env_archcpu(env);
        tlb_flush(CPU(cpu));
    }
    raw_write(env, ri, value);
}

static void vmsa_tcr_ttbr_el2_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                    uint64_t value)
{
    /*
     * If we are running with E2&0 regime, then an ASID is active.
     * Flush if that might be changing.  Note we're not checking
     * TCR_EL2.A1 to know if this is really the TTBRx_EL2 that
     * holds the active ASID, only checking the field that might.
     */
    if (extract64(raw_read(env, ri) ^ value, 48, 16) &&
        (arm_hcr_el2_eff(env) & HCR_E2H)) {
        uint16_t mask = ARMMMUIdxBit_E20_2 |
                        ARMMMUIdxBit_E20_2_PAN |
                        ARMMMUIdxBit_E20_0;

        if (arm_is_secure_below_el3(env)) {
            mask >>= ARM_MMU_IDX_A_NS;
        }

        tlb_flush_by_mmuidx(env_cpu(env), mask);
    }
    raw_write(env, ri, value);
}

static void vttbr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                        uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    CPUState *cs = CPU(cpu);

    /*
     * A change in VMID to the stage2 page table (Stage2) invalidates
     * the combined stage 1&2 tlbs (EL10_1 and EL10_0).
     */
    if (raw_read(env, ri) != value) {
        uint16_t mask = ARMMMUIdxBit_E10_1 |
                        ARMMMUIdxBit_E10_1_PAN |
                        ARMMMUIdxBit_E10_0;

        if (arm_is_secure_below_el3(env)) {
            mask >>= ARM_MMU_IDX_A_NS;
        }

        tlb_flush_by_mmuidx(cs, mask);
        raw_write(env, ri, value);
    }
}

static const ARMCPRegInfo vmsa_pmsa_cp_reginfo[] = {
    { .name = "DFSR", .cp = 15, .crn = 5, .crm = 0, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm, .type = ARM_CP_ALIAS,
      .bank_fieldoffsets = { offsetoflow32(CPUARMState, cp15.dfsr_s),
                             offsetoflow32(CPUARMState, cp15.dfsr_ns) }, },
    { .name = "IFSR", .cp = 15, .crn = 5, .crm = 0, .opc1 = 0, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_tvm_trvm, .resetvalue = 0,
      .bank_fieldoffsets = { offsetoflow32(CPUARMState, cp15.ifsr_s),
                             offsetoflow32(CPUARMState, cp15.ifsr_ns) } },
    { .name = "DFAR", .cp = 15, .opc1 = 0, .crn = 6, .crm = 0, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm, .resetvalue = 0,
      .bank_fieldoffsets = { offsetof(CPUARMState, cp15.dfar_s),
                             offsetof(CPUARMState, cp15.dfar_ns) } },
    { .name = "FAR_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .crn = 6, .crm = 0, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .fieldoffset = offsetof(CPUARMState, cp15.far_el[1]),
      .resetvalue = 0, },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo vmsa_cp_reginfo[] = {
    { .name = "ESR_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .crn = 5, .crm = 2, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .fieldoffset = offsetof(CPUARMState, cp15.esr_el[1]), .resetvalue = 0, },
    { .name = "TTBR0_EL1", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 0, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .writefn = vmsa_ttbr_write, .resetvalue = 0,
      .bank_fieldoffsets = { offsetof(CPUARMState, cp15.ttbr0_s),
                             offsetof(CPUARMState, cp15.ttbr0_ns) } },
    { .name = "TTBR1_EL1", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 0, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .writefn = vmsa_ttbr_write, .resetvalue = 0,
      .bank_fieldoffsets = { offsetof(CPUARMState, cp15.ttbr1_s),
                             offsetof(CPUARMState, cp15.ttbr1_ns) } },
    { .name = "TCR_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .crn = 2, .crm = 0, .opc1 = 0, .opc2 = 2,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .writefn = vmsa_tcr_el12_write,
      .resetfn = vmsa_ttbcr_reset, .raw_writefn = raw_write,
      .fieldoffset = offsetof(CPUARMState, cp15.tcr_el[1]) },
    { .name = "TTBCR", .cp = 15, .crn = 2, .crm = 0, .opc1 = 0, .opc2 = 2,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .type = ARM_CP_ALIAS, .writefn = vmsa_ttbcr_write,
      .raw_writefn = vmsa_ttbcr_raw_write,
      .bank_fieldoffsets = { offsetoflow32(CPUARMState, cp15.tcr_el[3]),
                             offsetoflow32(CPUARMState, cp15.tcr_el[1])} },
    REGINFO_SENTINEL
};

/*
 * Note that unlike TTBCR, writing to TTBCR2 does not require flushing
 * qemu tlbs nor adjusting cached masks.
 */
static const ARMCPRegInfo ttbcr2_reginfo = {
    .name = "TTBCR2", .cp = 15, .opc1 = 0, .crn = 2, .crm = 0, .opc2 = 3,
    .access = PL1_RW, .accessfn = access_tvm_trvm,
    .type = ARM_CP_ALIAS,
    .bank_fieldoffsets = { offsetofhigh32(CPUARMState, cp15.tcr_el[3]),
                           offsetofhigh32(CPUARMState, cp15.tcr_el[1]) },
};

static void omap_ticonfig_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                uint64_t value)
{
    env->cp15.c15_ticonfig = value & 0xe7;
    /* The OS_TYPE bit in this register changes the reported CPUID! */
    env->cp15.c0_cpuid = (value & (1 << 5)) ?
        ARM_CPUID_TI915T : ARM_CPUID_TI925T;
}

static void omap_threadid_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                uint64_t value)
{
    env->cp15.c15_threadid = value & 0xffff;
}

static void omap_wfi_write(CPUARMState *env, const ARMCPRegInfo *ri,
                           uint64_t value)
{
    /* Wait-for-interrupt (deprecated) */
    cpu_interrupt(env_cpu(env), CPU_INTERRUPT_HALT);
}

static void omap_cachemaint_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                  uint64_t value)
{
    /*
     * On OMAP there are registers indicating the max/min index of dcache lines
     * containing a dirty line; cache flush operations have to reset these.
     */
    env->cp15.c15_i_max = 0x000;
    env->cp15.c15_i_min = 0xff0;
}

static const ARMCPRegInfo omap_cp_reginfo[] = {
    { .name = "DFSR", .cp = 15, .crn = 5, .crm = CP_ANY,
      .opc1 = CP_ANY, .opc2 = CP_ANY, .access = PL1_RW, .type = ARM_CP_OVERRIDE,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.esr_el[1]),
      .resetvalue = 0, },
    { .name = "", .cp = 15, .crn = 15, .crm = 0, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW, .type = ARM_CP_NOP },
    { .name = "TICONFIG", .cp = 15, .crn = 15, .crm = 1, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.c15_ticonfig), .resetvalue = 0,
      .writefn = omap_ticonfig_write },
    { .name = "IMAX", .cp = 15, .crn = 15, .crm = 2, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.c15_i_max), .resetvalue = 0, },
    { .name = "IMIN", .cp = 15, .crn = 15, .crm = 3, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW, .resetvalue = 0xff0,
      .fieldoffset = offsetof(CPUARMState, cp15.c15_i_min) },
    { .name = "THREADID", .cp = 15, .crn = 15, .crm = 4, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.c15_threadid), .resetvalue = 0,
      .writefn = omap_threadid_write },
    { .name = "TI925T_STATUS", .cp = 15, .crn = 15,
      .crm = 8, .opc1 = 0, .opc2 = 0, .access = PL1_RW,
      .type = ARM_CP_NO_RAW,
      .readfn = arm_cp_read_zero, .writefn = omap_wfi_write, },
    /*
     * TODO: Peripheral port remap register:
     * On OMAP2 mcr p15, 0, rn, c15, c2, 4 sets up the interrupt controller
     * base address at $rn & ~0xfff and map size of 0x200 << ($rn & 0xfff),
     * when MMU is off.
     */
    { .name = "OMAP_CACHEMAINT", .cp = 15, .crn = 7, .crm = CP_ANY,
      .opc1 = 0, .opc2 = CP_ANY, .access = PL1_W,
      .type = ARM_CP_OVERRIDE | ARM_CP_NO_RAW,
      .writefn = omap_cachemaint_write },
    { .name = "C9", .cp = 15, .crn = 9,
      .crm = CP_ANY, .opc1 = CP_ANY, .opc2 = CP_ANY, .access = PL1_RW,
      .type = ARM_CP_CONST | ARM_CP_OVERRIDE, .resetvalue = 0 },
    REGINFO_SENTINEL
};

static void xscale_cpar_write(CPUARMState *env, const ARMCPRegInfo *ri,
                              uint64_t value)
{
    env->cp15.c15_cpar = value & 0x3fff;
}

static const ARMCPRegInfo xscale_cp_reginfo[] = {
    { .name = "XSCALE_CPAR",
      .cp = 15, .crn = 15, .crm = 1, .opc1 = 0, .opc2 = 0, .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.c15_cpar), .resetvalue = 0,
      .writefn = xscale_cpar_write, },
    { .name = "XSCALE_AUXCR",
      .cp = 15, .crn = 1, .crm = 0, .opc1 = 0, .opc2 = 1, .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.c1_xscaleauxcr),
      .resetvalue = 0, },
    /*
     * XScale specific cache-lockdown: since we have no cache we NOP these
     * and hope the guest does not really rely on cache behaviour.
     */
    { .name = "XSCALE_LOCK_ICACHE_LINE",
      .cp = 15, .opc1 = 0, .crn = 9, .crm = 1, .opc2 = 0,
      .access = PL1_W, .type = ARM_CP_NOP },
    { .name = "XSCALE_UNLOCK_ICACHE",
      .cp = 15, .opc1 = 0, .crn = 9, .crm = 1, .opc2 = 1,
      .access = PL1_W, .type = ARM_CP_NOP },
    { .name = "XSCALE_DCACHE_LOCK",
      .cp = 15, .opc1 = 0, .crn = 9, .crm = 2, .opc2 = 0,
      .access = PL1_RW, .type = ARM_CP_NOP },
    { .name = "XSCALE_UNLOCK_DCACHE",
      .cp = 15, .opc1 = 0, .crn = 9, .crm = 2, .opc2 = 1,
      .access = PL1_W, .type = ARM_CP_NOP },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo dummy_c15_cp_reginfo[] = {
    /*
     * RAZ/WI the whole crn=15 space, when we don't have a more specific
     * implementation of this implementation-defined space.
     * Ideally this should eventually disappear in favour of actually
     * implementing the correct behaviour for all cores.
     */
    { .name = "C15_IMPDEF", .cp = 15, .crn = 15,
      .crm = CP_ANY, .opc1 = CP_ANY, .opc2 = CP_ANY,
      .access = PL1_RW,
      .type = ARM_CP_CONST | ARM_CP_NO_RAW | ARM_CP_OVERRIDE,
      .resetvalue = 0 },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo cache_dirty_status_cp_reginfo[] = {
    /* Cache status: RAZ because we have no cache so it's always clean */
    { .name = "CDSR", .cp = 15, .crn = 7, .crm = 10, .opc1 = 0, .opc2 = 6,
      .access = PL1_R, .type = ARM_CP_CONST | ARM_CP_NO_RAW,
      .resetvalue = 0 },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo cache_block_ops_cp_reginfo[] = {
    /* We never have a a block transfer operation in progress */
    { .name = "BXSR", .cp = 15, .crn = 7, .crm = 12, .opc1 = 0, .opc2 = 4,
      .access = PL0_R, .type = ARM_CP_CONST | ARM_CP_NO_RAW,
      .resetvalue = 0 },
    /* The cache ops themselves: these all NOP for QEMU */
    { .name = "IICR", .cp = 15, .crm = 5, .opc1 = 0,
      .access = PL1_W, .type = ARM_CP_NOP|ARM_CP_64BIT },
    { .name = "IDCR", .cp = 15, .crm = 6, .opc1 = 0,
      .access = PL1_W, .type = ARM_CP_NOP|ARM_CP_64BIT },
    { .name = "CDCR", .cp = 15, .crm = 12, .opc1 = 0,
      .access = PL0_W, .type = ARM_CP_NOP|ARM_CP_64BIT },
    { .name = "PIR", .cp = 15, .crm = 12, .opc1 = 1,
      .access = PL0_W, .type = ARM_CP_NOP|ARM_CP_64BIT },
    { .name = "PDR", .cp = 15, .crm = 12, .opc1 = 2,
      .access = PL0_W, .type = ARM_CP_NOP|ARM_CP_64BIT },
    { .name = "CIDCR", .cp = 15, .crm = 14, .opc1 = 0,
      .access = PL1_W, .type = ARM_CP_NOP|ARM_CP_64BIT },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo cache_test_clean_cp_reginfo[] = {
    /*
     * The cache test-and-clean instructions always return (1 << 30)
     * to indicate that there are no dirty cache lines.
     */
    { .name = "TC_DCACHE", .cp = 15, .crn = 7, .crm = 10, .opc1 = 0, .opc2 = 3,
      .access = PL0_R, .type = ARM_CP_CONST | ARM_CP_NO_RAW,
      .resetvalue = (1 << 30) },
    { .name = "TCI_DCACHE", .cp = 15, .crn = 7, .crm = 14, .opc1 = 0, .opc2 = 3,
      .access = PL0_R, .type = ARM_CP_CONST | ARM_CP_NO_RAW,
      .resetvalue = (1 << 30) },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo strongarm_cp_reginfo[] = {
    /* Ignore ReadBuffer accesses */
    { .name = "C9_READBUFFER", .cp = 15, .crn = 9,
      .crm = CP_ANY, .opc1 = CP_ANY, .opc2 = CP_ANY,
      .access = PL1_RW, .resetvalue = 0,
      .type = ARM_CP_CONST | ARM_CP_OVERRIDE | ARM_CP_NO_RAW },
    REGINFO_SENTINEL
};

static uint64_t midr_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    unsigned int cur_el = arm_current_el(env);

    if (arm_is_el2_enabled(env) && cur_el == 1) {
        return env->cp15.vpidr_el2;
    }
    return raw_read(env, ri);
}

static uint64_t mpidr_read_val(CPUARMState *env)
{
    ARMCPU *cpu = env_archcpu(env);
    uint64_t mpidr = cpu->mp_affinity;

    if (arm_feature(env, ARM_FEATURE_V7MP)) {
        mpidr |= (1U << 31);
        /*
         * Cores which are uniprocessor (non-coherent)
         * but still implement the MP extensions set
         * bit 30. (For instance, Cortex-R5).
         */
        if (cpu->mp_is_up) {
            mpidr |= (1u << 30);
        }
    }
    return mpidr;
}

static uint64_t mpidr_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    unsigned int cur_el = arm_current_el(env);

    if (arm_is_el2_enabled(env) && cur_el == 1) {
        return env->cp15.vmpidr_el2;
    }
    return mpidr_read_val(env);
}

static const ARMCPRegInfo lpae_cp_reginfo[] = {
    /* NOP AMAIR0/1 */
    { .name = "AMAIR0", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .crn = 10, .crm = 3, .opc1 = 0, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    /* AMAIR1 is mapped to AMAIR_EL1[63:32] */
    { .name = "AMAIR1", .cp = 15, .crn = 10, .crm = 3, .opc1 = 0, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "PAR", .cp = 15, .crm = 7, .opc1 = 0,
      .access = PL1_RW, .type = ARM_CP_64BIT, .resetvalue = 0,
      .bank_fieldoffsets = { offsetof(CPUARMState, cp15.par_s),
                             offsetof(CPUARMState, cp15.par_ns)} },
    { .name = "TTBR0", .cp = 15, .crm = 2, .opc1 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .type = ARM_CP_64BIT | ARM_CP_ALIAS,
      .bank_fieldoffsets = { offsetof(CPUARMState, cp15.ttbr0_s),
                             offsetof(CPUARMState, cp15.ttbr0_ns) },
      .writefn = vmsa_ttbr_write, },
    { .name = "TTBR1", .cp = 15, .crm = 2, .opc1 = 1,
      .access = PL1_RW, .accessfn = access_tvm_trvm,
      .type = ARM_CP_64BIT | ARM_CP_ALIAS,
      .bank_fieldoffsets = { offsetof(CPUARMState, cp15.ttbr1_s),
                             offsetof(CPUARMState, cp15.ttbr1_ns) },
      .writefn = vmsa_ttbr_write, },
    REGINFO_SENTINEL
};

static uint64_t aa64_fpcr_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return vfp_get_fpcr(env);
}

static void aa64_fpcr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                            uint64_t value)
{
    vfp_set_fpcr(env, value);
}

static uint64_t aa64_fpsr_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return vfp_get_fpsr(env);
}

static void aa64_fpsr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                            uint64_t value)
{
    vfp_set_fpsr(env, value);
}

static CPAccessResult aa64_daif_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                       bool isread)
{
    if (arm_current_el(env) == 0 && !(arm_sctlr(env, 0) & SCTLR_UMA)) {
        return CP_ACCESS_TRAP;
    }
    return CP_ACCESS_OK;
}

static void aa64_daif_write(CPUARMState *env, const ARMCPRegInfo *ri,
                            uint64_t value)
{
    env->daif = value & PSTATE_DAIF;
}

static uint64_t aa64_pan_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return env->pstate & PSTATE_PAN;
}

static void aa64_pan_write(CPUARMState *env, const ARMCPRegInfo *ri,
                           uint64_t value)
{
    env->pstate = (env->pstate & ~PSTATE_PAN) | (value & PSTATE_PAN);
}

static const ARMCPRegInfo pan_reginfo = {
    .name = "PAN", .state = ARM_CP_STATE_AA64,
    .opc0 = 3, .opc1 = 0, .crn = 4, .crm = 2, .opc2 = 3,
    .type = ARM_CP_NO_RAW, .access = PL1_RW,
    .readfn = aa64_pan_read, .writefn = aa64_pan_write
};

static uint64_t aa64_uao_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return env->pstate & PSTATE_UAO;
}

static void aa64_uao_write(CPUARMState *env, const ARMCPRegInfo *ri,
                           uint64_t value)
{
    env->pstate = (env->pstate & ~PSTATE_UAO) | (value & PSTATE_UAO);
}

static const ARMCPRegInfo uao_reginfo = {
    .name = "UAO", .state = ARM_CP_STATE_AA64,
    .opc0 = 3, .opc1 = 0, .crn = 4, .crm = 2, .opc2 = 4,
    .type = ARM_CP_NO_RAW, .access = PL1_RW,
    .readfn = aa64_uao_read, .writefn = aa64_uao_write
};

static uint64_t aa64_dit_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return env->pstate & PSTATE_DIT;
}

static void aa64_dit_write(CPUARMState *env, const ARMCPRegInfo *ri,
                           uint64_t value)
{
    env->pstate = (env->pstate & ~PSTATE_DIT) | (value & PSTATE_DIT);
}

static const ARMCPRegInfo dit_reginfo = {
    .name = "DIT", .state = ARM_CP_STATE_AA64,
    .opc0 = 3, .opc1 = 3, .crn = 4, .crm = 2, .opc2 = 5,
    .type = ARM_CP_NO_RAW, .access = PL0_RW,
    .readfn = aa64_dit_read, .writefn = aa64_dit_write
};

static uint64_t aa64_ssbs_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return env->pstate & PSTATE_SSBS;
}

static void aa64_ssbs_write(CPUARMState *env, const ARMCPRegInfo *ri,
                           uint64_t value)
{
    env->pstate = (env->pstate & ~PSTATE_SSBS) | (value & PSTATE_SSBS);
}

static const ARMCPRegInfo ssbs_reginfo = {
    .name = "SSBS", .state = ARM_CP_STATE_AA64,
    .opc0 = 3, .opc1 = 3, .crn = 4, .crm = 2, .opc2 = 6,
    .type = ARM_CP_NO_RAW, .access = PL0_RW,
    .readfn = aa64_ssbs_read, .writefn = aa64_ssbs_write
};

static CPAccessResult aa64_cacheop_poc_access(CPUARMState *env,
                                              const ARMCPRegInfo *ri,
                                              bool isread)
{
    /* Cache invalidate/clean to Point of Coherency or Persistence...  */
    switch (arm_current_el(env)) {
    case 0:
        /* ... EL0 must UNDEF unless SCTLR_EL1.UCI is set.  */
        if (!(arm_sctlr(env, 0) & SCTLR_UCI)) {
            return CP_ACCESS_TRAP;
        }
        /* fall through */
    case 1:
        /* ... EL1 must trap to EL2 if HCR_EL2.TPCP is set.  */
        if (arm_hcr_el2_eff(env) & HCR_TPCP) {
            return CP_ACCESS_TRAP_EL2;
        }
        break;
    }
    return CP_ACCESS_OK;
}

static CPAccessResult aa64_cacheop_pou_access(CPUARMState *env,
                                              const ARMCPRegInfo *ri,
                                              bool isread)
{
    /* Cache invalidate/clean to Point of Unification... */
    switch (arm_current_el(env)) {
    case 0:
        /* ... EL0 must UNDEF unless SCTLR_EL1.UCI is set.  */
        if (!(arm_sctlr(env, 0) & SCTLR_UCI)) {
            return CP_ACCESS_TRAP;
        }
        /* fall through */
    case 1:
        /* ... EL1 must trap to EL2 if HCR_EL2.TPU is set.  */
        if (arm_hcr_el2_eff(env) & HCR_TPU) {
            return CP_ACCESS_TRAP_EL2;
        }
        break;
    }
    return CP_ACCESS_OK;
}

/*
 * See: D4.7.2 TLB maintenance requirements and the TLB maintenance instructions
 * Page D4-1736 (DDI0487A.b)
 */

static int vae1_tlbmask(CPUARMState *env)
{
    uint64_t hcr = arm_hcr_el2_eff(env);
    uint16_t mask;

    if ((hcr & (HCR_E2H | HCR_TGE)) == (HCR_E2H | HCR_TGE)) {
        mask = ARMMMUIdxBit_E20_2 |
               ARMMMUIdxBit_E20_2_PAN |
               ARMMMUIdxBit_E20_0;
    } else {
        mask = ARMMMUIdxBit_E10_1 |
               ARMMMUIdxBit_E10_1_PAN |
               ARMMMUIdxBit_E10_0;
    }

    if (arm_is_secure_below_el3(env)) {
        mask >>= ARM_MMU_IDX_A_NS;
    }

    return mask;
}

/* Return 56 if TBI is enabled, 64 otherwise. */
static int tlbbits_for_regime(CPUARMState *env, ARMMMUIdx mmu_idx,
                              uint64_t addr)
{
    uint64_t tcr = regime_tcr(env, mmu_idx)->raw_tcr;
    int tbi = aa64_va_parameter_tbi(tcr, mmu_idx);
    int select = extract64(addr, 55, 1);

    return (tbi >> select) & 1 ? 56 : 64;
}

static int vae1_tlbbits(CPUARMState *env, uint64_t addr)
{
    uint64_t hcr = arm_hcr_el2_eff(env);
    ARMMMUIdx mmu_idx;

    /* Only the regime of the mmu_idx below is significant. */
    if ((hcr & (HCR_E2H | HCR_TGE)) == (HCR_E2H | HCR_TGE)) {
        mmu_idx = ARMMMUIdx_E20_0;
    } else {
        mmu_idx = ARMMMUIdx_E10_0;
    }

    if (arm_is_secure_below_el3(env)) {
        mmu_idx &= ~ARM_MMU_IDX_A_NS;
    }

    return tlbbits_for_regime(env, mmu_idx, addr);
}

static void tlbi_aa64_vmalle1is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                      uint64_t value)
{
    CPUState *cs = env_cpu(env);
    int mask = vae1_tlbmask(env);

    tlb_flush_by_mmuidx_all_cpus_synced(cs, mask);
}

static void tlbi_aa64_vmalle1_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                    uint64_t value)
{
    CPUState *cs = env_cpu(env);
    int mask = vae1_tlbmask(env);

    if (tlb_force_broadcast(env)) {
        tlb_flush_by_mmuidx_all_cpus_synced(cs, mask);
    } else {
        tlb_flush_by_mmuidx(cs, mask);
    }
}

static int alle1_tlbmask(CPUARMState *env)
{
    /*
     * Note that the 'ALL' scope must invalidate both stage 1 and
     * stage 2 translations, whereas most other scopes only invalidate
     * stage 1 translations.
     */
    if (arm_is_secure_below_el3(env)) {
        return ARMMMUIdxBit_SE10_1 |
               ARMMMUIdxBit_SE10_1_PAN |
               ARMMMUIdxBit_SE10_0;
    } else {
        return ARMMMUIdxBit_E10_1 |
               ARMMMUIdxBit_E10_1_PAN |
               ARMMMUIdxBit_E10_0;
    }
}

static int e2_tlbmask(CPUARMState *env)
{
    if (arm_is_secure_below_el3(env)) {
        return ARMMMUIdxBit_SE20_0 |
               ARMMMUIdxBit_SE20_2 |
               ARMMMUIdxBit_SE20_2_PAN |
               ARMMMUIdxBit_SE2;
    } else {
        return ARMMMUIdxBit_E20_0 |
               ARMMMUIdxBit_E20_2 |
               ARMMMUIdxBit_E20_2_PAN |
               ARMMMUIdxBit_E2;
    }
}

static void tlbi_aa64_alle1_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                  uint64_t value)
{
    CPUState *cs = env_cpu(env);
    int mask = alle1_tlbmask(env);

    tlb_flush_by_mmuidx(cs, mask);
}

static void tlbi_aa64_alle2_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                  uint64_t value)
{
    CPUState *cs = env_cpu(env);
    int mask = e2_tlbmask(env);

    tlb_flush_by_mmuidx(cs, mask);
}

static void tlbi_aa64_alle3_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                  uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    CPUState *cs = CPU(cpu);

    tlb_flush_by_mmuidx(cs, ARMMMUIdxBit_SE3);
}

static void tlbi_aa64_alle1is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                    uint64_t value)
{
    CPUState *cs = env_cpu(env);
    int mask = alle1_tlbmask(env);

    tlb_flush_by_mmuidx_all_cpus_synced(cs, mask);
}

static void tlbi_aa64_alle2is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                    uint64_t value)
{
    CPUState *cs = env_cpu(env);
    int mask = e2_tlbmask(env);

    tlb_flush_by_mmuidx_all_cpus_synced(cs, mask);
}

static void tlbi_aa64_alle3is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                    uint64_t value)
{
    CPUState *cs = env_cpu(env);

    tlb_flush_by_mmuidx_all_cpus_synced(cs, ARMMMUIdxBit_SE3);
}

static void tlbi_aa64_vae2_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                 uint64_t value)
{
    /*
     * Invalidate by VA, EL2
     * Currently handles both VAE2 and VALE2, since we don't support
     * flush-last-level-only.
     */
    CPUState *cs = env_cpu(env);
    int mask = e2_tlbmask(env);
    uint64_t pageaddr = sextract64(value << 12, 0, 56);

    tlb_flush_page_by_mmuidx(cs, pageaddr, mask);
}

static void tlbi_aa64_vae3_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                 uint64_t value)
{
    /*
     * Invalidate by VA, EL3
     * Currently handles both VAE3 and VALE3, since we don't support
     * flush-last-level-only.
     */
    ARMCPU *cpu = env_archcpu(env);
    CPUState *cs = CPU(cpu);
    uint64_t pageaddr = sextract64(value << 12, 0, 56);

    tlb_flush_page_by_mmuidx(cs, pageaddr, ARMMMUIdxBit_SE3);
}

static void tlbi_aa64_vae1is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                   uint64_t value)
{
    CPUState *cs = env_cpu(env);
    int mask = vae1_tlbmask(env);
    uint64_t pageaddr = sextract64(value << 12, 0, 56);
    int bits = vae1_tlbbits(env, pageaddr);

    tlb_flush_page_bits_by_mmuidx_all_cpus_synced(cs, pageaddr, mask, bits);
}

static void tlbi_aa64_vae1_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                 uint64_t value)
{
    /*
     * Invalidate by VA, EL1&0 (AArch64 version).
     * Currently handles all of VAE1, VAAE1, VAALE1 and VALE1,
     * since we don't support flush-for-specific-ASID-only or
     * flush-last-level-only.
     */
    CPUState *cs = env_cpu(env);
    int mask = vae1_tlbmask(env);
    uint64_t pageaddr = sextract64(value << 12, 0, 56);
    int bits = vae1_tlbbits(env, pageaddr);

    if (tlb_force_broadcast(env)) {
        tlb_flush_page_bits_by_mmuidx_all_cpus_synced(cs, pageaddr, mask, bits);
    } else {
        tlb_flush_page_bits_by_mmuidx(cs, pageaddr, mask, bits);
    }
}

static void tlbi_aa64_vae2is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                   uint64_t value)
{
    CPUState *cs = env_cpu(env);
    uint64_t pageaddr = sextract64(value << 12, 0, 56);
    bool secure = arm_is_secure_below_el3(env);
    int mask = secure ? ARMMMUIdxBit_SE2 : ARMMMUIdxBit_E2;
    int bits = tlbbits_for_regime(env, secure ? ARMMMUIdx_SE2 : ARMMMUIdx_E2,
                                  pageaddr);

    tlb_flush_page_bits_by_mmuidx_all_cpus_synced(cs, pageaddr, mask, bits);
}

static void tlbi_aa64_vae3is_write(CPUARMState *env, const ARMCPRegInfo *ri,
                                   uint64_t value)
{
    CPUState *cs = env_cpu(env);
    uint64_t pageaddr = sextract64(value << 12, 0, 56);
    int bits = tlbbits_for_regime(env, ARMMMUIdx_SE3, pageaddr);

    tlb_flush_page_bits_by_mmuidx_all_cpus_synced(cs, pageaddr,
                                                  ARMMMUIdxBit_SE3, bits);
}

static CPAccessResult aa64_zva_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                      bool isread)
{
    int cur_el = arm_current_el(env);

    if (cur_el < 2) {
        uint64_t hcr = arm_hcr_el2_eff(env);

        if (cur_el == 0) {
            if ((hcr & (HCR_E2H | HCR_TGE)) == (HCR_E2H | HCR_TGE)) {
                if (!(env->cp15.sctlr_el[2] & SCTLR_DZE)) {
                    return CP_ACCESS_TRAP_EL2;
                }
            } else {
                if (!(env->cp15.sctlr_el[1] & SCTLR_DZE)) {
                    return CP_ACCESS_TRAP;
                }
                if (hcr & HCR_TDZ) {
                    return CP_ACCESS_TRAP_EL2;
                }
            }
        } else if (hcr & HCR_TDZ) {
            return CP_ACCESS_TRAP_EL2;
        }
    }
    return CP_ACCESS_OK;
}

static uint64_t aa64_dczid_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    ARMCPU *cpu = env_archcpu(env);
    int dzp_bit = 1 << 4;

    /* DZP indicates whether DC ZVA access is allowed */
    if (aa64_zva_access(env, NULL, false) == CP_ACCESS_OK) {
        dzp_bit = 0;
    }
    return cpu->dcz_blocksize | dzp_bit;
}

static CPAccessResult sp_el0_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                    bool isread)
{
    if (!(env->pstate & PSTATE_SP)) {
        /*
         * Access to SP_EL0 is undefined if it's being used as
         * the stack pointer.
         */
        return CP_ACCESS_TRAP_UNCATEGORIZED;
    }
    return CP_ACCESS_OK;
}

static uint64_t spsel_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return env->pstate & PSTATE_SP;
}

static void spsel_write(CPUARMState *env, const ARMCPRegInfo *ri, uint64_t val)
{
    update_spsel(env, val);
}

static void sctlr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                        uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);

    if (arm_feature(env, ARM_FEATURE_PMSA) && !cpu->has_mpu) {
        /* M bit is RAZ/WI for PMSA with no MPU implemented */
        value &= ~SCTLR_M;
    }

    /* ??? Lots of these bits are not implemented.  */

    if (ri->state == ARM_CP_STATE_AA64 && !cpu_isar_feature(aa64_mte, cpu)) {
        if (ri->opc1 == 6) { /* SCTLR_EL3 */
            value &= ~(SCTLR_ITFSB | SCTLR_TCF | SCTLR_ATA);
        } else {
            value &= ~(SCTLR_ITFSB | SCTLR_TCF0 | SCTLR_TCF |
                       SCTLR_ATA0 | SCTLR_ATA);
        }
    }

    if (raw_read(env, ri) == value) {
        /*
         * Skip the TLB flush if nothing actually changed; Linux likes
         * to do a lot of pointless SCTLR writes.
         */
        return;
    }

    raw_write(env, ri, value);

    /* This may enable/disable the MMU, so do a TLB flush.  */
    tlb_flush(CPU(cpu));

    if (ri->type & ARM_CP_SUPPRESS_TB_END) {
        /*
         * Normally we would always end the TB on an SCTLR write; see the
         * comment in ARMCPRegInfo sctlr initialization below for why Xscale
         * is special.  Setting ARM_CP_SUPPRESS_TB_END also stops the rebuild
         * of hflags from the translator, so do it here.
         */
        arm_rebuild_hflags(env);
    }
}

static CPAccessResult fpexc32_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                     bool isread)
{
    if ((env->cp15.cptr_el[2] & CPTR_TFP) && arm_current_el(env) == 2) {
        return CP_ACCESS_TRAP_FP_EL2;
    }
    if (env->cp15.cptr_el[3] & CPTR_TFP) {
        return CP_ACCESS_TRAP_FP_EL3;
    }
    return CP_ACCESS_OK;
}

static void sdcr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                       uint64_t value)
{
    env->cp15.mdcr_el3 = value & SDCR_VALID_MASK;
}

static const ARMCPRegInfo v8_cp_reginfo[] = {
    /*
     * Minimal set of EL0-visible registers. This will need to be expanded
     * significantly for system emulation of AArch64 CPUs.
     */
    { .name = "NZCV", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .opc2 = 0, .crn = 4, .crm = 2,
      .access = PL0_RW, .type = ARM_CP_NZCV },
    { .name = "DAIF", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .opc2 = 1, .crn = 4, .crm = 2,
      .type = ARM_CP_NO_RAW,
      .access = PL0_RW, .accessfn = aa64_daif_access,
      .fieldoffset = offsetof(CPUARMState, daif),
      .writefn = aa64_daif_write, .resetfn = arm_cp_reset_ignore },
    { .name = "FPCR", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .opc2 = 0, .crn = 4, .crm = 4,
      .access = PL0_RW, .type = ARM_CP_FPU | ARM_CP_SUPPRESS_TB_END,
      .readfn = aa64_fpcr_read, .writefn = aa64_fpcr_write },
    { .name = "FPSR", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .opc2 = 1, .crn = 4, .crm = 4,
      .access = PL0_RW, .type = ARM_CP_FPU | ARM_CP_SUPPRESS_TB_END,
      .readfn = aa64_fpsr_read, .writefn = aa64_fpsr_write },
    { .name = "DCZID_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .opc2 = 7, .crn = 0, .crm = 0,
      .access = PL0_R, .type = ARM_CP_NO_RAW,
      .readfn = aa64_dczid_read },
    { .name = "DC_ZVA", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 4, .opc2 = 1,
      .access = PL0_W, .type = ARM_CP_DC_ZVA,
#ifndef CONFIG_USER_ONLY
      /* Avoid overhead of an access check that always passes in user-mode */
      .accessfn = aa64_zva_access,
#endif
    },
    { .name = "CURRENTEL", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .opc2 = 2, .crn = 4, .crm = 2,
      .access = PL1_R, .type = ARM_CP_CURRENTEL },
    /* Cache ops: all NOPs since we don't emulate caches */
    { .name = "IC_IALLUIS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 1, .opc2 = 0,
      .access = PL1_W, .type = ARM_CP_NOP,
      .accessfn = aa64_cacheop_pou_access },
    { .name = "IC_IALLU", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 5, .opc2 = 0,
      .access = PL1_W, .type = ARM_CP_NOP,
      .accessfn = aa64_cacheop_pou_access },
    { .name = "IC_IVAU", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 5, .opc2 = 1,
      .access = PL0_W, .type = ARM_CP_NOP,
      .accessfn = aa64_cacheop_pou_access },
    { .name = "DC_IVAC", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 6, .opc2 = 1,
      .access = PL1_W, .accessfn = aa64_cacheop_poc_access,
      .type = ARM_CP_NOP },
    { .name = "DC_ISW", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 6, .opc2 = 2,
      .access = PL1_W, .accessfn = access_tsw, .type = ARM_CP_NOP },
    { .name = "DC_CVAC", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 10, .opc2 = 1,
      .access = PL0_W, .type = ARM_CP_NOP,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_CSW", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 10, .opc2 = 2,
      .access = PL1_W, .accessfn = access_tsw, .type = ARM_CP_NOP },
    { .name = "DC_CVAU", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 11, .opc2 = 1,
      .access = PL0_W, .type = ARM_CP_NOP,
      .accessfn = aa64_cacheop_pou_access },
    { .name = "DC_CIVAC", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 14, .opc2 = 1,
      .access = PL0_W, .type = ARM_CP_NOP,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_CISW", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 14, .opc2 = 2,
      .access = PL1_W, .accessfn = access_tsw, .type = ARM_CP_NOP },
    /* TLBI operations */
    { .name = "TLBI_VMALLE1IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 0,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vmalle1is_write },
    { .name = "TLBI_VAE1IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 1,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae1is_write },
    { .name = "TLBI_ASIDE1IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 2,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vmalle1is_write },
    { .name = "TLBI_VAAE1IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 3,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae1is_write },
    { .name = "TLBI_VALE1IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 5,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae1is_write },
    { .name = "TLBI_VAALE1IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 7,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae1is_write },
    { .name = "TLBI_VMALLE1", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 0,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vmalle1_write },
    { .name = "TLBI_VAE1", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 1,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae1_write },
    { .name = "TLBI_ASIDE1", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 2,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vmalle1_write },
    { .name = "TLBI_VAAE1", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 3,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae1_write },
    { .name = "TLBI_VALE1", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 5,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae1_write },
    { .name = "TLBI_VAALE1", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 7,
      .access = PL1_W, .accessfn = access_ttlb, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae1_write },
    { .name = "TLBI_IPAS2E1IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 0, .opc2 = 1,
      .access = PL2_W, .type = ARM_CP_NOP },
    { .name = "TLBI_IPAS2LE1IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 0, .opc2 = 5,
      .access = PL2_W, .type = ARM_CP_NOP },
    { .name = "TLBI_ALLE1IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 3, .opc2 = 4,
      .access = PL2_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_alle1is_write },
    { .name = "TLBI_VMALLS12E1IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 3, .opc2 = 6,
      .access = PL2_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_alle1is_write },
    { .name = "TLBI_IPAS2E1", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 4, .opc2 = 1,
      .access = PL2_W, .type = ARM_CP_NOP },
    { .name = "TLBI_IPAS2LE1", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 4, .opc2 = 5,
      .access = PL2_W, .type = ARM_CP_NOP },
    { .name = "TLBI_ALLE1", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 7, .opc2 = 4,
      .access = PL2_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_alle1_write },
    { .name = "TLBI_VMALLS12E1", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 7, .opc2 = 6,
      .access = PL2_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_alle1is_write },
#ifndef CONFIG_USER_ONLY
    /* 64 bit address translation operations */
    { .name = "AT_S1E1R", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 8, .opc2 = 0,
      .access = PL1_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    { .name = "AT_S1E1W", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 8, .opc2 = 1,
      .access = PL1_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    { .name = "AT_S1E0R", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 8, .opc2 = 2,
      .access = PL1_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    { .name = "AT_S1E0W", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 8, .opc2 = 3,
      .access = PL1_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    { .name = "AT_S12E1R", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 7, .crm = 8, .opc2 = 4,
      .access = PL2_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    { .name = "AT_S12E1W", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 7, .crm = 8, .opc2 = 5,
      .access = PL2_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    { .name = "AT_S12E0R", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 7, .crm = 8, .opc2 = 6,
      .access = PL2_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    { .name = "AT_S12E0W", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 7, .crm = 8, .opc2 = 7,
      .access = PL2_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    /* AT S1E2* are elsewhere as they UNDEF from EL3 if EL2 is not present */
    { .name = "AT_S1E3R", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 6, .crn = 7, .crm = 8, .opc2 = 0,
      .access = PL3_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    { .name = "AT_S1E3W", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 6, .crn = 7, .crm = 8, .opc2 = 1,
      .access = PL3_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    { .name = "PAR_EL1", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 0, .crn = 7, .crm = 4, .opc2 = 0,
      .access = PL1_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.par_el[1]),
      .writefn = par_write },
#endif
    /* TLB invalidate last level of translation table walk */
    { .name = "TLBIMVALIS", .cp = 15, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 5,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbimva_is_write },
    { .name = "TLBIMVAALIS", .cp = 15, .opc1 = 0, .crn = 8, .crm = 3, .opc2 = 7,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbimvaa_is_write },
    { .name = "TLBIMVAL", .cp = 15, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 5,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbimva_write },
    { .name = "TLBIMVAAL", .cp = 15, .opc1 = 0, .crn = 8, .crm = 7, .opc2 = 7,
      .type = ARM_CP_NO_RAW, .access = PL1_W, .accessfn = access_ttlb,
      .writefn = tlbimvaa_write },
    { .name = "TLBIMVALH", .cp = 15, .opc1 = 4, .crn = 8, .crm = 7, .opc2 = 5,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbimva_hyp_write },
    { .name = "TLBIMVALHIS",
      .cp = 15, .opc1 = 4, .crn = 8, .crm = 3, .opc2 = 5,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbimva_hyp_is_write },
    { .name = "TLBIIPAS2",
      .cp = 15, .opc1 = 4, .crn = 8, .crm = 4, .opc2 = 1,
      .type = ARM_CP_NOP, .access = PL2_W },
    { .name = "TLBIIPAS2IS",
      .cp = 15, .opc1 = 4, .crn = 8, .crm = 0, .opc2 = 1,
      .type = ARM_CP_NOP, .access = PL2_W },
    { .name = "TLBIIPAS2L",
      .cp = 15, .opc1 = 4, .crn = 8, .crm = 4, .opc2 = 5,
      .type = ARM_CP_NOP, .access = PL2_W },
    { .name = "TLBIIPAS2LIS",
      .cp = 15, .opc1 = 4, .crn = 8, .crm = 0, .opc2 = 5,
      .type = ARM_CP_NOP, .access = PL2_W },
    /* 32 bit cache operations */
    { .name = "ICIALLUIS", .cp = 15, .opc1 = 0, .crn = 7, .crm = 1, .opc2 = 0,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = aa64_cacheop_pou_access },
    { .name = "BPIALLUIS", .cp = 15, .opc1 = 0, .crn = 7, .crm = 1, .opc2 = 6,
      .type = ARM_CP_NOP, .access = PL1_W },
    { .name = "ICIALLU", .cp = 15, .opc1 = 0, .crn = 7, .crm = 5, .opc2 = 0,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = aa64_cacheop_pou_access },
    { .name = "ICIMVAU", .cp = 15, .opc1 = 0, .crn = 7, .crm = 5, .opc2 = 1,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = aa64_cacheop_pou_access },
    { .name = "BPIALL", .cp = 15, .opc1 = 0, .crn = 7, .crm = 5, .opc2 = 6,
      .type = ARM_CP_NOP, .access = PL1_W },
    { .name = "BPIMVA", .cp = 15, .opc1 = 0, .crn = 7, .crm = 5, .opc2 = 7,
      .type = ARM_CP_NOP, .access = PL1_W },
    { .name = "DCIMVAC", .cp = 15, .opc1 = 0, .crn = 7, .crm = 6, .opc2 = 1,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = aa64_cacheop_poc_access },
    { .name = "DCISW", .cp = 15, .opc1 = 0, .crn = 7, .crm = 6, .opc2 = 2,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = access_tsw },
    { .name = "DCCMVAC", .cp = 15, .opc1 = 0, .crn = 7, .crm = 10, .opc2 = 1,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = aa64_cacheop_poc_access },
    { .name = "DCCSW", .cp = 15, .opc1 = 0, .crn = 7, .crm = 10, .opc2 = 2,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = access_tsw },
    { .name = "DCCMVAU", .cp = 15, .opc1 = 0, .crn = 7, .crm = 11, .opc2 = 1,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = aa64_cacheop_pou_access },
    { .name = "DCCIMVAC", .cp = 15, .opc1 = 0, .crn = 7, .crm = 14, .opc2 = 1,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = aa64_cacheop_poc_access },
    { .name = "DCCISW", .cp = 15, .opc1 = 0, .crn = 7, .crm = 14, .opc2 = 2,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = access_tsw },
    /* MMU Domain access control / MPU write buffer control */
    { .name = "DACR", .cp = 15, .opc1 = 0, .crn = 3, .crm = 0, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tvm_trvm, .resetvalue = 0,
      .writefn = dacr_write, .raw_writefn = raw_write,
      .bank_fieldoffsets = { offsetoflow32(CPUARMState, cp15.dacr_s),
                             offsetoflow32(CPUARMState, cp15.dacr_ns) } },
    { .name = "ELR_EL1", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 0, .crn = 4, .crm = 0, .opc2 = 1,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, elr_el[1]) },
    { .name = "SPSR_EL1", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 0, .crn = 4, .crm = 0, .opc2 = 0,
      .access = PL1_RW,
      .fieldoffset = offsetof(CPUARMState, banked_spsr[BANK_SVC]) },
    /*
     * We rely on the access checks not allowing the guest to write to the
     * state field when SPSel indicates that it's being used as the stack
     * pointer.
     */
    { .name = "SP_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 4, .crm = 1, .opc2 = 0,
      .access = PL1_RW, .accessfn = sp_el0_access,
      .type = ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, sp_el[0]) },
    { .name = "SP_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 4, .crm = 1, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, sp_el[1]) },
    { .name = "SPSel", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 4, .crm = 2, .opc2 = 0,
      .type = ARM_CP_NO_RAW,
      .access = PL1_RW, .readfn = spsel_read, .writefn = spsel_write },
    { .name = "FPEXC32_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 5, .crm = 3, .opc2 = 0,
      .type = ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, vfp.xregs[ARM_VFP_FPEXC]),
      .access = PL2_RW, .accessfn = fpexc32_access },
    { .name = "DACR32_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 3, .crm = 0, .opc2 = 0,
      .access = PL2_RW, .resetvalue = 0,
      .writefn = dacr_write, .raw_writefn = raw_write,
      .fieldoffset = offsetof(CPUARMState, cp15.dacr32_el2) },
    { .name = "IFSR32_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 5, .crm = 0, .opc2 = 1,
      .access = PL2_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.ifsr32_el2) },
    { .name = "SPSR_IRQ", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 4, .crn = 4, .crm = 3, .opc2 = 0,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, banked_spsr[BANK_IRQ]) },
    { .name = "SPSR_ABT", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 4, .crn = 4, .crm = 3, .opc2 = 1,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, banked_spsr[BANK_ABT]) },
    { .name = "SPSR_UND", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 4, .crn = 4, .crm = 3, .opc2 = 2,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, banked_spsr[BANK_UND]) },
    { .name = "SPSR_FIQ", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 4, .crn = 4, .crm = 3, .opc2 = 3,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, banked_spsr[BANK_FIQ]) },
    { .name = "MDCR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 1, .crm = 3, .opc2 = 1,
      .resetvalue = 0,
      .access = PL3_RW, .fieldoffset = offsetof(CPUARMState, cp15.mdcr_el3) },
    { .name = "SDCR", .type = ARM_CP_ALIAS,
      .cp = 15, .opc1 = 0, .crn = 1, .crm = 3, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_trap_aa32s_el1,
      .writefn = sdcr_write,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.mdcr_el3) },
    REGINFO_SENTINEL
};

/* Used to describe the behaviour of EL2 regs when EL2 does not exist.  */
static const ARMCPRegInfo el3_no_el2_cp_reginfo[] = {
    { .name = "VBAR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 12, .crm = 0, .opc2 = 0,
      .access = PL2_RW,
      .readfn = arm_cp_read_zero, .writefn = arm_cp_write_ignore },
    { .name = "HCR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 0,
      .access = PL2_RW,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "HACR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 7,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "ESR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 5, .crm = 2, .opc2 = 0,
      .access = PL2_RW,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "CPTR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 2,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "MAIR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 10, .crm = 2, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "HMAIR1", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 4, .crn = 10, .crm = 2, .opc2 = 1,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "AMAIR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 10, .crm = 3, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "HAMAIR1", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 4, .crn = 10, .crm = 3, .opc2 = 1,
      .access = PL2_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "AFSR0_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 5, .crm = 1, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "AFSR1_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 5, .crm = 1, .opc2 = 1,
      .access = PL2_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "TCR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 0, .opc2 = 2,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "VTCR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 1, .opc2 = 2,
      .access = PL2_RW, .accessfn = access_el3_aa32ns,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "VTTBR", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 6, .crm = 2,
      .access = PL2_RW, .accessfn = access_el3_aa32ns,
      .type = ARM_CP_CONST | ARM_CP_64BIT, .resetvalue = 0 },
    { .name = "VTTBR_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 1, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "SCTLR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 0, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "TPIDR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 13, .crm = 0, .opc2 = 2,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "TTBR0_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 0, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "HTTBR", .cp = 15, .opc1 = 4, .crm = 2,
      .access = PL2_RW, .type = ARM_CP_64BIT | ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "CNTHCTL_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 1, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "CNTVOFF_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 0, .opc2 = 3,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "CNTVOFF", .cp = 15, .opc1 = 4, .crm = 14,
      .access = PL2_RW, .type = ARM_CP_64BIT | ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "CNTHP_CVAL_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 2, .opc2 = 2,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "CNTHP_CVAL", .cp = 15, .opc1 = 6, .crm = 14,
      .access = PL2_RW, .type = ARM_CP_64BIT | ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "CNTHP_TVAL_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 2, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "CNTHP_CTL_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 2, .opc2 = 1,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "MDCR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 1,
      .access = PL2_RW, .accessfn = access_tda,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "HPFAR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 6, .crm = 0, .opc2 = 4,
      .access = PL2_RW, .accessfn = access_el3_aa32ns,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "HSTR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 3,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "FAR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 6, .crm = 0, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "HIFAR", .state = ARM_CP_STATE_AA32,
      .type = ARM_CP_CONST,
      .cp = 15, .opc1 = 4, .crn = 6, .crm = 0, .opc2 = 2,
      .access = PL2_RW, .resetvalue = 0 },
    REGINFO_SENTINEL
};

/* Ditto, but for registers which exist in ARMv8 but not v7 */
static const ARMCPRegInfo el3_no_el2_v8_cp_reginfo[] = {
    { .name = "HCR2", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 4,
      .access = PL2_RW,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    REGINFO_SENTINEL
};

static void do_hcr_write(CPUARMState *env, uint64_t value, uint64_t valid_mask)
{
    ARMCPU *cpu = env_archcpu(env);

    if (arm_feature(env, ARM_FEATURE_V8)) {
        valid_mask |= MAKE_64BIT_MASK(0, 34);  /* ARMv8.0 */
    } else {
        valid_mask |= MAKE_64BIT_MASK(0, 28);  /* ARMv7VE */
    }

    if (arm_feature(env, ARM_FEATURE_EL3)) {
        valid_mask &= ~HCR_HCD;
    } else if (cpu->psci_conduit != QEMU_PSCI_CONDUIT_SMC) {
        /*
         * Architecturally HCR.TSC is RES0 if EL3 is not implemented.
         * However, if we're using the SMC PSCI conduit then QEMU is
         * effectively acting like EL3 firmware and so the guest at
         * EL2 should retain the ability to prevent EL1 from being
         * able to make SMC calls into the ersatz firmware, so in
         * that case HCR.TSC should be read/write.
         */
        valid_mask &= ~HCR_TSC;
    }

    if (arm_feature(env, ARM_FEATURE_AARCH64)) {
        if (cpu_isar_feature(aa64_vh, cpu)) {
            valid_mask |= HCR_E2H;
        }
        if (cpu_isar_feature(aa64_lor, cpu)) {
            valid_mask |= HCR_TLOR;
        }
        if (cpu_isar_feature(aa64_pauth, cpu)) {
            valid_mask |= HCR_API | HCR_APK;
        }
        if (cpu_isar_feature(aa64_mte, cpu)) {
            valid_mask |= HCR_ATA | HCR_DCT | HCR_TID5;
        }
    }

    /* Clear RES0 bits.  */
    value &= valid_mask;

    /*
     * These bits change the MMU setup:
     * HCR_VM enables stage 2 translation
     * HCR_PTW forbids certain page-table setups
     * HCR_DC disables stage1 and enables stage2 translation
     * HCR_DCT enables tagging on (disabled) stage1 translation
     */
    if ((env->cp15.hcr_el2 ^ value) & (HCR_VM | HCR_PTW | HCR_DC | HCR_DCT)) {
        tlb_flush(CPU(cpu));
    }
    env->cp15.hcr_el2 = value;

    /*
     * Updates to VI and VF require us to update the status of
     * virtual interrupts, which are the logical OR of these bits
     * and the state of the input lines from the GIC. (This requires
     * that we have the iothread lock, which is done by marking the
     * reginfo structs as ARM_CP_IO.)
     * Note that if a write to HCR pends a VIRQ or VFIQ it is never
     * possible for it to be taken immediately, because VIRQ and
     * VFIQ are masked unless running at EL0 or EL1, and HCR
     * can only be written at EL2.
     */
    g_assert(qemu_mutex_iothread_locked());
    arm_cpu_update_virq(cpu);
    arm_cpu_update_vfiq(cpu);
}

static void hcr_write(CPUARMState *env, const ARMCPRegInfo *ri, uint64_t value)
{
    do_hcr_write(env, value, 0);
}

static void hcr_writehigh(CPUARMState *env, const ARMCPRegInfo *ri,
                          uint64_t value)
{
    /* Handle HCR2 write, i.e. write to high half of HCR_EL2 */
    value = deposit64(env->cp15.hcr_el2, 32, 32, value);
    do_hcr_write(env, value, MAKE_64BIT_MASK(0, 32));
}

static void hcr_writelow(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
    /* Handle HCR write, i.e. write to low half of HCR_EL2 */
    value = deposit64(env->cp15.hcr_el2, 0, 32, value);
    do_hcr_write(env, value, MAKE_64BIT_MASK(32, 32));
}

static void cptr_el2_write(CPUARMState *env, const ARMCPRegInfo *ri,
                           uint64_t value)
{
    /*
     * For A-profile AArch32 EL3, if NSACR.CP10
     * is 0 then HCPTR.{TCP11,TCP10} ignore writes and read as 1.
     */
    if (arm_feature(env, ARM_FEATURE_EL3) && !arm_el_is_aa64(env, 3) &&
        !arm_is_secure(env) && !extract32(env->cp15.nsacr, 10, 1)) {
        value &= ~(0x3 << 10);
        value |= env->cp15.cptr_el[2] & (0x3 << 10);
    }
    env->cp15.cptr_el[2] = value;
}

static uint64_t cptr_el2_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    /*
     * For A-profile AArch32 EL3, if NSACR.CP10
     * is 0 then HCPTR.{TCP11,TCP10} ignore writes and read as 1.
     */
    uint64_t value = env->cp15.cptr_el[2];

    if (arm_feature(env, ARM_FEATURE_EL3) && !arm_el_is_aa64(env, 3) &&
        !arm_is_secure(env) && !extract32(env->cp15.nsacr, 10, 1)) {
        value |= 0x3 << 10;
    }
    return value;
}

static const ARMCPRegInfo el2_cp_reginfo[] = {
    { .name = "HCR_EL2", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_IO,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 0,
      .access = PL2_RW, .fieldoffset = offsetof(CPUARMState, cp15.hcr_el2),
      .writefn = hcr_write },
    { .name = "HCR", .state = ARM_CP_STATE_AA32,
      .type = ARM_CP_ALIAS | ARM_CP_IO,
      .cp = 15, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 0,
      .access = PL2_RW, .fieldoffset = offsetof(CPUARMState, cp15.hcr_el2),
      .writefn = hcr_writelow },
    { .name = "HACR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 7,
      .access = PL2_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "ELR_EL2", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 4, .crn = 4, .crm = 0, .opc2 = 1,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, elr_el[2]) },
    { .name = "ESR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 5, .crm = 2, .opc2 = 0,
      .access = PL2_RW, .fieldoffset = offsetof(CPUARMState, cp15.esr_el[2]) },
    { .name = "FAR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 6, .crm = 0, .opc2 = 0,
      .access = PL2_RW, .fieldoffset = offsetof(CPUARMState, cp15.far_el[2]) },
    { .name = "HIFAR", .state = ARM_CP_STATE_AA32,
      .type = ARM_CP_ALIAS,
      .cp = 15, .opc1 = 4, .crn = 6, .crm = 0, .opc2 = 2,
      .access = PL2_RW,
      .fieldoffset = offsetofhigh32(CPUARMState, cp15.far_el[2]) },
    { .name = "SPSR_EL2", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 4, .crn = 4, .crm = 0, .opc2 = 0,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, banked_spsr[BANK_HYP]) },
    { .name = "VBAR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 12, .crm = 0, .opc2 = 0,
      .access = PL2_RW, .writefn = vbar_write,
      .fieldoffset = offsetof(CPUARMState, cp15.vbar_el[2]),
      .resetvalue = 0 },
    { .name = "SP_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 4, .crm = 1, .opc2 = 0,
      .access = PL3_RW, .type = ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, sp_el[2]) },
    { .name = "CPTR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 2,
      .access = PL2_RW, .accessfn = cptr_access, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.cptr_el[2]),
      .readfn = cptr_el2_read, .writefn = cptr_el2_write },
    { .name = "MAIR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 10, .crm = 2, .opc2 = 0,
      .access = PL2_RW, .fieldoffset = offsetof(CPUARMState, cp15.mair_el[2]),
      .resetvalue = 0 },
    { .name = "HMAIR1", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 4, .crn = 10, .crm = 2, .opc2 = 1,
      .access = PL2_RW, .type = ARM_CP_ALIAS,
      .fieldoffset = offsetofhigh32(CPUARMState, cp15.mair_el[2]) },
    { .name = "AMAIR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 10, .crm = 3, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    /* HAMAIR1 is mapped to AMAIR_EL2[63:32] */
    { .name = "HAMAIR1", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 4, .crn = 10, .crm = 3, .opc2 = 1,
      .access = PL2_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "AFSR0_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 5, .crm = 1, .opc2 = 0,
      .access = PL2_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "AFSR1_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 5, .crm = 1, .opc2 = 1,
      .access = PL2_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "TCR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 0, .opc2 = 2,
      .access = PL2_RW, .writefn = vmsa_tcr_el12_write,
      /* no .raw_writefn or .resetfn needed as we never use mask/base_mask */
      .fieldoffset = offsetof(CPUARMState, cp15.tcr_el[2]) },
    { .name = "VTCR", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 4, .crn = 2, .crm = 1, .opc2 = 2,
      .type = ARM_CP_ALIAS,
      .access = PL2_RW, .accessfn = access_el3_aa32ns,
      .fieldoffset = offsetof(CPUARMState, cp15.vtcr_el2) },
    { .name = "VTCR_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 1, .opc2 = 2,
      .access = PL2_RW,
      /*
       * no .writefn needed as this can't cause an ASID change;
       * no .raw_writefn or .resetfn needed as we never use mask/base_mask
       */
      .fieldoffset = offsetof(CPUARMState, cp15.vtcr_el2) },
    { .name = "VTTBR", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 6, .crm = 2,
      .type = ARM_CP_64BIT | ARM_CP_ALIAS,
      .access = PL2_RW, .accessfn = access_el3_aa32ns,
      .fieldoffset = offsetof(CPUARMState, cp15.vttbr_el2),
      .writefn = vttbr_write },
    { .name = "VTTBR_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 1, .opc2 = 0,
      .access = PL2_RW, .writefn = vttbr_write,
      .fieldoffset = offsetof(CPUARMState, cp15.vttbr_el2) },
    { .name = "SCTLR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 0, .opc2 = 0,
      .access = PL2_RW, .raw_writefn = raw_write, .writefn = sctlr_write,
      .fieldoffset = offsetof(CPUARMState, cp15.sctlr_el[2]) },
    { .name = "TPIDR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 13, .crm = 0, .opc2 = 2,
      .access = PL2_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.tpidr_el[2]) },
    { .name = "TTBR0_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 0, .opc2 = 0,
      .access = PL2_RW, .resetvalue = 0, .writefn = vmsa_tcr_ttbr_el2_write,
      .fieldoffset = offsetof(CPUARMState, cp15.ttbr0_el[2]) },
    { .name = "HTTBR", .cp = 15, .opc1 = 4, .crm = 2,
      .access = PL2_RW, .type = ARM_CP_64BIT | ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, cp15.ttbr0_el[2]) },
    { .name = "TLBIALLNSNH",
      .cp = 15, .opc1 = 4, .crn = 8, .crm = 7, .opc2 = 4,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbiall_nsnh_write },
    { .name = "TLBIALLNSNHIS",
      .cp = 15, .opc1 = 4, .crn = 8, .crm = 3, .opc2 = 4,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbiall_nsnh_is_write },
    { .name = "TLBIALLH", .cp = 15, .opc1 = 4, .crn = 8, .crm = 7, .opc2 = 0,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbiall_hyp_write },
    { .name = "TLBIALLHIS", .cp = 15, .opc1 = 4, .crn = 8, .crm = 3, .opc2 = 0,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbiall_hyp_is_write },
    { .name = "TLBIMVAH", .cp = 15, .opc1 = 4, .crn = 8, .crm = 7, .opc2 = 1,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbimva_hyp_write },
    { .name = "TLBIMVAHIS", .cp = 15, .opc1 = 4, .crn = 8, .crm = 3, .opc2 = 1,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbimva_hyp_is_write },
    { .name = "TLBI_ALLE2", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 7, .opc2 = 0,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbi_aa64_alle2_write },
    { .name = "TLBI_VAE2", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 7, .opc2 = 1,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbi_aa64_vae2_write },
    { .name = "TLBI_VALE2", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 7, .opc2 = 5,
      .access = PL2_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae2_write },
    { .name = "TLBI_ALLE2IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 3, .opc2 = 0,
      .access = PL2_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_alle2is_write },
    { .name = "TLBI_VAE2IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 3, .opc2 = 1,
      .type = ARM_CP_NO_RAW, .access = PL2_W,
      .writefn = tlbi_aa64_vae2is_write },
    { .name = "TLBI_VALE2IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 8, .crm = 3, .opc2 = 5,
      .access = PL2_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae2is_write },
#ifndef CONFIG_USER_ONLY
    /*
     * Unlike the other EL2-related AT operations, these must
     * UNDEF from EL3 if EL2 is not implemented, which is why we
     * define them here rather than with the rest of the AT ops.
     */
    { .name = "AT_S1E2R", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 7, .crm = 8, .opc2 = 0,
      .access = PL2_W, .accessfn = at_s1e2_access,
      .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC, .writefn = ats_write64 },
    { .name = "AT_S1E2W", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 4, .crn = 7, .crm = 8, .opc2 = 1,
      .access = PL2_W, .accessfn = at_s1e2_access,
      .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC, .writefn = ats_write64 },
    /*
     * The AArch32 ATS1H* operations are CONSTRAINED UNPREDICTABLE
     * if EL2 is not implemented; we choose to UNDEF. Behaviour at EL3
     * with SCR.NS == 0 outside Monitor mode is UNPREDICTABLE; we choose
     * to behave as if SCR.NS was 1.
     */
    { .name = "ATS1HR", .cp = 15, .opc1 = 4, .crn = 7, .crm = 8, .opc2 = 0,
      .access = PL2_W,
      .writefn = ats1h_write, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC },
    { .name = "ATS1HW", .cp = 15, .opc1 = 4, .crn = 7, .crm = 8, .opc2 = 1,
      .access = PL2_W,
      .writefn = ats1h_write, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC },
    { .name = "CNTHCTL_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 1, .opc2 = 0,
      /*
       * ARMv7 requires bit 0 and 1 to reset to 1. ARMv8 defines the
       * reset values as IMPDEF. We choose to reset to 3 to comply with
       * both ARMv7 and ARMv8.
       */
      .access = PL2_RW, .resetvalue = 3,
      .fieldoffset = offsetof(CPUARMState, cp15.cnthctl_el2) },
    { .name = "CNTVOFF_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 0, .opc2 = 3,
      .access = PL2_RW, .type = ARM_CP_IO, .resetvalue = 0,
      .writefn = gt_cntvoff_write,
      .fieldoffset = offsetof(CPUARMState, cp15.cntvoff_el2) },
    { .name = "CNTVOFF", .cp = 15, .opc1 = 4, .crm = 14,
      .access = PL2_RW, .type = ARM_CP_64BIT | ARM_CP_ALIAS | ARM_CP_IO,
      .writefn = gt_cntvoff_write,
      .fieldoffset = offsetof(CPUARMState, cp15.cntvoff_el2) },
    { .name = "CNTHP_CVAL_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 2, .opc2 = 2,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_HYP].cval),
      .type = ARM_CP_IO, .access = PL2_RW,
      .writefn = gt_hyp_cval_write, .raw_writefn = raw_write },
    { .name = "CNTHP_CVAL", .cp = 15, .opc1 = 6, .crm = 14,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_HYP].cval),
      .access = PL2_RW, .type = ARM_CP_64BIT | ARM_CP_IO,
      .writefn = gt_hyp_cval_write, .raw_writefn = raw_write },
    { .name = "CNTHP_TVAL_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 2, .opc2 = 0,
      .type = ARM_CP_NO_RAW | ARM_CP_IO, .access = PL2_RW,
      .resetfn = gt_hyp_timer_reset,
      .readfn = gt_hyp_tval_read, .writefn = gt_hyp_tval_write },
    { .name = "CNTHP_CTL_EL2", .state = ARM_CP_STATE_BOTH,
      .type = ARM_CP_IO,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 2, .opc2 = 1,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_HYP].ctl),
      .resetvalue = 0,
      .writefn = gt_hyp_ctl_write, .raw_writefn = raw_write },
#endif
    /* The only field of MDCR_EL2 that has a defined architectural reset value
     * is MDCR_EL2.HPMN which should reset to the value of PMCR_EL0.N.
     */
    { .name = "MDCR_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 1,
      .access = PL2_RW, .resetvalue = PMCR_NUM_COUNTERS,
      .fieldoffset = offsetof(CPUARMState, cp15.mdcr_el2), },
    { .name = "HPFAR", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 4, .crn = 6, .crm = 0, .opc2 = 4,
      .access = PL2_RW, .accessfn = access_el3_aa32ns,
      .fieldoffset = offsetof(CPUARMState, cp15.hpfar_el2) },
    { .name = "HPFAR_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 6, .crm = 0, .opc2 = 4,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.hpfar_el2) },
    { .name = "HSTR_EL2", .state = ARM_CP_STATE_BOTH,
      .cp = 15, .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 3,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.hstr_el2) },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo el2_v8_cp_reginfo[] = {
    { .name = "HCR2", .state = ARM_CP_STATE_AA32,
      .type = ARM_CP_ALIAS | ARM_CP_IO,
      .cp = 15, .opc1 = 4, .crn = 1, .crm = 1, .opc2 = 4,
      .access = PL2_RW,
      .fieldoffset = offsetofhigh32(CPUARMState, cp15.hcr_el2),
      .writefn = hcr_writehigh },
    REGINFO_SENTINEL
};

static CPAccessResult sel2_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                  bool isread)
{
    if (arm_current_el(env) == 3 || arm_is_secure_below_el3(env)) {
        return CP_ACCESS_OK;
    }
    return CP_ACCESS_TRAP_UNCATEGORIZED;
}

static const ARMCPRegInfo el2_sec_cp_reginfo[] = {
    { .name = "VSTTBR_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 6, .opc2 = 0,
      .access = PL2_RW, .accessfn = sel2_access,
      .fieldoffset = offsetof(CPUARMState, cp15.vsttbr_el2) },
    { .name = "VSTCR_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 6, .opc2 = 2,
      .access = PL2_RW, .accessfn = sel2_access,
      .fieldoffset = offsetof(CPUARMState, cp15.vstcr_el2) },
    REGINFO_SENTINEL
};

static CPAccessResult nsacr_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                   bool isread)
{
    /*
     * The NSACR is RW at EL3, and RO for NS EL1 and NS EL2.
     * At Secure EL1 it traps to EL3 or EL2.
     */
    if (arm_current_el(env) == 3) {
        return CP_ACCESS_OK;
    }
    if (arm_is_secure_below_el3(env)) {
        if (env->cp15.scr_el3 & SCR_EEL2) {
            return CP_ACCESS_TRAP_EL2;
        }
        return CP_ACCESS_TRAP_EL3;
    }
    /* Accesses from EL1 NS and EL2 NS are UNDEF for write but allow reads. */
    if (isread) {
        return CP_ACCESS_OK;
    }
    return CP_ACCESS_TRAP_UNCATEGORIZED;
}

static const ARMCPRegInfo el3_cp_reginfo[] = {
    { .name = "SCR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 1, .crm = 1, .opc2 = 0,
      .access = PL3_RW, .fieldoffset = offsetof(CPUARMState, cp15.scr_el3),
      .resetfn = scr_reset, .writefn = scr_write },
    { .name = "SCR",  .type = ARM_CP_ALIAS | ARM_CP_NEWEL,
      .cp = 15, .opc1 = 0, .crn = 1, .crm = 1, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_trap_aa32s_el1,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.scr_el3),
      .writefn = scr_write },
    { .name = "SDER32_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 1, .crm = 1, .opc2 = 1,
      .access = PL3_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.sder) },
    { .name = "SDER",
      .cp = 15, .opc1 = 0, .crn = 1, .crm = 1, .opc2 = 1,
      .access = PL3_RW, .resetvalue = 0,
      .fieldoffset = offsetoflow32(CPUARMState, cp15.sder) },
    { .name = "MVBAR", .cp = 15, .opc1 = 0, .crn = 12, .crm = 0, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_trap_aa32s_el1,
      .writefn = vbar_write, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.mvbar) },
    { .name = "TTBR0_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 2, .crm = 0, .opc2 = 0,
      .access = PL3_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.ttbr0_el[3]) },
    { .name = "TCR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 2, .crm = 0, .opc2 = 2,
      .access = PL3_RW,
      /*
       * no .writefn needed as this can't cause an ASID change;
       * we must provide a .raw_writefn and .resetfn because we handle
       * reset and migration for the AArch32 TTBCR(S), which might be
       * using mask and base_mask.
       */
      .resetfn = vmsa_ttbcr_reset, .raw_writefn = vmsa_ttbcr_raw_write,
      .fieldoffset = offsetof(CPUARMState, cp15.tcr_el[3]) },
    { .name = "ELR_EL3", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 6, .crn = 4, .crm = 0, .opc2 = 1,
      .access = PL3_RW,
      .fieldoffset = offsetof(CPUARMState, elr_el[3]) },
    { .name = "ESR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 5, .crm = 2, .opc2 = 0,
      .access = PL3_RW, .fieldoffset = offsetof(CPUARMState, cp15.esr_el[3]) },
    { .name = "FAR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 6, .crm = 0, .opc2 = 0,
      .access = PL3_RW, .fieldoffset = offsetof(CPUARMState, cp15.far_el[3]) },
    { .name = "SPSR_EL3", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_ALIAS,
      .opc0 = 3, .opc1 = 6, .crn = 4, .crm = 0, .opc2 = 0,
      .access = PL3_RW,
      .fieldoffset = offsetof(CPUARMState, banked_spsr[BANK_MON]) },
    { .name = "VBAR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 12, .crm = 0, .opc2 = 0,
      .access = PL3_RW, .writefn = vbar_write,
      .fieldoffset = offsetof(CPUARMState, cp15.vbar_el[3]),
      .resetvalue = 0 },
    { .name = "CPTR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 1, .crm = 1, .opc2 = 2,
      .access = PL3_RW, .accessfn = cptr_access, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.cptr_el[3]) },
    { .name = "TPIDR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 13, .crm = 0, .opc2 = 2,
      .access = PL3_RW, .resetvalue = 0,
      .fieldoffset = offsetof(CPUARMState, cp15.tpidr_el[3]) },
    { .name = "AMAIR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 10, .crm = 3, .opc2 = 0,
      .access = PL3_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "AFSR0_EL3", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 6, .crn = 5, .crm = 1, .opc2 = 0,
      .access = PL3_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "AFSR1_EL3", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 6, .crn = 5, .crm = 1, .opc2 = 1,
      .access = PL3_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    { .name = "TLBI_ALLE3IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 6, .crn = 8, .crm = 3, .opc2 = 0,
      .access = PL3_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_alle3is_write },
    { .name = "TLBI_VAE3IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 6, .crn = 8, .crm = 3, .opc2 = 1,
      .access = PL3_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae3is_write },
    { .name = "TLBI_VALE3IS", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 6, .crn = 8, .crm = 3, .opc2 = 5,
      .access = PL3_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae3is_write },
    { .name = "TLBI_ALLE3", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 6, .crn = 8, .crm = 7, .opc2 = 0,
      .access = PL3_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_alle3_write },
    { .name = "TLBI_VAE3", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 6, .crn = 8, .crm = 7, .opc2 = 1,
      .access = PL3_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae3_write },
    { .name = "TLBI_VALE3", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 6, .crn = 8, .crm = 7, .opc2 = 5,
      .access = PL3_W, .type = ARM_CP_NO_RAW,
      .writefn = tlbi_aa64_vae3_write },
    REGINFO_SENTINEL
};

#ifndef CONFIG_USER_ONLY
/* Test if system register redirection is to occur in the current state.  */
static bool redirect_for_e2h(CPUARMState *env)
{
    return arm_current_el(env) == 2 && (arm_hcr_el2_eff(env) & HCR_E2H);
}

static uint64_t el2_e2h_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    CPReadFn *readfn;

    if (redirect_for_e2h(env)) {
        /* Switch to the saved EL2 version of the register.  */
        ri = ri->opaque;
        readfn = ri->readfn;
    } else {
        readfn = ri->orig_readfn;
    }
    if (readfn == NULL) {
        readfn = raw_read;
    }
    return readfn(env, ri);
}

static void el2_e2h_write(CPUARMState *env, const ARMCPRegInfo *ri,
                          uint64_t value)
{
    CPWriteFn *writefn;

    if (redirect_for_e2h(env)) {
        /* Switch to the saved EL2 version of the register.  */
        ri = ri->opaque;
        writefn = ri->writefn;
    } else {
        writefn = ri->orig_writefn;
    }
    if (writefn == NULL) {
        writefn = raw_write;
    }
    writefn(env, ri, value);
}

static void define_arm_vh_e2h_redirects_aliases(ARMCPU *cpu)
{
    struct E2HAlias {
        uint32_t src_key, dst_key, new_key;
        const char *src_name, *dst_name, *new_name;
        bool (*feature)(const ARMISARegisters *id);
    };

#define K(op0, op1, crn, crm, op2) \
    ENCODE_AA64_CP_REG(CP_REG_ARM64_SYSREG_CP, crn, crm, op0, op1, op2)

    static const struct E2HAlias aliases[] = {
        { K(3, 0,  1, 0, 0), K(3, 4,  1, 0, 0), K(3, 5, 1, 0, 0),
          "SCTLR", "SCTLR_EL2", "SCTLR_EL12" },
        { K(3, 0,  1, 0, 2), K(3, 4,  1, 1, 2), K(3, 5, 1, 0, 2),
          "CPACR", "CPTR_EL2", "CPACR_EL12" },
        { K(3, 0,  2, 0, 0), K(3, 4,  2, 0, 0), K(3, 5, 2, 0, 0),
          "TTBR0_EL1", "TTBR0_EL2", "TTBR0_EL12" },
        { K(3, 0,  2, 0, 1), K(3, 4,  2, 0, 1), K(3, 5, 2, 0, 1),
          "TTBR1_EL1", "TTBR1_EL2", "TTBR1_EL12" },
        { K(3, 0,  2, 0, 2), K(3, 4,  2, 0, 2), K(3, 5, 2, 0, 2),
          "TCR_EL1", "TCR_EL2", "TCR_EL12" },
        { K(3, 0,  4, 0, 0), K(3, 4,  4, 0, 0), K(3, 5, 4, 0, 0),
          "SPSR_EL1", "SPSR_EL2", "SPSR_EL12" },
        { K(3, 0,  4, 0, 1), K(3, 4,  4, 0, 1), K(3, 5, 4, 0, 1),
          "ELR_EL1", "ELR_EL2", "ELR_EL12" },
        { K(3, 0,  5, 1, 0), K(3, 4,  5, 1, 0), K(3, 5, 5, 1, 0),
          "AFSR0_EL1", "AFSR0_EL2", "AFSR0_EL12" },
        { K(3, 0,  5, 1, 1), K(3, 4,  5, 1, 1), K(3, 5, 5, 1, 1),
          "AFSR1_EL1", "AFSR1_EL2", "AFSR1_EL12" },
        { K(3, 0,  5, 2, 0), K(3, 4,  5, 2, 0), K(3, 5, 5, 2, 0),
          "ESR_EL1", "ESR_EL2", "ESR_EL12" },
        { K(3, 0,  6, 0, 0), K(3, 4,  6, 0, 0), K(3, 5, 6, 0, 0),
          "FAR_EL1", "FAR_EL2", "FAR_EL12" },
        { K(3, 0, 10, 2, 0), K(3, 4, 10, 2, 0), K(3, 5, 10, 2, 0),
          "MAIR_EL1", "MAIR_EL2", "MAIR_EL12" },
        { K(3, 0, 10, 3, 0), K(3, 4, 10, 3, 0), K(3, 5, 10, 3, 0),
          "AMAIR0", "AMAIR_EL2", "AMAIR_EL12" },
        { K(3, 0, 12, 0, 0), K(3, 4, 12, 0, 0), K(3, 5, 12, 0, 0),
          "VBAR", "VBAR_EL2", "VBAR_EL12" },
        { K(3, 0, 13, 0, 1), K(3, 4, 13, 0, 1), K(3, 5, 13, 0, 1),
          "CONTEXTIDR_EL1", "CONTEXTIDR_EL2", "CONTEXTIDR_EL12" },
        { K(3, 0, 14, 1, 0), K(3, 4, 14, 1, 0), K(3, 5, 14, 1, 0),
          "CNTKCTL", "CNTHCTL_EL2", "CNTKCTL_EL12" },

        /*
         * Note that redirection of ZCR is mentioned in the description
         * of ZCR_EL2, and aliasing in the description of ZCR_EL1, but
         * not in the summary table.
         */
        { K(3, 0,  1, 2, 0), K(3, 4,  1, 2, 0), K(3, 5, 1, 2, 0),
          "ZCR_EL1", "ZCR_EL2", "ZCR_EL12", isar_feature_aa64_sve },

        { K(3, 0,  5, 6, 0), K(3, 4,  5, 6, 0), K(3, 5, 5, 6, 0),
          "TFSR_EL1", "TFSR_EL2", "TFSR_EL12", isar_feature_aa64_mte },

        /* TODO: ARMv8.2-SPE -- PMSCR_EL2 */
        /* TODO: ARMv8.4-Trace -- TRFCR_EL2 */
    };
#undef K

    size_t i;

    for (i = 0; i < ARRAY_SIZE(aliases); i++) {
        const struct E2HAlias *a = &aliases[i];
        ARMCPRegInfo *src_reg, *dst_reg;

        if (a->feature && !a->feature(&cpu->isar)) {
            continue;
        }

        src_reg = g_hash_table_lookup(cpu->cp_regs, &a->src_key);
        dst_reg = g_hash_table_lookup(cpu->cp_regs, &a->dst_key);
        g_assert(src_reg != NULL);
        g_assert(dst_reg != NULL);

        /* Cross-compare names to detect typos in the keys.  */
        g_assert(strcmp(src_reg->name, a->src_name) == 0);
        g_assert(strcmp(dst_reg->name, a->dst_name) == 0);

        /* None of the core system registers use opaque; we will.  */
        g_assert(src_reg->opaque == NULL);

        /* Create alias before redirection so we dup the right data. */
        if (a->new_key) {
            ARMCPRegInfo *new_reg = g_memdup(src_reg, sizeof(ARMCPRegInfo));
            uint32_t *new_key = g_memdup(&a->new_key, sizeof(uint32_t));
            bool ok;

            new_reg->name = a->new_name;
            new_reg->type |= ARM_CP_ALIAS;
            /* Remove PL1/PL0 access, leaving PL2/PL3 R/W in place.  */
            new_reg->access &= PL2_RW | PL3_RW;

            ok = g_hash_table_insert(cpu->cp_regs, new_key, new_reg);
            g_assert(ok);
        }

        src_reg->opaque = dst_reg;
        src_reg->orig_readfn = src_reg->readfn ?: raw_read;
        src_reg->orig_writefn = src_reg->writefn ?: raw_write;
        if (!src_reg->raw_readfn) {
            src_reg->raw_readfn = raw_read;
        }
        if (!src_reg->raw_writefn) {
            src_reg->raw_writefn = raw_write;
        }
        src_reg->readfn = el2_e2h_read;
        src_reg->writefn = el2_e2h_write;
    }
}
#endif

static CPAccessResult ctr_el0_access(CPUARMState *env, const ARMCPRegInfo *ri,
                                     bool isread)
{
    int cur_el = arm_current_el(env);

    if (cur_el < 2) {
        uint64_t hcr = arm_hcr_el2_eff(env);

        if (cur_el == 0) {
            if ((hcr & (HCR_E2H | HCR_TGE)) == (HCR_E2H | HCR_TGE)) {
                if (!(env->cp15.sctlr_el[2] & SCTLR_UCT)) {
                    return CP_ACCESS_TRAP_EL2;
                }
            } else {
                if (!(env->cp15.sctlr_el[1] & SCTLR_UCT)) {
                    return CP_ACCESS_TRAP;
                }
                if (hcr & HCR_TID2) {
                    return CP_ACCESS_TRAP_EL2;
                }
            }
        } else if (hcr & HCR_TID2) {
            return CP_ACCESS_TRAP_EL2;
        }
    }

    if (arm_current_el(env) < 2 && arm_hcr_el2_eff(env) & HCR_TID2) {
        return CP_ACCESS_TRAP_EL2;
    }

    return CP_ACCESS_OK;
}

static void oslar_write(CPUARMState *env, const ARMCPRegInfo *ri,
                        uint64_t value)
{
    /*
     * Writes to OSLAR_EL1 may update the OS lock status, which can be
     * read via a bit in OSLSR_EL1.
     */
    int oslock;

    if (ri->state == ARM_CP_STATE_AA32) {
        oslock = (value == 0xC5ACCE55);
    } else {
        oslock = value & 1;
    }

    env->cp15.oslsr_el1 = deposit32(env->cp15.oslsr_el1, 1, 1, oslock);
}

static const ARMCPRegInfo debug_cp_reginfo[] = {
    /*
     * DBGDRAR, DBGDSAR: always RAZ since we don't implement memory mapped
     * debug components. The AArch64 version of DBGDRAR is named MDRAR_EL1;
     * unlike DBGDRAR it is never accessible from EL0.
     * DBGDSAR is deprecated and must RAZ from v8 anyway, so it has no AArch64
     * accessor.
     */
    { .name = "DBGDRAR", .cp = 14, .crn = 1, .crm = 0, .opc1 = 0, .opc2 = 0,
      .access = PL0_R, .accessfn = access_tdra,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "MDRAR_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 2, .opc1 = 0, .crn = 1, .crm = 0, .opc2 = 0,
      .access = PL1_R, .accessfn = access_tdra,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "DBGDSAR", .cp = 14, .crn = 2, .crm = 0, .opc1 = 0, .opc2 = 0,
      .access = PL0_R, .accessfn = access_tdra,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    /* Monitor debug system control register; the 32-bit alias is DBGDSCRext. */
    { .name = "MDSCR_EL1", .state = ARM_CP_STATE_BOTH,
      .cp = 14, .opc0 = 2, .opc1 = 0, .crn = 0, .crm = 2, .opc2 = 2,
      .access = PL1_RW, .accessfn = access_tda,
      .fieldoffset = offsetof(CPUARMState, cp15.mdscr_el1),
      .resetvalue = 0 },
    /*
     * MDCCSR_EL0, aka DBGDSCRint. This is a read-only mirror of MDSCR_EL1.
     * We don't implement the configurable EL0 access.
     */
    { .name = "MDCCSR_EL0", .state = ARM_CP_STATE_BOTH,
      .cp = 14, .opc0 = 2, .opc1 = 0, .crn = 0, .crm = 1, .opc2 = 0,
      .type = ARM_CP_ALIAS,
      .access = PL1_R, .accessfn = access_tda,
      .fieldoffset = offsetof(CPUARMState, cp15.mdscr_el1), },
    { .name = "OSLAR_EL1", .state = ARM_CP_STATE_BOTH,
      .cp = 14, .opc0 = 2, .opc1 = 0, .crn = 1, .crm = 0, .opc2 = 4,
      .access = PL1_W, .type = ARM_CP_NO_RAW,
      .accessfn = access_tdosa,
      .writefn = oslar_write },
    { .name = "OSLSR_EL1", .state = ARM_CP_STATE_BOTH,
      .cp = 14, .opc0 = 2, .opc1 = 0, .crn = 1, .crm = 1, .opc2 = 4,
      .access = PL1_R, .resetvalue = 10,
      .accessfn = access_tdosa,
      .fieldoffset = offsetof(CPUARMState, cp15.oslsr_el1) },
    /* Dummy OSDLR_EL1: 32-bit Linux will read this */
    { .name = "OSDLR_EL1", .state = ARM_CP_STATE_BOTH,
      .cp = 14, .opc0 = 2, .opc1 = 0, .crn = 1, .crm = 3, .opc2 = 4,
      .access = PL1_RW, .accessfn = access_tdosa,
      .type = ARM_CP_NOP },
    /*
     * Dummy DBGVCR: Linux wants to clear this on startup, but we don't
     * implement vector catch debug events yet.
     */
    { .name = "DBGVCR",
      .cp = 14, .opc1 = 0, .crn = 0, .crm = 7, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tda,
      .type = ARM_CP_NOP },
    /*
     * Dummy DBGVCR32_EL2 (which is only for a 64-bit hypervisor
     * to save and restore a 32-bit guest's DBGVCR)
     */
    { .name = "DBGVCR32_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 2, .opc1 = 4, .crn = 0, .crm = 7, .opc2 = 0,
      .access = PL2_RW, .accessfn = access_tda,
      .type = ARM_CP_NOP },
    /*
     * Dummy MDCCINT_EL1, since we don't implement the Debug Communications
     * Channel but Linux may try to access this register. The 32-bit
     * alias is DBGDCCINT.
     */
    { .name = "MDCCINT_EL1", .state = ARM_CP_STATE_BOTH,
      .cp = 14, .opc0 = 2, .opc1 = 0, .crn = 0, .crm = 2, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_tda,
      .type = ARM_CP_NOP },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo debug_lpae_cp_reginfo[] = {
    /* 64 bit access versions of the (dummy) debug registers */
    { .name = "DBGDRAR", .cp = 14, .crm = 1, .opc1 = 0,
      .access = PL0_R, .type = ARM_CP_CONST|ARM_CP_64BIT, .resetvalue = 0 },
    { .name = "DBGDSAR", .cp = 14, .crm = 2, .opc1 = 0,
      .access = PL0_R, .type = ARM_CP_CONST|ARM_CP_64BIT, .resetvalue = 0 },
    REGINFO_SENTINEL
};

#ifdef TARGET_AARCH64

static void zcr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                      uint64_t value)
{
    int cur_el = arm_current_el(env);
    int old_len = sve_zcr_len_for_el(env, cur_el);
    int new_len;

    /* Bits other than [3:0] are RAZ/WI.  */
    QEMU_BUILD_BUG_ON(ARM_MAX_VQ > 16);
    raw_write(env, ri, value & 0xf);

    /*
     * Because we arrived here, we know both FP and SVE are enabled;
     * otherwise we would have trapped access to the ZCR_ELn register.
     */
    new_len = sve_zcr_len_for_el(env, cur_el);
    if (new_len < old_len) {
        aarch64_sve_narrow_vq(env, new_len + 1);
    }
}

static const ARMCPRegInfo zcr_el1_reginfo = {
    .name = "ZCR_EL1", .state = ARM_CP_STATE_AA64,
    .opc0 = 3, .opc1 = 0, .crn = 1, .crm = 2, .opc2 = 0,
    .access = PL1_RW, .type = ARM_CP_SVE,
    .fieldoffset = offsetof(CPUARMState, vfp.zcr_el[1]),
    .writefn = zcr_write, .raw_writefn = raw_write
};

static const ARMCPRegInfo zcr_el2_reginfo = {
    .name = "ZCR_EL2", .state = ARM_CP_STATE_AA64,
    .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 2, .opc2 = 0,
    .access = PL2_RW, .type = ARM_CP_SVE,
    .fieldoffset = offsetof(CPUARMState, vfp.zcr_el[2]),
    .writefn = zcr_write, .raw_writefn = raw_write
};

static const ARMCPRegInfo zcr_no_el2_reginfo = {
    .name = "ZCR_EL2", .state = ARM_CP_STATE_AA64,
    .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 2, .opc2 = 0,
    .access = PL2_RW, .type = ARM_CP_SVE,
    .readfn = arm_cp_read_zero, .writefn = arm_cp_write_ignore
};

static const ARMCPRegInfo zcr_el3_reginfo = {
    .name = "ZCR_EL3", .state = ARM_CP_STATE_AA64,
    .opc0 = 3, .opc1 = 6, .crn = 1, .crm = 2, .opc2 = 0,
    .access = PL3_RW, .type = ARM_CP_SVE,
    .fieldoffset = offsetof(CPUARMState, vfp.zcr_el[3]),
    .writefn = zcr_write, .raw_writefn = raw_write
};

#endif /* TARGET_AARCH64 */

static void dbgwvr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    int i = ri->crm;

    /*
     * Bits [63:49] are hardwired to the value of bit [48]; that is, the
     * register reads and behaves as if values written are sign extended.
     * Bits [1:0] are RES0.
     */
    value = sextract64(value, 0, 49) & ~3ULL;

    raw_write(env, ri, value);
    hw_watchpoint_update(cpu, i);
}

static void dbgwcr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    int i = ri->crm;

    raw_write(env, ri, value);
    hw_watchpoint_update(cpu, i);
}

static void dbgbvr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    int i = ri->crm;

    raw_write(env, ri, value);
    hw_breakpoint_update(cpu, i);
}

static void dbgbcr_write(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    int i = ri->crm;

    /*
     * BAS[3] is a read-only copy of BAS[2], and BAS[1] a read-only
     * copy of BAS[0].
     */
    value = deposit64(value, 6, 1, extract64(value, 5, 1));
    value = deposit64(value, 8, 1, extract64(value, 7, 1));

    raw_write(env, ri, value);
    hw_breakpoint_update(cpu, i);
}

static void define_debug_regs(ARMCPU *cpu)
{
    /*
     * Define v7 and v8 architectural debug registers.
     * These are just dummy implementations for now.
     */
    int i;
    int wrps, brps, ctx_cmps;

    /*
     * The Arm ARM says DBGDIDR is optional and deprecated if EL1 cannot
     * use AArch32.  Given that bit 15 is RES1, if the value is 0 then
     * the register must not exist for this cpu.
     */
    if (cpu->isar.dbgdidr != 0) {
        ARMCPRegInfo dbgdidr = {
            .name = "DBGDIDR", .cp = 14, .crn = 0, .crm = 0,
            .opc1 = 0, .opc2 = 0,
            .access = PL0_R, .accessfn = access_tda,
            .type = ARM_CP_CONST, .resetvalue = cpu->isar.dbgdidr,
        };
        define_one_arm_cp_reg(cpu, &dbgdidr);
    }

    /* Note that all these register fields hold "number of Xs minus 1". */
    brps = arm_num_brps(cpu);
    wrps = arm_num_wrps(cpu);
    ctx_cmps = arm_num_ctx_cmps(cpu);

    assert(ctx_cmps <= brps);

    define_arm_cp_regs(cpu, debug_cp_reginfo);

    if (arm_feature(&cpu->env, ARM_FEATURE_LPAE)) {
        define_arm_cp_regs(cpu, debug_lpae_cp_reginfo);
    }

    for (i = 0; i < brps; i++) {
        ARMCPRegInfo dbgregs[] = {
            { .name = "DBGBVR", .state = ARM_CP_STATE_BOTH,
              .cp = 14, .opc0 = 2, .opc1 = 0, .crn = 0, .crm = i, .opc2 = 4,
              .access = PL1_RW, .accessfn = access_tda,
              .fieldoffset = offsetof(CPUARMState, cp15.dbgbvr[i]),
              .writefn = dbgbvr_write, .raw_writefn = raw_write
            },
            { .name = "DBGBCR", .state = ARM_CP_STATE_BOTH,
              .cp = 14, .opc0 = 2, .opc1 = 0, .crn = 0, .crm = i, .opc2 = 5,
              .access = PL1_RW, .accessfn = access_tda,
              .fieldoffset = offsetof(CPUARMState, cp15.dbgbcr[i]),
              .writefn = dbgbcr_write, .raw_writefn = raw_write
            },
            REGINFO_SENTINEL
        };
        define_arm_cp_regs(cpu, dbgregs);
    }

    for (i = 0; i < wrps; i++) {
        ARMCPRegInfo dbgregs[] = {
            { .name = "DBGWVR", .state = ARM_CP_STATE_BOTH,
              .cp = 14, .opc0 = 2, .opc1 = 0, .crn = 0, .crm = i, .opc2 = 6,
              .access = PL1_RW, .accessfn = access_tda,
              .fieldoffset = offsetof(CPUARMState, cp15.dbgwvr[i]),
              .writefn = dbgwvr_write, .raw_writefn = raw_write
            },
            { .name = "DBGWCR", .state = ARM_CP_STATE_BOTH,
              .cp = 14, .opc0 = 2, .opc1 = 0, .crn = 0, .crm = i, .opc2 = 7,
              .access = PL1_RW, .accessfn = access_tda,
              .fieldoffset = offsetof(CPUARMState, cp15.dbgwcr[i]),
              .writefn = dbgwcr_write, .raw_writefn = raw_write
            },
            REGINFO_SENTINEL
        };
        define_arm_cp_regs(cpu, dbgregs);
    }
}

static void define_pmu_regs(ARMCPU *cpu)
{
    /*
     * v7 performance monitor control register: same implementor
     * field as main ID register, and we implement four counters in
     * addition to the cycle count register.
     */
    unsigned int i, pmcrn = PMCR_NUM_COUNTERS;
    ARMCPRegInfo pmcr = {
        .name = "PMCR", .cp = 15, .crn = 9, .crm = 12, .opc1 = 0, .opc2 = 0,
        .access = PL0_RW,
        .type = ARM_CP_IO | ARM_CP_ALIAS,
        .fieldoffset = offsetoflow32(CPUARMState, cp15.c9_pmcr),
        .accessfn = pmreg_access, .writefn = pmcr_write,
        .raw_writefn = raw_write,
    };
    ARMCPRegInfo pmcr64 = {
        .name = "PMCR_EL0", .state = ARM_CP_STATE_AA64,
        .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 12, .opc2 = 0,
        .access = PL0_RW, .accessfn = pmreg_access,
        .type = ARM_CP_IO,
        .fieldoffset = offsetof(CPUARMState, cp15.c9_pmcr),
        .resetvalue = (cpu->midr & 0xff000000) | (pmcrn << PMCRN_SHIFT) |
                      PMCRLC,
        .writefn = pmcr_write, .raw_writefn = raw_write,
    };
    define_one_arm_cp_reg(cpu, &pmcr);
    define_one_arm_cp_reg(cpu, &pmcr64);
    for (i = 0; i < pmcrn; i++) {
        char *pmevcntr_name = g_strdup_printf("PMEVCNTR%d", i);
        char *pmevcntr_el0_name = g_strdup_printf("PMEVCNTR%d_EL0", i);
        char *pmevtyper_name = g_strdup_printf("PMEVTYPER%d", i);
        char *pmevtyper_el0_name = g_strdup_printf("PMEVTYPER%d_EL0", i);
        ARMCPRegInfo pmev_regs[] = {
            { .name = pmevcntr_name, .cp = 15, .crn = 14,
              .crm = 8 | (3 & (i >> 3)), .opc1 = 0, .opc2 = i & 7,
              .access = PL0_RW, .type = ARM_CP_IO | ARM_CP_ALIAS,
              .readfn = pmevcntr_readfn, .writefn = pmevcntr_writefn,
              .accessfn = pmreg_access },
            { .name = pmevcntr_el0_name, .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 8 | (3 & (i >> 3)),
              .opc2 = i & 7, .access = PL0_RW, .accessfn = pmreg_access,
              .type = ARM_CP_IO,
              .readfn = pmevcntr_readfn, .writefn = pmevcntr_writefn,
              .raw_readfn = pmevcntr_rawread,
              .raw_writefn = pmevcntr_rawwrite },
            { .name = pmevtyper_name, .cp = 15, .crn = 14,
              .crm = 12 | (3 & (i >> 3)), .opc1 = 0, .opc2 = i & 7,
              .access = PL0_RW, .type = ARM_CP_IO | ARM_CP_ALIAS,
              .readfn = pmevtyper_readfn, .writefn = pmevtyper_writefn,
              .accessfn = pmreg_access },
            { .name = pmevtyper_el0_name, .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 12 | (3 & (i >> 3)),
              .opc2 = i & 7, .access = PL0_RW, .accessfn = pmreg_access,
              .type = ARM_CP_IO,
              .readfn = pmevtyper_readfn, .writefn = pmevtyper_writefn,
              .raw_writefn = pmevtyper_rawwrite },
            REGINFO_SENTINEL
        };
        define_arm_cp_regs(cpu, pmev_regs);
        g_free(pmevcntr_name);
        g_free(pmevcntr_el0_name);
        g_free(pmevtyper_name);
        g_free(pmevtyper_el0_name);
    }
    if (cpu_isar_feature(aa32_pmu_8_1, cpu)) {
        ARMCPRegInfo v81_pmu_regs[] = {
            { .name = "PMCEID2", .state = ARM_CP_STATE_AA32,
              .cp = 15, .opc1 = 0, .crn = 9, .crm = 14, .opc2 = 4,
              .access = PL0_R, .accessfn = pmreg_access, .type = ARM_CP_CONST,
              .resetvalue = extract64(cpu->pmceid0, 32, 32) },
            { .name = "PMCEID3", .state = ARM_CP_STATE_AA32,
              .cp = 15, .opc1 = 0, .crn = 9, .crm = 14, .opc2 = 5,
              .access = PL0_R, .accessfn = pmreg_access, .type = ARM_CP_CONST,
              .resetvalue = extract64(cpu->pmceid1, 32, 32) },
            REGINFO_SENTINEL
        };
        define_arm_cp_regs(cpu, v81_pmu_regs);
    }
    if (cpu_isar_feature(any_pmu_8_4, cpu)) {
        static const ARMCPRegInfo v84_pmmir = {
            .name = "PMMIR_EL1", .state = ARM_CP_STATE_BOTH,
            .opc0 = 3, .opc1 = 0, .crn = 9, .crm = 14, .opc2 = 6,
            .access = PL1_R, .accessfn = pmreg_access, .type = ARM_CP_CONST,
            .resetvalue = 0
        };
        define_one_arm_cp_reg(cpu, &v84_pmmir);
    }
}

/*
 * We don't know until after realize whether there's a GICv3
 * attached, and that is what registers the gicv3 sysregs.
 * So we have to fill in the GIC fields in ID_PFR/ID_PFR1_EL1/ID_AA64PFR0_EL1
 * at runtime.
 */
static uint64_t id_pfr1_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    ARMCPU *cpu = env_archcpu(env);
    uint64_t pfr1 = cpu->isar.id_pfr1;

    if (env->gicv3state) {
        pfr1 |= 1 << 28;
    }
    return pfr1;
}

#ifndef CONFIG_USER_ONLY
static uint64_t id_aa64pfr0_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    ARMCPU *cpu = env_archcpu(env);
    uint64_t pfr0 = cpu->isar.id_aa64pfr0;

    if (env->gicv3state) {
        pfr0 |= 1 << 24;
    }
    return pfr0;
}
#endif

/*
 * Shared logic between LORID and the rest of the LOR* registers.
 * Secure state exclusion has already been dealt with.
 */
static CPAccessResult access_lor_ns(CPUARMState *env,
                                    const ARMCPRegInfo *ri, bool isread)
{
    int el = arm_current_el(env);

    if (el < 2 && (arm_hcr_el2_eff(env) & HCR_TLOR)) {
        return CP_ACCESS_TRAP_EL2;
    }
    if (el < 3 && (env->cp15.scr_el3 & SCR_TLOR)) {
        return CP_ACCESS_TRAP_EL3;
    }
    return CP_ACCESS_OK;
}

static CPAccessResult access_lor_other(CPUARMState *env,
                                       const ARMCPRegInfo *ri, bool isread)
{
    if (arm_is_secure_below_el3(env)) {
        /* Access denied in secure mode.  */
        return CP_ACCESS_TRAP;
    }
    return access_lor_ns(env, ri, isread);
}

/*
 * A trivial implementation of ARMv8.1-LOR leaves all of these
 * registers fixed at 0, which indicates that there are zero
 * supported Limited Ordering regions.
 */
static const ARMCPRegInfo lor_reginfo[] = {
    { .name = "LORSA_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 10, .crm = 4, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_lor_other,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "LOREA_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 10, .crm = 4, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_lor_other,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "LORN_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 10, .crm = 4, .opc2 = 2,
      .access = PL1_RW, .accessfn = access_lor_other,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "LORC_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 10, .crm = 4, .opc2 = 3,
      .access = PL1_RW, .accessfn = access_lor_other,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "LORID_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 10, .crm = 4, .opc2 = 7,
      .access = PL1_R, .accessfn = access_lor_ns,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    REGINFO_SENTINEL
};

#ifdef TARGET_AARCH64
static CPAccessResult access_pauth(CPUARMState *env, const ARMCPRegInfo *ri,
                                   bool isread)
{
    int el = arm_current_el(env);

    if (el < 2 &&
        arm_feature(env, ARM_FEATURE_EL2) &&
        !(arm_hcr_el2_eff(env) & HCR_APK)) {
        return CP_ACCESS_TRAP_EL2;
    }
    if (el < 3 &&
        arm_feature(env, ARM_FEATURE_EL3) &&
        !(env->cp15.scr_el3 & SCR_APK)) {
        return CP_ACCESS_TRAP_EL3;
    }
    return CP_ACCESS_OK;
}

static const ARMCPRegInfo pauth_reginfo[] = {
    { .name = "APDAKEYLO_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 2, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_pauth,
      .fieldoffset = offsetof(CPUARMState, keys.apda.lo) },
    { .name = "APDAKEYHI_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 2, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_pauth,
      .fieldoffset = offsetof(CPUARMState, keys.apda.hi) },
    { .name = "APDBKEYLO_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 2, .opc2 = 2,
      .access = PL1_RW, .accessfn = access_pauth,
      .fieldoffset = offsetof(CPUARMState, keys.apdb.lo) },
    { .name = "APDBKEYHI_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 2, .opc2 = 3,
      .access = PL1_RW, .accessfn = access_pauth,
      .fieldoffset = offsetof(CPUARMState, keys.apdb.hi) },
    { .name = "APGAKEYLO_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 3, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_pauth,
      .fieldoffset = offsetof(CPUARMState, keys.apga.lo) },
    { .name = "APGAKEYHI_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 3, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_pauth,
      .fieldoffset = offsetof(CPUARMState, keys.apga.hi) },
    { .name = "APIAKEYLO_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 1, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_pauth,
      .fieldoffset = offsetof(CPUARMState, keys.apia.lo) },
    { .name = "APIAKEYHI_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 1, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_pauth,
      .fieldoffset = offsetof(CPUARMState, keys.apia.hi) },
    { .name = "APIBKEYLO_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 1, .opc2 = 2,
      .access = PL1_RW, .accessfn = access_pauth,
      .fieldoffset = offsetof(CPUARMState, keys.apib.lo) },
    { .name = "APIBKEYHI_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 2, .crm = 1, .opc2 = 3,
      .access = PL1_RW, .accessfn = access_pauth,
      .fieldoffset = offsetof(CPUARMState, keys.apib.hi) },
    REGINFO_SENTINEL
};

static uint64_t rndr_readfn(CPUARMState *env, const ARMCPRegInfo *ri)
{
    Error *err = NULL;
    uint64_t ret;

    /* Success sets NZCV = 0000.  */
    env->NF = env->CF = env->VF = 0, env->ZF = 1;

    if (qemu_guest_getrandom(&ret, sizeof(ret), &err) < 0) {
        /*
         * ??? Failed, for unknown reasons in the crypto subsystem.
         * The best we can do is log the reason and return the
         * timed-out indication to the guest.  There is no reason
         * we know to expect this failure to be transitory, so the
         * guest may well hang retrying the operation.
         */
        qemu_log_mask(LOG_UNIMP, "%s: Crypto failure: %s",
                      ri->name, error_get_pretty(err));
        error_free(err);

        env->ZF = 0; /* NZCF = 0100 */
        return 0;
    }
    return ret;
}

/* We do not support re-seeding, so the two registers operate the same.  */
static const ARMCPRegInfo rndr_reginfo[] = {
    { .name = "RNDR", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_NO_RAW | ARM_CP_SUPPRESS_TB_END | ARM_CP_IO,
      .opc0 = 3, .opc1 = 3, .crn = 2, .crm = 4, .opc2 = 0,
      .access = PL0_R, .readfn = rndr_readfn },
    { .name = "RNDRRS", .state = ARM_CP_STATE_AA64,
      .type = ARM_CP_NO_RAW | ARM_CP_SUPPRESS_TB_END | ARM_CP_IO,
      .opc0 = 3, .opc1 = 3, .crn = 2, .crm = 4, .opc2 = 1,
      .access = PL0_R, .readfn = rndr_readfn },
    REGINFO_SENTINEL
};

#ifndef CONFIG_USER_ONLY
static void dccvap_writefn(CPUARMState *env, const ARMCPRegInfo *opaque,
                          uint64_t value)
{
    ARMCPU *cpu = env_archcpu(env);
    /* CTR_EL0 System register -> DminLine, bits [19:16] */
    uint64_t dline_size = 4 << ((cpu->ctr >> 16) & 0xF);
    uint64_t vaddr_in = (uint64_t) value;
    uint64_t vaddr = vaddr_in & ~(dline_size - 1);
    void *haddr;
    int mem_idx = cpu_mmu_index(env, false);

    /* This won't be crossing page boundaries */
    haddr = probe_read(env, vaddr, dline_size, mem_idx, GETPC());
    if (haddr) {

        ram_addr_t offset;
        MemoryRegion *mr;

        /* RCU lock is already being held */
        mr = memory_region_from_host(haddr, &offset);

        if (mr) {
            memory_region_writeback(mr, offset, dline_size);
        }
    }
}

static const ARMCPRegInfo dcpop_reg[] = {
    { .name = "DC_CVAP", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 12, .opc2 = 1,
      .access = PL0_W, .type = ARM_CP_NO_RAW | ARM_CP_SUPPRESS_TB_END,
      .accessfn = aa64_cacheop_poc_access, .writefn = dccvap_writefn },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo dcpodp_reg[] = {
    { .name = "DC_CVADP", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 13, .opc2 = 1,
      .access = PL0_W, .type = ARM_CP_NO_RAW | ARM_CP_SUPPRESS_TB_END,
      .accessfn = aa64_cacheop_poc_access, .writefn = dccvap_writefn },
    REGINFO_SENTINEL
};
#endif /*CONFIG_USER_ONLY*/

static CPAccessResult access_aa64_tid5(CPUARMState *env, const ARMCPRegInfo *ri,
                                       bool isread)
{
    if ((arm_current_el(env) < 2) && (arm_hcr_el2_eff(env) & HCR_TID5)) {
        return CP_ACCESS_TRAP_EL2;
    }

    return CP_ACCESS_OK;
}

static CPAccessResult access_mte(CPUARMState *env, const ARMCPRegInfo *ri,
                                 bool isread)
{
    int el = arm_current_el(env);

    if (el < 2 && arm_feature(env, ARM_FEATURE_EL2)) {
        uint64_t hcr = arm_hcr_el2_eff(env);
        if (!(hcr & HCR_ATA) && (!(hcr & HCR_E2H) || !(hcr & HCR_TGE))) {
            return CP_ACCESS_TRAP_EL2;
        }
    }
    if (el < 3 &&
        arm_feature(env, ARM_FEATURE_EL3) &&
        !(env->cp15.scr_el3 & SCR_ATA)) {
        return CP_ACCESS_TRAP_EL3;
    }
    return CP_ACCESS_OK;
}

static uint64_t tco_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return env->pstate & PSTATE_TCO;
}

static void tco_write(CPUARMState *env, const ARMCPRegInfo *ri, uint64_t val)
{
    env->pstate = (env->pstate & ~PSTATE_TCO) | (val & PSTATE_TCO);
}

static const ARMCPRegInfo mte_reginfo[] = {
    { .name = "TFSRE0_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 5, .crm = 6, .opc2 = 1,
      .access = PL1_RW, .accessfn = access_mte,
      .fieldoffset = offsetof(CPUARMState, cp15.tfsr_el[0]) },
    { .name = "TFSR_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 5, .crm = 6, .opc2 = 0,
      .access = PL1_RW, .accessfn = access_mte,
      .fieldoffset = offsetof(CPUARMState, cp15.tfsr_el[1]) },
    { .name = "TFSR_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 5, .crm = 6, .opc2 = 0,
      .access = PL2_RW, .accessfn = access_mte,
      .fieldoffset = offsetof(CPUARMState, cp15.tfsr_el[2]) },
    { .name = "TFSR_EL3", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 6, .crn = 5, .crm = 6, .opc2 = 0,
      .access = PL3_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.tfsr_el[3]) },
    { .name = "RGSR_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 1, .crm = 0, .opc2 = 5,
      .access = PL1_RW, .accessfn = access_mte,
      .fieldoffset = offsetof(CPUARMState, cp15.rgsr_el1) },
    { .name = "GCR_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 0, .crn = 1, .crm = 0, .opc2 = 6,
      .access = PL1_RW, .accessfn = access_mte,
      .fieldoffset = offsetof(CPUARMState, cp15.gcr_el1) },
    { .name = "GMID_EL1", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 1, .crn = 0, .crm = 0, .opc2 = 4,
      .access = PL1_R, .accessfn = access_aa64_tid5,
      .type = ARM_CP_CONST, .resetvalue = GMID_EL1_BS },
    { .name = "TCO", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 4, .crm = 2, .opc2 = 7,
      .type = ARM_CP_NO_RAW,
      .access = PL0_RW, .readfn = tco_read, .writefn = tco_write },
    { .name = "DC_IGVAC", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 6, .opc2 = 3,
      .type = ARM_CP_NOP, .access = PL1_W,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_IGSW", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 6, .opc2 = 4,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = access_tsw },
    { .name = "DC_IGDVAC", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 6, .opc2 = 5,
      .type = ARM_CP_NOP, .access = PL1_W,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_IGDSW", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 6, .opc2 = 6,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = access_tsw },
    { .name = "DC_CGSW", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 10, .opc2 = 4,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = access_tsw },
    { .name = "DC_CGDSW", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 10, .opc2 = 6,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = access_tsw },
    { .name = "DC_CIGSW", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 14, .opc2 = 4,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = access_tsw },
    { .name = "DC_CIGDSW", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 14, .opc2 = 6,
      .type = ARM_CP_NOP, .access = PL1_W, .accessfn = access_tsw },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo mte_tco_ro_reginfo[] = {
    { .name = "TCO", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 4, .crm = 2, .opc2 = 7,
      .type = ARM_CP_CONST, .access = PL0_RW, },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo mte_el0_cacheop_reginfo[] = {
    { .name = "DC_CGVAC", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 10, .opc2 = 3,
      .type = ARM_CP_NOP, .access = PL0_W,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_CGDVAC", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 10, .opc2 = 5,
      .type = ARM_CP_NOP, .access = PL0_W,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_CGVAP", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 12, .opc2 = 3,
      .type = ARM_CP_NOP, .access = PL0_W,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_CGDVAP", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 12, .opc2 = 5,
      .type = ARM_CP_NOP, .access = PL0_W,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_CGVADP", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 13, .opc2 = 3,
      .type = ARM_CP_NOP, .access = PL0_W,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_CGDVADP", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 13, .opc2 = 5,
      .type = ARM_CP_NOP, .access = PL0_W,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_CIGVAC", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 14, .opc2 = 3,
      .type = ARM_CP_NOP, .access = PL0_W,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_CIGDVAC", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 14, .opc2 = 5,
      .type = ARM_CP_NOP, .access = PL0_W,
      .accessfn = aa64_cacheop_poc_access },
    { .name = "DC_GVA", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 4, .opc2 = 3,
      .access = PL0_W, .type = ARM_CP_DC_GVA,
#ifndef CONFIG_USER_ONLY
      /* Avoid overhead of an access check that always passes in user-mode */
      .accessfn = aa64_zva_access,
#endif
    },
    { .name = "DC_GZVA", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 4, .opc2 = 4,
      .access = PL0_W, .type = ARM_CP_DC_GZVA,
#ifndef CONFIG_USER_ONLY
      /* Avoid overhead of an access check that always passes in user-mode */
      .accessfn = aa64_zva_access,
#endif
    },
    REGINFO_SENTINEL
};

#endif

static CPAccessResult access_predinv(CPUARMState *env, const ARMCPRegInfo *ri,
                                     bool isread)
{
    int el = arm_current_el(env);

    if (el == 0) {
        uint64_t sctlr = arm_sctlr(env, el);
        if (!(sctlr & SCTLR_EnRCTX)) {
            return CP_ACCESS_TRAP;
        }
    } else if (el == 1) {
        uint64_t hcr = arm_hcr_el2_eff(env);
        if (hcr & HCR_NV) {
            return CP_ACCESS_TRAP_EL2;
        }
    }
    return CP_ACCESS_OK;
}

static const ARMCPRegInfo predinv_reginfo[] = {
    { .name = "CFP_RCTX", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 3, .opc2 = 4,
      .type = ARM_CP_NOP, .access = PL0_W, .accessfn = access_predinv },
    { .name = "DVP_RCTX", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 3, .opc2 = 5,
      .type = ARM_CP_NOP, .access = PL0_W, .accessfn = access_predinv },
    { .name = "CPP_RCTX", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 3, .crn = 7, .crm = 3, .opc2 = 7,
      .type = ARM_CP_NOP, .access = PL0_W, .accessfn = access_predinv },
    /*
     * Note the AArch32 opcodes have a different OPC1.
     */
    { .name = "CFPRCTX", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 0, .crn = 7, .crm = 3, .opc2 = 4,
      .type = ARM_CP_NOP, .access = PL0_W, .accessfn = access_predinv },
    { .name = "DVPRCTX", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 0, .crn = 7, .crm = 3, .opc2 = 5,
      .type = ARM_CP_NOP, .access = PL0_W, .accessfn = access_predinv },
    { .name = "CPPRCTX", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 0, .crn = 7, .crm = 3, .opc2 = 7,
      .type = ARM_CP_NOP, .access = PL0_W, .accessfn = access_predinv },
    REGINFO_SENTINEL
};

static uint64_t ccsidr2_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    /* Read the high 32 bits of the current CCSIDR */
    return extract64(ccsidr_read(env, ri), 32, 32);
}

static const ARMCPRegInfo ccsidr2_reginfo[] = {
    { .name = "CCSIDR2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 1, .crn = 0, .crm = 0, .opc2 = 2,
      .access = PL1_R,
      .accessfn = access_aa64_tid2,
      .readfn = ccsidr2_read, .type = ARM_CP_NO_RAW },
    REGINFO_SENTINEL
};

static CPAccessResult access_aa64_tid3(CPUARMState *env, const ARMCPRegInfo *ri,
                                       bool isread)
{
    if ((arm_current_el(env) < 2) && (arm_hcr_el2_eff(env) & HCR_TID3)) {
        return CP_ACCESS_TRAP_EL2;
    }

    return CP_ACCESS_OK;
}

static CPAccessResult access_aa32_tid3(CPUARMState *env, const ARMCPRegInfo *ri,
                                       bool isread)
{
    if (arm_feature(env, ARM_FEATURE_V8)) {
        return access_aa64_tid3(env, ri, isread);
    }

    return CP_ACCESS_OK;
}

static CPAccessResult access_jazelle(CPUARMState *env, const ARMCPRegInfo *ri,
                                     bool isread)
{
    if (arm_current_el(env) == 1 && (arm_hcr_el2_eff(env) & HCR_TID0)) {
        return CP_ACCESS_TRAP_EL2;
    }

    return CP_ACCESS_OK;
}

static const ARMCPRegInfo jazelle_regs[] = {
    { .name = "JIDR",
      .cp = 14, .crn = 0, .crm = 0, .opc1 = 7, .opc2 = 0,
      .access = PL1_R, .accessfn = access_jazelle,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "JOSCR",
      .cp = 14, .crn = 1, .crm = 0, .opc1 = 7, .opc2 = 0,
      .access = PL1_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "JMCR",
      .cp = 14, .crn = 2, .crm = 0, .opc1 = 7, .opc2 = 0,
      .access = PL1_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo vhe_reginfo[] = {
    { .name = "CONTEXTIDR_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 13, .crm = 0, .opc2 = 1,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.contextidr_el[2]) },
    { .name = "TTBR1_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 2, .crm = 0, .opc2 = 1,
      .access = PL2_RW, .writefn = vmsa_tcr_ttbr_el2_write,
      .fieldoffset = offsetof(CPUARMState, cp15.ttbr1_el[2]) },
#ifndef CONFIG_USER_ONLY
    { .name = "CNTHV_CVAL_EL2", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 3, .opc2 = 2,
      .fieldoffset =
        offsetof(CPUARMState, cp15.c14_timer[GTIMER_HYPVIRT].cval),
      .type = ARM_CP_IO, .access = PL2_RW,
      .writefn = gt_hv_cval_write, .raw_writefn = raw_write },
    { .name = "CNTHV_TVAL_EL2", .state = ARM_CP_STATE_BOTH,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 3, .opc2 = 0,
      .type = ARM_CP_NO_RAW | ARM_CP_IO, .access = PL2_RW,
      .resetfn = gt_hv_timer_reset,
      .readfn = gt_hv_tval_read, .writefn = gt_hv_tval_write },
    { .name = "CNTHV_CTL_EL2", .state = ARM_CP_STATE_BOTH,
      .type = ARM_CP_IO,
      .opc0 = 3, .opc1 = 4, .crn = 14, .crm = 3, .opc2 = 1,
      .access = PL2_RW,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_HYPVIRT].ctl),
      .writefn = gt_hv_ctl_write, .raw_writefn = raw_write },
    { .name = "CNTP_CTL_EL02", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 5, .crn = 14, .crm = 2, .opc2 = 1,
      .type = ARM_CP_IO | ARM_CP_ALIAS,
      .access = PL2_RW, .accessfn = e2h_access,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_PHYS].ctl),
      .writefn = gt_phys_ctl_write, .raw_writefn = raw_write },
    { .name = "CNTV_CTL_EL02", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 5, .crn = 14, .crm = 3, .opc2 = 1,
      .type = ARM_CP_IO | ARM_CP_ALIAS,
      .access = PL2_RW, .accessfn = e2h_access,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_VIRT].ctl),
      .writefn = gt_virt_ctl_write, .raw_writefn = raw_write },
    { .name = "CNTP_TVAL_EL02", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 5, .crn = 14, .crm = 2, .opc2 = 0,
      .type = ARM_CP_NO_RAW | ARM_CP_IO | ARM_CP_ALIAS,
      .access = PL2_RW, .accessfn = e2h_access,
      .readfn = gt_phys_tval_read, .writefn = gt_phys_tval_write },
    { .name = "CNTV_TVAL_EL02", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 5, .crn = 14, .crm = 3, .opc2 = 0,
      .type = ARM_CP_NO_RAW | ARM_CP_IO | ARM_CP_ALIAS,
      .access = PL2_RW, .accessfn = e2h_access,
      .readfn = gt_virt_tval_read, .writefn = gt_virt_tval_write },
    { .name = "CNTP_CVAL_EL02", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 5, .crn = 14, .crm = 2, .opc2 = 2,
      .type = ARM_CP_IO | ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_PHYS].cval),
      .access = PL2_RW, .accessfn = e2h_access,
      .writefn = gt_phys_cval_write, .raw_writefn = raw_write },
    { .name = "CNTV_CVAL_EL02", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 5, .crn = 14, .crm = 3, .opc2 = 2,
      .type = ARM_CP_IO | ARM_CP_ALIAS,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_timer[GTIMER_VIRT].cval),
      .access = PL2_RW, .accessfn = e2h_access,
      .writefn = gt_virt_cval_write, .raw_writefn = raw_write },
#endif
    REGINFO_SENTINEL
};

#ifndef CONFIG_USER_ONLY
static const ARMCPRegInfo ats1e1_reginfo[] = {
    { .name = "AT_S1E1R", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 9, .opc2 = 0,
      .access = PL1_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    { .name = "AT_S1E1W", .state = ARM_CP_STATE_AA64,
      .opc0 = 1, .opc1 = 0, .crn = 7, .crm = 9, .opc2 = 1,
      .access = PL1_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write64 },
    REGINFO_SENTINEL
};

static const ARMCPRegInfo ats1cp_reginfo[] = {
    { .name = "ATS1CPRP",
      .cp = 15, .opc1 = 0, .crn = 7, .crm = 9, .opc2 = 0,
      .access = PL1_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write },
    { .name = "ATS1CPWP",
      .cp = 15, .opc1 = 0, .crn = 7, .crm = 9, .opc2 = 1,
      .access = PL1_W, .type = ARM_CP_NO_RAW | ARM_CP_RAISES_EXC,
      .writefn = ats_write },
    REGINFO_SENTINEL
};
#endif

/*
 * ACTLR2 and HACTLR2 map to ACTLR_EL1[63:32] and
 * ACTLR_EL2[63:32]. They exist only if the ID_MMFR4.AC2 field
 * is non-zero, which is never for ARMv7, optionally in ARMv8
 * and mandatorily for ARMv8.2 and up.
 * ACTLR2 is banked for S and NS if EL3 is AArch32. Since QEMU's
 * implementation is RAZ/WI we can ignore this detail, as we
 * do for ACTLR.
 */
static const ARMCPRegInfo actlr2_hactlr2_reginfo[] = {
    { .name = "ACTLR2", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 0, .crn = 1, .crm = 0, .opc2 = 3,
      .access = PL1_RW, .accessfn = access_tacr,
      .type = ARM_CP_CONST, .resetvalue = 0 },
    { .name = "HACTLR2", .state = ARM_CP_STATE_AA32,
      .cp = 15, .opc1 = 4, .crn = 1, .crm = 0, .opc2 = 3,
      .access = PL2_RW, .type = ARM_CP_CONST,
      .resetvalue = 0 },
    REGINFO_SENTINEL
};

void register_cp_regs_for_features(ARMCPU *cpu)
{
    /* Register all the coprocessor registers based on feature bits */
    CPUARMState *env = &cpu->env;
    if (arm_feature(env, ARM_FEATURE_M)) {
        /* M profile has no coprocessor registers */
        return;
    }

    define_arm_cp_regs(cpu, cp_reginfo);
    if (!arm_feature(env, ARM_FEATURE_V8)) {
        /*
         * Must go early as it is full of wildcards that may be
         * overridden by later definitions.
         */
        define_arm_cp_regs(cpu, not_v8_cp_reginfo);
    }

    if (arm_feature(env, ARM_FEATURE_V6)) {
        /* The ID registers all have impdef reset values */
        ARMCPRegInfo v6_idregs[] = {
            { .name = "ID_PFR0", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 1, .opc2 = 0,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_pfr0 },
            /*
             * ID_PFR1 is not a plain ARM_CP_CONST because we don't know
             * the value of the GIC field until after we define these regs.
             */
            { .name = "ID_PFR1", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 1, .opc2 = 1,
              .access = PL1_R, .type = ARM_CP_NO_RAW,
              .accessfn = access_aa32_tid3,
              .readfn = id_pfr1_read,
              .writefn = arm_cp_write_ignore },
            { .name = "ID_DFR0", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 1, .opc2 = 2,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_dfr0 },
            { .name = "ID_AFR0", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 1, .opc2 = 3,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->id_afr0 },
            { .name = "ID_MMFR0", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 1, .opc2 = 4,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_mmfr0 },
            { .name = "ID_MMFR1", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 1, .opc2 = 5,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_mmfr1 },
            { .name = "ID_MMFR2", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 1, .opc2 = 6,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_mmfr2 },
            { .name = "ID_MMFR3", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 1, .opc2 = 7,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_mmfr3 },
            { .name = "ID_ISAR0", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 2, .opc2 = 0,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_isar0 },
            { .name = "ID_ISAR1", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 2, .opc2 = 1,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_isar1 },
            { .name = "ID_ISAR2", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 2, .opc2 = 2,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_isar2 },
            { .name = "ID_ISAR3", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 2, .opc2 = 3,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_isar3 },
            { .name = "ID_ISAR4", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 2, .opc2 = 4,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_isar4 },
            { .name = "ID_ISAR5", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 2, .opc2 = 5,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_isar5 },
            { .name = "ID_MMFR4", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 2, .opc2 = 6,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_mmfr4 },
            { .name = "ID_ISAR6", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 2, .opc2 = 7,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa32_tid3,
              .resetvalue = cpu->isar.id_isar6 },
            REGINFO_SENTINEL
        };
        define_arm_cp_regs(cpu, v6_idregs);
        define_arm_cp_regs(cpu, v6_cp_reginfo);
    } else {
        define_arm_cp_regs(cpu, not_v6_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_V6K)) {
        define_arm_cp_regs(cpu, v6k_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_V7MP) &&
        !arm_feature(env, ARM_FEATURE_PMSA)) {
        define_arm_cp_regs(cpu, v7mp_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_V7VE)) {
        define_arm_cp_regs(cpu, pmovsset_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_V7)) {
        ARMCPRegInfo clidr = {
            .name = "CLIDR", .state = ARM_CP_STATE_BOTH,
            .opc0 = 3, .crn = 0, .crm = 0, .opc1 = 1, .opc2 = 1,
            .access = PL1_R, .type = ARM_CP_CONST,
            .accessfn = access_aa64_tid2,
            .resetvalue = cpu->clidr
        };
        define_one_arm_cp_reg(cpu, &clidr);
        define_arm_cp_regs(cpu, v7_cp_reginfo);
        define_debug_regs(cpu);
        define_pmu_regs(cpu);
    } else {
        define_arm_cp_regs(cpu, not_v7_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_V8)) {
        /*
         * AArch64 ID registers, which all have impdef reset values.
         * Note that within the ID register ranges the unused slots
         * must all RAZ, not UNDEF; future architecture versions may
         * define new registers here.
         */
        ARMCPRegInfo v8_idregs[] = {
            /*
             * ID_AA64PFR0_EL1 is not a plain ARM_CP_CONST in system
             * emulation because we don't know the right value for the
             * GIC field until after we define these regs.
             */
            { .name = "ID_AA64PFR0_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 4, .opc2 = 0,
              .access = PL1_R,
#ifdef CONFIG_USER_ONLY
              .type = ARM_CP_CONST,
              .resetvalue = cpu->isar.id_aa64pfr0
#else
              .type = ARM_CP_NO_RAW,
              .accessfn = access_aa64_tid3,
              .readfn = id_aa64pfr0_read,
              .writefn = arm_cp_write_ignore
#endif
            },
            { .name = "ID_AA64PFR1_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 4, .opc2 = 1,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.id_aa64pfr1},
            { .name = "ID_AA64PFR2_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 4, .opc2 = 2,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64PFR3_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 4, .opc2 = 3,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64ZFR0_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 4, .opc2 = 4,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              /* At present, only SVEver == 0 is defined anyway.  */
              .resetvalue = 0 },
            { .name = "ID_AA64PFR5_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 4, .opc2 = 5,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64PFR6_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 4, .opc2 = 6,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64PFR7_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 4, .opc2 = 7,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64DFR0_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 5, .opc2 = 0,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.id_aa64dfr0 },
            { .name = "ID_AA64DFR1_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 5, .opc2 = 1,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.id_aa64dfr1 },
            { .name = "ID_AA64DFR2_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 5, .opc2 = 2,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64DFR3_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 5, .opc2 = 3,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64AFR0_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 5, .opc2 = 4,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->id_aa64afr0 },
            { .name = "ID_AA64AFR1_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 5, .opc2 = 5,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->id_aa64afr1 },
            { .name = "ID_AA64AFR2_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 5, .opc2 = 6,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64AFR3_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 5, .opc2 = 7,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64ISAR0_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 6, .opc2 = 0,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.id_aa64isar0 },
            { .name = "ID_AA64ISAR1_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 6, .opc2 = 1,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.id_aa64isar1 },
            { .name = "ID_AA64ISAR2_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 6, .opc2 = 2,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64ISAR3_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 6, .opc2 = 3,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64ISAR4_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 6, .opc2 = 4,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64ISAR5_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 6, .opc2 = 5,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64ISAR6_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 6, .opc2 = 6,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64ISAR7_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 6, .opc2 = 7,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64MMFR0_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 7, .opc2 = 0,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.id_aa64mmfr0 },
            { .name = "ID_AA64MMFR1_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 7, .opc2 = 1,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.id_aa64mmfr1 },
            { .name = "ID_AA64MMFR2_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 7, .opc2 = 2,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.id_aa64mmfr2 },
            { .name = "ID_AA64MMFR3_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 7, .opc2 = 3,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64MMFR4_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 7, .opc2 = 4,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64MMFR5_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 7, .opc2 = 5,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64MMFR6_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 7, .opc2 = 6,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_AA64MMFR7_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 7, .opc2 = 7,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "MVFR0_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 3, .opc2 = 0,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.mvfr0 },
            { .name = "MVFR1_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 3, .opc2 = 1,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.mvfr1 },
            { .name = "MVFR2_EL1", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 3, .opc2 = 2,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.mvfr2 },
            { .name = "MVFR3_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 3, .opc2 = 3,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "ID_PFR2", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 3, .opc2 = 4,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = cpu->isar.id_pfr2 },
            { .name = "MVFR5_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 3, .opc2 = 5,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "MVFR6_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 3, .opc2 = 6,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "MVFR7_EL1_RESERVED", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 3, .opc2 = 7,
              .access = PL1_R, .type = ARM_CP_CONST,
              .accessfn = access_aa64_tid3,
              .resetvalue = 0 },
            { .name = "PMCEID0", .state = ARM_CP_STATE_AA32,
              .cp = 15, .opc1 = 0, .crn = 9, .crm = 12, .opc2 = 6,
              .access = PL0_R, .accessfn = pmreg_access, .type = ARM_CP_CONST,
              .resetvalue = extract64(cpu->pmceid0, 0, 32) },
            { .name = "PMCEID0_EL0", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 12, .opc2 = 6,
              .access = PL0_R, .accessfn = pmreg_access, .type = ARM_CP_CONST,
              .resetvalue = cpu->pmceid0 },
            { .name = "PMCEID1", .state = ARM_CP_STATE_AA32,
              .cp = 15, .opc1 = 0, .crn = 9, .crm = 12, .opc2 = 7,
              .access = PL0_R, .accessfn = pmreg_access, .type = ARM_CP_CONST,
              .resetvalue = extract64(cpu->pmceid1, 0, 32) },
            { .name = "PMCEID1_EL0", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 3, .crn = 9, .crm = 12, .opc2 = 7,
              .access = PL0_R, .accessfn = pmreg_access, .type = ARM_CP_CONST,
              .resetvalue = cpu->pmceid1 },
            REGINFO_SENTINEL
        };
#ifdef CONFIG_USER_ONLY
        ARMCPRegUserSpaceInfo v8_user_idregs[] = {
            { .name = "ID_AA64PFR0_EL1",
              .exported_bits = 0x000f000f00ff0000,
              .fixed_bits    = 0x0000000000000011 },
            { .name = "ID_AA64PFR1_EL1",
              .exported_bits = 0x00000000000000f0 },
            { .name = "ID_AA64PFR*_EL1_RESERVED",
              .is_glob = true                     },
            { .name = "ID_AA64ZFR0_EL1"           },
            { .name = "ID_AA64MMFR0_EL1",
              .fixed_bits    = 0x00000000ff000000 },
            { .name = "ID_AA64MMFR1_EL1"          },
            { .name = "ID_AA64MMFR*_EL1_RESERVED",
              .is_glob = true                     },
            { .name = "ID_AA64DFR0_EL1",
              .fixed_bits    = 0x0000000000000006 },
            { .name = "ID_AA64DFR1_EL1"           },
            { .name = "ID_AA64DFR*_EL1_RESERVED",
              .is_glob = true                     },
            { .name = "ID_AA64AFR*",
              .is_glob = true                     },
            { .name = "ID_AA64ISAR0_EL1",
              .exported_bits = 0x00fffffff0fffff0 },
            { .name = "ID_AA64ISAR1_EL1",
              .exported_bits = 0x000000f0ffffffff },
            { .name = "ID_AA64ISAR*_EL1_RESERVED",
              .is_glob = true                     },
            REGUSERINFO_SENTINEL
        };
        modify_arm_cp_regs(v8_idregs, v8_user_idregs);
#endif
        /* RVBAR_EL1 is only implemented if EL1 is the highest EL */
        if (!arm_feature(env, ARM_FEATURE_EL3) &&
            !arm_feature(env, ARM_FEATURE_EL2)) {
            ARMCPRegInfo rvbar = {
                .name = "RVBAR_EL1", .state = ARM_CP_STATE_AA64,
                .opc0 = 3, .opc1 = 0, .crn = 12, .crm = 0, .opc2 = 1,
                .type = ARM_CP_CONST, .access = PL1_R, .resetvalue = cpu->rvbar
            };
            define_one_arm_cp_reg(cpu, &rvbar);
        }
        define_arm_cp_regs(cpu, v8_idregs);
        define_arm_cp_regs(cpu, v8_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_EL2)) {
        uint64_t vmpidr_def = mpidr_read_val(env);
        ARMCPRegInfo vpidr_regs[] = {
            { .name = "VPIDR", .state = ARM_CP_STATE_AA32,
              .cp = 15, .opc1 = 4, .crn = 0, .crm = 0, .opc2 = 0,
              .access = PL2_RW, .accessfn = access_el3_aa32ns,
              .resetvalue = cpu->midr, .type = ARM_CP_ALIAS,
              .fieldoffset = offsetoflow32(CPUARMState, cp15.vpidr_el2) },
            { .name = "VPIDR_EL2", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 4, .crn = 0, .crm = 0, .opc2 = 0,
              .access = PL2_RW, .resetvalue = cpu->midr,
              .fieldoffset = offsetof(CPUARMState, cp15.vpidr_el2) },
            { .name = "VMPIDR", .state = ARM_CP_STATE_AA32,
              .cp = 15, .opc1 = 4, .crn = 0, .crm = 0, .opc2 = 5,
              .access = PL2_RW, .accessfn = access_el3_aa32ns,
              .resetvalue = vmpidr_def, .type = ARM_CP_ALIAS,
              .fieldoffset = offsetoflow32(CPUARMState, cp15.vmpidr_el2) },
            { .name = "VMPIDR_EL2", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 4, .crn = 0, .crm = 0, .opc2 = 5,
              .access = PL2_RW,
              .resetvalue = vmpidr_def,
              .fieldoffset = offsetof(CPUARMState, cp15.vmpidr_el2) },
            REGINFO_SENTINEL
        };
        define_arm_cp_regs(cpu, vpidr_regs);
        define_arm_cp_regs(cpu, el2_cp_reginfo);
        if (arm_feature(env, ARM_FEATURE_V8)) {
            define_arm_cp_regs(cpu, el2_v8_cp_reginfo);
        }
        if (cpu_isar_feature(aa64_sel2, cpu)) {
            define_arm_cp_regs(cpu, el2_sec_cp_reginfo);
        }
        /* RVBAR_EL2 is only implemented if EL2 is the highest EL */
        if (!arm_feature(env, ARM_FEATURE_EL3)) {
            ARMCPRegInfo rvbar = {
                .name = "RVBAR_EL2", .state = ARM_CP_STATE_AA64,
                .opc0 = 3, .opc1 = 4, .crn = 12, .crm = 0, .opc2 = 1,
                .type = ARM_CP_CONST, .access = PL2_R, .resetvalue = cpu->rvbar
            };
            define_one_arm_cp_reg(cpu, &rvbar);
        }
    } else {
        /*
         * If EL2 is missing but higher ELs are enabled, we need to
         * register the no_el2 reginfos.
         */
        if (arm_feature(env, ARM_FEATURE_EL3)) {
            /*
             * When EL3 exists but not EL2, VPIDR and VMPIDR take the value
             * of MIDR_EL1 and MPIDR_EL1.
             */
            ARMCPRegInfo vpidr_regs[] = {
                { .name = "VPIDR_EL2", .state = ARM_CP_STATE_BOTH,
                  .opc0 = 3, .opc1 = 4, .crn = 0, .crm = 0, .opc2 = 0,
                  .access = PL2_RW, .accessfn = access_el3_aa32ns,
                  .type = ARM_CP_CONST, .resetvalue = cpu->midr,
                  .fieldoffset = offsetof(CPUARMState, cp15.vpidr_el2) },
                { .name = "VMPIDR_EL2", .state = ARM_CP_STATE_BOTH,
                  .opc0 = 3, .opc1 = 4, .crn = 0, .crm = 0, .opc2 = 5,
                  .access = PL2_RW, .accessfn = access_el3_aa32ns,
                  .type = ARM_CP_NO_RAW,
                  .writefn = arm_cp_write_ignore, .readfn = mpidr_read },
                REGINFO_SENTINEL
            };
            define_arm_cp_regs(cpu, vpidr_regs);
            define_arm_cp_regs(cpu, el3_no_el2_cp_reginfo);
            if (arm_feature(env, ARM_FEATURE_V8)) {
                define_arm_cp_regs(cpu, el3_no_el2_v8_cp_reginfo);
            }
        }
    }
    if (arm_feature(env, ARM_FEATURE_EL3)) {
        define_arm_cp_regs(cpu, el3_cp_reginfo);
        ARMCPRegInfo el3_regs[] = {
            { .name = "RVBAR_EL3", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 6, .crn = 12, .crm = 0, .opc2 = 1,
              .type = ARM_CP_CONST, .access = PL3_R, .resetvalue = cpu->rvbar },
            { .name = "SCTLR_EL3", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 6, .crn = 1, .crm = 0, .opc2 = 0,
              .access = PL3_RW,
              .raw_writefn = raw_write, .writefn = sctlr_write,
              .fieldoffset = offsetof(CPUARMState, cp15.sctlr_el[3]),
              .resetvalue = cpu->reset_sctlr },
            REGINFO_SENTINEL
        };

        define_arm_cp_regs(cpu, el3_regs);
    }
    /*
     * The behaviour of NSACR is sufficiently various that we don't
     * try to describe it in a single reginfo:
     *  if EL3 is 64 bit, then trap to EL3 from S EL1,
     *     reads as constant 0xc00 from NS EL1 and NS EL2
     *  if EL3 is 32 bit, then RW at EL3, RO at NS EL1 and NS EL2
     *  if v7 without EL3, register doesn't exist
     *  if v8 without EL3, reads as constant 0xc00 from NS EL1 and NS EL2
     */
    if (arm_feature(env, ARM_FEATURE_EL3)) {
        if (arm_feature(env, ARM_FEATURE_AARCH64)) {
            ARMCPRegInfo nsacr = {
                .name = "NSACR", .type = ARM_CP_CONST,
                .cp = 15, .opc1 = 0, .crn = 1, .crm = 1, .opc2 = 2,
                .access = PL1_RW, .accessfn = nsacr_access,
                .resetvalue = 0xc00
            };
            define_one_arm_cp_reg(cpu, &nsacr);
        } else {
            ARMCPRegInfo nsacr = {
                .name = "NSACR",
                .cp = 15, .opc1 = 0, .crn = 1, .crm = 1, .opc2 = 2,
                .access = PL3_RW | PL1_R,
                .resetvalue = 0,
                .fieldoffset = offsetof(CPUARMState, cp15.nsacr)
            };
            define_one_arm_cp_reg(cpu, &nsacr);
        }
    } else {
        if (arm_feature(env, ARM_FEATURE_V8)) {
            ARMCPRegInfo nsacr = {
                .name = "NSACR", .type = ARM_CP_CONST,
                .cp = 15, .opc1 = 0, .crn = 1, .crm = 1, .opc2 = 2,
                .access = PL1_R,
                .resetvalue = 0xc00
            };
            define_one_arm_cp_reg(cpu, &nsacr);
        }
    }

    if (arm_feature(env, ARM_FEATURE_PMSA)) {
        if (arm_feature(env, ARM_FEATURE_V6)) {
            /* PMSAv6 not implemented */
            assert(arm_feature(env, ARM_FEATURE_V7));
            define_arm_cp_regs(cpu, vmsa_pmsa_cp_reginfo);
            define_arm_cp_regs(cpu, pmsav7_cp_reginfo);
        } else {
            define_arm_cp_regs(cpu, pmsav5_cp_reginfo);
        }
    } else {
        define_arm_cp_regs(cpu, vmsa_pmsa_cp_reginfo);
        define_arm_cp_regs(cpu, vmsa_cp_reginfo);
        /* TTCBR2 is introduced with ARMv8.2-AA32HPD.  */
        if (cpu_isar_feature(aa32_hpd, cpu)) {
            define_one_arm_cp_reg(cpu, &ttbcr2_reginfo);
        }
    }
    if (arm_feature(env, ARM_FEATURE_THUMB2EE)) {
        define_arm_cp_regs(cpu, t2ee_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_GENERIC_TIMER)) {
        define_arm_cp_regs(cpu, generic_timer_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_VAPA)) {
        define_arm_cp_regs(cpu, vapa_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_CACHE_TEST_CLEAN)) {
        define_arm_cp_regs(cpu, cache_test_clean_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_CACHE_DIRTY_REG)) {
        define_arm_cp_regs(cpu, cache_dirty_status_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_CACHE_BLOCK_OPS)) {
        define_arm_cp_regs(cpu, cache_block_ops_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_OMAPCP)) {
        define_arm_cp_regs(cpu, omap_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_STRONGARM)) {
        define_arm_cp_regs(cpu, strongarm_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_XSCALE)) {
        define_arm_cp_regs(cpu, xscale_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_DUMMY_C15_REGS)) {
        define_arm_cp_regs(cpu, dummy_c15_cp_reginfo);
    }
    if (arm_feature(env, ARM_FEATURE_LPAE)) {
        define_arm_cp_regs(cpu, lpae_cp_reginfo);
    }
    if (cpu_isar_feature(aa32_jazelle, cpu)) {
        define_arm_cp_regs(cpu, jazelle_regs);
    }
    /*
     * Slightly awkwardly, the OMAP and StrongARM cores need all of
     * cp15 crn=0 to be writes-ignored, whereas for other cores they should
     * be read-only (ie write causes UNDEF exception).
     */
    {
        ARMCPRegInfo id_pre_v8_midr_cp_reginfo[] = {
            /*
             * Pre-v8 MIDR space.
             * Note that the MIDR isn't a simple constant register because
             * of the TI925 behaviour where writes to another register can
             * cause the MIDR value to change.
             *
             * Unimplemented registers in the c15 0 0 0 space default to
             * MIDR. Define MIDR first as this entire space, then CTR, TCMTR
             * and friends override accordingly.
             */
            { .name = "MIDR",
              .cp = 15, .crn = 0, .crm = 0, .opc1 = 0, .opc2 = CP_ANY,
              .access = PL1_R, .resetvalue = cpu->midr,
              .writefn = arm_cp_write_ignore, .raw_writefn = raw_write,
              .readfn = midr_read,
              .fieldoffset = offsetof(CPUARMState, cp15.c0_cpuid),
              .type = ARM_CP_OVERRIDE },
            /* crn = 0 op1 = 0 crm = 3..7 : currently unassigned; we RAZ. */
            { .name = "DUMMY",
              .cp = 15, .crn = 0, .crm = 3, .opc1 = 0, .opc2 = CP_ANY,
              .access = PL1_R, .type = ARM_CP_CONST, .resetvalue = 0 },
            { .name = "DUMMY",
              .cp = 15, .crn = 0, .crm = 4, .opc1 = 0, .opc2 = CP_ANY,
              .access = PL1_R, .type = ARM_CP_CONST, .resetvalue = 0 },
            { .name = "DUMMY",
              .cp = 15, .crn = 0, .crm = 5, .opc1 = 0, .opc2 = CP_ANY,
              .access = PL1_R, .type = ARM_CP_CONST, .resetvalue = 0 },
            { .name = "DUMMY",
              .cp = 15, .crn = 0, .crm = 6, .opc1 = 0, .opc2 = CP_ANY,
              .access = PL1_R, .type = ARM_CP_CONST, .resetvalue = 0 },
            { .name = "DUMMY",
              .cp = 15, .crn = 0, .crm = 7, .opc1 = 0, .opc2 = CP_ANY,
              .access = PL1_R, .type = ARM_CP_CONST, .resetvalue = 0 },
            REGINFO_SENTINEL
        };
        ARMCPRegInfo id_v8_midr_cp_reginfo[] = {
            { .name = "MIDR_EL1", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 0, .opc2 = 0,
              .access = PL1_R, .type = ARM_CP_NO_RAW, .resetvalue = cpu->midr,
              .fieldoffset = offsetof(CPUARMState, cp15.c0_cpuid),
              .readfn = midr_read },
            /* crn = 0 op1 = 0 crm = 0 op2 = 4,7 : AArch32 aliases of MIDR */
            { .name = "MIDR", .type = ARM_CP_ALIAS | ARM_CP_CONST,
              .cp = 15, .crn = 0, .crm = 0, .opc1 = 0, .opc2 = 4,
              .access = PL1_R, .resetvalue = cpu->midr },
            { .name = "MIDR", .type = ARM_CP_ALIAS | ARM_CP_CONST,
              .cp = 15, .crn = 0, .crm = 0, .opc1 = 0, .opc2 = 7,
              .access = PL1_R, .resetvalue = cpu->midr },
            { .name = "REVIDR_EL1", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 0, .crm = 0, .opc2 = 6,
              .access = PL1_R,
              .accessfn = access_aa64_tid1,
              .type = ARM_CP_CONST, .resetvalue = cpu->revidr },
            REGINFO_SENTINEL
        };
        ARMCPRegInfo id_cp_reginfo[] = {
            /* These are common to v8 and pre-v8 */
            { .name = "CTR",
              .cp = 15, .crn = 0, .crm = 0, .opc1 = 0, .opc2 = 1,
              .access = PL1_R, .accessfn = ctr_el0_access,
              .type = ARM_CP_CONST, .resetvalue = cpu->ctr },
            { .name = "CTR_EL0", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 3, .opc2 = 1, .crn = 0, .crm = 0,
              .access = PL0_R, .accessfn = ctr_el0_access,
              .type = ARM_CP_CONST, .resetvalue = cpu->ctr },
            /* TCMTR and TLBTR exist in v8 but have no 64-bit versions */
            { .name = "TCMTR",
              .cp = 15, .crn = 0, .crm = 0, .opc1 = 0, .opc2 = 2,
              .access = PL1_R,
              .accessfn = access_aa32_tid1,
              .type = ARM_CP_CONST, .resetvalue = 0 },
            REGINFO_SENTINEL
        };
        /* TLBTR is specific to VMSA */
        ARMCPRegInfo id_tlbtr_reginfo = {
              .name = "TLBTR",
              .cp = 15, .crn = 0, .crm = 0, .opc1 = 0, .opc2 = 3,
              .access = PL1_R,
              .accessfn = access_aa32_tid1,
              .type = ARM_CP_CONST, .resetvalue = 0,
        };
        /* MPUIR is specific to PMSA V6+ */
        ARMCPRegInfo id_mpuir_reginfo = {
              .name = "MPUIR",
              .cp = 15, .crn = 0, .crm = 0, .opc1 = 0, .opc2 = 4,
              .access = PL1_R, .type = ARM_CP_CONST,
              .resetvalue = cpu->pmsav7_dregion << 8
        };
        ARMCPRegInfo crn0_wi_reginfo = {
            .name = "CRN0_WI", .cp = 15, .crn = 0, .crm = CP_ANY,
            .opc1 = CP_ANY, .opc2 = CP_ANY, .access = PL1_W,
            .type = ARM_CP_NOP | ARM_CP_OVERRIDE
        };
#ifdef CONFIG_USER_ONLY
        ARMCPRegUserSpaceInfo id_v8_user_midr_cp_reginfo[] = {
            { .name = "MIDR_EL1",
              .exported_bits = 0x00000000ffffffff },
            { .name = "REVIDR_EL1"                },
            REGUSERINFO_SENTINEL
        };
        modify_arm_cp_regs(id_v8_midr_cp_reginfo, id_v8_user_midr_cp_reginfo);
#endif
        if (arm_feature(env, ARM_FEATURE_OMAPCP) ||
            arm_feature(env, ARM_FEATURE_STRONGARM)) {
            ARMCPRegInfo *r;
            /*
             * Register the blanket "writes ignored" value first to cover the
             * whole space. Then update the specific ID registers to allow write
             * access, so that they ignore writes rather than causing them to
             * UNDEF.
             */
            define_one_arm_cp_reg(cpu, &crn0_wi_reginfo);
            for (r = id_pre_v8_midr_cp_reginfo;
                 r->type != ARM_CP_SENTINEL; r++) {
                r->access = PL1_RW;
            }
            for (r = id_cp_reginfo; r->type != ARM_CP_SENTINEL; r++) {
                r->access = PL1_RW;
            }
            id_mpuir_reginfo.access = PL1_RW;
            id_tlbtr_reginfo.access = PL1_RW;
        }
        if (arm_feature(env, ARM_FEATURE_V8)) {
            define_arm_cp_regs(cpu, id_v8_midr_cp_reginfo);
        } else {
            define_arm_cp_regs(cpu, id_pre_v8_midr_cp_reginfo);
        }
        define_arm_cp_regs(cpu, id_cp_reginfo);
        if (!arm_feature(env, ARM_FEATURE_PMSA)) {
            define_one_arm_cp_reg(cpu, &id_tlbtr_reginfo);
        } else if (arm_feature(env, ARM_FEATURE_V7)) {
            define_one_arm_cp_reg(cpu, &id_mpuir_reginfo);
        }
    }

    if (arm_feature(env, ARM_FEATURE_MPIDR)) {
        ARMCPRegInfo mpidr_cp_reginfo[] = {
            { .name = "MPIDR_EL1", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .crn = 0, .crm = 0, .opc1 = 0, .opc2 = 5,
              .access = PL1_R, .readfn = mpidr_read, .type = ARM_CP_NO_RAW },
            REGINFO_SENTINEL
        };
#ifdef CONFIG_USER_ONLY
        ARMCPRegUserSpaceInfo mpidr_user_cp_reginfo[] = {
            { .name = "MPIDR_EL1",
              .fixed_bits = 0x0000000080000000 },
            REGUSERINFO_SENTINEL
        };
        modify_arm_cp_regs(mpidr_cp_reginfo, mpidr_user_cp_reginfo);
#endif
        define_arm_cp_regs(cpu, mpidr_cp_reginfo);
    }

    if (arm_feature(env, ARM_FEATURE_AUXCR)) {
        ARMCPRegInfo auxcr_reginfo[] = {
            { .name = "ACTLR_EL1", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 0, .crn = 1, .crm = 0, .opc2 = 1,
              .access = PL1_RW, .accessfn = access_tacr,
              .type = ARM_CP_CONST, .resetvalue = cpu->reset_auxcr },
            { .name = "ACTLR_EL2", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .opc1 = 4, .crn = 1, .crm = 0, .opc2 = 1,
              .access = PL2_RW, .type = ARM_CP_CONST,
              .resetvalue = 0 },
            { .name = "ACTLR_EL3", .state = ARM_CP_STATE_AA64,
              .opc0 = 3, .opc1 = 6, .crn = 1, .crm = 0, .opc2 = 1,
              .access = PL3_RW, .type = ARM_CP_CONST,
              .resetvalue = 0 },
            REGINFO_SENTINEL
        };
        define_arm_cp_regs(cpu, auxcr_reginfo);
        if (cpu_isar_feature(aa32_ac2, cpu)) {
            define_arm_cp_regs(cpu, actlr2_hactlr2_reginfo);
        }
    }

    if (arm_feature(env, ARM_FEATURE_CBAR)) {
        /*
         * CBAR is IMPDEF, but common on Arm Cortex-A implementations.
         * There are two flavours:
         *  (1) older 32-bit only cores have a simple 32-bit CBAR
         *  (2) 64-bit cores have a 64-bit CBAR visible to AArch64, plus a
         *      32-bit register visible to AArch32 at a different encoding
         *      to the "flavour 1" register and with the bits rearranged to
         *      be able to squash a 64-bit address into the 32-bit view.
         * We distinguish the two via the ARM_FEATURE_AARCH64 flag, but
         * in future if we support AArch32-only configs of some of the
         * AArch64 cores we might need to add a specific feature flag
         * to indicate cores with "flavour 2" CBAR.
         */
        if (arm_feature(env, ARM_FEATURE_AARCH64)) {
            /* 32 bit view is [31:18] 0...0 [43:32]. */
            uint32_t cbar32 = (extract64(cpu->reset_cbar, 18, 14) << 18)
                | extract64(cpu->reset_cbar, 32, 12);
            ARMCPRegInfo cbar_reginfo[] = {
                { .name = "CBAR",
                  .type = ARM_CP_CONST,
                  .cp = 15, .crn = 15, .crm = 3, .opc1 = 1, .opc2 = 0,
                  .access = PL1_R, .resetvalue = cbar32 },
                { .name = "CBAR_EL1", .state = ARM_CP_STATE_AA64,
                  .type = ARM_CP_CONST,
                  .opc0 = 3, .opc1 = 1, .crn = 15, .crm = 3, .opc2 = 0,
                  .access = PL1_R, .resetvalue = cpu->reset_cbar },
                REGINFO_SENTINEL
            };
            /* We don't implement a r/w 64 bit CBAR currently */
            assert(arm_feature(env, ARM_FEATURE_CBAR_RO));
            define_arm_cp_regs(cpu, cbar_reginfo);
        } else {
            ARMCPRegInfo cbar = {
                .name = "CBAR",
                .cp = 15, .crn = 15, .crm = 0, .opc1 = 4, .opc2 = 0,
                .access = PL1_R|PL3_W, .resetvalue = cpu->reset_cbar,
                .fieldoffset = offsetof(CPUARMState,
                                        cp15.c15_config_base_address)
            };
            if (arm_feature(env, ARM_FEATURE_CBAR_RO)) {
                cbar.access = PL1_R;
                cbar.fieldoffset = 0;
                cbar.type = ARM_CP_CONST;
            }
            define_one_arm_cp_reg(cpu, &cbar);
        }
    }

    if (arm_feature(env, ARM_FEATURE_VBAR)) {
        ARMCPRegInfo vbar_cp_reginfo[] = {
            { .name = "VBAR", .state = ARM_CP_STATE_BOTH,
              .opc0 = 3, .crn = 12, .crm = 0, .opc1 = 0, .opc2 = 0,
              .access = PL1_RW, .writefn = vbar_write,
              .bank_fieldoffsets = { offsetof(CPUARMState, cp15.vbar_s),
                                     offsetof(CPUARMState, cp15.vbar_ns) },
              .resetvalue = 0 },
            REGINFO_SENTINEL
        };
        define_arm_cp_regs(cpu, vbar_cp_reginfo);
    }

    /* Generic registers whose values depend on the implementation */
    {
        ARMCPRegInfo sctlr = {
            .name = "SCTLR", .state = ARM_CP_STATE_BOTH,
            .opc0 = 3, .opc1 = 0, .crn = 1, .crm = 0, .opc2 = 0,
            .access = PL1_RW, .accessfn = access_tvm_trvm,
            .bank_fieldoffsets = { offsetof(CPUARMState, cp15.sctlr_s),
                                   offsetof(CPUARMState, cp15.sctlr_ns) },
            .writefn = sctlr_write, .resetvalue = cpu->reset_sctlr,
            .raw_writefn = raw_write,
        };
        if (arm_feature(env, ARM_FEATURE_XSCALE)) {
            /* Normally we would always end the TB on an SCTLR write, but Linux
             * arch/arm/mach-pxa/sleep.S expects two instructions following
             * an MMU enable to execute from cache.  Imitate this behaviour.
             */
            sctlr.type |= ARM_CP_SUPPRESS_TB_END;
        }
        define_one_arm_cp_reg(cpu, &sctlr);
    }

    if (cpu_isar_feature(aa64_lor, cpu)) {
        define_arm_cp_regs(cpu, lor_reginfo);
    }
    if (cpu_isar_feature(aa64_pan, cpu)) {
        define_one_arm_cp_reg(cpu, &pan_reginfo);
    }
#ifndef CONFIG_USER_ONLY
    if (cpu_isar_feature(aa64_ats1e1, cpu)) {
        define_arm_cp_regs(cpu, ats1e1_reginfo);
    }
    if (cpu_isar_feature(aa32_ats1e1, cpu)) {
        define_arm_cp_regs(cpu, ats1cp_reginfo);
    }
#endif
    if (cpu_isar_feature(aa64_uao, cpu)) {
        define_one_arm_cp_reg(cpu, &uao_reginfo);
    }

    if (cpu_isar_feature(aa64_dit, cpu)) {
        define_one_arm_cp_reg(cpu, &dit_reginfo);
    }
    if (cpu_isar_feature(aa64_ssbs, cpu)) {
        define_one_arm_cp_reg(cpu, &ssbs_reginfo);
    }

    if (arm_feature(env, ARM_FEATURE_EL2) && cpu_isar_feature(aa64_vh, cpu)) {
        define_arm_cp_regs(cpu, vhe_reginfo);
    }

#ifdef TARGET_AARCH64
    if (cpu_isar_feature(aa64_sve, cpu)) {
        define_one_arm_cp_reg(cpu, &zcr_el1_reginfo);
        if (arm_feature(env, ARM_FEATURE_EL2)) {
            define_one_arm_cp_reg(cpu, &zcr_el2_reginfo);
        } else {
            define_one_arm_cp_reg(cpu, &zcr_no_el2_reginfo);
        }
        if (arm_feature(env, ARM_FEATURE_EL3)) {
            define_one_arm_cp_reg(cpu, &zcr_el3_reginfo);
        }
    }

    if (cpu_isar_feature(aa64_pauth, cpu)) {
        define_arm_cp_regs(cpu, pauth_reginfo);
    }
    if (cpu_isar_feature(aa64_rndr, cpu)) {
        define_arm_cp_regs(cpu, rndr_reginfo);
    }
#ifndef CONFIG_USER_ONLY
    /* Data Cache clean instructions up to PoP */
    if (cpu_isar_feature(aa64_dcpop, cpu)) {
        define_one_arm_cp_reg(cpu, dcpop_reg);

        if (cpu_isar_feature(aa64_dcpodp, cpu)) {
            define_one_arm_cp_reg(cpu, dcpodp_reg);
        }
    }
#endif /*CONFIG_USER_ONLY*/

    /*
     * If full MTE is enabled, add all of the system registers.
     * If only "instructions available at EL0" are enabled,
     * then define only a RAZ/WI version of PSTATE.TCO.
     */
    if (cpu_isar_feature(aa64_mte, cpu)) {
        define_arm_cp_regs(cpu, mte_reginfo);
        define_arm_cp_regs(cpu, mte_el0_cacheop_reginfo);
    } else if (cpu_isar_feature(aa64_mte_insn_reg, cpu)) {
        define_arm_cp_regs(cpu, mte_tco_ro_reginfo);
        define_arm_cp_regs(cpu, mte_el0_cacheop_reginfo);
    }
#endif /* TARGET_AARCH64 */

    if (cpu_isar_feature(any_predinv, cpu)) {
        define_arm_cp_regs(cpu, predinv_reginfo);
    }

    if (cpu_isar_feature(any_ccidx, cpu)) {
        define_arm_cp_regs(cpu, ccsidr2_reginfo);
    }

#ifndef CONFIG_USER_ONLY
    /*
     * Register redirections and aliases must be done last,
     * after the registers from the other extensions have been defined.
     */
    if (arm_feature(env, ARM_FEATURE_EL2) && cpu_isar_feature(aa64_vh, cpu)) {
        define_arm_vh_e2h_redirects_aliases(cpu);
    }
#endif
}

/*
 * Modify ARMCPRegInfo for access from userspace.
 *
 * This is a data driven modification directed by
 * ARMCPRegUserSpaceInfo. All registers become ARM_CP_CONST as
 * user-space cannot alter any values and dynamic values pertaining to
 * execution state are hidden from user space view anyway.
 */
void modify_arm_cp_regs(ARMCPRegInfo *regs, const ARMCPRegUserSpaceInfo *mods)
{
    const ARMCPRegUserSpaceInfo *m;
    ARMCPRegInfo *r;

    for (m = mods; m->name; m++) {
        GPatternSpec *pat = NULL;
        if (m->is_glob) {
            pat = g_pattern_spec_new(m->name);
        }
        for (r = regs; r->type != ARM_CP_SENTINEL; r++) {
            if (pat && g_pattern_match_string(pat, r->name)) {
                r->type = ARM_CP_CONST;
                r->access = PL0U_R;
                r->resetvalue = 0;
                /* continue */
            } else if (strcmp(r->name, m->name) == 0) {
                r->type = ARM_CP_CONST;
                r->access = PL0U_R;
                r->resetvalue &= m->exported_bits;
                r->resetvalue |= m->fixed_bits;
                break;
            }
        }
        if (pat) {
            g_pattern_spec_free(pat);
        }
    }
}
