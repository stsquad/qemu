/*
 * ARM generic helpers. (TCG-only internal helper prototypes)
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#ifndef HELPER_TCG_H
#define HELPER_TCG_H

uint64_t cycles_get_count(CPUARMState *env);

void par_write(CPUARMState *env, const ARMCPRegInfo *ri, uint64_t value);

CPAccessResult aa64_cacheop_poc_access(CPUARMState *env,
                                       const ARMCPRegInfo *ri,
                                       bool isread);
extern const ARMCPRegInfo generic_timer_cp_reginfo[];

/* Return true if the translation regime is using LPAE format page tables */
static inline bool regime_using_lpae_format(CPUARMState *env,
                                            ARMMMUIdx mmu_idx)
{
    int el = regime_el(env, mmu_idx);
    if (el == 2 || arm_el_is_aa64(env, el)) {
        return true;
    }
    if (arm_feature(env, ARM_FEATURE_LPAE)
        && (regime_tcr(env, mmu_idx)->raw_tcr & TTBCR_EAE)) {
        return true;
    }
    return false;
}

#ifndef CONFIG_USER_ONLY
int64_t cycles_ns_per(uint64_t cycles);
bool instructions_supported(CPUARMState *env);
uint64_t instructions_get_count(CPUARMState *env);
int64_t instructions_ns_per(uint64_t icount);
CPAccessResult at_s1e2_access(CPUARMState *env, const ARMCPRegInfo *ri,
                              bool isread);
CPAccessResult e2h_access(CPUARMState *env, const ARMCPRegInfo *ri,
                          bool isread);
void ats1h_write(CPUARMState *env, const ARMCPRegInfo *ri, uint64_t value);
uint64_t id_aa64pfr0_read(CPUARMState *env, const ARMCPRegInfo *ri);
extern const ARMCPRegInfo ats1e1_reginfo[];
extern const ARMCPRegInfo ats1cp_reginfo[];
extern const ARMCPRegInfo dcpop_reg[];
extern const ARMCPRegInfo dcpodp_reg[];
extern const ARMCPRegInfo v8_cp_reginfo_softmmu[];
extern const ARMCPRegInfo el2_cp_reginfo_softmmu[];
extern const ARMCPRegInfo vhe_reginfo_softmmu[];
extern const ARMCPRegInfo vapa_cp_reginfo_softmmu[];

void define_arm_vh_e2h_redirects_aliases(ARMCPU *cpu);
#endif /* !CONFIG_USER_ONLY */

#ifdef CONFIG_USER_ONLY
extern const ARMCPRegUserSpaceInfo v8_user_idregs[];
extern const ARMCPRegUserSpaceInfo id_v8_user_midr_cp_reginfo[];
extern const ARMCPRegUserSpaceInfo mpidr_user_cp_reginfo[];
#endif /* CONFIG_USER_ONLY */

#endif /* HELPER_TCG_H */
