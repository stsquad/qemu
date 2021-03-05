/*
 * QEMU ARM CPU address translation related code
 *
 * Copyright (c) 2012 SUSE LINUX Products GmbH
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see
 * <http://www.gnu.org/licenses/gpl-2.0.html>
 */
#ifndef ARM_CPU_MMU_H
#define ARM_CPU_MMU_H

#include "cpu.h"
#include "internals.h"

/*
 * Parameters of a given virtual address, as extracted from the
 * translation control register (TCR) for a given regime.
 */
typedef struct ARMVAParameters {
    unsigned tsz    : 8;
    unsigned select : 1;
    bool tbi        : 1;
    bool epd        : 1;
    bool hpd        : 1;
    bool using16k   : 1;
    bool using64k   : 1;
} ARMVAParameters;

/* cpu-mmu.c */

int aa64_va_parameter_tbi(uint64_t tcr, ARMMMUIdx mmu_idx);
int aa64_va_parameter_tbid(uint64_t tcr, ARMMMUIdx mmu_idx);
int aa64_va_parameter_tcma(uint64_t tcr, ARMMMUIdx mmu_idx);
ARMVAParameters aa64_va_parameters(CPUARMState *env, uint64_t va,
                                   ARMMMUIdx mmu_idx, bool data);

/* Return the SCTLR value which controls this address translation regime */
static inline uint64_t regime_sctlr(CPUARMState *env, ARMMMUIdx mmu_idx)
{
    return env->cp15.sctlr_el[regime_el(env, mmu_idx)];
}

/* Convert a possible stage1+2 MMU index into the appropriate
 * stage 1 MMU index
 */
static inline ARMMMUIdx stage_1_mmu_idx(ARMMMUIdx mmu_idx)
{
    switch (mmu_idx) {
    case ARMMMUIdx_SE10_0:
        return ARMMMUIdx_Stage1_SE0;
    case ARMMMUIdx_SE10_1:
        return ARMMMUIdx_Stage1_SE1;
    case ARMMMUIdx_SE10_1_PAN:
        return ARMMMUIdx_Stage1_SE1_PAN;
    case ARMMMUIdx_E10_0:
        return ARMMMUIdx_Stage1_E0;
    case ARMMMUIdx_E10_1:
        return ARMMMUIdx_Stage1_E1;
    case ARMMMUIdx_E10_1_PAN:
        return ARMMMUIdx_Stage1_E1_PAN;
    default:
        return mmu_idx;
    }
}

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

/* cpu-mmu-sysemu.c */

void v8m_security_lookup(CPUARMState *env, uint32_t address,
                         MMUAccessType access_type, ARMMMUIdx mmu_idx,
                         V8M_SAttributes *sattrs);

bool pmsav8_mpu_lookup(CPUARMState *env, uint32_t address,
                       MMUAccessType access_type, ARMMMUIdx mmu_idx,
                       hwaddr *phys_ptr, MemTxAttrs *txattrs,
                       int *prot, bool *is_subpage,
                       ARMMMUFaultInfo *fi, uint32_t *mregion);

bool get_phys_addr(CPUARMState *env, target_ulong address,
                   MMUAccessType access_type, ARMMMUIdx mmu_idx,
                   hwaddr *phys_ptr, MemTxAttrs *attrs, int *prot,
                   target_ulong *page_size,
                   ARMMMUFaultInfo *fi, ARMCacheAttrs *cacheattrs)
    __attribute__((nonnull));

hwaddr arm_cpu_get_phys_page_attrs_debug(CPUState *cs, vaddr addr,
                                         MemTxAttrs *attrs);

#endif /* !CONFIG_USER_ONLY */

#endif /* ARM_CPU_MMU_H */
