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

#include "qemu/osdep.h"
#include "sysemu/tcg.h"
#include "cpu-mmu.h"

int aa64_va_parameter_tbi(uint64_t tcr, ARMMMUIdx mmu_idx)
{
    if (regime_has_2_ranges(mmu_idx)) {
        return extract64(tcr, 37, 2);
    } else if (mmu_idx == ARMMMUIdx_Stage2 || mmu_idx == ARMMMUIdx_Stage2_S) {
        return 0; /* VTCR_EL2 */
    } else {
        /* Replicate the single TBI bit so we always have 2 bits.  */
        return extract32(tcr, 20, 1) * 3;
    }
}

int aa64_va_parameter_tbid(uint64_t tcr, ARMMMUIdx mmu_idx)
{
    if (regime_has_2_ranges(mmu_idx)) {
        return extract64(tcr, 51, 2);
    } else if (mmu_idx == ARMMMUIdx_Stage2 || mmu_idx == ARMMMUIdx_Stage2_S) {
        return 0; /* VTCR_EL2 */
    } else {
        /* Replicate the single TBID bit so we always have 2 bits.  */
        return extract32(tcr, 29, 1) * 3;
    }
}

int aa64_va_parameter_tcma(uint64_t tcr, ARMMMUIdx mmu_idx)
{
    if (regime_has_2_ranges(mmu_idx)) {
        return extract64(tcr, 57, 2);
    } else {
        /* Replicate the single TCMA bit so we always have 2 bits.  */
        return extract32(tcr, 30, 1) * 3;
    }
}

ARMVAParameters aa64_va_parameters(CPUARMState *env, uint64_t va,
                                   ARMMMUIdx mmu_idx, bool data)
{
    uint64_t tcr = regime_tcr(env, mmu_idx)->raw_tcr;
    bool epd, hpd, using16k, using64k;
    int select, tsz, tbi, max_tsz;

    if (!regime_has_2_ranges(mmu_idx)) {
        select = 0;
        tsz = extract32(tcr, 0, 6);
        using64k = extract32(tcr, 14, 1);
        using16k = extract32(tcr, 15, 1);
        if (mmu_idx == ARMMMUIdx_Stage2 || mmu_idx == ARMMMUIdx_Stage2_S) {
            /* VTCR_EL2 */
            hpd = false;
        } else {
            hpd = extract32(tcr, 24, 1);
        }
        epd = false;
    } else {
        /*
         * Bit 55 is always between the two regions, and is canonical for
         * determining if address tagging is enabled.
         */
        select = extract64(va, 55, 1);
        if (!select) {
            tsz = extract32(tcr, 0, 6);
            epd = extract32(tcr, 7, 1);
            using64k = extract32(tcr, 14, 1);
            using16k = extract32(tcr, 15, 1);
            hpd = extract64(tcr, 41, 1);
        } else {
            int tg = extract32(tcr, 30, 2);
            using16k = tg == 1;
            using64k = tg == 3;
            tsz = extract32(tcr, 16, 6);
            epd = extract32(tcr, 23, 1);
            hpd = extract64(tcr, 42, 1);
        }
    }

    if (cpu_isar_feature(aa64_st, env_archcpu(env))) {
        max_tsz = 48 - using64k;
    } else {
        max_tsz = 39;
    }

    tsz = MIN(tsz, max_tsz);
    tsz = MAX(tsz, 16);  /* TODO: ARMv8.2-LVA  */

    /* Present TBI as a composite with TBID.  */
    tbi = aa64_va_parameter_tbi(tcr, mmu_idx);
    if (!data) {
        tbi &= ~aa64_va_parameter_tbid(tcr, mmu_idx);
    }
    tbi = (tbi >> select) & 1;

    return (ARMVAParameters) {
        .tsz = tsz,
        .select = select,
        .tbi = tbi,
        .epd = epd,
        .hpd = hpd,
        .using16k = using16k,
        .using64k = using64k,
    };
}

/* Return the exception level we're running at if this is our mmu_idx */
int arm_mmu_idx_to_el(ARMMMUIdx mmu_idx)
{
    if (mmu_idx & ARM_MMU_IDX_M) {
        return mmu_idx & ARM_MMU_IDX_M_PRIV;
    }

    switch (mmu_idx) {
    case ARMMMUIdx_E10_0:
    case ARMMMUIdx_E20_0:
    case ARMMMUIdx_SE10_0:
    case ARMMMUIdx_SE20_0:
        return 0;
    case ARMMMUIdx_E10_1:
    case ARMMMUIdx_E10_1_PAN:
    case ARMMMUIdx_SE10_1:
    case ARMMMUIdx_SE10_1_PAN:
        return 1;
    case ARMMMUIdx_E2:
    case ARMMMUIdx_E20_2:
    case ARMMMUIdx_E20_2_PAN:
    case ARMMMUIdx_SE2:
    case ARMMMUIdx_SE20_2:
    case ARMMMUIdx_SE20_2_PAN:
        return 2;
    case ARMMMUIdx_SE3:
        return 3;
    default:
        g_assert_not_reached();
    }
}

ARMMMUIdx arm_mmu_idx_el(CPUARMState *env, int el)
{
    ARMMMUIdx idx;
    uint64_t hcr;

    if (tcg_enabled()) {
        if (arm_feature(env, ARM_FEATURE_M)) {
            return arm_v7m_mmu_idx_for_secstate(env, env->v7m.secure);
        }
    }

    /* See ARM pseudo-function ELIsInHost.  */
    switch (el) {
    case 0:
        hcr = arm_hcr_el2_eff(env);
        if ((hcr & (HCR_E2H | HCR_TGE)) == (HCR_E2H | HCR_TGE)) {
            idx = ARMMMUIdx_E20_0;
        } else {
            idx = ARMMMUIdx_E10_0;
        }
        break;
    case 1:
        if (env->pstate & PSTATE_PAN) {
            idx = ARMMMUIdx_E10_1_PAN;
        } else {
            idx = ARMMMUIdx_E10_1;
        }
        break;
    case 2:
        /* Note that TGE does not apply at EL2.  */
        if (arm_hcr_el2_eff(env) & HCR_E2H) {
            if (env->pstate & PSTATE_PAN) {
                idx = ARMMMUIdx_E20_2_PAN;
            } else {
                idx = ARMMMUIdx_E20_2;
            }
        } else {
            idx = ARMMMUIdx_E2;
        }
        break;
    case 3:
        return ARMMMUIdx_SE3;
    default:
        g_assert_not_reached();
    }

    if (arm_is_secure_below_el3(env)) {
        idx &= ~ARM_MMU_IDX_A_NS;
    }

    return idx;
}

ARMMMUIdx arm_mmu_idx(CPUARMState *env)
{
    return arm_mmu_idx_el(env, arm_current_el(env));
}
