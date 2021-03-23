/*
 * ARM v7m generic helpers.
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "cpu.h"
#include "internals.h"
#include "tcg/m_helper.h"

void v7m_msr_xpsr(CPUARMState *env, uint32_t mask, uint32_t reg, uint32_t val)
{
    /* Only APSR is actually writable */
    if (!(reg & 4)) {
        uint32_t apsrmask = 0;

        if (mask & 8) {
            apsrmask |= XPSR_NZCV | XPSR_Q;
        }
        if ((mask & 4) && arm_feature(env, ARM_FEATURE_THUMB_DSP)) {
            apsrmask |= XPSR_GE;
        }
        xpsr_write(env, val, apsrmask);
    }
}

uint32_t v7m_mrs_xpsr(CPUARMState *env, uint32_t reg, unsigned el)
{
    uint32_t mask = 0;

    if ((reg & 1) && el) {
        mask |= XPSR_EXCP; /* IPSR (unpriv. reads as zero) */
    }
    if (!(reg & 4)) {
        mask |= XPSR_NZCV | XPSR_Q; /* APSR */
        if (arm_feature(env, ARM_FEATURE_THUMB_DSP)) {
            mask |= XPSR_GE;
        }
    }
    /* EPSR reads as zero */
    return xpsr_read(env) & mask;
}

uint32_t v7m_mrs_control(CPUARMState *env, uint32_t secure)
{
    uint32_t value = env->v7m.control[secure];

    if (!secure) {
        /* SFPA is RAZ/WI from NS; FPCA is stored in the M_REG_S bank */
        value |= env->v7m.control[M_REG_S] & R_V7M_CONTROL_FPCA_MASK;
    }
    return value;
}

ARMMMUIdx arm_v7m_mmu_idx_all(CPUARMState *env,
                              bool secstate, bool priv, bool negpri)
{
    ARMMMUIdx mmu_idx = ARM_MMU_IDX_M;

    if (priv) {
        mmu_idx |= ARM_MMU_IDX_M_PRIV;
    }

    if (negpri) {
        mmu_idx |= ARM_MMU_IDX_M_NEGPRI;
    }

    if (secstate) {
        mmu_idx |= ARM_MMU_IDX_M_S;
    }

    return mmu_idx;
}

ARMMMUIdx arm_v7m_mmu_idx_for_secstate_and_priv(CPUARMState *env,
                                                bool secstate, bool priv)
{
    bool negpri = armv7m_nvic_neg_prio_requested(env->nvic, secstate);

    return arm_v7m_mmu_idx_all(env, secstate, priv, negpri);
}

/* Return the MMU index for a v7M CPU in the specified security state */
ARMMMUIdx arm_v7m_mmu_idx_for_secstate(CPUARMState *env, bool secstate)
{
    bool priv = arm_v7m_is_handler_mode(env) ||
        !(env->v7m.control[secstate] & 1);

    return arm_v7m_mmu_idx_for_secstate_and_priv(env, secstate, priv);
}
