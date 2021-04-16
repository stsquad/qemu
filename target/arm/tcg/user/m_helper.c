/*
 * ARM v7m generic helpers.
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "cpu.h"
#include "exec/helper-proto.h"

#include "tcg/m_helper.h"

void HELPER(v7m_msr)(CPUARMState *env, uint32_t maskreg, uint32_t val)
{
    uint32_t mask = extract32(maskreg, 8, 4);
    uint32_t reg = extract32(maskreg, 0, 8);

    switch (reg) {
    case 0 ... 7: /* xPSR sub-fields */
        v7m_msr_xpsr(env, mask, reg, val);
        break;
    case 20: /* CONTROL */
        /* There are no sub-fields that are actually writable from EL0. */
        break;
    default:
        /* Unprivileged writes to other registers are ignored */
        break;
    }
}

uint32_t HELPER(v7m_mrs)(CPUARMState *env, uint32_t reg)
{
    switch (reg) {
    case 0 ... 7: /* xPSR sub-fields */
        return v7m_mrs_xpsr(env, reg, 0);
    case 20: /* CONTROL */
        return v7m_mrs_control(env, 0);
    default:
        /* Unprivileged reads others as zero.  */
        return 0;
    }
}

void HELPER(v7m_bxns)(CPUARMState *env, uint32_t dest)
{
    /* translate.c should never generate calls here in user-only mode */
    g_assert_not_reached();
}

void HELPER(v7m_blxns)(CPUARMState *env, uint32_t dest)
{
    /* translate.c should never generate calls here in user-only mode */
    g_assert_not_reached();
}

void HELPER(v7m_preserve_fp_state)(CPUARMState *env)
{
    /* translate.c should never generate calls here in user-only mode */
    g_assert_not_reached();
}

void HELPER(v7m_vlstm)(CPUARMState *env, uint32_t fptr)
{
    /* translate.c should never generate calls here in user-only mode */
    g_assert_not_reached();
}

void HELPER(v7m_vlldm)(CPUARMState *env, uint32_t fptr)
{
    /* translate.c should never generate calls here in user-only mode */
    g_assert_not_reached();
}

uint32_t HELPER(v7m_tt)(CPUARMState *env, uint32_t addr, uint32_t op)
{
    /*
     * The TT instructions can be used by unprivileged code, but in
     * user-only emulation we don't have the MPU.
     * Luckily since we know we are NonSecure unprivileged (and that in
     * turn means that the A flag wasn't specified), all the bits in the
     * register must be zero:
     *  IREGION: 0 because IRVALID is 0
     *  IRVALID: 0 because NS
     *  S: 0 because NS
     *  NSRW: 0 because NS
     *  NSR: 0 because NS
     *  RW: 0 because unpriv and A flag not set
     *  R: 0 because unpriv and A flag not set
     *  SRVALID: 0 because NS
     *  MRVALID: 0 because unpriv and A flag not set
     *  SREGION: 0 becaus SRVALID is 0
     *  MREGION: 0 because MRVALID is 0
     */
    return 0;
}
