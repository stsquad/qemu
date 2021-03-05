/*
 * QEMU ARM KVM stubs for some helper functions
 *
 * Copyright 2021 SUSE LLC
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#include "qemu/osdep.h"
#include "cpu.h"

/* return the effective value of HCR_EL2. For KVM, always 0. */
uint64_t arm_hcr_el2_eff(CPUARMState *env)
{
    return 0;
}

int fp_exception_el(CPUARMState *env, int cur_el)
{
    return 0;
}

void write_v7m_exception(CPUARMState *env, uint32_t new_exc)
{
    g_assert_not_reached();
}

/* XXX this is used all over in hw/arm, needs Philippe's work to remove */
void arm_rebuild_hflags(CPUARMState *env)
{
    g_assert_not_reached();
}
