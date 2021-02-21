/*
 * QEMU ARM TCG stubs, for builds without TCG
 *
 * Copyright 2021 SUSE LLC
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#include "qemu/osdep.h"
#include "cpu.h"

void arm_pmu_timer_cb(void *opaque)
{
}

void write_v7m_exception(CPUARMState *env, uint32_t new_exc)
{
    g_assert_not_reached();
}
