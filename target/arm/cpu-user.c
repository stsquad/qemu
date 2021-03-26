/*
 * ARM CPU user-mode only code
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "qemu/log.h"
#include "qom/object.h"
#include "qapi/qapi-commands-machine-target.h"
#include "qapi/error.h"
#include "cpu.h"
#include "cpu-exceptions-aa64.h"
#include "internals.h"

void switch_mode(CPUARMState *env, int mode)
{
    ARMCPU *cpu = env_archcpu(env);

    if (mode != ARM_CPU_MODE_USR) {
        cpu_abort(CPU(cpu), "Tried to switch out of user mode\n");
    }
}

void aarch64_sync_64_to_32(CPUARMState *env)
{
    g_assert_not_reached();
}

uint32_t arm_phys_excp_target_el(CPUState *cs, uint32_t excp_idx,
                                 uint32_t cur_el, bool secure)
{
    return 1;
}

int sve_exception_el(CPUARMState *env, int el)
{
    return 0;
}
