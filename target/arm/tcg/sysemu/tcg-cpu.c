/*
 * QEMU ARM TCG CPU (sysemu code)
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
#include "qapi/error.h"
#include "qemu/timer.h"
#include "cpu.h"
#include "semihosting/common-semi.h"
#include "qemu/log.h"
#include "tcg/tcg-cpu.h"
#include "internals.h"

/*
 * Do semihosting call and set the appropriate return value. All the
 * permission and validity checks have been done at translate time.
 *
 * We only see semihosting exceptions in TCG only as they are not
 * trapped to the hypervisor in KVM.
 */
void tcg_handle_semihosting(CPUState *cs)
{
    ARMCPU *cpu = ARM_CPU(cs);
    CPUARMState *env = &cpu->env;

    if (is_a64(env)) {
        qemu_log_mask(CPU_LOG_INT,
                      "...handling as semihosting call 0x%" PRIx64 "\n",
                      env->xregs[0]);
        env->xregs[0] = do_common_semihosting(cs);
        env->pc += 4;
    } else {
        qemu_log_mask(CPU_LOG_INT,
                      "...handling as semihosting call 0x%x\n",
                      env->regs[0]);
        env->regs[0] = do_common_semihosting(cs);
        env->regs[15] += env->thumb ? 2 : 4;
    }
}

bool tcg_cpu_realizefn(CPUState *cs, Error **errp)
{
    ARMCPU *cpu = ARM_CPU(cs);
    CPUARMState *env = &cpu->env;

    /*
     * The NVIC and M-profile CPU are two halves of a single piece of
     * hardware; trying to use one without the other is a command line
     * error and will result in segfaults if not caught here.
     */
    if (arm_feature(env, ARM_FEATURE_M)) {
        if (!env->nvic) {
            error_setg(errp, "This board cannot be used with Cortex-M CPUs");
            return false;
        }
    } else {
        if (env->nvic) {
            error_setg(errp, "This board can only be used with Cortex-M CPUs");
            return false;
        }
    }
    return true;
}
