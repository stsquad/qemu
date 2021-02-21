/*
 * QEMU ARM CPU
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
#include "qemu/qemu-print.h"
#include "qemu-common.h"
#include "target/arm/idau.h"
#include "qemu/module.h"
#include "qapi/error.h"
#include "qapi/visitor.h"
#include "cpu.h"
#ifdef CONFIG_TCG
#include "hw/core/tcg-cpu-ops.h"
#endif /* CONFIG_TCG */
#include "internals.h"
#include "exec/exec-all.h"
#include "hw/qdev-properties.h"
#if !defined(CONFIG_USER_ONLY)
#include "hw/loader.h"
#include "hw/boards.h"
#endif
#include "sysemu/sysemu.h"
#include "sysemu/tcg.h"
#include "sysemu/hw_accel.h"
#include "kvm_arm.h"
#include "disas/capstone.h"
#include "fpu/softfloat.h"

/*
 * Function used to synchronize QEMU's AArch32 register set with AArch64
 * register set.  This is necessary when switching between AArch32 and AArch64
 * execution state.
 */
void aarch64_sync_64_to_32(CPUARMState *env)
{
    g_assert_not_reached();
}

void switch_mode(CPUARMState *env, int mode)
{
    ARMCPU *cpu = env_archcpu(env);

    if (mode != ARM_CPU_MODE_USR) {
        cpu_abort(CPU(cpu), "Tried to switch out of user mode\n");
    }
}

/*
 * Return the exception level to which FP-disabled exceptions should
 * be taken, or 0 if FP is enabled.
 */
int fp_exception_el(CPUARMState *env, int cur_el)
{
    return 0; /* FP is enabled */
}

int sve_exception_el(CPUARMState *env, int el)
{
    return 0;
}
