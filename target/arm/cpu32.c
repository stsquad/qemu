/*
 * QEMU ARM CPU models (32bit)
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
#include "cpregs.h"
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
#include "cpu-mmu.h"
#include "cpu32.h"

/* we can move this to tcg/ after the cleanup of ARM boards configurations */
static const ARMCPUInfo arm32_cpus[] = {
};

static gchar *arm_gdb_arch_name(CPUState *cs)
{
    ARMCPU *cpu = ARM_CPU(cs);
    CPUARMState *env = &cpu->env;

    if (arm_feature(env, ARM_FEATURE_IWMMXT)) {
        return g_strdup("iwmmxt");
    }
    return g_strdup("arm");
}

void arm32_cpu_class_init(ObjectClass *oc, void *data)
{
    CPUClass *cc = CPU_CLASS(oc);

    cc->gdb_read_register = arm_cpu_gdb_read_register;
    cc->gdb_write_register = arm_cpu_gdb_write_register;
    cc->gdb_num_core_regs = 26;
    cc->gdb_core_xml_file = "arm-core.xml";
    cc->gdb_arch_name = arm_gdb_arch_name;
    cc->dump_state = arm_cpu_dump_state;
}

static void arm32_cpu_instance_init(Object *obj)
{
    ARMCPUClass *acc = ARM_CPU_GET_CLASS(obj);

    acc->info->initfn(obj);
    arm_cpu_post_init(obj);
}

static void arm32_cpu_register_class_init(ObjectClass *oc, void *data)
{
    ARMCPUClass *acc = ARM_CPU_CLASS(oc);

    acc->info = data;
}

void arm32_cpu_register(const ARMCPUInfo *info)
{
    TypeInfo type_info = {
        .parent = TYPE_ARM_CPU,
        .instance_size = sizeof(ARMCPU),
        .instance_align = __alignof__(ARMCPU),
        .instance_init = arm32_cpu_instance_init,
        .class_size = sizeof(ARMCPUClass),
        .class_init = info->class_init ?: arm32_cpu_register_class_init,
        .class_data = (void *)info,
    };

    type_info.name = g_strdup_printf("%s-" TYPE_ARM_CPU, info->name);
    type_register(&type_info);
    g_free((void *)type_info.name);
}

static void arm32_cpu_register_types(void)
{
    const size_t cpu_count = ARRAY_SIZE(arm32_cpus);

    if (cpu_count) {
        size_t i;

        for (i = 0; i < cpu_count; ++i) {
            arm32_cpu_register(&arm32_cpus[i]);
        }
    }
}

type_init(arm32_cpu_register_types)
