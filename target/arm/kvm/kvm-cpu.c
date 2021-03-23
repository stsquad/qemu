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
#include "qemu-common.h"
#include "cpu.h"
#include "hw/core/accel-cpu.h"
#include "qapi/error.h"

#include "kvm/kvm_arm.h"
#include "internals.h"

static void arm_cpu_kvm_set_irq(void *opaque, int irq, int level)
{
    ARMCPU *cpu = opaque;
    CPUARMState *env = &cpu->env;
    CPUState *cs = CPU(cpu);
    uint32_t linestate_bit;
    int irq_id;

    switch (irq) {
    case ARM_CPU_IRQ:
        irq_id = KVM_ARM_IRQ_CPU_IRQ;
        linestate_bit = CPU_INTERRUPT_HARD;
        break;
    case ARM_CPU_FIQ:
        irq_id = KVM_ARM_IRQ_CPU_FIQ;
        linestate_bit = CPU_INTERRUPT_FIQ;
        break;
    default:
        g_assert_not_reached();
    }

    if (level) {
        env->irq_line_state |= linestate_bit;
    } else {
        env->irq_line_state &= ~linestate_bit;
    }
    kvm_arm_set_irq(cs->cpu_index, KVM_ARM_IRQ_TYPE_CPU, irq_id, !!level);
}

static void kvm_cpu_instance_init(CPUState *cs)
{
    /*
     * VIRQ and VFIQ are unused with KVM but we add them to maintain
     * the same interface as non-KVM CPUs.
     */
    qdev_init_gpio_in(DEVICE(cs), arm_cpu_kvm_set_irq, 4);
    kvm_arm_add_vcpu_properties(OBJECT(cs));
}

static bool kvm_cpu_realizefn(CPUState *cs, Error **errp)
{
    /*
     * If we needed to query the host kernel for the CPU features
     * then it's possible that might have failed in the initfn, but
     * this is the first point where we can report it.
     */
    ARMCPU *cpu = ARM_CPU(cs);

    if (cpu->host_cpu_probe_failed) {
        error_setg(errp, "Failed to retrieve host CPU features");
        return false;
    }
    return true;
}

static void host_cpu_instance_init(Object *obj)
{
    ARMCPU *cpu = ARM_CPU(obj);

    kvm_arm_set_cpu_features_from_host(cpu);
    cpu_sve_add_props(obj);
    arm_cpu_post_init(obj);
}

static void kvm_cpu_reset(CPUState *cs)
{
    kvm_arm_reset_vcpu(ARM_CPU(cs));
}

static const TypeInfo host_cpu_type_info = {
    .name = ARM_CPU_TYPE_NAME("host"),
    .parent = TYPE_AARCH64_CPU,
    .instance_init = host_cpu_instance_init,
};

static void kvm_cpu_accel_class_init(ObjectClass *oc, void *data)
{
    AccelCPUClass *acc = ACCEL_CPU_CLASS(oc);

    acc->cpu_realizefn = kvm_cpu_realizefn;
    acc->cpu_instance_init = kvm_cpu_instance_init;
    acc->cpu_reset = kvm_cpu_reset;
}

static const TypeInfo kvm_cpu_accel_type_info = {
    .name = ACCEL_CPU_NAME("kvm"),
    .parent = TYPE_ACCEL_CPU,
    .class_init = kvm_cpu_accel_class_init,
    .abstract = true,
};

static void kvm_cpu_accel_register_types(void)
{
    type_register_static(&host_cpu_type_info);
    type_register_static(&kvm_cpu_accel_type_info);
}

type_init(kvm_cpu_accel_register_types);
