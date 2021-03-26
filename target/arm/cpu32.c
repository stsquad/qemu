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
#include "qemu/module.h"
#include "cpu.h"
#include "cpregs.h"
#if !defined(CONFIG_USER_ONLY)
#include "hw/boards.h"
#endif
#include "cpu-mmu.h"
#include "cpu32.h"

#if !defined(CONFIG_USER_ONLY) || !defined(TARGET_AARCH64)

#ifndef CONFIG_USER_ONLY
static uint64_t a15_l2ctlr_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    MachineState *ms = MACHINE(qdev_get_machine());

    /*
     * Linux wants the number of processors from here.
     * Might as well set the interrupt-controller bit too.
     */
    return ((ms->smp.cpus - 1) << 24) | (1 << 23);
}
#endif

const ARMCPRegInfo cortexa15_cp_reginfo[] = {
#ifndef CONFIG_USER_ONLY
    { .name = "L2CTLR", .cp = 15, .crn = 9, .crm = 0, .opc1 = 1, .opc2 = 2,
      .access = PL1_RW, .resetvalue = 0, .readfn = a15_l2ctlr_read,
      .writefn = arm_cp_write_ignore, },
#endif
    { .name = "L2ECTLR", .cp = 15, .crn = 9, .crm = 0, .opc1 = 1, .opc2 = 3,
      .access = PL1_RW, .type = ARM_CP_CONST, .resetvalue = 0 },
    REGINFO_SENTINEL
};

void cortex_a15_initfn(Object *obj)
{
    ARMCPU *cpu = ARM_CPU(obj);

    cpu->dtb_compatible = "arm,cortex-a15";
    set_feature(&cpu->env, ARM_FEATURE_V7VE);
    set_feature(&cpu->env, ARM_FEATURE_NEON);
    set_feature(&cpu->env, ARM_FEATURE_THUMB2EE);
    set_feature(&cpu->env, ARM_FEATURE_GENERIC_TIMER);
    set_feature(&cpu->env, ARM_FEATURE_DUMMY_C15_REGS);
    set_feature(&cpu->env, ARM_FEATURE_CBAR_RO);
    set_feature(&cpu->env, ARM_FEATURE_EL2);
    set_feature(&cpu->env, ARM_FEATURE_EL3);
    set_feature(&cpu->env, ARM_FEATURE_PMU);
    cpu->kvm_target = QEMU_KVM_ARM_TARGET_CORTEX_A15;
    cpu->midr = 0x412fc0f1;
    cpu->reset_fpsid = 0x410430f0;
    cpu->isar.mvfr0 = 0x10110222;
    cpu->isar.mvfr1 = 0x11111111;
    cpu->ctr = 0x8444c004;
    cpu->reset_sctlr = 0x00c50078;
    cpu->isar.id_pfr0 = 0x00001131;
    cpu->isar.id_pfr1 = 0x00011011;
    cpu->isar.id_dfr0 = 0x02010555;
    cpu->id_afr0 = 0x00000000;
    cpu->isar.id_mmfr0 = 0x10201105;
    cpu->isar.id_mmfr1 = 0x20000000;
    cpu->isar.id_mmfr2 = 0x01240000;
    cpu->isar.id_mmfr3 = 0x02102211;
    cpu->isar.id_isar0 = 0x02101110;
    cpu->isar.id_isar1 = 0x13112111;
    cpu->isar.id_isar2 = 0x21232041;
    cpu->isar.id_isar3 = 0x11112131;
    cpu->isar.id_isar4 = 0x10011142;
    cpu->isar.dbgdidr = 0x3515f021;
    cpu->clidr = 0x0a200023;
    cpu->ccsidr[0] = 0x701fe00a; /* 32K L1 dcache */
    cpu->ccsidr[1] = 0x201fe00a; /* 32K L1 icache */
    cpu->ccsidr[2] = 0x711fe07a; /* 4096K L2 unified cache */
    define_arm_cp_regs(cpu, cortexa15_cp_reginfo);
}

#endif /* !CONFIG_USER_ONLY || !TARGET_AARCH64 */

/* we can move this to tcg/ after the cleanup of ARM boards configurations */
static const ARMCPUInfo arm32_cpus[] = {
#if !defined(CONFIG_USER_ONLY) || !defined(TARGET_AARCH64)
    { .name = "cortex-a15",  .initfn = cortex_a15_initfn },
#endif /* !CONFIG_USER_ONLY || !TARGET_AARCH64 */
};

static gchar *arm32_gdb_arch_name(CPUState *cs)
{
    ARMCPU *cpu = ARM_CPU(cs);
    CPUARMState *env = &cpu->env;

    if (arm_feature(env, ARM_FEATURE_IWMMXT)) {
        return g_strdup("iwmmxt");
    }
    return g_strdup("arm");
}

void aarch32_cpu_dump_state(CPUState *cs, FILE *f, int flags)
{
    ARMCPU *cpu = ARM_CPU(cs);
    CPUARMState *env = &cpu->env;
    int i;

    assert(!is_a64(env));

    for (i = 0; i < 16; i++) {
        qemu_fprintf(f, "R%02d=%08x", i, env->regs[i]);
        if ((i % 4) == 3) {
            qemu_fprintf(f, "\n");
        } else {
            qemu_fprintf(f, " ");
        }
    }

    if (arm_feature(env, ARM_FEATURE_M)) {
        uint32_t xpsr = xpsr_read(env);
        const char *mode;
        const char *ns_status = "";

        if (arm_feature(env, ARM_FEATURE_M_SECURITY)) {
            ns_status = env->v7m.secure ? "S " : "NS ";
        }

        if (xpsr & XPSR_EXCP) {
            mode = "handler";
        } else {
            if (env->v7m.control[env->v7m.secure] & R_V7M_CONTROL_NPRIV_MASK) {
                mode = "unpriv-thread";
            } else {
                mode = "priv-thread";
            }
        }

        qemu_fprintf(f, "XPSR=%08x %c%c%c%c %c %s%s\n",
                     xpsr,
                     xpsr & XPSR_N ? 'N' : '-',
                     xpsr & XPSR_Z ? 'Z' : '-',
                     xpsr & XPSR_C ? 'C' : '-',
                     xpsr & XPSR_V ? 'V' : '-',
                     xpsr & XPSR_T ? 'T' : 'A',
                     ns_status,
                     mode);
    } else {
        uint32_t psr = cpsr_read(env);
        const char *ns_status = "";

        if (arm_feature(env, ARM_FEATURE_EL3) &&
            (psr & CPSR_M) != ARM_CPU_MODE_MON) {
            ns_status = env->cp15.scr_el3 & SCR_NS ? "NS " : "S ";
        }

        qemu_fprintf(f, "PSR=%08x %c%c%c%c %c %s%s%d\n",
                     psr,
                     psr & CPSR_N ? 'N' : '-',
                     psr & CPSR_Z ? 'Z' : '-',
                     psr & CPSR_C ? 'C' : '-',
                     psr & CPSR_V ? 'V' : '-',
                     psr & CPSR_T ? 'T' : 'A',
                     ns_status,
                     aarch32_mode_name(psr), (psr & 0x10) ? 32 : 26);
    }

    if (flags & CPU_DUMP_FPU) {
        int numvfpregs = 0;
        if (cpu_isar_feature(aa32_simd_r32, cpu)) {
            numvfpregs = 32;
        } else if (cpu_isar_feature(aa32_vfp_simd, cpu)) {
            numvfpregs = 16;
        }
        for (i = 0; i < numvfpregs; i++) {
            uint64_t v = *aa32_vfp_dreg(env, i);
            qemu_fprintf(f, "s%02d=%08x s%02d=%08x d%02d=%016" PRIx64 "\n",
                         i * 2, (uint32_t)v,
                         i * 2 + 1, (uint32_t)(v >> 32),
                         i, v);
        }
        qemu_fprintf(f, "FPSCR: %08x\n", vfp_get_fpscr(env));
    }
}

void arm32_cpu_class_init(ObjectClass *oc, void *data)
{
    CPUClass *cc = CPU_CLASS(oc);

    cc->gdb_read_register = arm32_cpu_gdb_read_register;
    cc->gdb_write_register = arm32_cpu_gdb_write_register;
    cc->gdb_num_core_regs = 26;
    cc->gdb_core_xml_file = "arm-core.xml";
    cc->gdb_arch_name = arm32_gdb_arch_name;
    cc->dump_state = aarch32_cpu_dump_state;
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
