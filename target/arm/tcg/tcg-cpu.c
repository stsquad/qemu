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
#include "cpu.h"
#include "qapi/error.h"
#include "tcg-cpu.h"
#include "cpregs.h"
#include "internals.h"
#include "exec/exec-all.h"

#ifdef TARGET_AARCH64
#include "tcg/cpu-pauth.h"
#endif

void arm_cpu_synchronize_from_tb(CPUState *cs,
                                 const TranslationBlock *tb)
{
    ARMCPU *cpu = ARM_CPU(cs);
    CPUARMState *env = &cpu->env;

    /*
     * It's OK to look at env for the current mode here, because it's
     * never possible for an AArch64 TB to chain to an AArch32 TB.
     */
    if (is_a64(env)) {
        env->pc = tb->pc;
    } else {
        env->regs[15] = tb->pc;
    }
}

static inline bool arm_excp_unmasked(CPUState *cs, unsigned int excp_idx,
                                     unsigned int target_el,
                                     unsigned int cur_el, bool secure,
                                     uint64_t hcr_el2)
{
    CPUARMState *env = cs->env_ptr;
    bool pstate_unmasked;
    bool unmasked = false;

    /*
     * Don't take exceptions if they target a lower EL.
     * This check should catch any exceptions that would not be taken
     * but left pending.
     */
    if (cur_el > target_el) {
        return false;
    }

    switch (excp_idx) {
    case EXCP_FIQ:
        pstate_unmasked = !(env->daif & PSTATE_F);
        break;

    case EXCP_IRQ:
        pstate_unmasked = !(env->daif & PSTATE_I);
        break;

    case EXCP_VFIQ:
        if (!(hcr_el2 & HCR_FMO) || (hcr_el2 & HCR_TGE)) {
            /* VFIQs are only taken when hypervized.  */
            return false;
        }
        return !(env->daif & PSTATE_F);
    case EXCP_VIRQ:
        if (!(hcr_el2 & HCR_IMO) || (hcr_el2 & HCR_TGE)) {
            /* VIRQs are only taken when hypervized.  */
            return false;
        }
        return !(env->daif & PSTATE_I);
    default:
        g_assert_not_reached();
    }

    /*
     * Use the target EL, current execution state and SCR/HCR settings to
     * determine whether the corresponding CPSR bit is used to mask the
     * interrupt.
     */
    if ((target_el > cur_el) && (target_el != 1)) {
        /* Exceptions targeting a higher EL may not be maskable */
        if (arm_feature(env, ARM_FEATURE_AARCH64)) {
            /*
             * 64-bit masking rules are simple: exceptions to EL3
             * can't be masked, and exceptions to EL2 can only be
             * masked from Secure state. The HCR and SCR settings
             * don't affect the masking logic, only the interrupt routing.
             */
            if (target_el == 3 || !secure || (env->cp15.scr_el3 & SCR_EEL2)) {
                unmasked = true;
            }
        } else {
            /*
             * The old 32-bit-only environment has a more complicated
             * masking setup. HCR and SCR bits not only affect interrupt
             * routing but also change the behaviour of masking.
             */
            bool hcr, scr;

            switch (excp_idx) {
            case EXCP_FIQ:
                /*
                 * If FIQs are routed to EL3 or EL2 then there are cases where
                 * we override the CPSR.F in determining if the exception is
                 * masked or not. If neither of these are set then we fall back
                 * to the CPSR.F setting otherwise we further assess the state
                 * below.
                 */
                hcr = hcr_el2 & HCR_FMO;
                scr = (env->cp15.scr_el3 & SCR_FIQ);

                /*
                 * When EL3 is 32-bit, the SCR.FW bit controls whether the
                 * CPSR.F bit masks FIQ interrupts when taken in non-secure
                 * state. If SCR.FW is set then FIQs can be masked by CPSR.F
                 * when non-secure but only when FIQs are only routed to EL3.
                 */
                scr = scr && !((env->cp15.scr_el3 & SCR_FW) && !hcr);
                break;
            case EXCP_IRQ:
                /*
                 * When EL3 execution state is 32-bit, if HCR.IMO is set then
                 * we may override the CPSR.I masking when in non-secure state.
                 * The SCR.IRQ setting has already been taken into consideration
                 * when setting the target EL, so it does not have a further
                 * affect here.
                 */
                hcr = hcr_el2 & HCR_IMO;
                scr = false;
                break;
            default:
                g_assert_not_reached();
            }

            if ((scr || hcr) && !secure) {
                unmasked = true;
            }
        }
    }

    /*
     * The PSTATE bits only mask the interrupt if we have not overriden the
     * ability above.
     */
    return unmasked || pstate_unmasked;
}

static bool arm_cpu_exec_interrupt(CPUState *cs, int interrupt_request)
{
    CPUClass *cc = CPU_GET_CLASS(cs);
    CPUARMState *env = cs->env_ptr;
    uint32_t cur_el = arm_current_el(env);
    bool secure = arm_is_secure(env);
    uint64_t hcr_el2 = arm_hcr_el2_eff(env);
    uint32_t target_el;
    uint32_t excp_idx;

    /* The prioritization of interrupts is IMPLEMENTATION DEFINED. */

    if (interrupt_request & CPU_INTERRUPT_FIQ) {
        excp_idx = EXCP_FIQ;
        target_el = arm_phys_excp_target_el(cs, excp_idx, cur_el, secure);
        if (arm_excp_unmasked(cs, excp_idx, target_el,
                              cur_el, secure, hcr_el2)) {
            goto found;
        }
    }
    if (interrupt_request & CPU_INTERRUPT_HARD) {
        excp_idx = EXCP_IRQ;
        target_el = arm_phys_excp_target_el(cs, excp_idx, cur_el, secure);
        if (arm_excp_unmasked(cs, excp_idx, target_el,
                              cur_el, secure, hcr_el2)) {
            goto found;
        }
    }
    if (interrupt_request & CPU_INTERRUPT_VIRQ) {
        excp_idx = EXCP_VIRQ;
        target_el = 1;
        if (arm_excp_unmasked(cs, excp_idx, target_el,
                              cur_el, secure, hcr_el2)) {
            goto found;
        }
    }
    if (interrupt_request & CPU_INTERRUPT_VFIQ) {
        excp_idx = EXCP_VFIQ;
        target_el = 1;
        if (arm_excp_unmasked(cs, excp_idx, target_el,
                              cur_el, secure, hcr_el2)) {
            goto found;
        }
    }
    return false;

 found:
    cs->exception_index = excp_idx;
    env->exception.target_el = target_el;
    cc->tcg_ops->do_interrupt(cs);
    return true;
}

static struct TCGCPUOps arm_tcg_ops = {
    .initialize = arm_translate_init,
    .synchronize_from_tb = arm_cpu_synchronize_from_tb,
    .cpu_exec_interrupt = arm_cpu_exec_interrupt,
    .tlb_fill = arm_cpu_tlb_fill,
    .debug_excp_handler = arm_debug_excp_handler,

#if !defined(CONFIG_USER_ONLY)
    .do_interrupt = arm_cpu_do_interrupt,
    .do_transaction_failed = arm_cpu_do_transaction_failed,
    .do_unaligned_access = arm_cpu_do_unaligned_access,
    .adjust_watchpoint_address = arm_adjust_watchpoint_address,
    .debug_check_watchpoint = arm_debug_check_watchpoint,
#endif /* !CONFIG_USER_ONLY */
};

#ifdef TARGET_AARCH64
static void tcg_cpu_max_instance_init(CPUState *cs)
{
    uint64_t t;
    uint32_t u;
    Object *obj = OBJECT(cs);
    ARMCPU *cpu = ARM_CPU(cs);

    /*
     * Reset MIDR so the guest doesn't mistake our 'max' CPU type for a real
     * one and try to apply errata workarounds or use impdef features we
     * don't provide.
     * An IMPLEMENTER field of 0 means "reserved for software use";
     * ARCHITECTURE must be 0xf indicating "v7 or later, check ID registers
     * to see which features are present";
     * the VARIANT, PARTNUM and REVISION fields are all implementation
     * defined and we choose to define PARTNUM just in case guest
     * code needs to distinguish this QEMU CPU from other software
     * implementations, though this shouldn't be needed.
     */
    t = FIELD_DP64(0, MIDR_EL1, IMPLEMENTER, 0);
    t = FIELD_DP64(t, MIDR_EL1, ARCHITECTURE, 0xf);
    t = FIELD_DP64(t, MIDR_EL1, PARTNUM, 'Q');
    t = FIELD_DP64(t, MIDR_EL1, VARIANT, 0);
    t = FIELD_DP64(t, MIDR_EL1, REVISION, 0);
    cpu->midr = t;

    t = cpu->isar.id_aa64isar0;
    t = FIELD_DP64(t, ID_AA64ISAR0, AES, 2); /* AES + PMULL */
    t = FIELD_DP64(t, ID_AA64ISAR0, SHA1, 1);
    t = FIELD_DP64(t, ID_AA64ISAR0, SHA2, 2); /* SHA512 */
    t = FIELD_DP64(t, ID_AA64ISAR0, CRC32, 1);
    t = FIELD_DP64(t, ID_AA64ISAR0, ATOMIC, 2);
    t = FIELD_DP64(t, ID_AA64ISAR0, RDM, 1);
    t = FIELD_DP64(t, ID_AA64ISAR0, SHA3, 1);
    t = FIELD_DP64(t, ID_AA64ISAR0, SM3, 1);
    t = FIELD_DP64(t, ID_AA64ISAR0, SM4, 1);
    t = FIELD_DP64(t, ID_AA64ISAR0, DP, 1);
    t = FIELD_DP64(t, ID_AA64ISAR0, FHM, 1);
    t = FIELD_DP64(t, ID_AA64ISAR0, TS, 2); /* v8.5-CondM */
    t = FIELD_DP64(t, ID_AA64ISAR0, RNDR, 1);
    cpu->isar.id_aa64isar0 = t;

    t = cpu->isar.id_aa64isar1;
    t = FIELD_DP64(t, ID_AA64ISAR1, DPB, 2);
    t = FIELD_DP64(t, ID_AA64ISAR1, JSCVT, 1);
    t = FIELD_DP64(t, ID_AA64ISAR1, FCMA, 1);
    t = FIELD_DP64(t, ID_AA64ISAR1, SB, 1);
    t = FIELD_DP64(t, ID_AA64ISAR1, SPECRES, 1);
    t = FIELD_DP64(t, ID_AA64ISAR1, FRINTTS, 1);
    t = FIELD_DP64(t, ID_AA64ISAR1, LRCPC, 2); /* ARMv8.4-RCPC */
    cpu->isar.id_aa64isar1 = t;

    t = cpu->isar.id_aa64pfr0;
    t = FIELD_DP64(t, ID_AA64PFR0, SVE, 1);
    t = FIELD_DP64(t, ID_AA64PFR0, FP, 1);
    t = FIELD_DP64(t, ID_AA64PFR0, ADVSIMD, 1);
    t = FIELD_DP64(t, ID_AA64PFR0, SEL2, 1);
    t = FIELD_DP64(t, ID_AA64PFR0, DIT, 1);
    cpu->isar.id_aa64pfr0 = t;

    t = cpu->isar.id_aa64pfr1;
    t = FIELD_DP64(t, ID_AA64PFR1, BT, 1);
    t = FIELD_DP64(t, ID_AA64PFR1, SSBS, 2);
    /*
     * Begin with full support for MTE. This will be downgraded to MTE=0
     * during realize if the board provides no tag memory, much like
     * we do for EL2 with the virtualization=on property.
     */
    t = FIELD_DP64(t, ID_AA64PFR1, MTE, 2);
    cpu->isar.id_aa64pfr1 = t;

    t = cpu->isar.id_aa64mmfr0;
    t = FIELD_DP64(t, ID_AA64MMFR0, PARANGE, 5); /* PARange: 48 bits */
    cpu->isar.id_aa64mmfr0 = t;

    t = cpu->isar.id_aa64mmfr1;
    t = FIELD_DP64(t, ID_AA64MMFR1, HPDS, 1); /* HPD */
    t = FIELD_DP64(t, ID_AA64MMFR1, LO, 1);
    t = FIELD_DP64(t, ID_AA64MMFR1, VH, 1);
    t = FIELD_DP64(t, ID_AA64MMFR1, PAN, 2); /* ATS1E1 */
    t = FIELD_DP64(t, ID_AA64MMFR1, VMIDBITS, 2); /* VMID16 */
    t = FIELD_DP64(t, ID_AA64MMFR1, XNX, 1); /* TTS2UXN */
    cpu->isar.id_aa64mmfr1 = t;

    t = cpu->isar.id_aa64mmfr2;
    t = FIELD_DP64(t, ID_AA64MMFR2, UAO, 1);
    t = FIELD_DP64(t, ID_AA64MMFR2, CNP, 1); /* TTCNP */
    t = FIELD_DP64(t, ID_AA64MMFR2, ST, 1); /* TTST */
    cpu->isar.id_aa64mmfr2 = t;

    /* Replicate the same data to the 32-bit id registers.  */
    u = cpu->isar.id_isar5;
    u = FIELD_DP32(u, ID_ISAR5, AES, 2); /* AES + PMULL */
    u = FIELD_DP32(u, ID_ISAR5, SHA1, 1);
    u = FIELD_DP32(u, ID_ISAR5, SHA2, 1);
    u = FIELD_DP32(u, ID_ISAR5, CRC32, 1);
    u = FIELD_DP32(u, ID_ISAR5, RDM, 1);
    u = FIELD_DP32(u, ID_ISAR5, VCMA, 1);
    cpu->isar.id_isar5 = u;

    u = cpu->isar.id_isar6;
    u = FIELD_DP32(u, ID_ISAR6, JSCVT, 1);
    u = FIELD_DP32(u, ID_ISAR6, DP, 1);
    u = FIELD_DP32(u, ID_ISAR6, FHM, 1);
    u = FIELD_DP32(u, ID_ISAR6, SB, 1);
    u = FIELD_DP32(u, ID_ISAR6, SPECRES, 1);
    cpu->isar.id_isar6 = u;

    u = cpu->isar.id_pfr0;
    u = FIELD_DP32(u, ID_PFR0, DIT, 1);
    cpu->isar.id_pfr0 = u;

    u = cpu->isar.id_pfr2;
    u = FIELD_DP32(u, ID_PFR2, SSBS, 1);
    cpu->isar.id_pfr2 = u;

    u = cpu->isar.id_mmfr3;
    u = FIELD_DP32(u, ID_MMFR3, PAN, 2); /* ATS1E1 */
    cpu->isar.id_mmfr3 = u;

    u = cpu->isar.id_mmfr4;
    u = FIELD_DP32(u, ID_MMFR4, HPDS, 1); /* AA32HPD */
    u = FIELD_DP32(u, ID_MMFR4, AC2, 1); /* ACTLR2, HACTLR2 */
    u = FIELD_DP32(u, ID_MMFR4, CNP, 1); /* TTCNP */
    u = FIELD_DP32(u, ID_MMFR4, XNX, 1); /* TTS2UXN */
    cpu->isar.id_mmfr4 = u;

    t = cpu->isar.id_aa64dfr0;
    t = FIELD_DP64(t, ID_AA64DFR0, PMUVER, 5); /* v8.4-PMU */
    cpu->isar.id_aa64dfr0 = t;

    u = cpu->isar.id_dfr0;
    u = FIELD_DP32(u, ID_DFR0, PERFMON, 5); /* v8.4-PMU */
    cpu->isar.id_dfr0 = u;

    u = cpu->isar.mvfr1;
    u = FIELD_DP32(u, MVFR1, FPHP, 3);      /* v8.2-FP16 */
    u = FIELD_DP32(u, MVFR1, SIMDHP, 2);    /* v8.2-FP16 */
    cpu->isar.mvfr1 = u;

#ifdef CONFIG_USER_ONLY
    /*
     * For usermode -cpu max we can use a larger and more efficient DCZ
     * blocksize since we don't have to follow what the hardware does.
     */
    cpu->ctr = 0x80038003; /* 32 byte I and D cacheline size, VIPT icache */
    cpu->dcz_blocksize = 7; /*  512 bytes */
#endif
    cpu_pauth_add_props(obj);
}

#else /* !TARGET_AARCH64 */
static void tcg_cpu_max_instance_init(CPUState *cs)
{
    ARMCPU *cpu = ARM_CPU(cs);

    /* old-style VFP short-vector support */
    cpu->isar.mvfr0 = FIELD_DP32(cpu->isar.mvfr0, MVFR0, FPSHVEC, 1);

#ifdef CONFIG_USER_ONLY
    /*
     * We don't set these in system emulation mode for the moment,
     * since we don't correctly set (all of) the ID registers to
     * advertise them.
     */
    set_feature(&cpu->env, ARM_FEATURE_V8);
    {
        uint32_t t;

        t = cpu->isar.id_isar5;
        t = FIELD_DP32(t, ID_ISAR5, AES, 2);
        t = FIELD_DP32(t, ID_ISAR5, SHA1, 1);
        t = FIELD_DP32(t, ID_ISAR5, SHA2, 1);
        t = FIELD_DP32(t, ID_ISAR5, CRC32, 1);
        t = FIELD_DP32(t, ID_ISAR5, RDM, 1);
        t = FIELD_DP32(t, ID_ISAR5, VCMA, 1);
        cpu->isar.id_isar5 = t;

        t = cpu->isar.id_isar6;
        t = FIELD_DP32(t, ID_ISAR6, JSCVT, 1);
        t = FIELD_DP32(t, ID_ISAR6, DP, 1);
        t = FIELD_DP32(t, ID_ISAR6, FHM, 1);
        t = FIELD_DP32(t, ID_ISAR6, SB, 1);
        t = FIELD_DP32(t, ID_ISAR6, SPECRES, 1);
        cpu->isar.id_isar6 = t;

        t = cpu->isar.mvfr1;
        t = FIELD_DP32(t, MVFR1, FPHP, 3);     /* v8.2-FP16 */
        t = FIELD_DP32(t, MVFR1, SIMDHP, 2);   /* v8.2-FP16 */
        cpu->isar.mvfr1 = t;

        t = cpu->isar.mvfr2;
        t = FIELD_DP32(t, MVFR2, SIMDMISC, 3); /* SIMD MaxNum */
        t = FIELD_DP32(t, MVFR2, FPMISC, 4);   /* FP MaxNum */
        cpu->isar.mvfr2 = t;

        t = cpu->isar.id_mmfr3;
        t = FIELD_DP32(t, ID_MMFR3, PAN, 2); /* ATS1E1 */
        cpu->isar.id_mmfr3 = t;

        t = cpu->isar.id_mmfr4;
        t = FIELD_DP32(t, ID_MMFR4, HPDS, 1); /* AA32HPD */
        t = FIELD_DP32(t, ID_MMFR4, AC2, 1); /* ACTLR2, HACTLR2 */
        t = FIELD_DP32(t, ID_MMFR4, CNP, 1); /* TTCNP */
        t = FIELD_DP32(t, ID_MMFR4, XNX, 1); /* TTS2UXN */
        cpu->isar.id_mmfr4 = t;

        t = cpu->isar.id_pfr0;
        t = FIELD_DP32(t, ID_PFR0, DIT, 1);
        cpu->isar.id_pfr0 = t;

        t = cpu->isar.id_pfr2;
        t = FIELD_DP32(t, ID_PFR2, SSBS, 1);
        cpu->isar.id_pfr2 = t;
    }
#endif /* CONFIG_USER_ONLY */
}
#endif /* TARGET_AARCH64 */

static void tcg_cpu_instance_init(CPUState *cs)
{
    ARMCPU *cpu = ARM_CPU(cs);

    cpu->psci_version = 2; /* TCG implements PSCI 0.2 */
    if (cpu->max_features) {
        tcg_cpu_max_instance_init(cs);
    }
}

static void tcg_cpu_reset(CPUState *cs)
{
    ARMCPU *cpu = ARM_CPU(cs);
    CPUARMState *env = &cpu->env;

    hw_breakpoint_update_all(cpu);
    hw_watchpoint_update_all(cpu);
    arm_rebuild_hflags(env);
}

void tcg_arm_init_accel_cpu(AccelCPUClass *accel_cpu, CPUClass *cc)
{
    cc->tcg_ops = &arm_tcg_ops;
}

static void tcg_cpu_accel_class_init(ObjectClass *oc, void *data)
{
    AccelCPUClass *acc = ACCEL_CPU_CLASS(oc);

#ifndef CONFIG_USER_ONLY
    acc->cpu_realizefn = tcg_cpu_realizefn;
#endif /* CONFIG_USER_ONLY */

    acc->cpu_instance_init = tcg_cpu_instance_init;
    acc->cpu_reset = tcg_cpu_reset;
}
static const TypeInfo tcg_cpu_accel_type_info = {
    .name = ACCEL_CPU_NAME("tcg"),

    .parent = TYPE_ACCEL_CPU,
    .class_init = tcg_cpu_accel_class_init,
    .abstract = true,
};
static void tcg_cpu_accel_register_types(void)
{
    type_register_static(&tcg_cpu_accel_type_info);
}
type_init(tcg_cpu_accel_register_types);
