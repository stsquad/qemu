/*
 * QEMU AArch64 CPU Exceptions Sysemu code
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
#include "qemu/log.h"
#include "cpu.h"
#include "tcg/tcg-sve.h"
#include "internals.h"
#include "sysemu/tcg.h"

#include "cpu-exceptions-aa64.h"

static int aarch64_regnum(CPUARMState *env, int aarch32_reg)
{
    /*
     * Return the register number of the AArch64 view of the AArch32
     * register @aarch32_reg. The CPUARMState CPSR is assumed to still
     * be that of the AArch32 mode the exception came from.
     */
    int mode = env->uncached_cpsr & CPSR_M;

    switch (aarch32_reg) {
    case 0 ... 7:
        return aarch32_reg;
    case 8 ... 12:
        return mode == ARM_CPU_MODE_FIQ ? aarch32_reg + 16 : aarch32_reg;
    case 13:
        switch (mode) {
        case ARM_CPU_MODE_USR:
        case ARM_CPU_MODE_SYS:
            return 13;
        case ARM_CPU_MODE_HYP:
            return 15;
        case ARM_CPU_MODE_IRQ:
            return 17;
        case ARM_CPU_MODE_SVC:
            return 19;
        case ARM_CPU_MODE_ABT:
            return 21;
        case ARM_CPU_MODE_UND:
            return 23;
        case ARM_CPU_MODE_FIQ:
            return 29;
        default:
            g_assert_not_reached();
        }
    case 14:
        switch (mode) {
        case ARM_CPU_MODE_USR:
        case ARM_CPU_MODE_SYS:
        case ARM_CPU_MODE_HYP:
            return 14;
        case ARM_CPU_MODE_IRQ:
            return 16;
        case ARM_CPU_MODE_SVC:
            return 18;
        case ARM_CPU_MODE_ABT:
            return 20;
        case ARM_CPU_MODE_UND:
            return 22;
        case ARM_CPU_MODE_FIQ:
            return 30;
        default:
            g_assert_not_reached();
        }
    case 15:
        return 31;
    default:
        g_assert_not_reached();
    }
}

static uint32_t cpsr_read_for_spsr_elx(CPUARMState *env)
{
    uint32_t ret = cpsr_read(env);

    /* Move DIT to the correct location for SPSR_ELx */
    if (ret & CPSR_DIT) {
        ret &= ~CPSR_DIT;
        ret |= PSTATE_DIT;
    }
    /* Merge PSTATE.SS into SPSR_ELx */
    ret |= env->pstate & PSTATE_SS;

    return ret;
}

/* Handle exception entry to a target EL which is using AArch64 */
void arm_cpu_do_interrupt_aarch64(CPUState *cs)
{
    ARMCPU *cpu = ARM_CPU(cs);
    CPUARMState *env = &cpu->env;
    unsigned int new_el = env->exception.target_el;
    target_ulong addr = env->cp15.vbar_el[new_el];
    unsigned int new_mode = aarch64_pstate_mode(new_el, true);
    unsigned int old_mode;
    unsigned int cur_el = arm_current_el(env);
    int rt;

    if (tcg_enabled()) {
        /*
         * Note that new_el can never be 0.  If cur_el is 0, then
         * el0_a64 is is_a64(), else el0_a64 is ignored.
         */
        aarch64_sve_change_el(env, cur_el, new_el, is_a64(env));
    }

    if (cur_el < new_el) {
        /*
         * Entry vector offset depends on whether the implemented EL
         * immediately lower than the target level is using AArch32 or AArch64
         */
        bool is_aa64;
        uint64_t hcr;

        switch (new_el) {
        case 3:
            is_aa64 = (env->cp15.scr_el3 & SCR_RW) != 0;
            break;
        case 2:
            hcr = arm_hcr_el2_eff(env);
            if ((hcr & (HCR_E2H | HCR_TGE)) != (HCR_E2H | HCR_TGE)) {
                is_aa64 = (hcr & HCR_RW) != 0;
                break;
            }
            /* fall through */
        case 1:
            is_aa64 = is_a64(env);
            break;
        default:
            g_assert_not_reached();
        }

        if (is_aa64) {
            addr += 0x400;
        } else {
            addr += 0x600;
        }
    } else if (pstate_read(env) & PSTATE_SP) {
        addr += 0x200;
    }

    switch (cs->exception_index) {
    case EXCP_PREFETCH_ABORT:
    case EXCP_DATA_ABORT:
        env->cp15.far_el[new_el] = env->exception.vaddress;
        qemu_log_mask(CPU_LOG_INT, "...with FAR 0x%" PRIx64 "\n",
                      env->cp15.far_el[new_el]);
        /* fall through */
    case EXCP_BKPT:
    case EXCP_UDEF:
    case EXCP_SWI:
    case EXCP_HVC:
    case EXCP_HYP_TRAP:
    case EXCP_SMC:
        switch (syn_get_ec(env->exception.syndrome)) {
        case EC_ADVSIMDFPACCESSTRAP:
            /*
             * QEMU internal FP/SIMD syndromes from AArch32 include the
             * TA and coproc fields which are only exposed if the exception
             * is taken to AArch32 Hyp mode. Mask them out to get a valid
             * AArch64 format syndrome.
             */
            env->exception.syndrome &= ~MAKE_64BIT_MASK(0, 20);
            break;
        case EC_CP14RTTRAP:
        case EC_CP15RTTRAP:
        case EC_CP14DTTRAP:
            /*
             * For a trap on AArch32 MRC/MCR/LDC/STC the Rt field is currently
             * the raw register field from the insn; when taking this to
             * AArch64 we must convert it to the AArch64 view of the register
             * number. Notice that we read a 4-bit AArch32 register number and
             * write back a 5-bit AArch64 one.
             */
            rt = extract32(env->exception.syndrome, 5, 4);
            rt = aarch64_regnum(env, rt);
            env->exception.syndrome = deposit32(env->exception.syndrome,
                                                5, 5, rt);
            break;
        case EC_CP15RRTTRAP:
        case EC_CP14RRTTRAP:
            /* Similarly for MRRC/MCRR traps for Rt and Rt2 fields */
            rt = extract32(env->exception.syndrome, 5, 4);
            rt = aarch64_regnum(env, rt);
            env->exception.syndrome = deposit32(env->exception.syndrome,
                                                5, 5, rt);
            rt = extract32(env->exception.syndrome, 10, 4);
            rt = aarch64_regnum(env, rt);
            env->exception.syndrome = deposit32(env->exception.syndrome,
                                                10, 5, rt);
            break;
        }
        env->cp15.esr_el[new_el] = env->exception.syndrome;
        break;
    case EXCP_IRQ:
    case EXCP_VIRQ:
        addr += 0x80;
        break;
    case EXCP_FIQ:
    case EXCP_VFIQ:
        addr += 0x100;
        break;
    default:
        cpu_abort(cs, "Unhandled exception 0x%x\n", cs->exception_index);
    }

    if (is_a64(env)) {
        old_mode = pstate_read(env);
        aarch64_save_sp(env, arm_current_el(env));
        env->elr_el[new_el] = env->pc;
    } else {
        old_mode = cpsr_read_for_spsr_elx(env);
        env->elr_el[new_el] = env->regs[15];

        aarch64_sync_32_to_64(env);

        env->condexec_bits = 0;
    }
    env->banked_spsr[aarch64_banked_spsr_index(new_el)] = old_mode;

    qemu_log_mask(CPU_LOG_INT, "...with ELR 0x%" PRIx64 "\n",
                  env->elr_el[new_el]);

    if (cpu_isar_feature(aa64_pan, cpu)) {
        /* The value of PSTATE.PAN is normally preserved, except when ... */
        new_mode |= old_mode & PSTATE_PAN;
        switch (new_el) {
        case 2:
            /* ... the target is EL2 with HCR_EL2.{E2H,TGE} == '11' ...  */
            if ((arm_hcr_el2_eff(env) & (HCR_E2H | HCR_TGE))
                != (HCR_E2H | HCR_TGE)) {
                break;
            }
            /* fall through */
        case 1:
            /* ... the target is EL1 ... */
            /* ... and SCTLR_ELx.SPAN == 0, then set to 1.  */
            if ((env->cp15.sctlr_el[new_el] & SCTLR_SPAN) == 0) {
                new_mode |= PSTATE_PAN;
            }
            break;
        }
    }
    if (cpu_isar_feature(aa64_mte, cpu)) {
        new_mode |= PSTATE_TCO;
    }

    pstate_write(env, PSTATE_DAIF | new_mode);
    env->aarch64 = 1;
    aarch64_restore_sp(env, new_el);
    if (tcg_enabled()) {
        arm_rebuild_hflags(env);
    }

    env->pc = addr;

    qemu_log_mask(CPU_LOG_INT, "...to EL%d PC 0x%" PRIx64 " PSTATE 0x%x\n",
                  new_el, env->pc, pstate_read(env));
}
