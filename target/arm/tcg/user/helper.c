/*
 * ARM generic helpers.
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "qemu/units.h"
#include "target/arm/idau.h"
#include "trace.h"
#include "cpu.h"
#include "internals.h"
#include "exec/gdbstub.h"
#include "exec/helper-proto.h"
#include "qemu/host-utils.h"
#include "qemu/main-loop.h"
#include "qemu/bitops.h"
#include "qemu/crc32c.h"
#include "qemu/qemu-print.h"
#include "exec/exec-all.h"
#include <zlib.h> /* For crc32 */
#include "hw/irq.h"
#include "hw/semihosting/semihost.h"
#include "sysemu/cpus.h"
#include "sysemu/cpu-timers.h"
#include "sysemu/kvm.h"
#include "sysemu/tcg.h"
#include "qemu/range.h"
#include "qapi/qapi-commands-machine-target.h"
#include "qapi/error.h"
#include "qemu/guest-random.h"
#ifdef CONFIG_TCG
#include "arm_ldst.h"
#include "exec/cpu_ldst.h"
#include "hw/semihosting/common-semi.h"
#include "tcg/helper-tcg.h"
#endif

/*
 * On softmmu, it returns the underlying cycle count for the PMU cycle counters.
 * If we're in usermode, simply return 0.
 * XXX claudio: comment does not seem to match code (cpu_get_host_ticks)
 */
uint64_t cycles_get_count(CPUARMState *env)
{
    return cpu_get_host_ticks();
}

/*
 * In user-mode most of the generic timer registers are inaccessible
 * however modern kernels (4.12+) allow access to cntvct_el0
 */

static uint64_t gt_virt_cnt_read(CPUARMState *env, const ARMCPRegInfo *ri)
{
    ARMCPU *cpu = env_archcpu(env);

    /* Currently we have no support for QEMUTimer in linux-user so we
     * can't call gt_get_countervalue(env), instead we directly
     * call the lower level functions.
     */
    return cpu_get_clock() / gt_cntfrq_period_ns(cpu);
}

const ARMCPRegInfo generic_timer_cp_reginfo[] = {
    { .name = "CNTFRQ_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 0, .opc2 = 0,
      .type = ARM_CP_CONST, .access = PL0_R /* no PL1_RW in linux-user */,
      .fieldoffset = offsetof(CPUARMState, cp15.c14_cntfrq),
      .resetvalue = NANOSECONDS_PER_SECOND / GTIMER_SCALE,
    },
    { .name = "CNTVCT_EL0", .state = ARM_CP_STATE_AA64,
      .opc0 = 3, .opc1 = 3, .crn = 14, .crm = 0, .opc2 = 2,
      .access = PL0_R, .type = ARM_CP_NO_RAW | ARM_CP_IO,
      .readfn = gt_virt_cnt_read,
    },
    REGINFO_SENTINEL
};

const ARMCPRegUserSpaceInfo v8_user_idregs[] = {
    { .name = "ID_AA64PFR0_EL1",
      .exported_bits = 0x000f000f00ff0000,
      .fixed_bits    = 0x0000000000000011 },
    { .name = "ID_AA64PFR1_EL1",
      .exported_bits = 0x00000000000000f0 },
    { .name = "ID_AA64PFR*_EL1_RESERVED",
      .is_glob = true                     },
    { .name = "ID_AA64ZFR0_EL1"           },
    { .name = "ID_AA64MMFR0_EL1",
      .fixed_bits    = 0x00000000ff000000 },
    { .name = "ID_AA64MMFR1_EL1"          },
    { .name = "ID_AA64MMFR*_EL1_RESERVED",
      .is_glob = true                     },
    { .name = "ID_AA64DFR0_EL1",
      .fixed_bits    = 0x0000000000000006 },
    { .name = "ID_AA64DFR1_EL1"           },
    { .name = "ID_AA64DFR*_EL1_RESERVED",
      .is_glob = true                     },
    { .name = "ID_AA64AFR*",
      .is_glob = true                     },
    { .name = "ID_AA64ISAR0_EL1",
      .exported_bits = 0x00fffffff0fffff0 },
    { .name = "ID_AA64ISAR1_EL1",
      .exported_bits = 0x000000f0ffffffff },
    { .name = "ID_AA64ISAR*_EL1_RESERVED",
      .is_glob = true                     },
    REGUSERINFO_SENTINEL
};

const ARMCPRegUserSpaceInfo id_v8_user_midr_cp_reginfo[] = {
    { .name = "MIDR_EL1",
      .exported_bits = 0x00000000ffffffff },
    { .name = "REVIDR_EL1"                },
    REGUSERINFO_SENTINEL
};

const ARMCPRegUserSpaceInfo mpidr_user_cp_reginfo[] = {
    { .name = "MPIDR_EL1",
      .fixed_bits = 0x0000000080000000 },
    REGUSERINFO_SENTINEL
};

int sve_exception_el(CPUARMState *env, int el)
{
    return 0;
}

void switch_mode(CPUARMState *env, int mode)
{
    ARMCPU *cpu = env_archcpu(env);

    if (mode != ARM_CPU_MODE_USR) {
        cpu_abort(CPU(cpu), "Tried to switch out of user mode\n");
    }
}

uint32_t arm_phys_excp_target_el(CPUState *cs, uint32_t excp_idx,
                                 uint32_t cur_el, bool secure)
{
    return 1;
}

/* Return the exception level to which FP-disabled exceptions should
 * be taken, or 0 if FP is enabled.
 */
int fp_exception_el(CPUARMState *env, int cur_el)
{
    return 0;
}
