/*
 * ARM implementation of KVM hooks, 64 bit specific code
 *
 * Copyright Mian-M. Hamayun 2013, Virtual Open Systems
 * Copyright Alex Bennée 2014, Linaro
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/ptrace.h>
#include <asm/ptrace.h>

#include <linux/elf.h>
#include <linux/kvm.h>

#include "qemu-common.h"
#include "qemu/timer.h"
#include "qemu/host-utils.h"
#include "qemu/error-report.h"
#include "exec/gdbstub.h"
#include "sysemu/sysemu.h"
#include "sysemu/kvm.h"
#include "kvm_arm.h"
#include "cpu.h"
#include "internals.h"
#include "hw/arm/arm.h"

/* Max and current break/watch point counts */
int max_hw_bp, max_hw_wp;
int cur_hw_bp, cur_hw_wp;
struct kvm_guest_debug_arch guest_debug_registers;

/**
 * kvm_arm_init_debug()
 * @cs: CPUState
 *
 * kvm_check_extension returns 0 if we have no debug registers or the
 * number we have.
 *
 */
static void kvm_arm_init_debug(CPUState *cs) {
    max_hw_wp = kvm_check_extension(cs->kvm_state, KVM_CAP_GUEST_DEBUG_HW_WPS);
    max_hw_bp = kvm_check_extension(cs->kvm_state, KVM_CAP_GUEST_DEBUG_HW_BPS);
    return;
}

/**
 * insert_hw_breakpoint()
 * @addr: address of breakpoint
 *
 * See ARM ARM D2.9.1 for details but here we are only going to create
 * simple un-linked breakpoints (i.e. we don't chain breakpoints
 * together to match address and context or vmid). The hardware is
 * capable of fancier matching but that will require exposing that
 * fanciness to GDB's interface
 *
 * D7.3.2 DBGBCR<n>_EL1, Debug Breakpoint Control Registers
 *
 *  31  24 23  20 19   16 15 14  13  12   9 8   5 4    3 2   1  0
 * +------+------+-------+-----+----+------+-----+------+-----+---+
 * | RES0 |  BT  |  LBN  | SSC | HMC| RES0 | BAS | RES0 | PMC | E |
 * +------+------+-------+-----+----+------+-----+------+-----+---+
 *
 * BT: Breakpoint type (0 = unlinked address match)
 * LBN: Linked BP number (0 = unused)
 * SSC/HMC/PAC: Security, Higher and Priv access control (Table D-12)
 * BAS: Byte Address Select (RES1 for AArch64)
 * E: Enable bit
 */
static int insert_hw_breakpoint(target_ulong addr)
{
    uint32_t bcr = 0x1; /* E=1, enable */
    if (cur_hw_bp >= max_hw_bp) {
        return -ENOBUFS;
    }
    bcr = deposit32(bcr, 1, 2, 0x3);   /* PMC = 11 */
    bcr = deposit32(bcr, 5, 4, 0xf);   /* BAS = RES1 */
    guest_debug_registers.dbg_bcr[cur_hw_bp] = bcr;
    guest_debug_registers.dbg_bvr[cur_hw_bp] = addr;
    cur_hw_bp++;
    return 0;
}

/**
 * delete_hw_breakpoint()
 * @pc: address of breakpoint
 *
 * Delete a breakpoint and shuffle any above down
 */

static int delete_hw_breakpoint(target_ulong pc)
{
    int i;
    for (i=0; i<cur_hw_bp; i++) {
      if (guest_debug_registers.dbg_bvr[i] == pc) {
          while (i < cur_hw_bp) {
              if (i == max_hw_bp) {
                  guest_debug_registers.dbg_bvr[i] = 0;
                  guest_debug_registers.dbg_bcr[i] = 0;
              } else {
                  guest_debug_registers.dbg_bvr[i] =
                      guest_debug_registers.dbg_bvr[i+1];
                  guest_debug_registers.dbg_bcr[i] =
                      guest_debug_registers.dbg_bcr[i+1];
              }
              i++;
          }
          cur_hw_bp--;
          return 0;
      }
    }
    return -ENOENT;
}

/**
 * insert_hw_watchpoint()
 * @addr: address of watch point
 * @len: size of area
 * @type: type of watch point
 *
 * See ARM ARM D2.10. As with the breakpoints we can do some advanced
 * stuff if we want to. The watch points can be linked with the break
 * points above to make them context aware. However for simplicity
 * currently we only deal with simple read/write watch points.
 *
 * D7.3.11 DBGWCR<n>_EL1, Debug Watchpoint Control Registers
 *
 *  31  29 28   24 23  21  20  19 16 15 14  13   12  5 4   3 2   1  0
 * +------+-------+------+----+-----+-----+-----+-----+-----+-----+---+
 * | RES0 |  MASK | RES0 | WT | LBN | SSC | HMC | BAS | LSC | PAC | E |
 * +------+-------+------+----+-----+-----+-----+-----+-----+-----+---+
 *
 * MASK: num bits addr mask (0=none,01/10=res,11=3 bits (8 bytes))
 * WT: 0 - unlinked, 1 - linked (not currently used)
 * LBN: Linked BP number (not currently used)
 * SSC/HMC/PAC: Security, Higher and Priv access control (Table D-12)
 * BAS: Byte Address Select
 * LSC: Load/Store control (01: load, 10: store, 11: both)
 * E: Enable
 *
 * The bottom 2 bits of the value register are masked. Therefor to
 * break on an unaligned byte you need to set mask=0, bas=byte in question.
 */

static int insert_hw_watchpoint(target_ulong addr,
                                target_ulong len, int type)
{
    uint32_t dbgwcr = 1; /* E=1, enable */
    uint64_t dbgwvr = addr & (~0x7ULL);

    if (cur_hw_wp >= max_hw_wp) {
        return -ENOBUFS;
    }
    switch (type) {
    case GDB_WATCHPOINT_READ:
        dbgwcr = deposit32(dbgwcr, 3, 2, 1);
        break;
    case GDB_WATCHPOINT_WRITE:
        dbgwcr = deposit32(dbgwcr, 3, 2, 2);
        break;
    case GDB_WATCHPOINT_ACCESS:
        dbgwcr = deposit32(dbgwcr, 3, 2, 3);
        break;
    default:
        g_assert_not_reached();
        break;
    }
    if (len < 8) {
        /* we align the address and set the bits in BAS */
        int off = addr & 0x7;
        dbgwcr = deposit32(dbgwcr, 5+off, 8-off, (1 << len)-1);
    } else {
        /* For ranges above 8 bytes we need to be a power of 2 */
        if (ctpop64(len)==1) {
            dbgwcr = deposit32(dbgwcr, 24, 4, ctz64(len));
        } else {
            return -ENOBUFS;
        }
    }

    guest_debug_registers.dbg_wcr[cur_hw_wp] = dbgwcr;
    guest_debug_registers.dbg_wvr[cur_hw_wp] = dbgwvr;
    cur_hw_wp++;
    return 0;
}

/**
 * delete_hw_watchpoint()
 * @addr: address of breakpoint
 *
 * Delete a breakpoint and shuffle any above down
 */

static int delete_hw_watchpoint(target_ulong addr,
                                target_ulong len, int type)
{
    int i;
    for (i=0; i<cur_hw_wp; i++) {
      if (guest_debug_registers.dbg_wvr[i] == addr) {
          /* TODO and check type/range */
          while (i < cur_hw_wp) {
              if (i == max_hw_wp) {
                  guest_debug_registers.dbg_wvr[i] = 0;
                  guest_debug_registers.dbg_wcr[i] = 0;
              } else {
                  guest_debug_registers.dbg_wvr[i] =
                      guest_debug_registers.dbg_wvr[i+1];
                  guest_debug_registers.dbg_wcr[i] =
                      guest_debug_registers.dbg_wcr[i+1];
              }
              i++;
          }
          cur_hw_wp--;
          return 0;
      }
    }
    return -ENOENT;
}


int kvm_arch_insert_hw_breakpoint(target_ulong addr,
                                  target_ulong len, int type)
{
    switch (type) {
    case GDB_BREAKPOINT_HW:
        return insert_hw_breakpoint(addr);
        break;
    case GDB_WATCHPOINT_READ:
    case GDB_WATCHPOINT_WRITE:
    case GDB_WATCHPOINT_ACCESS:
        return insert_hw_watchpoint(addr, len, type);
    default:
        return -ENOSYS;
    }
}

int kvm_arch_remove_hw_breakpoint(target_ulong addr,
                                  target_ulong len, int type)
{
    switch (type) {
    case GDB_BREAKPOINT_HW:
        return delete_hw_breakpoint(addr);
        break;
    case GDB_WATCHPOINT_READ:
    case GDB_WATCHPOINT_WRITE:
    case GDB_WATCHPOINT_ACCESS:
        return delete_hw_watchpoint(addr, len, type);
    default:
        return -ENOSYS;
    }
}


void kvm_arch_remove_all_hw_breakpoints(void)
{
    memset((void *)&guest_debug_registers, 0, sizeof(guest_debug_registers));
    cur_hw_bp = 0;
    cur_hw_wp = 0;
}

void kvm_copy_hw_breakpoint_data(struct kvm_guest_debug_arch *ptr)
{
    /* Compile time assert? */
    g_assert(sizeof(struct kvm_guest_debug_arch) == sizeof(guest_debug_registers));
    memcpy(ptr, &guest_debug_registers, sizeof(guest_debug_registers));
}

bool kvm_hw_breakpoints_active(CPUState *cs)
{
    return ( (cur_hw_bp > 0) || (cur_hw_wp >0) ) ? TRUE:FALSE;
}

bool kvm_arm_find_hw_breakpoint(CPUState *cpu, target_ulong pc)
{
  if (kvm_hw_breakpoints_active(cpu)) {
    int i;
    for (i=0; i<cur_hw_bp; i++) {
      if (guest_debug_registers.dbg_bvr[i] == pc) {
        return true;
      }
    }
  }
  return false;
}

bool kvm_arm_find_hw_watchpoint(CPUState *cpu, target_ulong addr)
{
  if (kvm_hw_breakpoints_active(cpu)) {
    int i;
    for (i=0; i<cur_hw_wp; i++) {
      /* TODO */
    }
  }
  return false;
}


static inline void set_feature(uint64_t *features, int feature)
{
    *features |= 1ULL << feature;
}

bool kvm_arm_get_host_cpu_features(ARMHostCPUClass *ahcc)
{
    /* Identify the feature bits corresponding to the host CPU, and
     * fill out the ARMHostCPUClass fields accordingly. To do this
     * we have to create a scratch VM, create a single CPU inside it,
     * and then query that CPU for the relevant ID registers.
     * For AArch64 we currently don't care about ID registers at
     * all; we just want to know the CPU type.
     */
    int fdarray[3];
    uint64_t features = 0;
    /* Old kernels may not know about the PREFERRED_TARGET ioctl: however
     * we know these will only support creating one kind of guest CPU,
     * which is its preferred CPU type. Fortunately these old kernels
     * support only a very limited number of CPUs.
     */
    static const uint32_t cpus_to_try[] = {
        KVM_ARM_TARGET_AEM_V8,
        KVM_ARM_TARGET_FOUNDATION_V8,
        KVM_ARM_TARGET_CORTEX_A57,
        QEMU_KVM_ARM_TARGET_NONE
    };
    struct kvm_vcpu_init init;

    if (!kvm_arm_create_scratch_host_vcpu(cpus_to_try, fdarray, &init)) {
        return false;
    }

    ahcc->target = init.target;
    ahcc->dtb_compatible = "arm,arm-v8";

    kvm_arm_destroy_scratch_host_vcpu(fdarray);

   /* We can assume any KVM supporting CPU is at least a v8
     * with VFPv4+Neon; this in turn implies most of the other
     * feature bits.
     */
    set_feature(&features, ARM_FEATURE_V8);
    set_feature(&features, ARM_FEATURE_VFP4);
    set_feature(&features, ARM_FEATURE_NEON);
    set_feature(&features, ARM_FEATURE_AARCH64);

    ahcc->features = features;

    return true;
}

int kvm_arch_init_vcpu(CPUState *cs)
{
    int ret;
    ARMCPU *cpu = ARM_CPU(cs);

    if (cpu->kvm_target == QEMU_KVM_ARM_TARGET_NONE ||
        !arm_feature(&cpu->env, ARM_FEATURE_AARCH64)) {
        fprintf(stderr, "KVM is not supported for this guest CPU type\n");
        return -EINVAL;
    }

    /* Determine init features for this CPU */
    memset(cpu->kvm_init_features, 0, sizeof(cpu->kvm_init_features));
    if (cpu->start_powered_off) {
        cpu->kvm_init_features[0] |= 1 << KVM_ARM_VCPU_POWER_OFF;
    }
    if (kvm_check_extension(cs->kvm_state, KVM_CAP_ARM_PSCI_0_2)) {
        cpu->psci_version = 2;
        cpu->kvm_init_features[0] |= 1 << KVM_ARM_VCPU_PSCI_0_2;
    }

    /* Do KVM_ARM_VCPU_INIT ioctl */
    ret = kvm_arm_vcpu_init(cs);
    if (ret) {
        return ret;
    }

    kvm_arm_init_debug(cs);
    /* TODO : support for save/restore/reset of system regs via tuple list */

    return 0;
}

#define AARCH64_CORE_REG(x)   (KVM_REG_ARM64 | KVM_REG_SIZE_U64 | \
                 KVM_REG_ARM_CORE | KVM_REG_ARM_CORE_REG(x))

int kvm_arch_put_registers(CPUState *cs, int level)
{
    struct kvm_one_reg reg;
    uint64_t val;
    int i;
    int ret;

    ARMCPU *cpu = ARM_CPU(cs);
    CPUARMState *env = &cpu->env;

    for (i = 0; i < 31; i++) {
        reg.id = AARCH64_CORE_REG(regs.regs[i]);
        reg.addr = (uintptr_t) &env->xregs[i];
        ret = kvm_vcpu_ioctl(cs, KVM_SET_ONE_REG, &reg);
        if (ret) {
            return ret;
        }
    }

    /* KVM puts SP_EL0 in regs.sp and SP_EL1 in regs.sp_el1. On the
     * QEMU side we keep the current SP in xregs[31] as well.
     */
    aarch64_save_sp(env, 1);

    reg.id = AARCH64_CORE_REG(regs.sp);
    reg.addr = (uintptr_t) &env->sp_el[0];
    ret = kvm_vcpu_ioctl(cs, KVM_SET_ONE_REG, &reg);
    if (ret) {
        return ret;
    }

    reg.id = AARCH64_CORE_REG(sp_el1);
    reg.addr = (uintptr_t) &env->sp_el[1];
    ret = kvm_vcpu_ioctl(cs, KVM_SET_ONE_REG, &reg);
    if (ret) {
        return ret;
    }

    /* Note that KVM thinks pstate is 64 bit but we use a uint32_t */
    val = pstate_read(env);
    reg.id = AARCH64_CORE_REG(regs.pstate);
    reg.addr = (uintptr_t) &val;
    ret = kvm_vcpu_ioctl(cs, KVM_SET_ONE_REG, &reg);
    if (ret) {
        return ret;
    }

    reg.id = AARCH64_CORE_REG(regs.pc);
    reg.addr = (uintptr_t) &env->pc;
    ret = kvm_vcpu_ioctl(cs, KVM_SET_ONE_REG, &reg);
    if (ret) {
        return ret;
    }

    reg.id = AARCH64_CORE_REG(elr_el1);
    reg.addr = (uintptr_t) &env->elr_el[1];
    ret = kvm_vcpu_ioctl(cs, KVM_SET_ONE_REG, &reg);
    if (ret) {
        return ret;
    }

    for (i = 0; i < KVM_NR_SPSR; i++) {
        reg.id = AARCH64_CORE_REG(spsr[i]);
        reg.addr = (uintptr_t) &env->banked_spsr[i - 1];
        ret = kvm_vcpu_ioctl(cs, KVM_SET_ONE_REG, &reg);
        if (ret) {
            return ret;
        }
    }

    /* TODO:
     * FP state
     * system registers
     */
    return ret;
}

int kvm_arch_get_registers(CPUState *cs)
{
    struct kvm_one_reg reg;
    uint64_t val;
    int i;
    int ret;

    ARMCPU *cpu = ARM_CPU(cs);
    CPUARMState *env = &cpu->env;

    for (i = 0; i < 31; i++) {
        reg.id = AARCH64_CORE_REG(regs.regs[i]);
        reg.addr = (uintptr_t) &env->xregs[i];
        ret = kvm_vcpu_ioctl(cs, KVM_GET_ONE_REG, &reg);
        if (ret) {
            return ret;
        }
    }

    reg.id = AARCH64_CORE_REG(regs.sp);
    reg.addr = (uintptr_t) &env->sp_el[0];
    ret = kvm_vcpu_ioctl(cs, KVM_GET_ONE_REG, &reg);
    if (ret) {
        return ret;
    }

    reg.id = AARCH64_CORE_REG(sp_el1);
    reg.addr = (uintptr_t) &env->sp_el[1];
    ret = kvm_vcpu_ioctl(cs, KVM_GET_ONE_REG, &reg);
    if (ret) {
        return ret;
    }

    reg.id = AARCH64_CORE_REG(regs.pstate);
    reg.addr = (uintptr_t) &val;
    ret = kvm_vcpu_ioctl(cs, KVM_GET_ONE_REG, &reg);
    if (ret) {
        return ret;
    }
    pstate_write(env, val);

    /* KVM puts SP_EL0 in regs.sp and SP_EL1 in regs.sp_el1. On the
     * QEMU side we keep the current SP in xregs[31] as well.
     */
    aarch64_restore_sp(env, 1);

    reg.id = AARCH64_CORE_REG(regs.pc);
    reg.addr = (uintptr_t) &env->pc;
    ret = kvm_vcpu_ioctl(cs, KVM_GET_ONE_REG, &reg);
    if (ret) {
        return ret;
    }

    reg.id = AARCH64_CORE_REG(elr_el1);
    reg.addr = (uintptr_t) &env->elr_el[1];
    ret = kvm_vcpu_ioctl(cs, KVM_GET_ONE_REG, &reg);
    if (ret) {
        return ret;
    }

    for (i = 0; i < KVM_NR_SPSR; i++) {
        reg.id = AARCH64_CORE_REG(spsr[i]);
        reg.addr = (uintptr_t) &env->banked_spsr[i - 1];
        ret = kvm_vcpu_ioctl(cs, KVM_GET_ONE_REG, &reg);
        if (ret) {
            return ret;
        }
    }

    /* TODO: other registers */
    return ret;
}

void kvm_arm_reset_vcpu(ARMCPU *cpu)
{
    /* Re-init VCPU so that all registers are set to
     * their respective reset values.
     */
    kvm_arm_vcpu_init(CPU(cpu));
}
