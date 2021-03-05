/*
 * ARM CP registers
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#ifndef CPREGS_H
#define CPREGS_H

typedef enum CPAccessResult {
    /* Access is permitted */
    CP_ACCESS_OK = 0,
    /*
     * Access fails due to a configurable trap or enable which would
     * result in a categorized exception syndrome giving information about
     * the failing instruction (ie syndrome category 0x3, 0x4, 0x5, 0x6,
     * 0xc or 0x18). The exception is taken to the usual target EL (EL1 or
     * PL1 if in EL0, otherwise to the current EL).
     */
    CP_ACCESS_TRAP = 1,
    /*
     * Access fails and results in an exception syndrome 0x0 ("uncategorized").
     * Note that this is not a catch-all case -- the set of cases which may
     * result in this failure is specifically defined by the architecture.
     */
    CP_ACCESS_TRAP_UNCATEGORIZED = 2,
    /* As CP_ACCESS_TRAP, but for traps directly to EL2 or EL3 */
    CP_ACCESS_TRAP_EL2 = 3,
    CP_ACCESS_TRAP_EL3 = 4,
    /* As CP_ACCESS_UNCATEGORIZED, but for traps directly to EL2 or EL3 */
    CP_ACCESS_TRAP_UNCATEGORIZED_EL2 = 5,
    CP_ACCESS_TRAP_UNCATEGORIZED_EL3 = 6,
    /*
     * Access fails and results in an exception syndrome for an FP access,
     * trapped directly to EL2 or EL3
     */
    CP_ACCESS_TRAP_FP_EL2 = 7,
    CP_ACCESS_TRAP_FP_EL3 = 8,
} CPAccessResult;

/*
 * Access functions for coprocessor registers. These cannot fail and
 * may not raise exceptions.
 */
typedef uint64_t CPReadFn(CPUARMState *env, const ARMCPRegInfo *opaque);
typedef void CPWriteFn(CPUARMState *env, const ARMCPRegInfo *opaque,
                       uint64_t value);
/* Access permission check functions for coprocessor registers. */
typedef CPAccessResult CPAccessFn(CPUARMState *env,
                                  const ARMCPRegInfo *opaque,
                                  bool isread);
/* Hook function for register reset */
typedef void CPResetFn(CPUARMState *env, const ARMCPRegInfo *opaque);

#define CP_ANY 0xff

/* Definition of an ARM coprocessor register */
struct ARMCPRegInfo {
    /* Name of register (useful mainly for debugging, need not be unique) */
    const char *name;
    /*
     * Location of register: coprocessor number and (crn,crm,opc1,opc2)
     * tuple. Any of crm, opc1 and opc2 may be CP_ANY to indicate a
     * 'wildcard' field -- any value of that field in the MRC/MCR insn
     * will be decoded to this register. The register read and write
     * callbacks will be passed an ARMCPRegInfo with the crn/crm/opc1/opc2
     * used by the program, so it is possible to register a wildcard and
     * then behave differently on read/write if necessary.
     * For 64 bit registers, only crm and opc1 are relevant; crn and opc2
     * must both be zero.
     * For AArch64-visible registers, opc0 is also used.
     * Since there are no "coprocessors" in AArch64, cp is purely used as a
     * way to distinguish (for KVM's benefit) guest-visible system registers
     * from demuxed ones provided to preserve the "no side effects on
     * KVM register read/write from QEMU" semantics. cp==0x13 is guest
     * visible (to match KVM's encoding); cp==0 will be converted to
     * cp==0x13 when the ARMCPRegInfo is registered, for convenience.
     */
    uint8_t cp;
    uint8_t crn;
    uint8_t crm;
    uint8_t opc0;
    uint8_t opc1;
    uint8_t opc2;
    /* Execution state in which this register is visible: ARM_CP_STATE_* */
    int state;
    /* Register type: ARM_CP_* bits/values */
    int type;
    /* Access rights: PL*_[RW] */
    int access;
    /* Security state: ARM_CP_SECSTATE_* bits/values */
    int secure;
    /*
     * The opaque pointer passed to define_arm_cp_regs_with_opaque() when
     * this register was defined: can be used to hand data through to the
     * register read/write functions, since they are passed the ARMCPRegInfo*.
     */
    void *opaque;
    /*
     * Value of this register, if it is ARM_CP_CONST. Otherwise, if
     * fieldoffset is non-zero, the reset value of the register.
     */
    uint64_t resetvalue;
    /*
     * Offset of the field in CPUARMState for this register.
     *
     * This is not needed if either:
     *  1. type is ARM_CP_CONST or one of the ARM_CP_SPECIALs
     *  2. both readfn and writefn are specified
     */
    ptrdiff_t fieldoffset; /* offsetof(CPUARMState, field) */

    /*
     * Offsets of the secure and non-secure fields in CPUARMState for the
     * register if it is banked.  These fields are only used during the static
     * registration of a register.  During hashing the bank associated
     * with a given security state is copied to fieldoffset which is used from
     * there on out.
     *
     * It is expected that register definitions use either fieldoffset or
     * bank_fieldoffsets in the definition but not both.  It is also expected
     * that both bank offsets are set when defining a banked register.  This
     * use indicates that a register is banked.
     */
    ptrdiff_t bank_fieldoffsets[2];

    /*
     * Function for making any access checks for this register in addition to
     * those specified by the 'access' permissions bits. If NULL, no extra
     * checks required. The access check is performed at runtime, not at
     * translate time.
     */
    CPAccessFn *accessfn;
    /*
     * Function for handling reads of this register. If NULL, then reads
     * will be done by loading from the offset into CPUARMState specified
     * by fieldoffset.
     */
    CPReadFn *readfn;
    /*
     * Function for handling writes of this register. If NULL, then writes
     * will be done by writing to the offset into CPUARMState specified
     * by fieldoffset.
     */
    CPWriteFn *writefn;
    /*
     * Function for doing a "raw" read; used when we need to copy
     * coprocessor state to the kernel for KVM or out for
     * migration. This only needs to be provided if there is also a
     * readfn and it has side effects (for instance clear-on-read bits).
     */
    CPReadFn *raw_readfn;
    /*
     * Function for doing a "raw" write; used when we need to copy KVM
     * kernel coprocessor state into userspace, or for inbound
     * migration. This only needs to be provided if there is also a
     * writefn and it masks out "unwritable" bits or has write-one-to-clear
     * or similar behaviour.
     */
    CPWriteFn *raw_writefn;
    /*
     * Function for resetting the register. If NULL, then reset will be done
     * by writing resetvalue to the field specified in fieldoffset. If
     * fieldoffset is 0 then no reset will be done.
     */
    CPResetFn *resetfn;

    /*
     * "Original" writefn and readfn.
     * For ARMv8.1-VHE register aliases, we overwrite the read/write
     * accessor functions of various EL1/EL0 to perform the runtime
     * check for which sysreg should actually be modified, and then
     * forwards the operation.  Before overwriting the accessors,
     * the original function is copied here, so that accesses that
     * really do go to the EL1/EL0 version proceed normally.
     * (The corresponding EL2 register is linked via opaque.)
     */
    CPReadFn *orig_readfn;
    CPWriteFn *orig_writefn;
};

/*
 * Macros which are lvalues for the field in CPUARMState for the
 * ARMCPRegInfo *ri.
 */
#define CPREG_FIELD32(env, ri) \
    (*(uint32_t *)((char *)(env) + (ri)->fieldoffset))
#define CPREG_FIELD64(env, ri) \
    (*(uint64_t *)((char *)(env) + (ri)->fieldoffset))

#define REGINFO_SENTINEL { .type = ARM_CP_SENTINEL }

void define_arm_cp_regs_with_opaque(ARMCPU *cpu,
                                    const ARMCPRegInfo *regs, void *opaque);
void define_one_arm_cp_reg_with_opaque(ARMCPU *cpu,
                                       const ARMCPRegInfo *regs, void *opaque);
static inline void define_arm_cp_regs(ARMCPU *cpu, const ARMCPRegInfo *regs)
{
    define_arm_cp_regs_with_opaque(cpu, regs, 0);
}
static inline void define_one_arm_cp_reg(ARMCPU *cpu, const ARMCPRegInfo *regs)
{
    define_one_arm_cp_reg_with_opaque(cpu, regs, 0);
}
const ARMCPRegInfo *get_arm_cp_reginfo(GHashTable *cpregs, uint32_t encoded_cp);

/*
 * Definition of an ARM co-processor register as viewed from
 * userspace. This is used for presenting sanitised versions of
 * registers to userspace when emulating the Linux AArch64 CPU
 * ID/feature ABI (advertised as HWCAP_CPUID).
 */
typedef struct ARMCPRegUserSpaceInfo {
    /* Name of register */
    const char *name;

    /* Is the name actually a glob pattern */
    bool is_glob;

    /* Only some bits are exported to user space */
    uint64_t exported_bits;

    /* Fixed bits are applied after the mask */
    uint64_t fixed_bits;
} ARMCPRegUserSpaceInfo;

#define REGUSERINFO_SENTINEL { .name = NULL }

/* CPWriteFn that can be used to implement writes-ignored behaviour */
void arm_cp_write_ignore(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value);
/* CPReadFn that can be used for read-as-zero behaviour */
uint64_t arm_cp_read_zero(CPUARMState *env, const ARMCPRegInfo *ri);

/*
 * CPResetFn that does nothing, for use if no reset is required even
 * if fieldoffset is non zero.
 */
void arm_cp_reset_ignore(CPUARMState *env, const ARMCPRegInfo *opaque);

/*
 * Return true if this reginfo struct's field in the cpu state struct
 * is 64 bits wide.
 */
static inline bool cpreg_field_is_64bit(const ARMCPRegInfo *ri)
{
    return (ri->state == ARM_CP_STATE_AA64) || (ri->type & ARM_CP_64BIT);
}

static inline bool cp_access_ok(int current_el,
                                const ARMCPRegInfo *ri, int isread)
{
    return (ri->access >> ((current_el * 2) + isread)) & 1;
}

/* Raw read of a coprocessor register (as needed for migration, etc) */
uint64_t read_raw_cp_reg(CPUARMState *env, const ARMCPRegInfo *ri);

#ifdef CONFIG_TCG
/* Modify ARMCPRegInfo for access from userspace. */
void modify_arm_cp_regs(ARMCPRegInfo *regs, const ARMCPRegUserSpaceInfo *mods);
#endif

/*
 * default raw read/write of coprocessor register field,
 * behavior if no other function defined, and not const.
 */
uint64_t raw_read(CPUARMState *env, const ARMCPRegInfo *ri);
void raw_write(CPUARMState *env, const ARMCPRegInfo *ri,
               uint64_t value);

#endif /* CPREGS_H */
