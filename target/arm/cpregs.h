/*
 * ARM CP registers
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#ifndef CPREGS_H
#define CPREGS_H

/*
 * Interface for defining coprocessor registers.
 * Registers are defined in tables of arm_cp_reginfo structs
 * which are passed to define_arm_cp_regs().
 */

/*
 * When looking up a coprocessor register we look for it
 * via an integer which encodes all of:
 *  coprocessor number
 *  Crn, Crm, opc1, opc2 fields
 *  32 or 64 bit register (ie is it accessed via MRC/MCR
 *    or via MRRC/MCRR?)
 *  non-secure/secure bank (AArch32 only)
 * We allow 4 bits for opc1 because MRRC/MCRR have a 4 bit field.
 * (In this case crn and opc2 should be zero.)
 * For AArch64, there is no 32/64 bit size distinction;
 * instead all registers have a 2 bit op0, 3 bit op1 and op2,
 * and 4 bit CRn and CRm. The encoding patterns are chosen
 * to be easy to convert to and from the KVM encodings, and also
 * so that the hashtable can contain both AArch32 and AArch64
 * registers (to allow for interprocessing where we might run
 * 32 bit code on a 64 bit core).
 */
/*
 * This bit is private to our hashtable cpreg; in KVM register
 * IDs the AArch64/32 distinction is the KVM_REG_ARM/ARM64
 * in the upper bits of the 64 bit ID.
 */
#define CP_REG_AA64_SHIFT 28
#define CP_REG_AA64_MASK (1 << CP_REG_AA64_SHIFT)

/*
 * To enable banking of coprocessor registers depending on ns-bit we
 * add a bit to distinguish between secure and non-secure cpregs in the
 * hashtable.
 */
#define CP_REG_NS_SHIFT 29
#define CP_REG_NS_MASK (1 << CP_REG_NS_SHIFT)

#define ENCODE_CP_REG(cp, is64, ns, crn, crm, opc1, opc2)   \
    ((ns) << CP_REG_NS_SHIFT | ((cp) << 16) | ((is64) << 15) |   \
     ((crn) << 11) | ((crm) << 7) | ((opc1) << 3) | (opc2))

#define ENCODE_AA64_CP_REG(cp, crn, crm, op0, op1, op2) \
    (CP_REG_AA64_MASK |                                 \
     ((cp) << CP_REG_ARM_COPROC_SHIFT) |                \
     ((op0) << CP_REG_ARM64_SYSREG_OP0_SHIFT) |         \
     ((op1) << CP_REG_ARM64_SYSREG_OP1_SHIFT) |         \
     ((crn) << CP_REG_ARM64_SYSREG_CRN_SHIFT) |         \
     ((crm) << CP_REG_ARM64_SYSREG_CRM_SHIFT) |         \
     ((op2) << CP_REG_ARM64_SYSREG_OP2_SHIFT))

/*
 * Convert a full 64 bit KVM register ID to the truncated 32 bit
 * version used as a key for the coprocessor register hashtable
 */
static inline uint32_t kvm_to_cpreg_id(uint64_t kvmid)
{
    uint32_t cpregid = kvmid;
    if ((kvmid & CP_REG_ARCH_MASK) == CP_REG_ARM64) {
        cpregid |= CP_REG_AA64_MASK;
    } else {
        if ((kvmid & CP_REG_SIZE_MASK) == CP_REG_SIZE_U64) {
            cpregid |= (1 << 15);
        }

        /*
         * KVM is always non-secure so add the NS flag on AArch32 register
         * entries.
         */
         cpregid |= 1 << CP_REG_NS_SHIFT;
    }
    return cpregid;
}

/*
 * Convert a truncated 32 bit hashtable key into the full
 * 64 bit KVM register ID.
 */
static inline uint64_t cpreg_to_kvm_id(uint32_t cpregid)
{
    uint64_t kvmid;

    if (cpregid & CP_REG_AA64_MASK) {
        kvmid = cpregid & ~CP_REG_AA64_MASK;
        kvmid |= CP_REG_SIZE_U64 | CP_REG_ARM64;
    } else {
        kvmid = cpregid & ~(1 << 15);
        if (cpregid & (1 << 15)) {
            kvmid |= CP_REG_SIZE_U64 | CP_REG_ARM;
        } else {
            kvmid |= CP_REG_SIZE_U32 | CP_REG_ARM;
        }
    }
    return kvmid;
}

/*
 * ARMCPRegInfo type field bits. If the SPECIAL bit is set this is a
 * special-behaviour cp reg and bits [11..8] indicate what behaviour
 * it has. Otherwise it is a simple cp reg, where CONST indicates that
 * TCG can assume the value to be constant (ie load at translate time)
 * and 64BIT indicates a 64 bit wide coprocessor register. SUPPRESS_TB_END
 * indicates that the TB should not be ended after a write to this register
 * (the default is that the TB ends after cp writes). OVERRIDE permits
 * a register definition to override a previous definition for the
 * same (cp, is64, crn, crm, opc1, opc2) tuple: either the new or the
 * old must have the OVERRIDE bit set.
 * ALIAS indicates that this register is an alias view of some underlying
 * state which is also visible via another register, and that the other
 * register is handling migration and reset; registers marked ALIAS will not be
 * migrated but may have their state set by syncing of register state from KVM.
 * NO_RAW indicates that this register has no underlying state and does not
 * support raw access for state saving/loading; it will not be used for either
 * migration or KVM state synchronization. (Typically this is for "registers"
 * which are actually used as instructions for cache maintenance and so on.)
 * IO indicates that this register does I/O and therefore its accesses
 * need to be marked with gen_io_start() and also end the TB. In particular,
 * registers which implement clocks or timers require this.
 * RAISES_EXC is for when the read or write hook might raise an exception;
 * the generated code will synchronize the CPU state before calling the hook
 * so that it is safe for the hook to call raise_exception().
 * NEWEL is for writes to registers that might change the exception
 * level - typically on older ARM chips. For those cases we need to
 * re-read the new el when recomputing the translation flags.
 */
#define ARM_CP_SPECIAL           0x0001
#define ARM_CP_CONST             0x0002
#define ARM_CP_64BIT             0x0004
#define ARM_CP_SUPPRESS_TB_END   0x0008
#define ARM_CP_OVERRIDE          0x0010
#define ARM_CP_ALIAS             0x0020
#define ARM_CP_IO                0x0040
#define ARM_CP_NO_RAW            0x0080
#define ARM_CP_NOP               (ARM_CP_SPECIAL | 0x0100)
#define ARM_CP_WFI               (ARM_CP_SPECIAL | 0x0200)
#define ARM_CP_NZCV              (ARM_CP_SPECIAL | 0x0300)
#define ARM_CP_CURRENTEL         (ARM_CP_SPECIAL | 0x0400)
#define ARM_CP_DC_ZVA            (ARM_CP_SPECIAL | 0x0500)
#define ARM_CP_DC_GVA            (ARM_CP_SPECIAL | 0x0600)
#define ARM_CP_DC_GZVA           (ARM_CP_SPECIAL | 0x0700)
#define ARM_LAST_SPECIAL         ARM_CP_DC_GZVA
#define ARM_CP_FPU               0x1000
#define ARM_CP_SVE               0x2000
#define ARM_CP_NO_GDB            0x4000
#define ARM_CP_RAISES_EXC        0x8000
#define ARM_CP_NEWEL             0x10000
/* Used only as a terminator for ARMCPRegInfo lists */
#define ARM_CP_SENTINEL          0xfffff
/* Mask of only the flag bits in a type field */
#define ARM_CP_FLAG_MASK         0x1f0ff

/*
 * Valid values for ARMCPRegInfo state field, indicating which of
 * the AArch32 and AArch64 execution states this register is visible in.
 * If the reginfo doesn't explicitly specify then it is AArch32 only.
 * If the reginfo is declared to be visible in both states then a second
 * reginfo is synthesised for the AArch32 view of the AArch64 register,
 * such that the AArch32 view is the lower 32 bits of the AArch64 one.
 * Note that we rely on the values of these enums as we iterate through
 * the various states in some places.
 */
enum {
    ARM_CP_STATE_AA32 = 0,
    ARM_CP_STATE_AA64 = 1,
    ARM_CP_STATE_BOTH = 2,
};

/*
 * ARM CP register secure state flags.  These flags identify security state
 * attributes for a given CP register entry.
 * The existence of both or neither secure and non-secure flags indicates that
 * the register has both a secure and non-secure hash entry.  A single one of
 * these flags causes the register to only be hashed for the specified
 * security state.
 * Although definitions may have any combination of the S/NS bits, each
 * registered entry will only have one to identify whether the entry is secure
 * or non-secure.
 */
enum {
    ARM_CP_SECSTATE_S =   (1 << 0), /* bit[0]: Secure state register */
    ARM_CP_SECSTATE_NS =  (1 << 1), /* bit[1]: Non-secure state register */
};

/*
 * Return true if cptype is a valid type field. This is used to try to
 * catch errors where the sentinel has been accidentally left off the end
 * of a list of registers.
 */
static inline bool cptype_valid(int cptype)
{
    return ((cptype & ~ARM_CP_FLAG_MASK) == 0)
        || ((cptype & ARM_CP_SPECIAL) &&
            ((cptype & ~ARM_CP_FLAG_MASK) <= ARM_LAST_SPECIAL));
}

/*
 * Access rights:
 * We define bits for Read and Write access for what rev C of the v7-AR ARM ARM
 * defines as PL0 (user), PL1 (fiq/irq/svc/abt/und/sys, ie privileged), and
 * PL2 (hyp). The other level which has Read and Write bits is Secure PL1
 * (ie any of the privileged modes in Secure state, or Monitor mode).
 * If a register is accessible in one privilege level it's always accessible
 * in higher privilege levels too. Since "Secure PL1" also follows this rule
 * (ie anything visible in PL2 is visible in S-PL1, some things are only
 * visible in S-PL1) but "Secure PL1" is a bit of a mouthful, we bend the
 * terminology a little and call this PL3.
 * In AArch64 things are somewhat simpler as the PLx bits line up exactly
 * with the ELx exception levels.
 *
 * If access permissions for a register are more complex than can be
 * described with these bits, then use a laxer set of restrictions, and
 * do the more restrictive/complex check inside a helper function.
 */
#define PL3_R 0x80
#define PL3_W 0x40
#define PL2_R (0x20 | PL3_R)
#define PL2_W (0x10 | PL3_W)
#define PL1_R (0x08 | PL2_R)
#define PL1_W (0x04 | PL2_W)
#define PL0_R (0x02 | PL1_R)
#define PL0_W (0x01 | PL1_W)

/*
 * For user-mode some registers are accessible to EL0 via a kernel
 * trap-and-emulate ABI. In this case we define the read permissions
 * as actually being PL0_R. However some bits of any given register
 * may still be masked.
 */
#ifdef CONFIG_USER_ONLY
#define PL0U_R PL0_R
#else
#define PL0U_R PL1_R
#endif

#define PL3_RW (PL3_R | PL3_W)
#define PL2_RW (PL2_R | PL2_W)
#define PL1_RW (PL1_R | PL1_W)
#define PL0_RW (PL0_R | PL0_W)

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
