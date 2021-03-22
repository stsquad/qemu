/*
 * ARM CP registers - common functionality
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "cpu.h"
#include "cpregs.h"

static bool raw_accessors_invalid(const ARMCPRegInfo *ri)
{
   /* Return true if the regdef would cause an assertion if you called
    * read_raw_cp_reg() or write_raw_cp_reg() on it (ie if it is a
    * program bug for it not to have the NO_RAW flag).
    * NB that returning false here doesn't necessarily mean that calling
    * read/write_raw_cp_reg() is safe, because we can't distinguish "has
    * read/write access functions which are safe for raw use" from "has
    * read/write access functions which have side effects but has forgotten
    * to provide raw access functions".
    * The tests here line up with the conditions in read/write_raw_cp_reg()
    * and assertions in raw_read()/raw_write().
    */
    if ((ri->type & ARM_CP_CONST) ||
        ri->fieldoffset ||
        ((ri->raw_writefn || ri->writefn) && (ri->raw_readfn || ri->readfn))) {
        return false;
    }
    return true;
}

static void add_cpreg_to_hashtable(ARMCPU *cpu, const ARMCPRegInfo *r,
                                   void *opaque, int state, int secstate,
                                   int crm, int opc1, int opc2,
                                   const char *name)
{
    /* Private utility function for define_one_arm_cp_reg_with_opaque():
     * add a single reginfo struct to the hash table.
     */
    uint32_t *key = g_new(uint32_t, 1);
    ARMCPRegInfo *r2 = g_memdup(r, sizeof(ARMCPRegInfo));
    int is64 = (r->type & ARM_CP_64BIT) ? 1 : 0;
    int ns = (secstate & ARM_CP_SECSTATE_NS) ? 1 : 0;

    r2->name = g_strdup(name);
    /* Reset the secure state to the specific incoming state.  This is
     * necessary as the register may have been defined with both states.
     */
    r2->secure = secstate;

    if (r->bank_fieldoffsets[0] && r->bank_fieldoffsets[1]) {
        /* Register is banked (using both entries in array).
         * Overwriting fieldoffset as the array is only used to define
         * banked registers but later only fieldoffset is used.
         */
        r2->fieldoffset = r->bank_fieldoffsets[ns];
    }

    if (state == ARM_CP_STATE_AA32) {
        if (r->bank_fieldoffsets[0] && r->bank_fieldoffsets[1]) {
            /* If the register is banked then we don't need to migrate or
             * reset the 32-bit instance in certain cases:
             *
             * 1) If the register has both 32-bit and 64-bit instances then we
             *    can count on the 64-bit instance taking care of the
             *    non-secure bank.
             * 2) If ARMv8 is enabled then we can count on a 64-bit version
             *    taking care of the secure bank.  This requires that separate
             *    32 and 64-bit definitions are provided.
             */
            if ((r->state == ARM_CP_STATE_BOTH && ns) ||
                (arm_feature(&cpu->env, ARM_FEATURE_V8) && !ns)) {
                r2->type |= ARM_CP_ALIAS;
            }
        } else if ((secstate != r->secure) && !ns) {
            /* The register is not banked so we only want to allow migration of
             * the non-secure instance.
             */
            r2->type |= ARM_CP_ALIAS;
        }

        if (r->state == ARM_CP_STATE_BOTH) {
            /* We assume it is a cp15 register if the .cp field is left unset.
             */
            if (r2->cp == 0) {
                r2->cp = 15;
            }

#ifdef HOST_WORDS_BIGENDIAN
            if (r2->fieldoffset) {
                r2->fieldoffset += sizeof(uint32_t);
            }
#endif
        }
    }
    if (state == ARM_CP_STATE_AA64) {
        /* To allow abbreviation of ARMCPRegInfo
         * definitions, we treat cp == 0 as equivalent to
         * the value for "standard guest-visible sysreg".
         * STATE_BOTH definitions are also always "standard
         * sysreg" in their AArch64 view (the .cp value may
         * be non-zero for the benefit of the AArch32 view).
         */
        if (r->cp == 0 || r->state == ARM_CP_STATE_BOTH) {
            r2->cp = CP_REG_ARM64_SYSREG_CP;
        }
        *key = ENCODE_AA64_CP_REG(r2->cp, r2->crn, crm,
                                  r2->opc0, opc1, opc2);
    } else {
        *key = ENCODE_CP_REG(r2->cp, is64, ns, r2->crn, crm, opc1, opc2);
    }
    if (opaque) {
        r2->opaque = opaque;
    }
    /* reginfo passed to helpers is correct for the actual access,
     * and is never ARM_CP_STATE_BOTH:
     */
    r2->state = state;
    /* Make sure reginfo passed to helpers for wildcarded regs
     * has the correct crm/opc1/opc2 for this reg, not CP_ANY:
     */
    r2->crm = crm;
    r2->opc1 = opc1;
    r2->opc2 = opc2;
    /* By convention, for wildcarded registers only the first
     * entry is used for migration; the others are marked as
     * ALIAS so we don't try to transfer the register
     * multiple times. Special registers (ie NOP/WFI) are
     * never migratable and not even raw-accessible.
     */
    if ((r->type & ARM_CP_SPECIAL)) {
        r2->type |= ARM_CP_NO_RAW;
    }
    if (((r->crm == CP_ANY) && crm != 0) ||
        ((r->opc1 == CP_ANY) && opc1 != 0) ||
        ((r->opc2 == CP_ANY) && opc2 != 0)) {
        r2->type |= ARM_CP_ALIAS | ARM_CP_NO_GDB;
    }

    /* Check that raw accesses are either forbidden or handled. Note that
     * we can't assert this earlier because the setup of fieldoffset for
     * banked registers has to be done first.
     */
    if (!(r2->type & ARM_CP_NO_RAW)) {
        assert(!raw_accessors_invalid(r2));
    }

    /* Overriding of an existing definition must be explicitly
     * requested.
     */
    if (!(r->type & ARM_CP_OVERRIDE)) {
        ARMCPRegInfo *oldreg;
        oldreg = g_hash_table_lookup(cpu->cp_regs, key);
        if (oldreg && !(oldreg->type & ARM_CP_OVERRIDE)) {
            fprintf(stderr, "Register redefined: cp=%d %d bit "
                    "crn=%d crm=%d opc1=%d opc2=%d, "
                    "was %s, now %s\n", r2->cp, 32 + 32 * is64,
                    r2->crn, r2->crm, r2->opc1, r2->opc2,
                    oldreg->name, r2->name);
            g_assert_not_reached();
        }
    }
    g_hash_table_insert(cpu->cp_regs, key, r2);
}

void define_one_arm_cp_reg_with_opaque(ARMCPU *cpu,
                                       const ARMCPRegInfo *r, void *opaque)
{
    /* Define implementations of coprocessor registers.
     * We store these in a hashtable because typically
     * there are less than 150 registers in a space which
     * is 16*16*16*8*8 = 262144 in size.
     * Wildcarding is supported for the crm, opc1 and opc2 fields.
     * If a register is defined twice then the second definition is
     * used, so this can be used to define some generic registers and
     * then override them with implementation specific variations.
     * At least one of the original and the second definition should
     * include ARM_CP_OVERRIDE in its type bits -- this is just a guard
     * against accidental use.
     *
     * The state field defines whether the register is to be
     * visible in the AArch32 or AArch64 execution state. If the
     * state is set to ARM_CP_STATE_BOTH then we synthesise a
     * reginfo structure for the AArch32 view, which sees the lower
     * 32 bits of the 64 bit register.
     *
     * Only registers visible in AArch64 may set r->opc0; opc0 cannot
     * be wildcarded. AArch64 registers are always considered to be 64
     * bits; the ARM_CP_64BIT* flag applies only to the AArch32 view of
     * the register, if any.
     */
    int crm, opc1, opc2, state;
    int crmmin = (r->crm == CP_ANY) ? 0 : r->crm;
    int crmmax = (r->crm == CP_ANY) ? 15 : r->crm;
    int opc1min = (r->opc1 == CP_ANY) ? 0 : r->opc1;
    int opc1max = (r->opc1 == CP_ANY) ? 7 : r->opc1;
    int opc2min = (r->opc2 == CP_ANY) ? 0 : r->opc2;
    int opc2max = (r->opc2 == CP_ANY) ? 7 : r->opc2;
    /* 64 bit registers have only CRm and Opc1 fields */
    assert(!((r->type & ARM_CP_64BIT) && (r->opc2 || r->crn)));
    /* op0 only exists in the AArch64 encodings */
    assert((r->state != ARM_CP_STATE_AA32) || (r->opc0 == 0));
    /* AArch64 regs are all 64 bit so ARM_CP_64BIT is meaningless */
    assert((r->state != ARM_CP_STATE_AA64) || !(r->type & ARM_CP_64BIT));
    /*
     * This API is only for Arm's system coprocessors (14 and 15) or
     * (M-profile or v7A-and-earlier only) for implementation defined
     * coprocessors in the range 0..7.  Our decode assumes this, since
     * 8..13 can be used for other insns including VFP and Neon. See
     * valid_cp() in translate.c.  Assert here that we haven't tried
     * to use an invalid coprocessor number.
     */
    switch (r->state) {
    case ARM_CP_STATE_BOTH:
        /* 0 has a special meaning, but otherwise the same rules as AA32. */
        if (r->cp == 0) {
            break;
        }
        /* fall through */
    case ARM_CP_STATE_AA32:
        if (arm_feature(&cpu->env, ARM_FEATURE_V8) &&
            !arm_feature(&cpu->env, ARM_FEATURE_M)) {
            assert(r->cp >= 14 && r->cp <= 15);
        } else {
            assert(r->cp < 8 || (r->cp >= 14 && r->cp <= 15));
        }
        break;
    case ARM_CP_STATE_AA64:
        assert(r->cp == 0 || r->cp == CP_REG_ARM64_SYSREG_CP);
        break;
    default:
        g_assert_not_reached();
    }
    /* The AArch64 pseudocode CheckSystemAccess() specifies that op1
     * encodes a minimum access level for the register. We roll this
     * runtime check into our general permission check code, so check
     * here that the reginfo's specified permissions are strict enough
     * to encompass the generic architectural permission check.
     */
    if (r->state != ARM_CP_STATE_AA32) {
        int mask = 0;
        switch (r->opc1) {
        case 0:
            /* min_EL EL1, but some accessible to EL0 via kernel ABI */
            mask = PL0U_R | PL1_RW;
            break;
        case 1: case 2:
            /* min_EL EL1 */
            mask = PL1_RW;
            break;
        case 3:
            /* min_EL EL0 */
            mask = PL0_RW;
            break;
        case 4:
        case 5:
            /* min_EL EL2 */
            mask = PL2_RW;
            break;
        case 6:
            /* min_EL EL3 */
            mask = PL3_RW;
            break;
        case 7:
            /* min_EL EL1, secure mode only (we don't check the latter) */
            mask = PL1_RW;
            break;
        default:
            /* broken reginfo with out-of-range opc1 */
            assert(false);
            break;
        }
        /* assert our permissions are not too lax (stricter is fine) */
        assert((r->access & ~mask) == 0);
    }

    /* Check that the register definition has enough info to handle
     * reads and writes if they are permitted.
     */
    if (!(r->type & (ARM_CP_SPECIAL|ARM_CP_CONST))) {
        if (r->access & PL3_R) {
            assert((r->fieldoffset ||
                   (r->bank_fieldoffsets[0] && r->bank_fieldoffsets[1])) ||
                   r->readfn);
        }
        if (r->access & PL3_W) {
            assert((r->fieldoffset ||
                   (r->bank_fieldoffsets[0] && r->bank_fieldoffsets[1])) ||
                   r->writefn);
        }
    }
    /* Bad type field probably means missing sentinel at end of reg list */
    assert(cptype_valid(r->type));
    for (crm = crmmin; crm <= crmmax; crm++) {
        for (opc1 = opc1min; opc1 <= opc1max; opc1++) {
            for (opc2 = opc2min; opc2 <= opc2max; opc2++) {
                for (state = ARM_CP_STATE_AA32;
                     state <= ARM_CP_STATE_AA64; state++) {
                    if (r->state != state && r->state != ARM_CP_STATE_BOTH) {
                        continue;
                    }
                    if (state == ARM_CP_STATE_AA32) {
                        /* Under AArch32 CP registers can be common
                         * (same for secure and non-secure world) or banked.
                         */
                        char *name;

                        switch (r->secure) {
                        case ARM_CP_SECSTATE_S:
                        case ARM_CP_SECSTATE_NS:
                            add_cpreg_to_hashtable(cpu, r, opaque, state,
                                                   r->secure, crm, opc1, opc2,
                                                   r->name);
                            break;
                        default:
                            name = g_strdup_printf("%s_S", r->name);
                            add_cpreg_to_hashtable(cpu, r, opaque, state,
                                                   ARM_CP_SECSTATE_S,
                                                   crm, opc1, opc2, name);
                            g_free(name);
                            add_cpreg_to_hashtable(cpu, r, opaque, state,
                                                   ARM_CP_SECSTATE_NS,
                                                   crm, opc1, opc2, r->name);
                            break;
                        }
                    } else {
                        /* AArch64 registers get mapped to non-secure instance
                         * of AArch32 */
                        add_cpreg_to_hashtable(cpu, r, opaque, state,
                                               ARM_CP_SECSTATE_NS,
                                               crm, opc1, opc2, r->name);
                    }
                }
            }
        }
    }
}

void define_arm_cp_regs_with_opaque(ARMCPU *cpu,
                                    const ARMCPRegInfo *regs, void *opaque)
{
    /* Define a whole list of registers */
    const ARMCPRegInfo *r;
    for (r = regs; r->type != ARM_CP_SENTINEL; r++) {
        define_one_arm_cp_reg_with_opaque(cpu, r, opaque);
    }
}

void arm_cp_write_ignore(CPUARMState *env, const ARMCPRegInfo *ri,
                         uint64_t value)
{
}

uint64_t arm_cp_read_zero(CPUARMState *env, const ARMCPRegInfo *ri)
{
    return 0;
}

void arm_cp_reset_ignore(CPUARMState *env, const ARMCPRegInfo *opaque)
{
}
