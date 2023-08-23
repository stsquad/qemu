/*
 * Register introspection internals
 *
 * Copyright (c) 2022 Linaro Ltd
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

/*
 * How to access the register value.
 */
typedef enum {
    REG_DIRECT_ENV,
    REG_32BIT_HELPER,
    REG_64BIT_HELPER,
    REG_VECTOR_HELPER
} RegType;


typedef struct {
    intptr_t offset;
} EnvReg;

typedef struct {
    reg_read32_fn *read;
    reg_write32_fn *write;
    reg_fmt32_fn *format;
    void *opaque;
} Helper32Reg;

typedef struct {
    reg_read64_fn *read;
    reg_write64_fn *write;
    reg_fmt64_fn *format;
    void *opaque;
} Helper64Reg;

typedef struct {
    reg_readvec_fn *read;
    reg_writevec_fn *write;
    reg_fmtvec_fn *format;
    void *opaque;
    RegVecFormats fmts;
    const char *gtp;
} HelperVecReg;

typedef struct {
    const char *name;
    int size; /* in bytes */
    RegType type;
    union {
        EnvReg env;
        Helper32Reg helper32;
        Helper64Reg helper64;
        HelperVecReg helpervec;
    } access;
} RegDef;

/**
 * struct RegGroup - internal tracking for groups of registers
 * @name: internal QEMU name for registers
 * @gdb_name: optional org.gnu.gdb.ARCH.FEATURE name for GDB
 * @xml: cached gdb XML for this group
 * @global_base: gdb-ism
 * @registers: array of RegDef definitions
 *
 * If @gdb_name is not set we report org.qemu.gdb.@name to GDB.
 */
typedef struct {
    const char *name;
    const char *gdb_name;
    const char *xml;
    int global_base;
    GArray *registers;
} RegGroup;

/*
 * Opaque handles used outside of the register subsystem
 */

/*
 * Internal API functions, shouldn't be used by the wider code base.
 */

/**
 * reg_add_definition: add register definition
 * @def: a RefDef struction
 * @group: the register group it belongs to
 */
void reg_add_definition(RegDef def, const char *group);

/**
 * reg_find_defintion: recall regdef
 * @name: name of register as a string
 *
 * Search through the list of registers for @name and return the
 * definition if found. The search is case insensitive.
 *
 * Returns a RegDef or NULL if not found.
 */
RegDef *reg_find_defintion(const char *name);

uint64_t reg_read_64bit_value(CPUState *cs, RegDef *def);
uint32_t reg_read_32bit_value(CPUState *cs, RegDef *def);

/**
 * reg_get_groups(): return internal RegGroup array
 */
GArray *reg_get_groups(void);

/**
 * reg_get_group(): return GArray of indexes to reg_get_registers
 * @group: name of group
 *
 * If group is not found it will create it and return a GArray of
 * indexs into reg_get_registers.
 */
GArray *reg_get_group(const char *group);

/**
 * reg_find_group(): return RegGroup for @group
 * @group: name of group
 *
 * Returns pointer to RegGroup or NULL if not found.
 */
RegGroup *reg_find_group(const char *group);

/**
 * reg_get_definition() - recall a regdef
 * @idx: global index of register
 *
 * Return the definition of a register based on its index. This will
 * come from either the core register list or one of the groups.
 */
RegDef *reg_get_definition(int index);

/**
 * reg_get_indirect_defintion() - recall a RegDef via a group array
 * @idx: index into group array
 *
 * Return the definition of a register based on its indirect group
 * index.
 */
static inline RegDef *reg_get_indirect_definition(GArray *grp, int grp_index)
{
    int *global_idx =  &g_array_index(grp, int, grp_index);
    g_assert(g_array_get_element_size(grp) == sizeof(int));
    return reg_get_definition(*global_idx);
}

/**
 * reg_register_with_gdb(): register things with gdb
 * @cs: CPUState
 *
 * Internal function to register all our definitions with the gdbstub.
 */
void reg_register_with_gdb(CPUState *cs);
