/*
 * Core register handling. This is where we maintain the data
 * structures to keep track of all the registers in the system.
 *
 * Copyright (c) 2022 Linaro Ltd
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "hw/core/cpu.h"
#include "registers/api.h"
#include "internals.h"

/* Array of RegDef register definitions */
static GArray *registers;

/* Array of indexes into @registers */
static GArray *core_gdb;

/* Array of RegGroup, each contains an array of indexes into @registers */
static GArray *groups;

static GArray *new_group(const char *grp)
{
    RegGroup new_grp = { .name = grp, .global_base = registers->len };
    new_grp.registers = g_array_new(true, true, sizeof(int));
    groups = g_array_append_val(groups, new_grp);
    return new_grp.registers;
}

GArray *reg_get_group(const char *group)
{
    int i;
    const char *grp = g_intern_static_string(group);

    /* no group name is a core register */
    if (!group) {
        return core_gdb;
    }

    /* search our existing groups */
    for (i = 0; i < groups->len; i++) {
        RegGroup *rg = &g_array_index(groups, RegGroup, i);
        if (rg->name == grp) {
            return rg->registers;
        }
    }

    /* create a new group */
    return new_group(grp);
}

/*
 * Add register to our global list of register definitions and create
 * an index entry in the appropriate group.
 *
 * !TODO: enforce sequential group definitions? move to core?
 */
void reg_add_definition(RegDef def, const char *group)
{
    GArray *grp_index = reg_get_group(group);
    gint new_reg_idx = registers->len;
    registers = g_array_append_val(registers, def);
    g_array_append_val(grp_index, new_reg_idx);
}

RegDef *reg_find_defintion(const char *name)
{
    int i;

    for (i = 0; i < registers->len; i++) {
        RegDef *reg = &g_array_index(registers, RegDef, i);
        if (g_ascii_strncasecmp(reg->name, name, strlen(reg->name)) == 0) {
            return reg;
        }
    }

    return NULL;
}

GArray *reg_get_registers(void)
{
    return registers;
}

/*
 * Get values
 */
uint64_t reg_read_64bit_value(CPUState *cs, RegDef *reg)
{
    switch (reg->type) {
    case REG_DIRECT_ENV:
    {
        uintptr_t env = (uintptr_t) cs->env_ptr;
        uintptr_t ptr = env + reg->access.env.offset;
        g_assert(reg->size == 8);
        return *(uint64_t *) ptr;
        break;
    }
    case REG_64BIT_HELPER:
    {
        void *opaque = reg->access.helper64.opaque;
        return reg->access.helper64.read(cs, opaque);
    }
    default:
        g_assert_not_reached();
    }

    return 0;
}

uint32_t reg_read_32bit_value(CPUState *cs, RegDef *reg)
{
    switch (reg->type) {
    case REG_DIRECT_ENV:
    {
        uintptr_t env = (uintptr_t) cs->env_ptr;
        uintptr_t ptr = env + reg->access.env.offset;
        g_assert(reg->size == 4);
        return *(uint32_t *) ptr;
        break;
    }
    case REG_32BIT_HELPER:
    {
        void *opaque = reg->access.helper64.opaque;
        return reg->access.helper64.read(cs, opaque);
    }
    default:
        g_assert_not_reached();
    }

    return 0;
}

__attribute__((constructor))
static void registers_init(void)
{
    registers = g_array_new(false, true, sizeof(RegDef));
    groups = g_array_new(true, true, sizeof(RegGroup));
    core_gdb = g_array_new(true, true, sizeof(int));
}
