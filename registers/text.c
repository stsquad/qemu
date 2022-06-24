/*
 * Textual representations of register state
 *
 * Copyright (c) 2022 Linaro Ltd
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "qemu/qemu-print.h"
#include "hw/core/cpu.h"
#include "registers/api.h"
#include "internals.h"

/*
 * Routines for formatting and dumping register state. These are used
 * for logging and HMP interactions.
 */

static void fmt_env_reg_string(GString *s, uintptr_t env, RegDef *reg)
{
    /* lets do ptr arithmetic what could possibly go wrong? */
    uintptr_t ptr = env + reg->access.env.offset;

    switch (reg->size) {
    case 8:
    {
        uint64_t val = *(uint64_t *) ptr;
        g_string_printf(s, "%3s=0x%016" PRIx64, reg->name, val);
        break;
    }
    case 4:
    {
        uint32_t val = *(uint32_t *) ptr;
        g_string_printf(s, "%3s=0x%08" PRIx32, reg->name, val);
        break;
    }
    default:
        g_assert_not_reached();
    }
}

/* Append a formatted representation of the register to a GString */
static void fmt_register(GString *s, CPUState *cs, RegDef *reg)
{
    g_autoptr(GString) fmt = g_string_new("");
    gchar *last_newline = g_strrstr(s->str, "\n");
    int line_len;

    switch (reg->type) {
    case REG_DIRECT_ENV:
    {
        uintptr_t env = (uintptr_t) cs->env_ptr;
        fmt_env_reg_string(fmt, env, reg);
        break;
    }
    case REG_32BIT_HELPER:
    {
        void *opaque = reg->access.helper32.opaque;
        if (reg->access.helper32.format) {
            reg->access.helper32.format(fmt, cs, opaque);
        } else {
            uint32_t v = reg->access.helper32.read(cs, opaque);
            g_string_printf(fmt, "%3s=0x%08" PRIx32, reg->name, v);
        }
        break;
    }
    case REG_64BIT_HELPER:
    {
        void *opaque = reg->access.helper64.opaque;
        if (reg->access.helper64.format) {
            reg->access.helper64.format(fmt, cs, opaque);
        } else {
            uint64_t v = reg->access.helper64.read(cs, opaque);
            g_string_printf(fmt, "%3s=0x%016" PRIx64, reg->name, v);
        }
        break;
    }
    case REG_VECTOR_HELPER:
    {
        void *opaque = reg->access.helpervec.opaque;
        if (reg->access.helpervec.format) {
            reg->access.helpervec.format(fmt, cs, opaque);
        } else {
            g_string_printf(fmt, "%3s=[TODO]", reg->name);
        }
        break;
    }
    default:
        g_assert_not_reached();
    }

    /* attempt to flow the formatting somewhat */
    if (last_newline) {
        line_len = (s->str + s->len) - last_newline;
    } else {
        line_len = s->len;
    }
    if (line_len + fmt->len > 84) {
        g_string_append_c(s, '\n');
    } else if (line_len > 0) {
        g_string_append_c(s, ' ');
    }
    g_string_append(s, fmt->str);
}



bool reg_get_value_hmp(CPUState *cs, const char *name, int64_t *val)
{
    RegDef *reg = reg_find_defintion(name);

    if (reg) {
        /* HMP can't handle more than 64bit values */
        if (reg->size > 8) {
            return false;
        }
        switch (reg->size) {
        case 8:
            *val = reg_read_64bit_value(cs, reg);
            return true;
        case 4:
            *val = reg_read_32bit_value(cs, reg);
            return true;
        default:
            return false;
        }
    }

    return false;
}

GString *reg_group_as_string(CPUState *cs, struct RegGroupHandle *handle)
{
    GString *str = g_string_new("");
    GArray *group = (GArray *) handle;
    int i;

    for (i = 0; i < group->len; i++) {
        RegDef *reg = reg_get_indirect_definition(group, i);
        fmt_register(str, cs, reg);
    }

    return str;
}

void reg_cpu_dump_state(CPUState *cs, FILE *f, int flags)
{
    GArray *core_regs = reg_get_group(NULL);
    g_autoptr(GString) fmt =
        reg_group_as_string(cs, (struct RegGroupHandle *) core_regs);
    g_string_append_c(fmt, '\n');
    qemu_fprintf(f, "%s", fmt->str);
}

GString *reg_group_list_as_string(void)
{
    GArray *groups = reg_get_groups();
    GString *grp_txt = g_string_new("");
    int i;

    for (i = 0; i < groups->len; i++) {
        RegGroup *rg = &g_array_index(groups, RegGroup, i);
        g_string_append_printf(grp_txt, "%s ", rg->name);
    }

    return grp_txt;
}
