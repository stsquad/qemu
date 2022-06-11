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

void reg_cpu_dump_state(CPUState *cs, FILE *f, int flags)
{
    GArray *registers = reg_get_registers();
    GArray *core_regs = reg_get_group(NULL);
    g_autoptr(GString) fmt = g_string_new("");

    int i;

    for (i = 0; i < core_regs->len; i++) {
        int *idx =  &g_array_index(core_regs, int, i);
        RegDef *reg = &g_array_index(registers, RegDef, *idx);

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

        qemu_fprintf(f, "%s", fmt->str);

        if ((i % 4) == 3) {
            qemu_fprintf(f, "\n");
        } else {
            qemu_fprintf(f, " ");
        }
    }
}
