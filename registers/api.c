/*
 * Register Introspection API
 *
 * The purpose of this API is to provide a link between the various
 * guest front-ends which understand and track the register state of
 * the guests and the various back-ends that might want to consume
 * that data.
 *
 * Copyright (c) 2022 Linaro Ltd
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "registers/api.h"
#include "internals.h"


void reg_add_env(const char *name, const char *grp, intptr_t offset, int bytes)
{
    RegDef reg = {
        .name = name,
        .size = bytes,
        .type = REG_DIRECT_ENV,
        .access.env.offset = offset
    };

    reg_add_definition(reg, grp);
}

void reg_add_i32_virt(const char *name, const char *grp, void *opaque,
                      reg_read32_fn rfn, reg_write32_fn wfn, reg_fmt32_fn ffn)
{
    RegDef reg = {
        .name = name,
        .size = 4,
        .type = REG_32BIT_HELPER,
        .access.helper32.read = rfn,
        .access.helper32.write = wfn,
        .access.helper32.format = ffn,
        .access.helper32.opaque = opaque
    };

    reg_add_definition(reg, grp);
}

void reg_add_i64_virt(const char *name, const char *grp, void *opaque,
                      reg_read64_fn rfn, reg_write64_fn wfn, reg_fmt64_fn ffn)
{
    RegDef reg = {
        .name = name,
        .size = 8,
        .type = REG_64BIT_HELPER,
        .access.helper64.read = rfn,
        .access.helper64.write = wfn,
        .access.helper64.format = ffn,
        .access.helper64.opaque = opaque
    };

    reg_add_definition(reg, grp);
}

void reg_add_vector(const char *name, const char *grp, void *opaque,
                    int size, RegVecFormats fmts,
                    reg_readvec_fn rfn, reg_writevec_fn wfn, reg_fmtvec_fn ffn)
{
    RegDef reg = {
        .name = name,
        .size = size,
        .type = REG_VECTOR_HELPER,
        .access.helpervec.read = rfn,
        .access.helpervec.write = wfn,
        .access.helpervec.format = ffn,
        .access.helpervec.opaque = opaque,
        .access.helpervec.fmts = fmts
    };

    reg_add_definition(reg, grp);
}

void reg_finalize_definitions(void)
{
}

int reg_get_number(const char *grp)
{
    GArray *regs = reg_get_group(grp);
    return regs->len;
}
