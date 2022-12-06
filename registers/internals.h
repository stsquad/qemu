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
} HelperVecReg;

typedef struct {
    const char *name;
    int size;
    RegType type;
    union {
        EnvReg env;
        Helper32Reg helper32;
        Helper64Reg helper64;
        HelperVecReg helpervec;
    } access;
} RegDef;

typedef struct {
    const char *name;
    int global_base;
    GArray *registers;
} RegGroup;

/*
 * Internal API functions, shouldn't be used by the wider code base.
 */

/**
 * reg_add_definition: add register definition
 * @def: a RefDef struction
 * @group: the register group it belongs to
 */
void reg_add_definition(RegDef def, const char *group);
GArray *reg_get_registers(void);
GArray *reg_get_group(const char *group);
