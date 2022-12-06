/*
 * Register unit tests
 *
 * Copyright (c) 2022 Linaro Ltd
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "registers/api.h"

/*
 * The helpers for the test case just treat CPUState as an anonymous
 * pointer for the purposes of this test.
 */
typedef CPUState CPUState;

#define REGS 2
#define VEC_SIZE 16

typedef struct {
    uint64_t reg64[REGS];
    uint32_t reg32[REGS];
    uint8_t reg_vec[REGS * VEC_SIZE];
    bool flag_a;
    bool flag_b;
    bool flag_c;
} FakeCPU;

static FakeCPU fcpu;

/*
 * Handle our fake flags registers - in a real QEMU opaque is used for
 * additional data and we would go looking in CPUState for the values
 * we need.
 */
static uint64_t r64_flags(CPUState *cs, const void *opaque)
{
    FakeCPU *f = &fcpu;
    uint64_t result = f->flag_a << 2 | f->flag_b << 1 | f->flag_c << 0;
    g_assert(f == opaque);
    return result;
}
static void w64_flags(CPUState *cs, const void *opaque, uint64_t value)
{
    FakeCPU *f = &fcpu;
    g_assert(f == opaque);
    f->flag_a = value & 0x4;
    f->flag_b = value & 0x2;
    f->flag_c = value & 0x1;
}
static uint32_t r32_flags(CPUState *cs, const void *opaque)
{
    FakeCPU *f = &fcpu;
    uint32_t result = f->flag_a << 2 | f->flag_b << 1 | f->flag_c << 0;
    g_assert(f == opaque);
    return result;
}
static void w32_flags(CPUState *cs, const void *opaque, uint32_t value)
{
    FakeCPU *f = &fcpu;
    g_assert(f == opaque);
    f->flag_a = value & 0x4;
    f->flag_b = value & 0x2;
    f->flag_c = value & 0x1;
}
static void r32_fmt(GString *s, CPUState *cs, const void *opaque)
{
    FakeCPU *f = &fcpu;
    g_assert(f == opaque);
    g_string_append_printf(s, "a:%s b:%s c:%s",
                           f->flag_a ? "true" : "false",
                           f->flag_b ? "true" : "false",
                           f->flag_c ? "true" : "false");
}

/*
 * Vector helpers
 */
static GByteArray *readvec(CPUState *cs, const void *opaque)
{
    unsigned int idx = GPOINTER_TO_UINT(opaque);
    uint8_t *ptr = &fcpu.reg_vec[idx * VEC_SIZE];
    return g_byte_array_new_take(ptr, VEC_SIZE);
}
static void writevec(CPUState *cs, const void *opaque, GByteArray *reg)
{
    unsigned int idx = GPOINTER_TO_UINT(opaque);
    uint8_t *dst = &fcpu.reg_vec[idx * VEC_SIZE];
    g_assert(reg->len == VEC_SIZE);
    memcpy(dst, reg->data, VEC_SIZE);
}
static void fmtvec(GString *s, CPUState *cs, const void *opaque)
{
    unsigned int idx = GPOINTER_TO_UINT(opaque);
    uint8_t *ptr = &fcpu.reg_vec[idx * VEC_SIZE];
    int i;
    for (i = 0; i < VEC_SIZE; i++) {
        g_string_append_printf(s, "0x%2x:", ptr[i]);
    }
    g_string_truncate(s, s->len - 1);
}

static void test_registration(void)
{
    /*
     * We cannot test register access for env based registers here as
     * we don't have a fully formed CPUState with attended
     * CPUArchState to contain the actual data.
     */
    reg_add_env("r64_0", NULL, offsetof(FakeCPU, reg64[0]), 8);
    reg_add_env("r64_1", NULL, offsetof(FakeCPU, reg64[1]), 8);

    reg_add_env("r32_0", NULL, offsetof(FakeCPU, reg32[0]), 8);
    reg_add_env("r32_1", NULL, offsetof(FakeCPU, reg32[1]), 8);

    /*
     * "virtual" registers are not directly represented in memory so
     * we provide helpers for them to access and (optionally) format
     * their data.
     */
    reg_add_i64_virt("r64_flags", "flags", &fcpu, r64_flags, w64_flags, NULL);
    reg_add_i32_virt("r32_flags", "flags", &fcpu, r32_flags, w32_flags, r32_fmt);

    /*
     * vector registers may be directly in memory but we always use
     * helpers to access them.
     */
    reg_add_vector("v0", "vectors", GUINT_TO_POINTER(0), VEC_SIZE * 8,
                   REG_VEC_UINT64 | REG_VEC_UINT32 | REG_VEC_UINT16 | REG_VEC_UINT8,
                   readvec, writevec, fmtvec);
    reg_add_vector("v1", "vectors", GUINT_TO_POINTER(1), VEC_SIZE * 8,
                   REG_VEC_UINT64 | REG_VEC_UINT32 | REG_VEC_UINT16 | REG_VEC_UINT8,
                   readvec, writevec, fmtvec);

    reg_finalize_definitions();

    g_assert(reg_get_number(NULL) == 4);
    g_assert(reg_get_number("flags") == 2);
}



int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);
    g_test_add_func("/registers/registration", test_registration);
    g_test_run();
    return 0;
}
