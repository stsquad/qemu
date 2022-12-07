/*
 * Register unit tests
 *
 * Copyright (c) 2022 Linaro Ltd
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include <glib/gstdio.h>
#include "exec/gdbstub.h"
#include "registers/api.h"

/*
 * The helpers for the test case just treat CPUState as an anonymous
 * pointer for the purposes of this test.
 */
typedef CPUState CPUState;

#define REGS 2
#define VEC_SIZE 16
#define BIG_VEC_SIZE 128

typedef struct {
    uint64_t reg64[REGS];
    uint32_t reg32[REGS];
    uint8_t reg_vec[REGS * VEC_SIZE];
    uint8_t reg_big_vec[REGS * BIG_VEC_SIZE];
    bool flag_a;
    bool flag_b;
    bool flag_c;
} FakeCPU;

static FakeCPU fcpu;

/*
 * Helper based access to the fake 32/64 bit registers. We can't use
 * the env based access in the unit test framework because we don't
 * have a fully formed CPUState
 */
static uint32_t r32_reg(CPUState *cs, const void *opaque)
{
    FakeCPU *f = &fcpu;
    int index = GPOINTER_TO_UINT(opaque);
    return f->reg32[index];
}
static void w32_reg(CPUState *cs, const void *opaque, uint32_t value)
{
    FakeCPU *f = &fcpu;
    int index = GPOINTER_TO_UINT(opaque);
    f->reg32[index] = value;
}
static uint64_t r64_reg(CPUState *cs, const void *opaque)
{
    FakeCPU *f = &fcpu;
    int index = GPOINTER_TO_UINT(opaque);
    return f->reg64[index];
}
static void w64_reg(CPUState *cs, const void *opaque, uint64_t value)
{
    FakeCPU *f = &fcpu;
    int index = GPOINTER_TO_UINT(opaque);
    f->reg64[index] = value;
}

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
/* "big" vectors */
static GByteArray *readbvec(CPUState *cs, const void *opaque)
{
    unsigned int idx = GPOINTER_TO_UINT(opaque);
    uint8_t *ptr = &fcpu.reg_big_vec[idx * BIG_VEC_SIZE];
    return g_byte_array_new_take(ptr, VEC_SIZE);
}
static void writebvec(CPUState *cs, const void *opaque, GByteArray *reg)
{
    unsigned int idx = GPOINTER_TO_UINT(opaque);
    uint8_t *dst = &fcpu.reg_big_vec[idx * BIG_VEC_SIZE];
    g_assert(reg->len == VEC_SIZE);
    memcpy(dst, reg->data, VEC_SIZE);
}

static void test_registration(void)
{
    /*
     * We cannot test register access for env based registers here as
     * we don't have a fully formed CPUState with attended
     * CPUArchState to contain the actual data. So for the purposes of
     * the test we will add the "core" registers with helpers.
     */
    reg_add_i32_virt("r32_0", NULL, GUINT_TO_POINTER(0), r32_reg, w32_reg, NULL);
    reg_add_i32_virt("r32_1", NULL, GUINT_TO_POINTER(1), r32_reg, w32_reg, NULL);

    reg_add_i64_virt("r64_0", NULL, GUINT_TO_POINTER(0), r64_reg, w64_reg, NULL);
    reg_add_i64_virt("r64_1", NULL, GUINT_TO_POINTER(1), r64_reg, w64_reg, NULL);

    reg_add_env("r64_0", "env_regs", offsetof(FakeCPU, reg64[0]), 8);
    reg_add_env("r64_1", "env_regs", offsetof(FakeCPU, reg64[1]), 8);

    reg_add_env("r32_0", "env_regs", offsetof(FakeCPU, reg32[0]), 4);
    reg_add_env("r32_1", "env_regs", offsetof(FakeCPU, reg32[1]), 4);

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
                   REG_VEC_ALL64 | REG_VEC_ALL32 |  REG_VEC_UINT8,
                   readvec, writevec, fmtvec);
    reg_add_vector("v1", "vectors", GUINT_TO_POINTER(1), VEC_SIZE * 8,
                   REG_VEC_ALL64 | REG_VEC_ALL32 |  REG_VEC_UINT8,
                   readvec, writevec, fmtvec);

    /* these are more like SVE in size */
    reg_add_vector("b0", "BigVectors", GUINT_TO_POINTER(0), BIG_VEC_SIZE * 8,
                   REG_VEC_ALL128 | REG_VEC_IEEEDOUBLE | REG_VEC_IEEESINGLE | REG_VEC_IEEEHALF,
                   readbvec, writebvec, NULL);
    reg_add_vector("b1", "BigVectors", GUINT_TO_POINTER(1), BIG_VEC_SIZE * 8,
                   REG_VEC_ALL128 | REG_VEC_IEEEDOUBLE | REG_VEC_IEEESINGLE | REG_VEC_IEEEHALF,
                   readbvec, writebvec, NULL);

    reg_finalize_definitions(NULL);

    g_assert(reg_get_number(NULL) == 4);
    g_assert(reg_get_number("env_regs") == 4);
    g_assert(reg_get_number("flags") == 2);
    g_assert(reg_get_number("vectors") == 2);
}

static void set_test_values(void)
{
    /* Some test data for registers */
    fcpu.reg32[0] = 0x12345678;
    fcpu.reg32[1] = 0xabcdef01;
    fcpu.reg64[0] = 0xabababababababab;
    fcpu.reg64[1] = 0xcccccccccccccccc;
}

static void test_dump(void)
{
    g_autofree char *tdir = g_strdup_printf("%s/registers.test.XXXXXX",
                                            g_get_tmp_dir());
    g_autofree char *tfile = NULL;
    gsize length;
    gchar *data;
    FILE *output;

    g_assert_nonnull(g_mkdtemp(tdir));
    tfile = g_strdup_printf("%s/output", tdir);
    output = g_fopen(tfile, "w");

    set_test_values();

    reg_cpu_dump_state(NULL, output, 0);

    fclose(output);

    /* Now read and check the results */
    g_assert(g_file_get_contents(tfile, &data, &length, NULL));
    g_assert_cmpstr(data, ==,
                    "r32_0=0x12345678 r32_1=0xabcdef01 "
                    "r64_0=0xabababababababab r64_1=0xcccccccccccccccc\n");
}

static void test_hmp_get(void)
{
    /* HMP stores everything as a 64 bit value */
    int64_t value;

    set_test_values();

    g_assert(reg_get_value_hmp(NULL, "r32_0", &value));
    g_assert(value == 0x12345678);
    g_assert(reg_get_value_hmp(NULL, "r32_1", &value));
    g_assert(value == 0xabcdef01);
    g_assert(reg_get_value_hmp(NULL, "r64_0", &value));
    g_assert(value == 0xabababababababab);
    g_assert(reg_get_value_hmp(NULL, "r64_1", &value));
    g_assert(value == 0xcccccccccccccccc);

    /* Also check we fail for fake registers */
    g_assert_false(reg_get_value_hmp(NULL, "fake_reg", &value));
}

static void test_reg_groups(void)
{
    g_assert(reg_get_group_handle("flags"));
    g_assert(reg_get_group_handle("vectors"));
    g_assert_false(reg_get_group_handle(NULL));
    g_assert_false(reg_get_group_handle("fake group"));
}

/*
 * gdbstub support
 */

void gdb_register_coprocessor(CPUState *cpu,
                              gdb_get_reg_cb get_reg, gdb_set_reg_cb set_reg,
                              int num_regs, const char *xml, int g_pos)
{
    /* do nothing stub */
}

static const GMarkupParser gdb_xml_parser = {

};

static bool do_one_gdb_xml(const char *group)
{
    g_autoptr(GMarkupParseContext) context = NULL;
    g_autoptr(GError) err = NULL;
    const char *xml = reg_gdb_get_dynamic_xml(NULL, group);
    bool ok;

    /* create a parser to validate the XML */
    context = g_markup_parse_context_new(&gdb_xml_parser, 0, NULL, NULL);
    ok = g_markup_parse_context_parse(context, xml, strlen(xml), &err);
    g_assert(ok);
    ok = g_markup_parse_context_end_parse(context, &err);

    if (g_test_verbose()) {
        fprintf(stderr, "%s: XML for %s\n%s", __func__, group, xml);
    }

    return ok;
}

static void test_gdb_xml(void)
{
    g_assert(do_one_gdb_xml("flags"));
    g_assert(do_one_gdb_xml("vectors"));
    g_assert(do_one_gdb_xml("BigVectors"));
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);
    g_test_add_func("/registers/registration", test_registration);
    g_test_add_func("/registers/dump", test_dump);
    g_test_add_func("/registers/hmp_get", test_hmp_get);
    g_test_add_func("/registers/groups", test_reg_groups);
    g_test_add_func("/registers/gdbxml", test_gdb_xml);
    g_test_run();
    return 0;
}
