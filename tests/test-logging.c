/*
 * logging unit-tests
 *
 * Copyright (C) 2016 Linaro Ltd.
 *
 *  Author: Alex Bennée <alex.bennee@linaro.org>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include <glib.h>

#include "qemu-common.h"
#include "include/qemu/log.h"

static void test_parse_range(void)
{
    qemu_set_dfilter_ranges("0x1000+0x100");

    g_assert_false(qemu_log_in_addr_range(0xfff));
    g_assert(qemu_log_in_addr_range(0x1000));
    g_assert(qemu_log_in_addr_range(0x1100));
    g_assert_false(qemu_log_in_addr_range(0x1101));

    qemu_set_dfilter_ranges("0x1000-0x100");

    g_assert_false(qemu_log_in_addr_range(0x1001));
    g_assert(qemu_log_in_addr_range(0x1000));
    g_assert(qemu_log_in_addr_range(0x0f00));
    g_assert_false(qemu_log_in_addr_range(0x0e99));

    qemu_set_dfilter_ranges("0x1000..0x1100");

    g_assert_false(qemu_log_in_addr_range(0xfff));
    g_assert(qemu_log_in_addr_range(0x1000));
    g_assert(qemu_log_in_addr_range(0x1100));
    g_assert_false(qemu_log_in_addr_range(0x1101));

    qemu_set_dfilter_ranges("0x1000..0x1000");

    g_assert_false(qemu_log_in_addr_range(0xfff));
    g_assert(qemu_log_in_addr_range(0x1000));
    g_assert_false(qemu_log_in_addr_range(0x1001));

    qemu_set_dfilter_ranges("0x1000+0x100,0x2100-0x100,0x3000..0x3100");
    g_assert(qemu_log_in_addr_range(0x1050));
    g_assert(qemu_log_in_addr_range(0x2050));
    g_assert(qemu_log_in_addr_range(0x3050));
}

/* As the only real failure from a bad log filename path spec is
 * reporting to the user we have to use the g_test_trap_subprocess
 * mechanism and check no errors reported on stderr.
 */
#ifdef CONFIG_HAS_GLIB_SUBPROCESS_TESTS
static void test_parse_path(void)
{
    /* All these should work without issue */
    if (g_test_subprocess()) {
        qemu_set_log_filename("/tmp/qemu.log");
        qemu_set_log_filename("/tmp/qemu-%d.log");
        qemu_set_log_filename("/tmp/qemu.log.%d");
        return;
    }

    g_test_trap_subprocess (NULL, 0, 0);
    g_test_trap_assert_passed();
    g_test_trap_assert_stdout("");
    g_test_trap_assert_stderr("");
}

static void test_parse_invalid_path(void)
{
    if (g_test_subprocess()) {
        qemu_set_log_filename("/tmp/qemu-%d%d.log");
        return;
    }

    g_test_trap_subprocess (NULL, 0, 0);
    g_test_trap_assert_passed();
    g_test_trap_assert_stdout("");
    g_test_trap_assert_stderr("Bad logfile format: /tmp/qemu-%d%d.log\n");
}
#endif /* CONFIG_HAS_GLIB_SUBPROCESS_TESTS */

int main(int argc, char **argv)
{
    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/logging/parse_range", test_parse_range);

#ifdef CONFIG_HAS_GLIB_SUBPROCESS_TESTS
    g_test_add_func("/logging/parse_path", test_parse_path);
    g_test_add_func("/logging/parse_invalid_path", test_parse_invalid_path);
#endif

    return g_test_run();
}
