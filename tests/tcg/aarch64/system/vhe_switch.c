/*
 * Exercise the various VHE switches
 *
 * Copyright (c) 2019 Linaro
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <inttypes.h>
#include <stdbool.h>
#include <minilib.h>

#define EL2_MAGIC 0x48595045
#define EL1_MAGIC 0x4b45524e
#define EL0_MAGIC 0x55534552

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

extern uintptr_t __svc_handler;
static uint32_t magic_test_value;

typedef void (*test_fn) (void);
typedef bool (*check_fn) (void);

typedef struct {
    const char *desc;
    test_fn test;
    check_fn check;
} TestDef;

/*
 * Helpers
 */
static int get_el(void)
{
    int el;
    asm("mrs %0, CurrentEL" :
        "=r" (el) :
        /* no inputs */
        );
    return el >> 2;
}

/* NB: only privileged ELs can call semihosting */
static void semi_exit(int code)
{
    uintptr_t exit_block[2] = {0x20026, code};
    register uintptr_t t asm("x0") = 0x18; /* SYS_REPORTEXC */
    register uintptr_t a0 asm("x1") = (uintptr_t) &exit_block;
    asm("hlt 0xf000"
        : /* no return */
        : "r" (t), "r" (a0));
}

/*
 * eret to EL0 and svc #0 return
 */

static void el0_function(void)
{
    magic_test_value = EL0_MAGIC;
    asm("hvc #0");
}

static bool check_eret_to_el0()
{
    return magic_test_value == EL0_MAGIC;
}

static void test_eret_to_el0(void)
{
    magic_test_value = EL2_MAGIC;
    asm("msr	spsr_el2, %0\n"
        "msr	elr_el2, %1\n"
        "eret" :
        /* no outputs */ :
        "r" (0), "r" (&el0_function) );
}

/*
 * eret to EL1 and svc #0 return
 */

static void el1_function(void)
{
    int el = get_el();

    if (el == 1) {
        magic_test_value = EL1_MAGIC;
    } else if (el > 0) {
        ml_printf("Weird EL%d\n", el);
    }
    asm("hvc #0");
}

static bool check_eret_to_el1()
{
    return magic_test_value == EL1_MAGIC;
}

static void test_eret_to_el1(void)
{
    magic_test_value = EL2_MAGIC;
    asm("msr	spsr_el2, %0\n"
        "msr	elr_el2, %1\n"
        "eret" :
        /* no outputs */ :
        "r" (5), "r" (&el1_function) );
}

static TestDef tests[] = {
    { "Check ERET to EL0", test_eret_to_el0, check_eret_to_el0 },
    { "Check ERET to EL1", test_eret_to_el1, check_eret_to_el1 },
};

static int test;
static int fails;

void test_run(void)
{
    ml_printf("%s: running test %s..", tests[test].desc);
    tests[test].test();
    /* never return */
}

void test_return_handler(void)
{
    bool pass = tests[test].check();
    ml_printf("%s\n", pass ? "Passed" : "Failed");
    if (!pass) {
        fails++;
    }
    test++;
    if (test > ARRAY_SIZE(tests)) {
        semi_exit(fails);
    } else {
        /* next test */
        test_run();
    }
}

int main(void)
{
    __svc_handler = (uintptr_t) &test_return_handler;

    ml_printf("Starting VHE Tests\n");
    test_run();
    /* never return - we should semi_exit() */
    return -1;
}
