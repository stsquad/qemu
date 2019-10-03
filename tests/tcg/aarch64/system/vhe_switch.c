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

extern uintptr_t __svc_handler;

static int get_el(void)
{
    int el;
    asm("mrs %0, CurrentEL" :
        "=r" (el) :
        /* no inputs */
        );
    return el >> 2;
}

static void el0_function(int magic)
{
    ml_printf("%s: with %x\n", __func__, magic);
    asm("svc #0");
}

static bool test_eret_to_el0_return()
{
    ml_printf("%s: at EL%d\n", __func__, get_el());
    return true;
}

static bool test_eret_to_el0(void)
{
    ml_printf("%s: at EL%d\n", __func__, get_el());

    __svc_handler = (uintptr_t) &test_eret_to_el0_return;

    asm("msr	spsr_el2, %0\n"
        "msr	elr_el2, %1\n"
        "eret" :
        /* no outputs */ :
        "r" (0), "r" (&el0_function) );

    /* we should come back via eret_to_el0_return */
    return false;
}

int main(void)
{
    bool ok = true;

    ml_printf("Starting VHE Tests\n");
    ok = test_eret_to_el0();

    ml_printf("Test complete: %s\n", ok ? "PASSED" : "FAILED");
    return ok ? 0 : -1;
}
