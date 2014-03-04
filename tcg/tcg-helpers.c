/*
 * TCG Common Helper Functions
 *
 * Copyright (c) 2014
 * Written by Alex Bennée <alex.bennee@linaro.org>
 *
 * This code is licensed under the GNU GPL v2.
 *
 * This file contains common non-architecture specific helper
 * functions that can be used from TCG generated code. Currently these
 * are mainly helpful for debugging.
 */

#include <stdlib.h>
#include <stdio.h>

#include "cpu.h"
#include "exec/exec-all.h"
#include "helper.h"

/* TCG Value Dumpers */
uint32_t HELPER(dump_u32)(uint32_t val, void *string)
{
    fprintf(stderr,"%s %x\n", (char *) string, val);
    return val;
}

uint64_t HELPER(dump_u64)(uint64_t val, void *string)
{
    fprintf(stderr,"%s %lx\n", (char *) string, val);
    return val;
}
