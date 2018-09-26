/*
 * Test floating point operations
 *
 * Copyright (c) 2018
 * Written by Alex Bennée <alex.bennee@linaro.org>
 *
 * This code is licensed under the GNU GPL v2.
 */

#include <assert.h>
#include <string.h>
#include <fenv.h>

#define HANDLE_FLOAT_OPS 1

#include "test-ops.h"

op_value_t fadd_double(op_value_t a, op_value_t b)
{
    double da = *(double *) &a.u64;
    double db = *(double *) &b.u64;
    op_value_t r = { .f64 = (da + db) };
    return r;
}

op_value_t fdiv_double(op_value_t a, op_value_t b)
{
    double da = *(double *) &a.u64;
    double db = *(double *) &b.u64;
    op_value_t r = { .f64 = (da / db) };
    return r;
}

/* This allows us to initialise some doubles as pure hex */
typedef union {
    double d;
    uint64_t h;
} test_doubles;

test_doubles a_numbers[] = {
    { .h = 0x3ff0000000000003UL },
    { .h = 0x3ff0000000000003UL },
    { .h = 0x03250f416dcdc6d0UL }  /* fdiv64 bug #1793119 */
};

test_doubles b_numbers[] = {
    { .h = 0x40dffffffe000000UL },
    { .h = 0x40dffffffdfffffeUL },
    { .h = 0x00029f4e5837c977UL }
};

test_doubles fadd_results[] = {
    { .h = 0x0080cffffffe000000UL },
    { .h = 0x0080cffffffdfff800UL },
    { .h = 0x0080cffffffdfff800UL },
};

test_doubles fdiv_results[] = {
    { .h = 0x0080cffffffe000000UL },
    { .h = 0x0080cffffffdfff800UL },
    { .h = 0x0043300fde9cbcf023UL },
};

/* fpcr = 0x00000000. */
/* x = 0x03250f416dcdc6d0. y = 0x00029f4e5837c977. r = 0x43300fde9cbcf023. q = 0x43300fde9cbcf023. */

test_data_t a_data = { sizeof(test_doubles), ARRAY_SIZE(a_numbers), &a_numbers[0]};
test_data_t b_data = { sizeof(test_doubles), ARRAY_SIZE(b_numbers), &b_numbers[0]};
test_data_t fadd_data = { sizeof(test_doubles), ARRAY_SIZE(fadd_results), &fadd_results[0]};
test_data_t fdiv_data = { sizeof(test_doubles), ARRAY_SIZE(fdiv_results), &fdiv_results[0]};

test_func_desc_t tests[] = {
    { TWO_OP, RES_INPUT,
      { .two = fadd_double },
      NULL,
      { &a_data, &b_data, NULL }, &fadd_data,
      "fadd_double", "add two double-precision floats"
    },
    { TWO_OP, RES_INPUT,
      { .two = fdiv_double },
      NULL,
      { &a_data, &b_data, NULL }, &fdiv_data,
      "fdiv_double", "divide one double-precision float by another"
    },
};

/* setup function called before each test */
static void setup_fpu(uintptr_t data)
{
    int rmode = (int) data;
    fesetround(rmode);
    feclearexcept(FE_ALL_EXCEPT);
}

static int rmodes[] = { FE_DOWNWARD };

static void run_tests(char *test, int index) {
    int i, j;

    for (i = 0; i < ARRAY_SIZE(tests); ++i) {
        test_func_desc_t *t = &tests[i];

        if (test && strcmp(t->name, test)!=0) {
            continue;
        }

        for (j = 0; j < ARRAY_SIZE(rmodes); j++) {
            switch (t->fn_type) {
            case TWO_OP:
                run_two_op_test(t, index, &setup_fpu, rmodes[j]);
                break;
            default:
                assert(false);
                break;
            }
        }
    }
}

int main(int argc, char *argv[])
{
    char *test = NULL;
    int index = 0;

    if (argc >= 2) {
        test = argv[1];
    }

    if (argc == 3) {
        index = strtol(argv[2], 0, 10);
    }

    run_tests(test, index);
    return 0;
}
