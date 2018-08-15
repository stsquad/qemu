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
    double da = (double) a.u64;
    double db = (double) b.u64;
    op_value_t r = { .u64 = (uint64_t) (da + db ) };
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
};

test_doubles b_numbers[] = {
    { .h = 0x40dffffffe000000UL },
    { .h = 0x40dffffffdfffffeUL },
};

test_doubles fadd_results[] = {
    { .h = 0x0080cffffffe000000UL },
    { .h = 0x0080cffffffdfff800UL },
};

test_data_t a_data = { sizeof(test_doubles), ARRAY_SIZE(a_numbers), &a_numbers[0]};
test_data_t b_data = { sizeof(test_doubles), ARRAY_SIZE(b_numbers), &b_numbers[0]};
test_data_t fadd_data = { sizeof(test_doubles), ARRAY_SIZE(fadd_results), &fadd_results[0]};

test_func_desc_t tests[] = {
    { TWO_OP, RES_INPUT,
      { .two = fadd_double },
      { &a_data, &b_data, NULL }, &fadd_data,
      "fadd_double", "add two double-precision floats"
    },
};


static void run_tests(void) {
    int i;

    for (i = 0; i < ARRAY_SIZE(tests); ++i) {
        test_func_desc_t *t = &tests[i];

        fesetround(FE_DOWNWARD);
        feclearexcept(FE_ALL_EXCEPT);

        switch (t->fn_type) {
        case TWO_OP:
            run_two_op_test(t);
            break;
        default:
            assert(false);
            break;
        }
    }
}

int main(int argc, char *argv[])
{
    run_tests();
    return 0;
}
