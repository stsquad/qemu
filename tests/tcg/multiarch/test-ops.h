/*
 * Test Op Structures
 *
 * Copyright (c) 2018 Linaro Ltd
 * Written by Alex Bennée <alex.bennee@linaro.org>
 *
 * This code is licensed under the GNU GPL v2.
 */

#ifndef __TEST_OPS_H__
#define __TEST_OPS_H__

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>

typedef union {
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
    /* hmm how to ensure alignment??? */
    float    f32;
    double   f64;
} op_value_t;

/* prototypes for op-test function */
typedef op_value_t (*one_op_fn) (op_value_t first);
typedef op_value_t (*two_op_fn) (op_value_t first, op_value_t second);
typedef op_value_t (*three_op_fn) (op_value_t first, op_value_t second, op_value_t third);

/* optional setup to run before each iteration and check for
 * side-effects (e.g. FP flags)
 */
typedef void (*setup_fn) (uintptr_t data);
typedef bool (*check_side_fn) (void *reference);


typedef enum {
    ONE_OP,
    TWO_OP,
    THREE_OP
} fnop_type_t;

/* describe test data */
typedef struct {
    size_t esize;
    size_t length;
    void *data;
} test_data_t;

typedef enum {
    RES_INPUT = 0,  /* same size as input */
    RES_SINGLE, /* single precision float */
    RES_DOUBLE, /* double precision float */
} result_t;

typedef struct {
    fnop_type_t fn_type;
    result_t fn_res;
    union {
        one_op_fn one;
        two_op_fn two;
        three_op_fn *three;
    } fn;
    /* optional */
    check_side_fn check;

    test_data_t *in_data[3];
    test_data_t *out_data;

    char *name;
    char *desc;
} test_func_desc_t;

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

/*
 * Utility Functions and Test Runner
 */

static inline op_value_t get_data(test_data_t *d, int element)
{
    op_value_t val;

    switch (d->esize) {
    case 4:
        val.u32 = ((uint32_t *) d->data)[element];
        break;
    case 8:
        val.u64 = ((uint64_t *) d->data)[element];
        break;
    default:
        assert(false);
        break;
    }

    return val;
}

static inline void print_data(test_func_desc_t *t, op_value_t v, size_t esz, int i)
{
    bool show_float = false;
    int float_flags = 0;
    char flag_str[256] = "";

    if (i) {
        printf("IN%d: ", i);
    } else {
        printf("OUT: ");
        if (t->fn_res != RES_INPUT) {
            switch (t->fn_res) {
            case RES_DOUBLE:
                esz = sizeof(uint64_t);
                show_float = true;
                break;
            case RES_SINGLE:
                esz = sizeof(uint32_t);
                show_float = true;
                break;
            default:
                assert(false);
            }
        }
    }

#if defined(HANDLE_FLOAT_OPS)
    if (show_float) {
        float_flags = fetestexcept(FE_ALL_EXCEPT);
        if (float_flags) {
            snprintf(flag_str, sizeof(flag_str), "%s %s %s %s %s",
                     float_flags & FE_OVERFLOW ? "OVERFLOW" : "",
                     float_flags & FE_UNDERFLOW ? "UNDERFLOW" : "",
                     float_flags & FE_DIVBYZERO ? "DIV0" : "",
                     float_flags & FE_INEXACT ? "INEXACT" : "",
                     float_flags & FE_INVALID ? "INVALID" : "");
        } else {
            snprintf(flag_str, sizeof(flag_str), "OK");
        }
    }
#endif

    switch (esz) {
    case 8:
        if (show_float) {
            double as_float = (double) v.u64;
            printf("DOUBLE %02.20e / %#020" PRIx64 " (%#x => %s)\n",
                   as_float, v.u64, float_flags, flag_str);
        } else {
            printf("%#020" PRIx64"\n", v.u64);
        }
        break;
    case 4:
        if (show_float) {
            float as_float = (float) v.u32;
            printf("SINGLE %02.20e / %#010x  (%#x => %s)\n",
                   as_float, v.u32, float_flags, flag_str);
        } else {
            printf("%#020" PRIx64"\n", v.u64);
        }
        break;
    case 2:
        printf("%#04x\n", v.u16);
        break;
    default:
        assert(false);
    }
}

static inline bool run_single_op_test(test_func_desc_t *test)
{
    int i;
    test_data_t *input = test->in_data[0];

    printf("%s: %zd tests\n", test->name, input->length);

    for (i = 0; i < input->length; i++) {
        op_value_t in, out;
        in = get_data(input, i);
        print_data(test, in, input->esize, 1);
        out = test->fn.one(in);
        print_data(test, out, input->esize, 0);
    }

    return true;
}

static inline bool run_two_op_test(test_func_desc_t *test, setup_fn setup, uintptr_t udata)
{
    int i;
    test_data_t *a = test->in_data[0];
    test_data_t *b = test->in_data[1];
    size_t esz = a->esize;

    assert(a->length == b->length);

    printf("%s: %zd tests\n", test->name, a->length);
    for (i = 0; i < a->length; i++) {
        op_value_t in1, in2, out;
        in1 = get_data(a, i);
        print_data(test, in1, a->esize, 1);
        in2 = get_data(b, i);
        print_data(test, in2, b->esize, 2);
        out = test->fn.two(in1, in2);
        print_data(test, out, esz, 0);
    }


    return true;
}

#endif /* __TEST_OPS_H__ */
