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
#include <assert.h>

typedef union {
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
} op_value_t;

/* prototypes for op-test function */
typedef op_value_t (*one_op_fn) (op_value_t first);
typedef op_value_t (*two_op_fn) (op_value_t first, op_value_t second);
typedef op_value_t (*three_op_fn) (op_value_t first, op_value_t second, op_value_t third);

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
    default:
        assert(false);
        break;
    }

    return val;
}

static inline void print_data(op_value_t v, size_t esz, int i)
{
    if (i) {
        printf("IN%d: ", i);
    } else {
        printf("OUT: ");
    }
    switch (esz) {
    case 4:
        printf("%#020" PRIx64"\n", v.u64);
        break;
    default:
        assert(false);
    }
}

static inline bool run_single_op_test(test_func_desc_t *test)
{
    int i;
    test_data_t *input = test->in_data[0];

    printf("test data of %ld elements\n", input->length);

    for (i = 0; i < input->length; i++) {
        op_value_t in, out;
        in = get_data(input, i);
        print_data(in, input->esize, 1);
        out = test->fn.one(in);
        print_data(out, input->esize, 0);
    }

    return true;
}

static inline bool run_two_op_test(test_func_desc_t *test)
{
    int i;
    test_data_t *a = test->in_data[0];
    test_data_t *b = test->in_data[1];
    size_t esz = a->esize;

    assert(a->length == b->length);

    printf("test data of %ld elements\n", a->length);
    for (i = 0; i < a->length; i++) {
        op_value_t in1, in2, out;
        in1 = get_data(a, i);
        print_data(in1, a->esize, 1);
        in2 = get_data(b, i);
        print_data(in2, b->esize, 2);
        out = test->fn.two(in1, in2);
        print_data(out, esz, 0);
    }

    return true;
}

#endif /* __TEST_OPS_H__ */
