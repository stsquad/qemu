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

typedef struct {
    fnop_type_t fn_type;
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


#endif /* __TEST_OPS_H__ */
