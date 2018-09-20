/*
 * Test operations
 *
 * This is a simple example of using the test-op framework for
 * exercising tcg guests.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <inttypes.h>
#include <assert.h>
#include <string.h>
#include "test-ops.h"

/*
 * Test Operations
 */

op_value_t invert_uint32(op_value_t a)
{
    op_value_t r = { .u32 = ~a.u32 };
    return r;
}

op_value_t add_uint32(op_value_t a, op_value_t b)
{
    op_value_t r = { .u32 = a.u32 + b.u32 };
    return r;
}

/*
 * Test Data
 */

uint32_t uint32_sequence[] = {
    0x00000000,
    0x00000001,
    0x00000010,
    0x00000100,
    0x00001000,
    0x00010000,
    0x00100000,
    0x01000000,
    0x10000000,
    0x20000000,
    0x40000000,
    0x80000000,
    0x80000001,
    0x8000001f,
    0x8000003f,
    0x8000007f,
    0x800000ff,
    0x800001ff,
    0x8000031f,
    0x800007ff,
    0x80000fff,
    0x8000ffff,
    0x800fffff,
    0x80ffffff,
    0x8fffffff,
    0x9fffffff,
    0xafffffff,
    0xbfffffff,
    0xcfffffff,
    0xdfffffff,
    0xefffffff,
    0xffffffff,
};

/*
 * Test Definitions
 */

test_data_t u32_data = { sizeof(uint32_t), ARRAY_SIZE(uint32_sequence), &uint32_sequence[0]};

test_func_desc_t tests[] = {
    { ONE_OP, RES_INPUT,
      { .one = invert_uint32  },
      NULL,
      { &u32_data, NULL, NULL }, NULL,
      "invert_uint32", "invert 32 bit uints"
    },
    { TWO_OP, RES_INPUT,
      { .two = add_uint32  },
      NULL,
      { &u32_data, &u32_data, NULL }, NULL,
      "add_uint32", "add to 32 bit uints"
    },
};


static void run_tests(char *tname) {
    int i;

    for (i = 0; i < ARRAY_SIZE(tests); ++i) {
        test_func_desc_t *t = &tests[i];

        if (tname && strcmp(t->name, tname)!=0)
            continue;

        printf("running %s test (%s)\n", t->name, t->desc);
        switch (t->fn_type) {
        case ONE_OP:
            run_single_op_test(t);
            break;
        case TWO_OP:
            run_two_op_test(t, NULL, 0);
            break;
        default:
            break;
        }
    }
}


static const char commands_string[] =
    " -t = dump result table\n";

static void usage_complete(char *argv[])
{
    int i;

    fprintf(stderr, "Usage: %s [options] [testname]\n", argv[0]);
    fprintf(stderr, "options:\n%s\n", commands_string);
    fprintf(stderr, "tests:\n");

    for (i = 0; i < ARRAY_SIZE(tests); ++i) {
        test_func_desc_t *t = &tests[i];
        fprintf(stderr, "  %20s (%s)\n", t->name, t->desc);
    }
}


static char * parse_args(int argc, char *argv[])
{
    int c;
    char *testname = NULL;

    for (;;) {
        c = getopt(argc, argv, "h");
        if (c < 0) {
            break;
        }
        switch (c) {
        case 'h':
            usage_complete(argv);
            exit(0);
        }
    }

    if (optind < argc) {
        testname = (char *) argv[optind];
    }

    return testname;
}

int main(int argc, char *argv[])
{
    char *tname = parse_args(argc, argv);
    run_tests(tname);
    return 0;
}
