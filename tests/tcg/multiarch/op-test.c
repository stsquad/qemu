/*
 * Test operations
 *
 * This is a simple example of using the test-op framework for
 * exercising tcg guests.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdlib.h>
#include <inttypes.h>
#include <assert.h>
#include <string.h>
#include "test-ops.h"

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

/*
 * Test Operations
 */

op_value_t invert_uint32(op_value_t a)
{
    op_value_t r = { .u32 = ~a.u32 };
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
    { ONE_OP,
      { .one = invert_uint32  },
      { &u32_data, NULL, NULL }, NULL,
      "invert_uint32", "invert 32 bit uints"
    },
};

/*
 * Utility Functions and Test Runner
 */

static op_value_t get_data(test_data_t *d, int element)
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

static void print_data(op_value_t v, size_t esz, int i)
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

static bool run_single_op_test(test_func_desc_t *test)
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
