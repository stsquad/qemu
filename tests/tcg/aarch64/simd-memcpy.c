/*
 * Memcpy Test and Benchmark using SIMD instructions
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <time.h>
#include <getopt.h>
#include <stdbool.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

typedef void (test_func)(void *dest, void *src, size_t n);

volatile uint64_t dump;

typedef struct {
    char        *name;
    char        *desc;
    test_func   *func;
} testdef_t;

/* Generate an array of psuedo-random data to feed into our
 * vectorisable kernel. The array needs to be nicely aligned to
 * help the vectoriser
 */
static void * get_aligned_block(size_t size)
{
    void *p;
    if (posix_memalign(&p, 16, size)!=0) {
        fprintf(stderr, "%s: failed to allocate memory\n", __func__);
        abort();
    }
    return p;
}

static void * __attribute__ ((noinline)) get_data(uint32_t seed, size_t size)
{
    unsigned long i;
    long int rseed = random();
    uint8_t *data = get_aligned_block(size * sizeof(uint8_t));

    for (i=0; i < size; i = i + 4)
    {
        data[i]   = ((rseed >>  8) ^ (seed >>  0) ^ i) & 0xff;
        data[i+1] = ((rseed >> 16) ^ (seed >>  8) ^ i) & 0xff;
        data[i+2] = ((rseed >> 24) ^ (seed >> 16) ^ i) & 0xff;
        data[i+3] = ((rseed >>  0) ^ (seed >> 24) ^ i) & 0xff;
    }

    return data;
}


static inline int64_t get_clock(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

/* Tests */
static void memcpy_libc(void *dest, void *src, size_t n)
{
    memcpy(dest, src, n);
}

static void memcpy_intreg(void *dest, void *src, size_t n)
{
    int i;

    for (i=0; i < n / 8; i++) {
        asm("ldr x0, [%[src], %[idx], lsl #3]\n\t"
            "str x0, [%[dst], %[idx], lsl #3]\n\t"
            : /* no outputs */
            : [src] "r" (src), [dst] "r" (dest), [idx] "r" (i)
            : "x0", "memory");
    }
}

static void memcpy_pairreg(void *dest, void *src, size_t n)
{
    int i;
    register void *d asm("x2") = dest;
    register void *s asm("x3") = src;

    for (i=0; i < n / 16; i++) {
        asm("ldp x0, x1, [x3], #16\n\t"
            "stp x0, x1, [x2], #16\n\t"
            : /* no outputs */
            : "r" (s), "r" (d)
            : "x0", "x1", "memory");
    }
}

static void memcpy_simd(void *dest, void *src, size_t n)
{
    int i;

    for (i=0; i < n / 16; i++) {
        asm("ldr q0, [%[src], %[idx], lsl #4]\n\t"
            "str q0, [%[dst], %[idx], lsl #4]\n\t"
            : /* no outputs */
            : [src] "r" (src), [dst] "r" (dest), [idx] "r" (i)
            : "q0", "memory");
    }
}

static void memcpy_simdpair(void *dest, void *src, size_t n)
{
    int i;
    register void *d asm("x2") = dest;
    register void *s asm("x3") = src;

    for (i=0; i < n / 32; i++) {
        asm("ldp q0, q1, [x3], #32\n\t"
            "stp q0, q1, [x2], #32\n\t"
            : /* no outputs */
            : "r" (s), "r" (d)
            : "q0", "q1", "memory");
    }
}

static testdef_t tests[] = {
    {
        .name = "libc",
        .desc = "use built-in memcpy",
        .func = memcpy_libc
    },
    {
        .name = "intreg",
        .desc = "simple loop using single integer register",
        .func = memcpy_intreg
    },
    {
        .name = "intpair",
        .desc = "simple loop using pairs of integer registers",
        .func = memcpy_pairreg
    },
    {
        .name = "simdreg",
        .desc = "simple loop using single AdvSIMD register",
        .func = memcpy_simd
    },
    {
        .name = "simdpair",
        .desc = "simple loop using pair of AdvSIMD registers",
        .func = memcpy_simdpair
    },
};

testdef_t * get_test(char *name) {
    int i;
    testdef_t *t = NULL;

    for (i = 0; i<ARRAY_SIZE(tests); i++) {
        if (strcmp(tests[i].name, name)==0) {
            t = &tests[i];
            break;
        }
    }
    return t;
}

void usage(void) {
    int i;

    fprintf(stderr, "Usage: simd-memcpy [-b bytes] <testname> [testname...]\n\n");
    fprintf(stderr, "Tests:\n");
    for (i = 0; i<ARRAY_SIZE(tests); i++) {
        fprintf(stderr, "%s: %s (%p)\n",
                tests[i].name, tests[i].desc, tests[i].func);
    }
}

int main(int argc, char **argv)
{
    testdef_t *test;
    unsigned long bytes = (1024 * 1024 * 1024);
    void *src, *dest;
    int i, fail_count = 0;

    for (;;) {
        static struct option longopts[] = {
            {"help", no_argument, 0, '?'},
            {"bytes", optional_argument, 0, 'b'},
            {0, 0, 0, 0}
        };
        int optidx = 0;
        int c = getopt_long(argc, argv, "hb", longopts, &optidx);
        if (c == -1) {
            break;
        }

        switch (c) {
            case 0:
            {
                /* flag set by getopt_long, do nothing */
                break;
            }
            case 'b':
            {
                bytes = strtoul(optarg, NULL, 10);
                break;
            }
            case 'h':
            case '?':
            {
                usage();
                exit(1);
            }
        }
    }

    if (optind >= argc) {
        fprintf(stderr, "No tests specified!\n");
        exit(1);
    }

    /* Initialise start memory */
    src = get_data(0xdeadbeef, bytes);
    dest = get_aligned_block(bytes);

    for (i = optind; i < argc; i++)
    {
        test = get_test(argv[i]);

        if (!test) {
            fprintf(stderr,"%s: failed to find test: %s\n", __func__, argv[i]);
            abort();
        } else {
            int64_t start, end, elapsed;
            int check;

            /* reset dest */
            memset(dest, 0, bytes);

            start = get_clock();
            test->func(dest, src, bytes);
            end = get_clock();

            elapsed = end - start;

            /* check copy */
            check = memcmp(dest, src, bytes);
            if (check)
            {
                fail_count++;
                fprintf(stdout, "%s, FAILED (%d)\n", test->name, check);
            } else {
                fprintf(stdout, "%s, %ld, %ld kb/s\n", test->name,
                        elapsed, (bytes/1024)/(elapsed/1000/1000));
            }
        }
    }

    return fail_count == 0;
}
