/*
 * Test various atomic primitives
 *
 */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <pthread.h>
#include <sys/wait.h>
#include <errno.h>
#include <getopt.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

#define THREADS     4
#define ITERATIONS  50000

typedef void *(test_func)(void *arg);

uint64_t    u64_value;

typedef struct {
    char            *name;
    char            *desc;
    test_func       *func;
    unsigned long   value;
    unsigned long   result;
} testdef_t;

void * atomic_increment_u64(void *arg)
{
    unsigned long i;

    for (i = 0; i < ITERATIONS; i++) {
        __atomic_fetch_add(&u64_value, 1, __ATOMIC_SEQ_CST);
    }
    return NULL;
}

void * atomic_add_u64(void *arg)
{
    testdef_t *t = (testdef_t *) arg;
    unsigned long i;

    for (i = 0; i < ITERATIONS; i++) {
        __atomic_fetch_add(&u64_value, t->value, __ATOMIC_SEQ_CST);
    }
    return NULL;
}

static testdef_t tests[] = {
    {
        .name = "atomic-inc-u64",
        .desc = "increment a 64 bit value",
        .func = &atomic_increment_u64,
        .result = THREADS * ITERATIONS
    },
    {
        .name = "atomic-add-u64",
        .desc = "add to a 64 bit value",
        .func = &atomic_add_u64,
        .value = 4,
        .result = (THREADS * ITERATIONS * 4)
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

void usage(char *binname) {
    int i;

    fprintf(stderr, "Usage: %s <testname> [<testname> [<testname>...]]\n\n", binname);
    fprintf(stderr, "Tests:\n");
    for (i = 0; i < ARRAY_SIZE(tests); i++) {
        fprintf(stderr, "%s: %s (%p)\n",
                tests[i].name, tests[i].desc, tests[i].func);
    }
}

int main(int argc, char **argv)
{
    testdef_t *test;
    pthread_t threads[THREADS];
    int i, j;

    for (;;) {
        static struct option longopts[] = {
            {"help", no_argument, 0, '?'},
            {0, 0, 0, 0}
        };
        int optidx = 0;
        int c = getopt_long(argc, argv, "h", longopts, &optidx);
        if (c == -1) {
            break;
        }

        switch (c) {
            case 'h':
            case '?':
            {
                usage(argv[0]);
                exit(1);
            }
        }
    }

    if (optind >= argc) {
        fprintf(stderr, "No tests specified!\n");
        exit(1);
    }

    for (i = optind; i < argc; i++) {
        test = get_test(argv[i]);

        if (!test) {
            fprintf(stderr,"%s: failed to find test: %s\n", __func__, argv[i]);
            abort();
        } else {
            u64_value = 0;

            fprintf(stdout, "%s: starting test %s with %d threads\n",
                    __func__, test->name, THREADS);

            for (j = 0; j < THREADS; j++) {
                pthread_t *thr = &threads[j];
                int r;
                r = pthread_create(thr, NULL, test->func, test);
                if (r) {
                    fprintf(stderr, "%s: failed to create thread %d (%s)\n",
                            __func__, j, strerror(errno));
                    return -1;
                } else {
                    fprintf(stdout, "%s: thread %d -> %p\n", __func__, j, thr);
                }
            }

            for (j = 0; j < THREADS; j++) {
                pthread_t *thr = &threads[j];
                int r;
                r = pthread_join(*thr, NULL);
                if (r) {
                    fprintf(stderr, "%s: failed to join thread %d (%s)\n",
                            __func__, j, strerror(errno));
                    return -1;
                }
            }

            if (u64_value != test->result) {
                fprintf(stderr, "%s: failed %ld != %ld\n", __func__, u64_value,
                        test->result);
                return -1;
            } else {
                fprintf(stdout, "%s: %s passed (%ld)\n", __func__, test->name,
                        u64_value);
            }
        }
    }

    fprintf(stdout, "%s: all tests finished\n", __func__);
    return 0;
}
