#include "qemu/osdep.h"
#include "qemu/xxhash.h"
#include "qemu/qht.h"

#define N 5000
#define SEED 1

static struct qht ht;
static int32_t arr[N];

static bool is_equal(const void *obj, const void *userp)
{
    const int32_t *a = obj;
    const int32_t *b = userp;

    return *a == *b;
}

static void insert(int a, int b)
{
    int i;

    for (i = a; i < b; i++) {
        uint32_t hash;

        arr[i] = i;
        hash = qemu_xxh32((uint32_t *)&arr[i], 1, SEED);

        qht_insert(&ht, &arr[i], hash);
    }
}

static void rm(int init, int end)
{
    int i;

    for (i = init; i < end; i++) {
        uint32_t hash;

        hash = qemu_xxh32((uint32_t *)&arr[i], 1, SEED);
        assert(qht_remove(&ht, &arr[i], hash));
    }
}

static void check(int a, int b, bool expected)
{
    int i;

    for (i = a; i < b; i++) {
        void *p;
        uint32_t hash;
        int32_t val;

        val = i;
        hash = qemu_xxh32((uint32_t *)&val, 1, SEED);
        p = qht_lookup(&ht, is_equal, &val, hash);
        assert(!!p == expected);
    }
}

static void count_func(struct qht *ht, void *p, uint32_t hash, void *userp)
{
    unsigned int *curr = userp;

    (*curr)++;
}

static void iter_check(unsigned int count)
{
    unsigned int curr = 0;

    qht_iter(&ht, count_func, &curr);
    assert(curr == count);
}

static void qht_test(unsigned int mode)
{
    qht_init(&ht, 0, mode);

    insert(0, N);
    check(0, N, true);
    check(-N, -1, false);
    iter_check(N);
    rm(1, 2);
    qht_reset_size(&ht, 0);
    check(0, N, false);

    qht_destroy(&ht);
}

int main(int argc, char *argv[])
{
    qht_test(0);
    qht_test(QHT_MODE_MRU_LOOKUP);
    qht_test(QHT_MODE_MRU_LOOKUP | QHT_MODE_MRU_INSERT);
    qht_test(QHT_MODE_MRU_LOOKUP | QHT_MODE_MRU_INSERT | QHT_MODE_AUTO_RESIZE);
    qht_test(QHT_MODE_AUTO_RESIZE | QHT_MODE_MRU_INSERT);
    qht_test(QHT_MODE_MRU_LOOKUP | QHT_MODE_MRU_INSERT);
    return 0;
}
