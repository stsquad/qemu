#include "qemu/osdep.h"
#include <glib.h>
#include "qemu/qht.h"
#include "exec/tb-hash-xx.h"

#define N 5000

static struct qht ht;
static int32_t arr[N];

/*
 * We might be tempted to use val as the hash. However, val
 * could be 0, and all hashes passed to qht must be !0.
 */
static inline uint32_t hash_func(uint32_t val)
{
    return tb_hash_func5(val, 0, 0);
}

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
        hash = hash_func(i);

        qht_insert(&ht, &arr[i], hash);
    }
}

static void rm(int init, int end)
{
    int i;

    for (i = init; i < end; i++) {
        uint32_t hash;

        hash = hash_func(arr[i]);
        g_assert_true(qht_remove(&ht, &arr[i], hash));
    }
}

static void check(int a, int b, bool expected)
{
    double avg_chain;
    size_t n_head_buckets;
    int i;

    for (i = a; i < b; i++) {
        void *p;
        uint32_t hash;
        int32_t val;

        val = i;
        hash = hash_func(i);
        p = qht_lookup(&ht, is_equal, &val, hash);
        g_assert_true(!!p == expected);
    }
    avg_chain = qht_avg_bucket_chain_length(&ht, &n_head_buckets);
    g_assert_cmpfloat(avg_chain, >=, 1.0);
    g_assert_cmpuint(n_head_buckets, >, 0);
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
    g_assert_cmpuint(curr, ==, count);
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

static void test_default(void)
{
    qht_test(0);
}

static void test_mru_lookup(void)
{
    qht_test(QHT_MODE_MRU_LOOKUP);
}

static void test_mru_all(void)
{
    qht_test(QHT_MODE_MRU_LOOKUP | QHT_MODE_MRU_INSERT);
}

static void test_mru_all_resize(void)
{
    qht_test(QHT_MODE_MRU_LOOKUP | QHT_MODE_MRU_INSERT | QHT_MODE_AUTO_RESIZE);
}

static void test_mru_insert_resize(void)
{
    qht_test(QHT_MODE_AUTO_RESIZE | QHT_MODE_MRU_INSERT);
}

int main(int argc, char *argv[])
{
    g_test_init(&argc, &argv, NULL);
    g_test_add_func("/qht/mode/default", test_default);
    g_test_add_func("/qht/mode/mru_lookup", test_mru_lookup);
    g_test_add_func("/qht/mode/mru_all", test_mru_all);
    g_test_add_func("/qht/mode/mru_all_with_resize", test_mru_all_resize);
    g_test_add_func("/qht/mode/mru_insert_with_resize", test_mru_insert_resize);
    return g_test_run();
}
