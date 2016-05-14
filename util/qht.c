/*
 * qht.c - QEMU Hash Table, designed to scale for read-mostly workloads.
 *
 * Copyright (C) 2016, Emilio G. Cota <cota@braap.org>
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 *
 * Assumptions:
 * - Writers and iterators must take an external lock.
 * - NULL cannot be inserted as a pointer value.
 * - Duplicate pointer values cannot be inserted.
 *
 * Features:
 * - Optional auto-resizing: the hash table resizes up if the load surpasses
 *   a certain threshold. Resizing is done concurrently with readers.
 *
 * The key structure is the bucket, which is cacheline-sized. Buckets
 * contain a few hash values and pointers; the u32 hash values are stored in
 * full so that resizing is fast. Having this structure instead of directly
 * chaining items has three advantages:
 * - Failed lookups fail fast, and touch a minimum number of cache lines.
 * - Resizing the hash table with concurrent lookups is easy.
 *
 * There are two types of buckets:
 * 1. "head" buckets are the ones allocated in the array of buckets in qht_map.
 * 2. all "non-head" buckets (i.e. all others) are members of a chain that
 *    starts from a head bucket.
 * Note that the seqlock and spinlock of a head bucket applies to all buckets
 * chained to it; these two fields are unused in non-head buckets.
 *
 * On removals, we move the last valid item in the chain to the position of the
 * just-removed entry. This makes lookups slightly faster, since the moment an
 * invalid entry is found, the (failed) lookup is over.
 *
 * Resizing is done by taking all spinlocks (so that no readers-turned-writers
 * can race with us) and then placing all elements into a new hash table. Last,
 * the ht->map pointer is set, and the old map is freed once no RCU readers can
 * see it anymore.
 *
 * Related Work:
 * - Idea of cacheline-sized buckets with full hashes taken from:
 *   David, Guerraoui & Trigonakis, "Asynchronized Concurrency:
 *   The Secret to Scaling Concurrent Search Data Structures", ASPLOS'15.
 * - Why not RCU-based hash tables? They would allow us to get rid of the
 *   seqlock, but resizing would take forever since RCU read critical
 *   sections in QEMU take quite a long time.
 *   More info on relativistic hash tables:
 *   + Triplett, McKenney & Walpole, "Resizable, Scalable, Concurrent Hash
 *     Tables via Relativistic Programming", USENIX ATC'11.
 *   + Corbet, "Relativistic hash tables, part 1: Algorithms", @ lwn.net, 2014.
 *     https://lwn.net/Articles/612021/
 */
#include "qemu/qht.h"
#include "qemu/atomic.h"

//#define QHT_DEBUG

/*
 * We want to avoid false sharing of cache lines. Most systems have 64-byte
 * cache lines so we go with it for simplicity.
 *
 * Note that systems with smaller cache lines will be fine (the struct is
 * almost 64-bytes); systems with larger cache lines might suffer from
 * some false sharing.
 */
#define QHT_BUCKET_ALIGN 64

/* define these to keep sizeof(qht_bucket) within QHT_BUCKET_ALIGN */
#if HOST_LONG_BITS == 32
#define QHT_BUCKET_ENTRIES 6
#else /* 64-bit */
#define QHT_BUCKET_ENTRIES 4
#endif

struct qht_bucket {
    QemuSpin lock;
    QemuSeqLock sequence;
    uint32_t hashes[QHT_BUCKET_ENTRIES];
    void *pointers[QHT_BUCKET_ENTRIES];
    struct qht_bucket *next;
} QEMU_ALIGNED(QHT_BUCKET_ALIGN);

QEMU_BUILD_BUG_ON(sizeof(struct qht_bucket) > QHT_BUCKET_ALIGN);

/**
 * struct qht_map - structure to track an array of buckets
 * @rcu: used by RCU. Keep it as the top field in the struct to help valgrind
 *       find the whole struct.
 * @buckets: array of head buckets. It is constant once the map is created.
 * @n: number of head buckets. It is constant once the map is created.
 * @n_added_buckets: number of added (i.e. "non-head") buckets
 * @n_added_buckets_threshold: threshold to trigger an upward resize once the
 *                             number of added buckets surpasses it.
 *
 * Buckets are tracked in what we call a "map", i.e. this structure.
 */
struct qht_map {
    struct rcu_head rcu;
    struct qht_bucket *buckets;
    size_t n;
    size_t n_added_buckets;
    size_t n_added_buckets_threshold;
};

/* trigger a resize when n_added_buckets > n_buckets / div */
#define QHT_NR_ADDED_BUCKETS_THRESHOLD_DIV 8

static void qht_do_resize(struct qht *ht, size_t n);

static inline struct qht_map *qht_map__atomic_mb(const struct qht *ht)
{
    struct qht_map *map;

    map = atomic_read(&ht->map);
    /* paired with smp_wmb() before setting ht->map */
    smp_rmb();
    return map;
}

/* helper for lockless bucket chain traversals */
static inline
struct qht_bucket *bucket_next__atomic_mb(const struct qht_bucket *b)
{
    struct qht_bucket *ret;

    ret = atomic_read(&b->next);
    /*
     * This barrier guarantees that we will read a properly initialized b->next;
     * it is paired with an smp_wmb() before setting b->next.
     */
    smp_rmb();
    return ret;
}

#ifdef QHT_DEBUG
static void qht_bucket_debug(struct qht_bucket *b)
{
    bool seen_empty = false;
    bool corrupt = false;
    int i;

    do {
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            if (b->pointers[i] == NULL) {
                seen_empty = true;
                continue;
            }
            if (seen_empty) {
                fprintf(stderr, "%s: b: %p, pos: %i, hash: 0x%x, p: %p\n",
                       __func__, b, i, b->hashes[i], b->pointers[i]);
                corrupt = true;
            }
        }
        b = b->next;
    } while (b);
    assert(!corrupt);
}

static void qht_map_debug(struct qht_map *map)
{
    int i;

    for (i = 0; i < map->n; i++) {
        qht_bucket_debug(&map->buckets[i]);
    }
}
#else
static inline void qht_bucket_debug(struct qht_bucket *b)
{ }

static inline void qht_map_debug(struct qht_map *map)
{ }
#endif /* QHT_DEBUG */

static inline size_t qht_elems_to_buckets(size_t n_elems)
{
    return pow2ceil(n_elems / QHT_BUCKET_ENTRIES);
}

static inline void qht_head_init(struct qht_bucket *b)
{
    memset(b, 0, sizeof(*b));
    qemu_spin_init(&b->lock);
    seqlock_init(&b->sequence);
}

static inline
struct qht_bucket *qht_map_to_bucket(struct qht_map *map, uint32_t hash)
{
    return &map->buckets[hash & (map->n - 1)];
}

/* acquire all bucket locks from a map */
static void qht_map_lock_buckets(struct qht_map *map)
{
    size_t i;

    for (i = 0; i < map->n; i++) {
        struct qht_bucket *b = &map->buckets[i];

        qemu_spin_lock(&b->lock);
    }
}

static void qht_map_unlock_buckets(struct qht_map *map)
{
    size_t i;

    for (i = 0; i < map->n; i++) {
        struct qht_bucket *b = &map->buckets[i];

        qemu_spin_unlock(&b->lock);
    }
}

static inline bool qht_map_needs_resize(struct qht_map *map)
{
    return atomic_read(&map->n_added_buckets) > map->n_added_buckets_threshold;
}

static inline void qht_chain_destroy(struct qht_bucket *head)
{
    struct qht_bucket *curr = head->next;
    struct qht_bucket *prev;

    while (curr) {
        prev = curr;
        curr = curr->next;
        qemu_vfree(prev);
    }
}

/* pass only an orphan map */
static void qht_map_destroy(struct qht_map *map)
{
    size_t i;

    for (i = 0; i < map->n; i++) {
        qht_chain_destroy(&map->buckets[i]);
    }
    qemu_vfree(map->buckets);
    g_free(map);
}

static void qht_map_reclaim(struct rcu_head *rcu)
{
    struct qht_map *map = container_of(rcu, struct qht_map, rcu);

    qht_map_destroy(map);
}

static struct qht_map *qht_map_create(size_t n)
{
    struct qht_map *map;
    size_t i;

    map = g_malloc(sizeof(*map));
    map->n = n;

    map->n_added_buckets = 0;
    map->n_added_buckets_threshold = n / QHT_NR_ADDED_BUCKETS_THRESHOLD_DIV;

    /* let tiny hash tables to at least add one non-head bucket */
    if (unlikely(map->n_added_buckets_threshold == 0)) {
        map->n_added_buckets_threshold = 1;
    }

    map->buckets = qemu_memalign(QHT_BUCKET_ALIGN, sizeof(*map->buckets) * n);
    for (i = 0; i < n; i++) {
        qht_head_init(&map->buckets[i]);
    }
    return map;
}

static inline void qht_publish(struct qht *ht, struct qht_map *new)
{
    /* Readers should see a properly initialized map; pair with smp_rmb() */
    smp_wmb();
    atomic_set(&ht->map, new);
}

void qht_init(struct qht *ht, size_t n_elems, unsigned int mode)
{
    struct qht_map *map;
    size_t n = qht_elems_to_buckets(n_elems);

    ht->mode = mode;
    map = qht_map_create(n);
    qht_publish(ht, map);
}

/* call only when there are no readers left */
void qht_destroy(struct qht *ht)
{
    qht_map_destroy(ht->map);
    memset(ht, 0, sizeof(*ht));
}

static void qht_bucket_reset(struct qht_bucket *head)
{
    struct qht_bucket *b = head;
    int i;

    qemu_spin_lock(&head->lock);
    seqlock_write_begin(&head->sequence);
    do {
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            if (b->pointers[i] == NULL) {
                goto done;
            }
            atomic_set(&b->hashes[i], 0);
            atomic_set(&b->pointers[i], NULL);
        }
        b = b->next;
    } while (b);
 done:
    seqlock_write_end(&head->sequence);
    qemu_spin_unlock(&head->lock);
}

/* call with an external lock held */
void qht_reset(struct qht *ht)
{
    struct qht_map *map = ht->map;
    size_t i;

    for (i = 0; i < map->n; i++) {
        qht_bucket_reset(&map->buckets[i]);
    }
    qht_map_debug(map);
}

/* call with an external lock held */
bool qht_reset_size(struct qht *ht, size_t n_elems)
{
    struct qht_map *old = ht->map;

    qht_reset(ht);
    if (old->n == qht_elems_to_buckets(n_elems)) {
        return false;
    }
    qht_init(ht, n_elems, ht->mode);
    call_rcu1(&old->rcu, qht_map_reclaim);
    return true;
}

static inline
void *qht_do_lookup(struct qht_bucket *head, qht_lookup_func_t func,
                    const void *userp, uint32_t hash)
{
    struct qht_bucket *b = head;
    int i;

    do {
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            if (atomic_read(&b->hashes[i]) == hash) {
                void *p = atomic_read(&b->pointers[i]);

                if (likely(p) && likely(func(p, userp))) {
                    return p;
                }
            }
        }
        b = bucket_next__atomic_mb(b);
    } while (b);

    return NULL;
}

static __attribute__((noinline))
void *qht_lookup__slowpath(struct qht_bucket *b, qht_lookup_func_t func,
                           const void *userp, uint32_t hash)
{
    uint32_t version;
    void *ret;

    do {
        version = seqlock_read_begin(&b->sequence);
        ret = qht_do_lookup(b, func, userp, hash);
    } while (seqlock_read_retry(&b->sequence, version));
    return ret;
}

void *qht_lookup(struct qht *ht, qht_lookup_func_t func, const void *userp,
                 uint32_t hash)
{
    struct qht_bucket *b;
    struct qht_map *map;
    uint32_t version;
    void *ret;

    map = qht_map__atomic_mb(ht);
    b = qht_map_to_bucket(map, hash);

    version = seqlock_read_begin(&b->sequence);
    ret = qht_do_lookup(b, func, userp, hash);
    if (likely(!seqlock_read_retry(&b->sequence, version))) {
        return ret;
    }
    /*
     * Removing the do/while from the fastpath gives a 4% perf. increase when
     * running a 100%-lookup microbenchmark.
     */
    return qht_lookup__slowpath(b, func, userp, hash);
}

/* call with head->lock held */
static bool qht_insert__locked(struct qht *ht, struct qht_map *map,
                               struct qht_bucket *head, void *p, uint32_t hash,
                               bool *needs_resize)
{
    struct qht_bucket *b = head;
    struct qht_bucket *prev = NULL;
    struct qht_bucket *new = NULL;
    int i;

    for (;;) {
        if (b == NULL) {
            b = qemu_memalign(QHT_BUCKET_ALIGN, sizeof(*b));
            memset(b, 0, sizeof(*b));
            new = b;
            atomic_inc(&map->n_added_buckets);
            if (unlikely(qht_map_needs_resize(map)) && needs_resize) {
                *needs_resize = true;
            }
        }
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            if (b->pointers[i]) {
                if (unlikely(b->pointers[i] == p)) {
                    return false;
                }
                continue;
            }
            /* found an empty key: acquire the seqlock and write */
            seqlock_write_begin(&head->sequence);
            if (new) {
                /*
                 * This barrier is paired with smp_rmb() after reading
                 * b->next when not holding b->lock.
                 */
                smp_wmb();
                atomic_set(&prev->next, b);
            }
            atomic_set(&b->hashes[i], hash);
            atomic_set(&b->pointers[i], p);
            seqlock_write_end(&head->sequence);
            return true;
        }
        prev = b;
        b = b->next;
    }
}

/* call with an external lock held */
bool qht_insert(struct qht *ht, void *p, uint32_t hash)
{
    struct qht_map *map = ht->map;
    struct qht_bucket *b = qht_map_to_bucket(map, hash);
    bool needs_resize = false;
    bool ret;

    /* NULL pointers are not supported */
    assert(p);

    qemu_spin_lock(&b->lock);
    ret = qht_insert__locked(ht, map, b, p, hash, &needs_resize);
    qht_bucket_debug(b);
    qemu_spin_unlock(&b->lock);

    if (unlikely(needs_resize) && ht->mode & QHT_MODE_AUTO_RESIZE) {
        qht_do_resize(ht, map->n * 2);
    }
    return ret;
}

static inline bool qht_entry_is_last(struct qht_bucket *b, int pos)
{
    if (pos == QHT_BUCKET_ENTRIES - 1) {
        if (b->next == NULL) {
            return true;
        }
        return b->next->pointers[0] == NULL;
    }
    return b->pointers[pos + 1] == NULL;
}

static void
qht_entry_move(struct qht_bucket *to, int i, struct qht_bucket *from, int j)
{
    assert(!(to == from && i == j));
    assert(to->pointers[i] == NULL);
    assert(from->pointers[j]);

    atomic_set(&to->hashes[i], from->hashes[j]);
    atomic_set(&to->pointers[i], from->pointers[j]);

    atomic_set(&from->hashes[j], 0);
    atomic_set(&from->pointers[j], NULL);
}

/*
 * Find the last valid entry in @head, and swap it with @orig[pos], which has
 * just been invalidated.
 */
static inline void qht_bucket_fill_hole(struct qht_bucket *orig, int pos)
{
    struct qht_bucket *b = orig;
    struct qht_bucket *prev = NULL;
    int i;

    if (qht_entry_is_last(orig, pos)) {
        return;
    }
    do {
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            if (b->pointers[i] || (b == orig && i == pos)) {
                continue;
            }
            if (i > 0) {
                return qht_entry_move(orig, pos, b, i - 1);
            }
            assert(prev);
            return qht_entry_move(orig, pos, prev, QHT_BUCKET_ENTRIES - 1);
        }
        prev = b;
        b = b->next;
    } while (b);
    /* no free entries other than orig[pos], so swap it with the last one */
    qht_entry_move(orig, pos, prev, QHT_BUCKET_ENTRIES - 1);
}

/* call with b->lock held */
static inline
bool qht_remove__locked(struct qht_map *map, struct qht_bucket *head,
                        const void *p, uint32_t hash)
{
    struct qht_bucket *b = head;
    int i;

    do {
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            void *q = b->pointers[i];

            if (unlikely(q == NULL)) {
                return false;
            }
            if (q == p) {
                assert(b->hashes[i] == hash);
                seqlock_write_begin(&head->sequence);
                atomic_set(&b->hashes[i], 0);
                atomic_set(&b->pointers[i], NULL);
                qht_bucket_fill_hole(b, i);
                seqlock_write_end(&head->sequence);
                return true;
            }
        }
        b = b->next;
    } while (b);
    return false;
}

/* call with an external lock held */
bool qht_remove(struct qht *ht, const void *p, uint32_t hash)
{
    struct qht_map *map = ht->map;
    struct qht_bucket *b = qht_map_to_bucket(map, hash);
    bool ret;

    qemu_spin_lock(&b->lock);
    ret = qht_remove__locked(map, b, p, hash);
    qht_bucket_debug(b);
    qemu_spin_unlock(&b->lock);
    return ret;
}

static inline void qht_bucket_iter(struct qht *ht, struct qht_bucket *b,
                                   qht_iter_func_t func, void *userp)
{
    int i;

    do {
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            if (b->pointers[i] == NULL) {
                return;
            }
            func(ht, b->pointers[i], b->hashes[i], userp);
        }
        b = b->next;
    } while (b);
}

/* external lock + all of the map's locks held */
static inline void qht_map_iter__locked(struct qht *ht, struct qht_map *map,
                                        qht_iter_func_t func, void *userp)
{
    size_t i;

    for (i = 0; i < map->n; i++) {
        qht_bucket_iter(ht, &map->buckets[i], func, userp);
    }
}

/* call with an external lock held */
void qht_iter(struct qht *ht, qht_iter_func_t func, void *userp)
{
    qht_map_lock_buckets(ht->map);
    qht_map_iter__locked(ht, ht->map, func, userp);
    qht_map_unlock_buckets(ht->map);
}

static void qht_map_copy(struct qht *ht, void *p, uint32_t hash, void *userp)
{
    struct qht_map *new = userp;
    struct qht_bucket *b = qht_map_to_bucket(new, hash);

    /* no need to acquire b->lock because no thread has seen this map yet */
    qht_insert__locked(ht, new, b, p, hash, NULL);
}

/* call with an external lock held */
static void qht_do_resize(struct qht *ht, size_t n)
{
    struct qht_map *old = ht->map;
    struct qht_map *new;

    g_assert_cmpuint(n, !=, old->n);
    new = qht_map_create(n);
    qht_iter(ht, qht_map_copy, new);
    qht_map_debug(new);

    qht_publish(ht, new);
    call_rcu1(&old->rcu, qht_map_reclaim);
}

/* call with an external lock held */
bool qht_resize(struct qht *ht, size_t n_elems)
{
    size_t n = qht_elems_to_buckets(n_elems);

    if (n == ht->map->n) {
        return false;
    }
    qht_do_resize(ht, n);
    return true;
}

/* pass @stats to qht_statistics_destroy() when done */
void qht_statistics_init(struct qht *ht, struct qht_stats *stats)
{
    struct qht_map *map;
    int i;

    map = qht_map__atomic_mb(ht);

    stats->head_buckets = map->n;
    stats->used_head_buckets = 0;
    stats->entries = 0;
    qdist_init(&stats->chain);
    qdist_init(&stats->occupancy);

    for (i = 0; i < map->n; i++) {
        struct qht_bucket *head = &map->buckets[i];
        struct qht_bucket *b;
        uint32_t version;
        size_t buckets;
        size_t entries;
        int j;

        do {
            version = seqlock_read_begin(&head->sequence);
            buckets = 0;
            entries = 0;
            b = head;
            do {
                for (j = 0; j < QHT_BUCKET_ENTRIES; j++) {
                    if (atomic_read(&b->pointers[j]) == NULL) {
                        break;
                    }
                    entries++;
                }
                buckets++;
                b = bucket_next__atomic_mb(b);
            } while (b);
        } while (seqlock_read_retry(&head->sequence, version));

        if (entries) {
            qdist_inc(&stats->chain, buckets);
            qdist_inc(&stats->occupancy,
                      (double)entries / QHT_BUCKET_ENTRIES / buckets);
            stats->used_head_buckets++;
            stats->entries += entries;
        } else {
            qdist_inc(&stats->occupancy, 0);
        }
    }
}

void qht_statistics_destroy(struct qht_stats *stats)
{
    qdist_destroy(&stats->occupancy);
    qdist_destroy(&stats->chain);
}
