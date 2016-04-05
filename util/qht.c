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
 * - A hash value of 0 is invalid.
 * - NULL cannot be inserted as a pointer value.
 *
 * Features:
 * - Optional auto-resizing: the hash table resizes up if the load surpasses
 *   a certain threshold. Resizing is done concurrently with readers.
 * - Optional bucket MRU promotion policy.
 *
 * The key structure is the bucket, which is cacheline-sized. Buckets
 * contain a few hash values and pointers; the u32 hash values are stored in
 * full so that resizing is fast. Having this structure instead of directly
 * chaining items has three advantages:
 * - Failed lookups fail fast, and touch a minimum number of cache lines.
 * - Resizing the hash table with concurrent lookups is easy.
 * - We can have a few Most-Recently-Used (MRU) hash-pointer pairs in the same
 *   head bucket. This helps scalability, since MRU promotions (i.e. writes to
 *   the bucket) become less common.
 *
 * For concurrent lookups we use a per-bucket seqlock; per-bucket spinlocks
 * allow readers (lookups) to upgrade to writers and thus implement an MRU
 * promotion policy; these MRU-induced writes do not touch the cache lines of
 * other head buckets.
 *
 * Note that there are two types of buckets:
 * 1. "head" buckets are the ones allocated in the array of buckets in qht_map.
 * 2. all "non-head" buckets (i.e. all others) are members of a chain that
 *    starts from a head bucket.
 * Note that the seqlock and spinlock of a head bucket applies to all buckets
 * chained to it; these two fields are unused in non-head buckets.
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

static inline uint64_t qht_elems_to_buckets(uint64_t n_elems)
{
    return pow2ceil(n_elems / QHT_BUCKET_ENTRIES);
}

static inline void qht_head_init(struct qht_bucket *b)
{
    memset(b, 0, sizeof(*b));
    qemu_spinlock_init(&b->lock);
    seqlock_init(&b->sequence);
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
    uint64_t i;

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

static struct qht_map *qht_map_create(uint64_t n)
{
    struct qht_map *map;
    uint64_t i;

    assert(n <= (1ULL << 32)); /* we're using a 32-bit hash func */
    map = g_malloc(sizeof(*map));
    map->n = n;
    map->n_items = 0;
    map->n_items_threshold = n * QHT_BUCKET_ENTRIES / 2;
    map->buckets = qemu_memalign(QEMU_CACHELINE, sizeof(*map->buckets) * n);
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

void qht_init(struct qht *ht, uint64_t n_elems, unsigned int mode)
{
    struct qht_map *map;
    uint64_t n = qht_elems_to_buckets(n_elems);

    map = qht_map_create(n);
    ht->mode = mode;
    qht_publish(ht, map);
}

/* call only when there are no readers left */
void qht_destroy(struct qht *ht)
{
    qht_map_destroy(ht->map);
    memset(ht, 0, sizeof(*ht));
}

static void __qht_bucket_reset(struct qht_bucket *b)
{
    int i;

    do {
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            atomic_set(&b->hashes[i], 0);
            atomic_set(&b->pointers[i], NULL);
        }
        b = b->next;
    } while (b);
}

static void qht_bucket_reset(struct qht_bucket *b)
{
    qemu_spinlock_lock(&b->lock);
    seqlock_write_begin(&b->sequence);
    __qht_bucket_reset(b);
    seqlock_write_end(&b->sequence);
    qemu_spinlock_unlock(&b->lock);
}

/* call with an external lock held */
void qht_reset(struct qht *ht)
{
    struct qht_map *map = ht->map;
    uint64_t i;

    for (i = 0; i < map->n; i++) {
        qht_bucket_reset(&map->buckets[i]);
    }
}

/* call with an external lock held */
void qht_reset_size(struct qht *ht, uint64_t n_elems)
{
    struct qht_map *old = ht->map;

    qht_reset(ht);
    if (old->n == qht_elems_to_buckets(n_elems)) {
        return;
    }
    qht_init(ht, n_elems, ht->mode);
    call_rcu1(&old->rcu, qht_map_reclaim);
}

/* Can only be called when at least orig is the 3rd link i.e. head->2nd->orig */
static void
__qht_move_bucket_to_front(struct qht_bucket *head, struct qht_bucket *orig)
{
    struct qht_bucket *b = head->next;

    for (;;) {
        if (b->next == orig) {
            atomic_set(&b->next, orig->next);
            atomic_set(&orig->next, head->next);
            atomic_set(&head->next, orig);
            return;
        }
        b = b->next;
    }
}

static inline void __qht_bucket_mru_head(struct qht_bucket *b, int pos)
{
    uint32_t orig_hash = b->hashes[pos];
    void *orig_p = b->pointers[pos];
    int i;

    for (i = 0; i < pos; i++) {
        atomic_set(&b->hashes[i + 1], b->hashes[i]);
        atomic_set(&b->pointers[i + 1], b->pointers[i]);
    }
    atomic_set(&b->hashes[0], orig_hash);
    atomic_set(&b->pointers[0], orig_p);
}


/* call with head->lock held */
static inline void
__qht_bucket_mru(struct qht_bucket *head, struct qht_bucket *orig, int pos)
{
    uint32_t *dest_hash;
    void **dest_p;
    void *p;
    uint32_t hash;
    int i;

    if (head == orig) {
        return __qht_bucket_mru_head(head, pos);
    }
    for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
        if (i == QHT_BUCKET_ENTRIES - 1) {
            dest_hash = &orig->hashes[pos];
            dest_p = &orig->pointers[pos];
        } else {
            dest_hash = &head->hashes[i + 1];
            dest_p = &head->pointers[i + 1];
        }
        hash = *dest_hash;
        p = *dest_p;

        atomic_set(dest_hash, head->hashes[i]);
        atomic_set(dest_p, head->pointers[i]);

        atomic_set(&head->hashes[i], hash);
        atomic_set(&head->pointers[i], p);
    }
    if (head->next != orig) {
        __qht_move_bucket_to_front(head, orig);
    }
}

void qht_bucket_mru(struct qht_bucket *head, struct qht_bucket *orig,
                    const void *p, int pos)
{
    qemu_spinlock_lock(&head->lock);
    if (unlikely(orig->pointers[pos] != p)) {
        /* while we acquired the lock, the bucket was updated, so bail out */
        goto out;
    }
    seqlock_write_begin(&head->sequence);
    __qht_bucket_mru(head, orig, pos);
    seqlock_write_end(&head->sequence);
 out:
    qemu_spinlock_unlock(&head->lock);
}

/* call with b->lock held */
static void __qht_insert(struct qht *ht, struct qht_map *map,
                         struct qht_bucket *b, void *p, uint32_t hash)
{
    struct qht_bucket *head = b;
    struct qht_bucket *prev = NULL;
    struct qht_bucket *new = NULL;
    unsigned int count = 0;
    int i;

    for (;;) {
        if (b == NULL) {
            b = qemu_memalign(QEMU_CACHELINE, sizeof(*b));
            memset(b, 0, sizeof(*b));
            new = b;
        }
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            if (b->hashes[i]) {
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
            if ((ht->mode & QHT_MODE_MRU_INSERT) && count) {
                __qht_bucket_mru(head, b, i);
            }
            seqlock_write_end(&head->sequence);
            map->n_items++;
            return;
        }
        prev = b;
        b = b->next;
        count++;
    }
}

/* call with an external lock held */
void qht_insert(struct qht *ht, void *p, uint32_t hash)
{
    struct qht_map *map = ht->map;
    struct qht_bucket *b = qht_map_to_bucket(map, hash);

    qemu_spinlock_lock(&b->lock);
    __qht_insert(ht, map, b, p, hash);
    qemu_spinlock_unlock(&b->lock);

    if ((ht->mode & QHT_MODE_AUTO_RESIZE) &&
        unlikely(map->n_items > map->n_items_threshold)) {
        qht_grow(ht);
    }
}

/* call with b->lock held */
static inline bool __qht_remove(struct qht_map *map, struct qht_bucket *b,
                                const void *p, uint32_t hash)
{
    int i;

    do {
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            if (b->hashes[i] == hash && b->pointers[i] == p) {
                atomic_set(&b->hashes[i], 0);
                atomic_set(&b->pointers[i], NULL);
                map->n_items--;
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

    qemu_spinlock_lock(&b->lock);
    seqlock_write_begin(&b->sequence);
    ret = __qht_remove(map, b, p, hash);
    seqlock_write_end(&b->sequence);
    qemu_spinlock_unlock(&b->lock);
    return ret;
}

/*
 * acquire all spinlocks from a map, so that writers cannot race with
 * readers-turned-writers.
 */
static void qht_lock(struct qht *ht)
{
    struct qht_map *map = ht->map;
    uint64_t i;

    /* if readers cannot upgrade, do nothing */
    if (!(ht->mode & QHT_MODE_MRU_LOOKUP)) {
        return;
    }
    for (i = 0; i < map->n; i++) {
        struct qht_bucket *b = &map->buckets[i];

        qemu_spinlock_lock(&b->lock);
    }
}

static void qht_unlock(struct qht *ht)
{
    struct qht_map *map = ht->map;
    uint64_t i;

    if (!(ht->mode & QHT_MODE_MRU_LOOKUP)) {
        return;
    }
    for (i = 0; i < map->n; i++) {
        struct qht_bucket *b = &map->buckets[i];

        qemu_spinlock_unlock(&b->lock);
    }
}

/* external lock + all of the map's locks held (if !MRU_LOOKUP) */
static inline void __qht_map_iter(struct qht *ht, struct qht_map *map,
                                  qht_iter_func_t func, void *userp)
{
    uint64_t i;

    for (i = 0; i < map->n; i++) {
        struct qht_bucket *b = &map->buckets[i];
        int j;

        do {
            for (j = 0; j < QHT_BUCKET_ENTRIES; j++) {
                if (b->hashes[j]) {
                    func(ht, b->pointers[j], b->hashes[j], userp);
                }
            }
            b = b->next;
        } while (b);
    }
}

/* call with an external lock held */
void qht_iter(struct qht *ht, qht_iter_func_t func, void *userp)
{
    qht_lock(ht);
    __qht_map_iter(ht, ht->map, func, userp);
    qht_unlock(ht);
}

static void qht_map_copy(struct qht *ht, void *p, uint32_t hash, void *userp)
{
    struct qht_map *new = userp;
    struct qht_bucket *b = qht_map_to_bucket(new, hash);

    /* no need to acquire b->lock because no thread has seen this map yet */
    __qht_insert(ht, new, b, p, hash);
}

/* call with an external lock held */
void qht_grow(struct qht *ht)
{
    struct qht_map *old = ht->map;
    struct qht_map *new;
    uint64_t n = old->n * 2;

    if (unlikely(n > (1ULL << 32))) {
        return;
    }
    new = qht_map_create(n);
    qht_iter(ht, qht_map_copy, new);

    qht_publish(ht, new);
    call_rcu1(&old->rcu, qht_map_reclaim);
}
