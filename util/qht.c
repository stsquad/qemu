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

struct qht_map {
    struct qht_bucket *buckets;
    size_t n;
    size_t n_items;
    size_t n_items_threshold;
    struct rcu_head rcu;
};

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
    map->n_items = 0;
    map->n_items_threshold = n * QHT_BUCKET_ENTRIES / 2;
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

static void qht_bucket_reset(struct qht_bucket *head)
{
    struct qht_bucket *b = head;
    int i;

    qemu_spin_lock(&head->lock);
    seqlock_write_begin(&head->sequence);
    do {
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            atomic_set(&b->hashes[i], 0);
            atomic_set(&b->pointers[i], NULL);
        }
        b = b->next;
    } while (b);
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
}

/* call with an external lock held */
void qht_reset_size(struct qht *ht, size_t n_elems)
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
static void qht_move_bucket_to_front__locked(struct qht_bucket *head,
                                             struct qht_bucket *orig)
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

/*
 * Call with head->lock held.
 *
 * This function moves the item in @orig[@pos] to @head[0]. In order to
 * do this, the item i in @head is moved to @head's i+1 position,
 * with the last item being moved to @orig[@pos].
 * Further, @orig is brought to the front of the bucket chain, i.e.
 * @head->next is set to point to @orig. This is done to make sure that
 * the last item in @head is not moved to a too-distant place. An
 * alternative would be to perform full MRU for the entire bucket
 * chain, but that would cause too much churn.
 *
 * Note: @orig == @head is a bug.
 */
static inline void qht_bucket_mru__locked(struct qht_bucket *head,
                                          struct qht_bucket *orig, int pos)
{
    uint32_t *dest_hash;
    void **dest_p;
    void *p;
    uint32_t hash;
    int i;

    assert(orig != head);
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
        qht_move_bucket_to_front__locked(head, orig);
    }
}

static void qht_bucket_mru(struct qht_bucket *head, struct qht_bucket *orig,
                           const void *p, int pos)
{
    qemu_spin_lock(&head->lock);
    if (unlikely(orig->pointers[pos] != p)) {
        /* while we acquired the lock, the bucket was updated, so bail out */
        goto out;
    }
    seqlock_write_begin(&head->sequence);
    qht_bucket_mru__locked(head, orig, pos);
    seqlock_write_end(&head->sequence);
 out:
    qemu_spin_unlock(&head->lock);
}

/*
 * @far_bucket and @pos are only filled in if the match is found in a bucket
 * that is not the head bucket.
 */
static inline
void *qht_do_lookup(struct qht_bucket *head, struct qht_bucket **far_bucket,
                    int *pos, qht_lookup_func_t func, const void *userp,
                    uint32_t hash)
{
    struct qht_bucket *b = head;
    int i;

    do {
        for (i = 0; i < QHT_BUCKET_ENTRIES; i++) {
            if (b->hashes[i] == hash) {
                void *p = atomic_read(&b->pointers[i]);

                /*
                 * p could be NULL if we're racing with a writer. We could use
                 * barriers here but for performance we only issue the ones
                 * in the seqlock.
                 */
                if (likely(p && func(p, userp))) {
                    if (unlikely(b != head)) {
                        *far_bucket = b;
                        *pos = i;
                    }
                    return p;
                }
            }
        }
        b = atomic_read(&b->next);
        /*
         * This barrier guarantees that we will read a properly initialized
         * b->next; it is paired with an smp_wmb() before setting b->next.
         */
        smp_rmb();
    } while (b);
    return NULL;
}

void *qht_lookup(struct qht *ht, qht_lookup_func_t func, const void *userp,
                 uint32_t hash)
{
    struct qht_bucket *far_bucket = NULL;
    struct qht_bucket *b;
    struct qht_map *map;
    uint32_t version;
    int pos = 0;
    void *ret;

    map = atomic_read(&ht->map);
    /* paired with smp_wmb() before setting ht->map */
    smp_rmb();
    b = qht_map_to_bucket(map, hash);

    do {
        version = seqlock_read_begin(&b->sequence);
        ret = qht_do_lookup(b, &far_bucket, &pos, func, userp, hash);
    } while (seqlock_read_retry(&b->sequence, version));
    if ((ht->mode & QHT_MODE_MRU_LOOKUP) && unlikely(far_bucket)) {
        qht_bucket_mru(b, far_bucket, ret, pos);
    }
    return ret;
}

/* call with b->lock held */
static void qht_insert__locked(struct qht *ht, struct qht_map *map,
                               struct qht_bucket *head, void *p, uint32_t hash)
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
            if ((ht->mode & QHT_MODE_MRU_INSERT) && b != head) {
                qht_bucket_mru__locked(head, b, i);
            }
            seqlock_write_end(&head->sequence);
            map->n_items++;
            return;
        }
        prev = b;
        b = b->next;
    }
}

/* call with an external lock held */
void qht_insert(struct qht *ht, void *p, uint32_t hash)
{
    struct qht_map *map = ht->map;
    struct qht_bucket *b = qht_map_to_bucket(map, hash);

    qemu_spin_lock(&b->lock);
    qht_insert__locked(ht, map, b, p, hash);
    qemu_spin_unlock(&b->lock);

    if ((ht->mode & QHT_MODE_AUTO_RESIZE) &&
        unlikely(map->n_items > map->n_items_threshold)) {
        qht_grow(ht);
    }
}

/* call with b->lock held */
static inline bool qht_remove__locked(struct qht_map *map, struct qht_bucket *b,
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

    qemu_spin_lock(&b->lock);
    seqlock_write_begin(&b->sequence);
    ret = qht_remove__locked(map, b, p, hash);
    seqlock_write_end(&b->sequence);
    qemu_spin_unlock(&b->lock);
    return ret;
}

/*
 * acquire all spinlocks from a map, so that writers cannot race with
 * readers-turned-writers.
 */
static void qht_lock(struct qht *ht)
{
    struct qht_map *map = ht->map;
    size_t i;

    /* if readers cannot upgrade, do nothing */
    if (!(ht->mode & QHT_MODE_MRU_LOOKUP)) {
        return;
    }
    for (i = 0; i < map->n; i++) {
        struct qht_bucket *b = &map->buckets[i];

        qemu_spin_lock(&b->lock);
    }
}

static void qht_unlock(struct qht *ht)
{
    struct qht_map *map = ht->map;
    size_t i;

    if (!(ht->mode & QHT_MODE_MRU_LOOKUP)) {
        return;
    }
    for (i = 0; i < map->n; i++) {
        struct qht_bucket *b = &map->buckets[i];

        qemu_spin_unlock(&b->lock);
    }
}

/* external lock + all of the map's locks held (if !MRU_LOOKUP) */
static inline void qht_map_iter__locked(struct qht *ht, struct qht_map *map,
                                        qht_iter_func_t func, void *userp)
{
    size_t i;

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
    qht_map_iter__locked(ht, ht->map, func, userp);
    qht_unlock(ht);
}

static void qht_map_copy(struct qht *ht, void *p, uint32_t hash, void *userp)
{
    struct qht_map *new = userp;
    struct qht_bucket *b = qht_map_to_bucket(new, hash);

    /* no need to acquire b->lock because no thread has seen this map yet */
    qht_insert__locked(ht, new, b, p, hash);
}

/* call with an external lock held */
void qht_grow(struct qht *ht)
{
    struct qht_map *old = ht->map;
    struct qht_map *new;
    size_t n = old->n * 2;

    new = qht_map_create(n);
    qht_iter(ht, qht_map_copy, new);

    qht_publish(ht, new);
    call_rcu1(&old->rcu, qht_map_reclaim);
}

/*
 * Returns the number of buckets that an average lookup will traverse.
 * The return value is always >= 1. With a good hashing function, this
 * value should be close to 1.
 * Note that each bucket tracks up to QHT_BUCKET_ENTRIES items.
 */
double qht_avg_bucket_chain_length(struct qht *ht, size_t *n_head_buckets)
{
    struct qht_map *map;
    size_t count = 0;
    size_t i;

    map = atomic_read(&ht->map);
    /* paired with smp_wmb() before setting ht->map */
    smp_rmb();

    for (i = 0; i < map->n; i++) {
        struct qht_bucket *head = &map->buckets[i];
        struct qht_bucket *b;
        size_t bucket_count;
        uint32_t version;

        do {
            version = seqlock_read_begin(&head->sequence);
            bucket_count = 0;
            b = head;
            do {
                bucket_count++;
                b = b->next;
            } while (b);
        } while (seqlock_read_retry(&head->sequence, version));
        count += bucket_count;
    }
    if (n_head_buckets) {
        *n_head_buckets = map->n;
    }
    return (double)count / map->n;
}
