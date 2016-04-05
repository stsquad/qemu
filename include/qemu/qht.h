/*
 * Copyright (C) 2016, Emilio G. Cota <cota@braap.org>
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 */
#ifndef QHT_H
#define QHT_H

#include <stdbool.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>

#include "qemu/osdep.h"
#include "qemu-common.h"
#include "qemu/spinlock.h"
#include "qemu/seqlock.h"
#include "qemu/rcu.h"

#if HOST_LONG_BITS == 32
#define QHT_BUCKET_ENTRIES 6
#else /* 64-bit */
#define QHT_BUCKET_ENTRIES 4
#endif

struct qht_bucket {
    QemuSpinLock lock;
    QemuSeqLock sequence;
    uint32_t hashes[QHT_BUCKET_ENTRIES];
    void *pointers[QHT_BUCKET_ENTRIES];
    struct qht_bucket *next;
} QEMU_CACHELINE_ALIGNED;

struct qht_map {
    struct qht_bucket *buckets;
    uint64_t n;
    uint64_t n_items;
    uint64_t n_items_threshold;
    struct rcu_head rcu;
};

struct qht {
    struct qht_map *map;
    unsigned int mode;
};

typedef bool (*qht_lookup_func_t)(const void *obj, const void *userp);
typedef void (*qht_iter_func_t)(struct qht *ht, void *p, uint32_t h, void *up);

#define QHT_MODE_MRU_LOOKUP  0x1 /* move looked-up items to head */
#define QHT_MODE_MRU_INSERT  0x2 /* insert new elements at the head */
#define QHT_MODE_AUTO_RESIZE 0x4 /* auto-resize when heavily loaded */

void qht_init(struct qht *ht, uint64_t n_elems, unsigned int mode);

/* call only when there are no readers left */
void qht_destroy(struct qht *ht);

/* call with an external lock held */
void qht_reset(struct qht *ht);

/* call with an external lock held */
void qht_reset_size(struct qht *ht, uint64_t n_elems);

/* call with an external lock held */
void qht_insert(struct qht *ht, void *p, uint32_t hash);

/* call with an external lock held */
bool qht_remove(struct qht *ht, const void *p, uint32_t hash);

/* call with an external lock held */
void qht_iter(struct qht *ht, qht_iter_func_t func, void *userp);

/* call with an external lock held */
void qht_grow(struct qht *ht);

void qht_bucket_mru(struct qht_bucket *b, struct qht_bucket *orig,
                    const void *p, int pos);

static inline
struct qht_bucket *qht_map_to_bucket(struct qht_map *map, uint32_t hash)
{
    return &map->buckets[hash & (map->n - 1)];
}

static inline
void *__qht_lookup(struct qht_bucket *b, struct qht_bucket **far_bucket,
                    int *pos, qht_lookup_func_t func, const void *userp,
                    uint32_t hash)
{
    unsigned int count = 0;
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
                    if (unlikely(count)) {
                        *far_bucket = b;
                        *pos = i;
                    }
                    return p;
                }
            }
        }
        count++;
        b = atomic_read(&b->next);
        /*
         * This barrier guarantees that we will read a properly initialized
         * b->next; it is paired with an smp_wmb() before setting b->next.
         */
        smp_rmb();
    } while (b);
    return NULL;
}

static inline void *qht_lookup(struct qht *ht, qht_lookup_func_t func,
                                const void *userp, uint32_t hash)
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
        ret = __qht_lookup(b, &far_bucket, &pos, func, userp, hash);
    } while (seqlock_read_retry(&b->sequence, version));
    if ((ht->mode & QHT_MODE_MRU_LOOKUP) && unlikely(far_bucket)) {
        qht_bucket_mru(b, far_bucket, ret, pos);
    }
    return ret;
}

#endif /* QHT_H */
