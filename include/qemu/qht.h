/*
 * Copyright (C) 2016, Emilio G. Cota <cota@braap.org>
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 */
#ifndef QEMU_QHT_H
#define QEMU_QHT_H

#include "qemu/osdep.h"
#include "qemu-common.h"
#include "qemu/seqlock.h"
#include "qemu/thread.h"
#include "qemu/qdist.h"
#include "qemu/rcu.h"

struct qht {
    struct qht_map *map;
    QemuSpin lock; /* serializes setters of ht->map */
    unsigned int mode;
};

struct qht_stats {
    size_t head_buckets;
    size_t used_head_buckets;
    size_t entries;
    struct qdist chain;
    struct qdist occupancy;
};

typedef bool (*qht_lookup_func_t)(const void *obj, const void *userp);
typedef void (*qht_iter_func_t)(struct qht *ht, void *p, uint32_t h, void *up);

#define QHT_MODE_AUTO_RESIZE 0x1 /* auto-resize when heavily loaded */

void qht_init(struct qht *ht, size_t n_elems, unsigned int mode);

/* call only when there are no readers/writers left */
void qht_destroy(struct qht *ht);

void qht_reset(struct qht *ht);

bool qht_reset_size(struct qht *ht, size_t n_elems);

bool qht_insert(struct qht *ht, void *p, uint32_t hash);

bool qht_remove(struct qht *ht, const void *p, uint32_t hash);

void qht_iter(struct qht *ht, qht_iter_func_t func, void *userp);

bool qht_resize(struct qht *ht, size_t n_elems);

/* if @func is NULL, then pointer comparison is used */
void *qht_lookup(struct qht *ht, qht_lookup_func_t func, const void *userp,
                 uint32_t hash);

/* pass @stats to qht_statistics_destroy() when done */
void qht_statistics_init(struct qht *ht, struct qht_stats *stats);

void qht_statistics_destroy(struct qht_stats *stats);

#endif /* QEMU_QHT_H */
