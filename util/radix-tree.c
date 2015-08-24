/*
 * radix-tree.c
 * Non-blocking radix tree.
 *
 * Features:
 * - Concurrent lookups and inserts.
 * - No support for deletions.
 *
 * Conventions:
 * - Height is counted starting from 0 at the bottom.
 * - The index is used from left to right, i.e. MSBs are used first. This way
 *   nearby addresses land in nearby slots, minimising cache/TLB misses.
 */
#include <glib.h>

#include "qemu/radix-tree.h"
#include "qemu/atomic.h"
#include "qemu/bitops.h"
#include "qemu/osdep.h"

typedef struct QemuRadixNode QemuRadixNode;

void *qemu_radix_tree_find_alloc(QemuRadixTree *tree, unsigned long index,
                                 void *(*create)(unsigned long),
                                 void (*delete)(void *))
{
    QemuRadixNode *parent;
    QemuRadixNode *node = tree->root;
    void **slot;
    int n_slots = BIT(tree->radix);
    int level = tree->max_height - 1;
    int shift = (level - 1) * tree->radix;

    do {
        parent = node;
        slot = parent->slots + ((index >> shift) & (n_slots - 1));
        node = atomic_read(slot);
        smp_read_barrier_depends();
        if (node == NULL) {
            void *old;
            void *new;

            if (!create) {
                return NULL;
            }

            if (level == 1) {
                node = create(index);
            } else {
                node = g_malloc0(sizeof(*node) + sizeof(void *) * n_slots);
            }
            new = node;
            /* atomic_cmpxchg is type-safe so we cannot use 'node' here */
            old = atomic_cmpxchg(slot, NULL, new);
            if (old) {
                if (level == 1) {
                    delete(node);
                } else {
                    g_free(node);
                }
                node = old;
            }
        }
        shift -= tree->radix;
        level--;
    } while (level > 0);
    return node;
}

void qemu_radix_tree_init(QemuRadixTree *tree, int bits, int radix)
{
    tree->radix = radix;
    tree->max_height = 1 + DIV_ROUND_UP(bits, radix);
    tree->root = g_malloc0(sizeof(*tree->root) + sizeof(void *) * BIT(radix));
}
