#ifndef RADIX_TREE_H
#define RADIX_TREE_H

#include <stddef.h>

typedef struct QemuRadixNode QemuRadixNode;
typedef struct QemuRadixTree QemuRadixTree;

struct QemuRadixNode {
    void *slots[0];
};

struct QemuRadixTree {
    QemuRadixNode *root;
    int radix;
    int max_height;
};

void qemu_radix_tree_init(QemuRadixTree *tree, int bits, int radix);
void *qemu_radix_tree_find_alloc(QemuRadixTree *tree, unsigned long index,
                                 void *(*create)(unsigned long),
                                 void (*delete)(void *));

static inline void *qemu_radix_tree_find(QemuRadixTree *t, unsigned long index)
{
    return qemu_radix_tree_find_alloc(t, index, NULL, NULL);
}

#endif /* RADIX_TREE_H */
