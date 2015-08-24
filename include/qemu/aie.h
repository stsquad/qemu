/*
 * Atomic instruction emulation (AIE)
 */
#ifndef AIE_H
#define AIE_H

#include "qemu/radix-tree.h"
#include "qemu/thread.h"
#include "qemu/bitops.h"

#include "exec/hwaddr.h"

typedef hwaddr aie_addr_t;

typedef struct AIEEntry AIEEntry;

struct AIEEntry {
    union {
        struct {
            QemuSpin lock;
            bool bm_set;
        };
        uint8_t pad[64];
    };
} __attribute((aligned(64)));

#define AIE_DISCARD_BITS 6

#define AIE_BM_BITS     21
#define AIE_BM_NR_ITEMS BIT(AIE_BM_BITS)

extern QemuRadixTree aie_rtree;
extern unsigned long *aie_bm;

static inline aie_addr_t to_aie(hwaddr paddr)
{
    return paddr >> AIE_DISCARD_BITS;
}

void aie_init(void);

AIEEntry *aie_entry_get_lock(hwaddr addr);

static inline bool aie_entry_exists(hwaddr addr)
{
    return test_bit(to_aie(addr) & (AIE_BM_NR_ITEMS - 1), aie_bm);
}

#endif /* AIE_H */
