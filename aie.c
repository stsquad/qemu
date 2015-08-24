/*
 * Atomic instruction emulation (AIE).
 * This applies to LL/SC and higher-order atomic instructions.
 * More info:
 *   http://en.wikipedia.org/wiki/Load-link/store-conditional
 */
#include "qemu-common.h"
#include "qemu/radix-tree.h"
#include "qemu/thread.h"
#include "qemu/aie.h"

#if defined(CONFIG_USER_ONLY)
# define AIE_FULL_ADDR_BITS  TARGET_VIRT_ADDR_SPACE_BITS
#else
#if HOST_LONG_BITS < TARGET_PHYS_ADDR_SPACE_BITS
/* in this case QEMU restricts the maximum RAM size to fit in the host */
# define AIE_FULL_ADDR_BITS  HOST_LONG_BITS
#else
# define AIE_FULL_ADDR_BITS  TARGET_PHYS_ADDR_SPACE_BITS
#endif
#endif /* CONFIG_USER_ONLY */

#define AIE_ADDR_BITS  (AIE_FULL_ADDR_BITS - AIE_DISCARD_BITS)
#define AIE_RADIX     8

QemuRadixTree aie_rtree;
unsigned long *aie_bm;

static void *aie_entry_init(unsigned long index)
{
    AIEEntry *entry;

    entry = qemu_memalign(64, sizeof(*entry));
    qemu_spin_init(&entry->lock);
    entry->bm_set = false;
    return entry;
}

AIEEntry *aie_entry_get_lock(hwaddr paddr)
{
    aie_addr_t idx = to_aie(paddr);
    AIEEntry *e;

    e = qemu_radix_tree_find_alloc(&aie_rtree, idx, aie_entry_init, qemu_vfree);
    qemu_spin_lock(&e->lock);
    if (!e->bm_set) {
        set_bit_atomic(idx & (AIE_BM_NR_ITEMS - 1), aie_bm);
        e->bm_set = true;
    }
    return e;
}

void aie_init(void)
{
    qemu_radix_tree_init(&aie_rtree, AIE_ADDR_BITS, AIE_RADIX);
    aie_bm = bitmap_new(AIE_BM_NR_ITEMS);
}
