/*
 * ARM v8.5-MemTag Operations - System Emulation
 *
 * Copyright (c) 2020 Linaro, Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "cpu.h"
#include "internals.h"
#include "exec/exec-all.h"
#include "exec/ram_addr.h"
#include "tcg/mte_helper.h"

uint8_t *allocation_tag_mem(CPUARMState *env, int ptr_mmu_idx,
                            uint64_t ptr, MMUAccessType ptr_access,
                            int ptr_size, MMUAccessType tag_access,
                            int tag_size, uintptr_t ra)
{
    uintptr_t index;
    CPUIOTLBEntry *iotlbentry;
    int in_page, flags;
    ram_addr_t ptr_ra;
    hwaddr ptr_paddr, tag_paddr, xlat;
    MemoryRegion *mr;
    ARMASIdx tag_asi;
    AddressSpace *tag_as;
    void *host;

    /*
     * Probe the first byte of the virtual address.  This raises an
     * exception for inaccessible pages, and resolves the virtual address
     * into the softmmu tlb.
     *
     * When RA == 0, this is for mte_probe1.  The page is expected to be
     * valid.  Indicate to probe_access_flags no-fault, then assert that
     * we received a valid page.
     */
    flags = probe_access_flags(env, ptr, ptr_access, ptr_mmu_idx,
                               ra == 0, &host, ra);
    assert(!(flags & TLB_INVALID_MASK));

    /*
     * Find the iotlbentry for ptr.  This *must* be present in the TLB
     * because we just found the mapping.
     * TODO: Perhaps there should be a cputlb helper that returns a
     * matching tlb entry + iotlb entry.
     */
    index = tlb_index(env, ptr_mmu_idx, ptr);
#ifdef CONFIG_DEBUG_TCG
    {
        CPUTLBEntry *entry = tlb_entry(env, ptr_mmu_idx, ptr);
        target_ulong comparator = (ptr_access == MMU_DATA_LOAD
                                   ? entry->addr_read
                                   : tlb_addr_write(entry));
        g_assert(tlb_hit(comparator, ptr));
    }
#endif
    iotlbentry = &env_tlb(env)->d[ptr_mmu_idx].iotlb[index];

    /* If the virtual page MemAttr != Tagged, access unchecked. */
    if (!arm_tlb_mte_tagged(&iotlbentry->attrs)) {
        return NULL;
    }

    /*
     * If not backed by host ram, there is no tag storage: access unchecked.
     * This is probably a guest os bug though, so log it.
     */
    if (unlikely(flags & TLB_MMIO)) {
        qemu_log_mask(LOG_GUEST_ERROR,
                      "Page @ 0x%" PRIx64 " indicates Tagged Normal memory "
                      "but is not backed by host ram\n", ptr);
        return NULL;
    }

    /*
     * The Normal memory access can extend to the next page.  E.g. a single
     * 8-byte access to the last byte of a page will check only the last
     * tag on the first page.
     * Any page access exception has priority over tag check exception.
     */
    in_page = -(ptr | TARGET_PAGE_MASK);
    if (unlikely(ptr_size > in_page)) {
        void *ignore;
        flags |= probe_access_flags(env, ptr + in_page, ptr_access,
                                    ptr_mmu_idx, ra == 0, &ignore, ra);
        assert(!(flags & TLB_INVALID_MASK));
    }

    /* Any debug exception has priority over a tag check exception. */
    if (unlikely(flags & TLB_WATCHPOINT)) {
        int wp = ptr_access == MMU_DATA_LOAD ? BP_MEM_READ : BP_MEM_WRITE;
        assert(ra != 0);
        cpu_check_watchpoint(env_cpu(env), ptr, ptr_size,
                             iotlbentry->attrs, wp, ra);
    }

    /*
     * Find the physical address within the normal mem space.
     * The memory region lookup must succeed because TLB_MMIO was
     * not set in the cputlb lookup above.
     */
    mr = memory_region_from_host(host, &ptr_ra);
    tcg_debug_assert(mr != NULL);
    tcg_debug_assert(memory_region_is_ram(mr));
    ptr_paddr = ptr_ra;
    do {
        ptr_paddr += mr->addr;
        mr = mr->container;
    } while (mr);

    /* Convert to the physical address in tag space.  */
    tag_paddr = ptr_paddr >> (LOG2_TAG_GRANULE + 1);

    /* Look up the address in tag space. */
    tag_asi = iotlbentry->attrs.secure ? ARMASIdx_TagS : ARMASIdx_TagNS;
    tag_as = cpu_get_address_space(env_cpu(env), tag_asi);
    mr = address_space_translate(tag_as, tag_paddr, &xlat, NULL,
                                 tag_access == MMU_DATA_STORE,
                                 iotlbentry->attrs);

    /*
     * Note that @mr will never be NULL.  If there is nothing in the address
     * space at @tag_paddr, the translation will return the unallocated memory
     * region.  For our purposes, the result must be ram.
     */
    if (unlikely(!memory_region_is_ram(mr))) {
        /* ??? Failure is a board configuration error. */
        qemu_log_mask(LOG_UNIMP,
                      "Tag Memory @ 0x%" HWADDR_PRIx " not found for "
                      "Normal Memory @ 0x%" HWADDR_PRIx "\n",
                      tag_paddr, ptr_paddr);
        return NULL;
    }

    /*
     * Ensure the tag memory is dirty on write, for migration.
     * Tag memory can never contain code or display memory (vga).
     */
    if (tag_access == MMU_DATA_STORE) {
        ram_addr_t tag_ra = memory_region_get_ram_addr(mr) + xlat;
        cpu_physical_memory_set_dirty_flag(tag_ra, DIRTY_MEMORY_MIGRATION);
    }

    return memory_region_get_ram_ptr(mr) + xlat;
}
