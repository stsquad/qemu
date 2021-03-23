/*
 * ARM v8.5-MemTag Operations - User-mode
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
#include "tcg/mte_helper.h"

uint8_t *allocation_tag_mem(CPUARMState *env, int ptr_mmu_idx,
                            uint64_t ptr, MMUAccessType ptr_access,
                            int ptr_size, MMUAccessType tag_access,
                            int tag_size, uintptr_t ra)
{
    uint64_t clean_ptr = useronly_clean_ptr(ptr);
    int flags = page_get_flags(clean_ptr);
    uint8_t *tags;
    uintptr_t index;

    if (!(flags & (ptr_access == MMU_DATA_STORE ? PAGE_WRITE : PAGE_READ))) {
        /* SIGSEGV */
        arm_cpu_tlb_fill(env_cpu(env), ptr, ptr_size, ptr_access,
                         ptr_mmu_idx, false, ra);
        g_assert_not_reached();
    }

    /* Require both MAP_ANON and PROT_MTE for the page. */
    if (!(flags & PAGE_ANON) || !(flags & PAGE_MTE)) {
        return NULL;
    }

    tags = page_get_target_data(clean_ptr);
    if (tags == NULL) {
        size_t alloc_size = TARGET_PAGE_SIZE >> (LOG2_TAG_GRANULE + 1);
        tags = page_alloc_target_data(clean_ptr, alloc_size);
        assert(tags != NULL);
    }

    index = extract32(ptr, LOG2_TAG_GRANULE + 1,
                      TARGET_PAGE_BITS - LOG2_TAG_GRANULE - 1);
    return tags + index;
}
