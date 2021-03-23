/*
 * ARM v8.5-MemTag Operations
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

#ifndef MTE_HELPER_H
#define MTE_HELPER_H
/**
 * allocation_tag_mem:
 * @env: the cpu environment
 * @ptr_mmu_idx: the addressing regime to use for the virtual address
 * @ptr: the virtual address for which to look up tag memory
 * @ptr_access: the access to use for the virtual address
 * @ptr_size: the number of bytes in the normal memory access
 * @tag_access: the access to use for the tag memory
 * @tag_size: the number of bytes in the tag memory access
 * @ra: the return address for exception handling
 *
 * Our tag memory is formatted as a sequence of little-endian nibbles.
 * That is, the byte at (addr >> (LOG2_TAG_GRANULE + 1)) contains two
 * tags, with the tag at [3:0] for the lower addr and the tag at [7:4]
 * for the higher addr.
 *
 * Here, resolve the physical address from the virtual address, and return
 * a pointer to the corresponding tag byte.  Exit with exception if the
 * virtual address is not accessible for @ptr_access.
 *
 * The @ptr_size and @tag_size values may not have an obvious relation
 * due to the alignment of @ptr, and the number of tag checks required.
 *
 * If there is no tag storage corresponding to @ptr, return NULL.
 */
uint8_t *allocation_tag_mem(CPUARMState *env, int ptr_mmu_idx,
                            uint64_t ptr, MMUAccessType ptr_access,
                            int ptr_size, MMUAccessType tag_access,
                            int tag_size, uintptr_t ra);

#endif /* MTE_HELPER_H */
