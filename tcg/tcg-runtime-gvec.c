/*
 *  Generic vectorized operation runtime
 *
 *  Copyright (c) 2017 Linaro
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
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
#include "qemu/host-utils.h"
#include "cpu.h"
#include "exec/helper-proto.h"

/* Virtually all hosts support 16-byte vectors.  Those that don't
   can emulate them via GCC's generic vector extension.

   In tcg-op-gvec.c, we asserted that both the size and alignment
   of the data are multiples of 16.  */

typedef uint8_t vec8 __attribute__((vector_size(16)));
typedef uint16_t vec16 __attribute__((vector_size(16)));
typedef uint32_t vec32 __attribute__((vector_size(16)));
typedef uint64_t vec64 __attribute__((vector_size(16)));

static inline intptr_t extract_opsz(uint32_t desc)
{
    return ((desc & 0xff) + 1) * 16;
}

static inline intptr_t extract_clsz(uint32_t desc)
{
    return (((desc >> 8) & 0xff) + 1) * 16;
}

static inline void clear_high(void *d, intptr_t opsz, uint32_t desc)
{
    intptr_t clsz = extract_clsz(desc);
    intptr_t i;

    if (unlikely(clsz > opsz)) {
        for (i = opsz; i < clsz; i += sizeof(vec64)) {
            *(vec64 *)(d + i) = (vec64){ 0 };
        }
    }
}

void HELPER(gvec_add8)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec8)) {
        *(vec8 *)(d + i) = *(vec8 *)(a + i) + *(vec8 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_add16)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec16)) {
        *(vec16 *)(d + i) = *(vec16 *)(a + i) + *(vec16 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_add32)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec32)) {
        *(vec32 *)(d + i) = *(vec32 *)(a + i) + *(vec32 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_add64)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) + *(vec64 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_sub8)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec8)) {
        *(vec8 *)(d + i) = *(vec8 *)(a + i) - *(vec8 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_sub16)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec16)) {
        *(vec16 *)(d + i) = *(vec16 *)(a + i) - *(vec16 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_sub32)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec32)) {
        *(vec32 *)(d + i) = *(vec32 *)(a + i) - *(vec32 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_sub64)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) - *(vec64 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_and8)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) & *(vec64 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_or8)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) | *(vec64 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_xor8)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) ^ *(vec64 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_andc8)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) &~ *(vec64 *)(b + i);
    }
    clear_high(d, opsz, desc);
}

void HELPER(gvec_orc8)(void *d, void *a, void *b, uint32_t desc)
{
    intptr_t opsz = extract_opsz(desc);
    intptr_t i;

    for (i = 0; i < opsz; i += sizeof(vec64)) {
        *(vec64 *)(d + i) = *(vec64 *)(a + i) |~ *(vec64 *)(b + i);
    }
    clear_high(d, opsz, desc);
}
