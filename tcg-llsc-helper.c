/*
 * Runtime helpers for atomic istruction emulation
 *
 * Copyright (c) 2015 Virtual Open Systems
 *
 * Authors:
 *  Alvise Rigo <a.rigo@virtualopensystems.com>
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
#include "exec/cpu_ldst.h"
#include "exec/helper-head.h"
#include "tcg-llsc-helper.h"

#define LDEX_HELPER(SUFF, OPC, FUNC)                                       \
uint32_t HELPER(ldlink_i##SUFF)(CPUArchState *env, target_ulong addr,      \
                                uint32_t index)                            \
{                                                                          \
    CPUArchState *state = env;                                             \
    TCGMemOpIdx op;                                                        \
                                                                           \
    op = make_memop_idx((OPC), index);                                     \
                                                                           \
    return (uint32_t)FUNC(state, addr, op, GETRA());                       \
}

#define STEX_HELPER(SUFF, DATA_TYPE, OPC, FUNC)                            \
target_ulong HELPER(stcond_i##SUFF)(CPUArchState *env, target_ulong addr,  \
                                    uint32_t val, uint32_t index)          \
{                                                                          \
    CPUArchState *state = env;                                             \
    TCGMemOpIdx op;                                                        \
                                                                           \
    op = make_memop_idx((OPC), index);                                     \
                                                                           \
    return (target_ulong)FUNC(state, addr, val, op, GETRA());              \
}


LDEX_HELPER(8, MO_UB, helper_ret_ldlinkub_mmu)
LDEX_HELPER(16_be, MO_BEUW, helper_be_ldlinkuw_mmu)
LDEX_HELPER(16_bea, MO_BEUW | MO_ALIGN, helper_be_ldlinkuw_mmu)
LDEX_HELPER(32_be, MO_BEUL, helper_be_ldlinkul_mmu)
LDEX_HELPER(32_bea, MO_BEUL | MO_ALIGN, helper_be_ldlinkul_mmu)
LDEX_HELPER(16_le, MO_LEUW, helper_le_ldlinkuw_mmu)
LDEX_HELPER(16_lea, MO_LEUW | MO_ALIGN, helper_le_ldlinkuw_mmu)
LDEX_HELPER(32_le, MO_LEUL, helper_le_ldlinkul_mmu)
LDEX_HELPER(32_lea, MO_LEUL | MO_ALIGN, helper_le_ldlinkul_mmu)

STEX_HELPER(8, uint8_t, MO_UB, helper_ret_stcondb_mmu)
STEX_HELPER(16_be, uint16_t, MO_BEUW, helper_be_stcondw_mmu)
STEX_HELPER(16_bea, uint16_t, MO_BEUW | MO_ALIGN, helper_be_stcondw_mmu)
STEX_HELPER(32_be, uint32_t, MO_BEUL, helper_be_stcondl_mmu)
STEX_HELPER(32_bea, uint32_t, MO_BEUL | MO_ALIGN, helper_be_stcondl_mmu)
STEX_HELPER(16_le, uint16_t, MO_LEUW, helper_le_stcondw_mmu)
STEX_HELPER(16_lea, uint16_t, MO_LEUW | MO_ALIGN, helper_le_stcondw_mmu)
STEX_HELPER(32_le, uint32_t, MO_LEUL, helper_le_stcondl_mmu)
STEX_HELPER(32_lea, uint32_t, MO_LEUL | MO_ALIGN, helper_le_stcondl_mmu)

#define LDEX_HELPER_64(SUFF, OPC, FUNC)                                     \
uint64_t HELPER(ldlink_i##SUFF)(CPUArchState *env, target_ulong addr,       \
                                uint32_t index)                             \
{                                                                           \
    CPUArchState *state = env;                                              \
    TCGMemOpIdx op;                                                         \
                                                                            \
    op = make_memop_idx((OPC), index);                                      \
                                                                            \
    return FUNC(state, addr, op, GETRA());                                  \
}

#define STEX_HELPER_64(SUFF, OPC, FUNC)                                     \
target_ulong HELPER(stcond_i##SUFF)(CPUArchState *env, target_ulong addr,   \
                                    uint64_t val, uint32_t index)           \
{                                                                           \
    CPUArchState *state = env;                                              \
    TCGMemOpIdx op;                                                         \
                                                                            \
    op = make_memop_idx((OPC), index);                                      \
                                                                            \
    return (target_ulong)FUNC(state, addr, val, op, GETRA());               \
}

LDEX_HELPER_64(64_be, MO_BEQ, helper_be_ldlinkq_mmu)
LDEX_HELPER_64(64_bea, MO_BEQ | MO_ALIGN, helper_be_ldlinkq_mmu)
LDEX_HELPER_64(64_le, MO_LEQ, helper_le_ldlinkq_mmu)
LDEX_HELPER_64(64_lea, MO_LEQ | MO_ALIGN, helper_le_ldlinkq_mmu)

STEX_HELPER_64(64_be, MO_BEQ, helper_be_stcondq_mmu)
STEX_HELPER_64(64_bea, MO_BEQ | MO_ALIGN, helper_be_stcondq_mmu)
STEX_HELPER_64(64_le, MO_LEQ, helper_le_stcondq_mmu)
STEX_HELPER_64(64_lea, MO_LEQ | MO_ALIGN, helper_le_stcondq_mmu)
