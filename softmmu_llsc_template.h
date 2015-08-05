/*
 *  Software MMU support (esclusive load/store operations)
 *
 * Generate helpers used by TCG for qemu_ldlink/stcond ops.
 *
 * Included from softmmu_template.h only.
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

/* This template does not generate together the le and be version, but only one
 * of the two depending on whether BIGENDIAN_EXCLUSIVE_HELPERS has been set.
 * The same nomenclature as softmmu_template.h is used for the exclusive
 * helpers.  */

#ifdef BIGENDIAN_EXCLUSIVE_HELPERS

#define helper_ldlink_name  glue(glue(helper_be_ldlink, USUFFIX), MMUSUFFIX)
#define helper_stcond_name  glue(glue(helper_be_stcond, SUFFIX), MMUSUFFIX)
#define helper_ld_legacy glue(glue(helper_be_ld, USUFFIX), MMUSUFFIX)
#define helper_st_legacy glue(glue(helper_be_st, SUFFIX), MMUSUFFIX)

#else /* LE helpers + 8bit helpers (generated only once for both LE end BE) */

#if DATA_SIZE > 1
#define helper_ldlink_name  glue(glue(helper_le_ldlink, USUFFIX), MMUSUFFIX)
#define helper_stcond_name  glue(glue(helper_le_stcond, SUFFIX), MMUSUFFIX)
#define helper_ld_legacy glue(glue(helper_le_ld, USUFFIX), MMUSUFFIX)
#define helper_st_legacy glue(glue(helper_le_st, SUFFIX), MMUSUFFIX)
#else /* DATA_SIZE <= 1 */
#define helper_ldlink_name  glue(glue(helper_ret_ldlink, USUFFIX), MMUSUFFIX)
#define helper_stcond_name  glue(glue(helper_ret_stcond, SUFFIX), MMUSUFFIX)
#define helper_ld_legacy glue(glue(helper_ret_ld, USUFFIX), MMUSUFFIX)
#define helper_st_legacy glue(glue(helper_ret_st, SUFFIX), MMUSUFFIX)
#endif

#endif

WORD_TYPE helper_ldlink_name(CPUArchState *env, target_ulong addr,
                                TCGMemOpIdx oi, uintptr_t retaddr)
{
    WORD_TYPE ret;
    int index;
    CPUState *cpu;
    hwaddr hw_addr;
    unsigned mmu_idx = get_mmuidx(oi);

    /* Use the proper load helper from cpu_ldst.h */
    ret = helper_ld_legacy(env, addr, mmu_idx, retaddr);

    index = (addr >> TARGET_PAGE_BITS) & (CPU_TLB_SIZE - 1);

    /* hw_addr = hwaddr of the page (i.e. section->mr->ram_addr + xlat)
     * plus the offset (i.e. addr & ~TARGET_PAGE_MASK) */
    hw_addr = (env->iotlb[mmu_idx][index].addr & TARGET_PAGE_MASK) + addr;

    cpu_physical_memory_clear_excl_dirty(hw_addr, ENV_GET_CPU(env)->cpu_index);
    /* If all the vCPUs have the EXCL bit set for this page there is no need
     * to request any flush. */
    if (cpu_physical_memory_excl_is_dirty(hw_addr, smp_cpus)) {
        CPU_FOREACH(cpu) {
            if (current_cpu != cpu) {
                if (cpu_physical_memory_excl_is_dirty(hw_addr,
                                                    cpu->cpu_index)) {
                    cpu_physical_memory_clear_excl_dirty(hw_addr,
                                                         cpu->cpu_index);
                    tlb_flush(cpu, 1);
                }
            }
        }
    }

    env->excl_protected_range.begin = hw_addr;
    env->excl_protected_range.end = hw_addr + DATA_SIZE;

    /* For this vCPU, just update the TLB entry, no need to flush. */
    env->tlb_table[mmu_idx][index].addr_write |= TLB_EXCL;

    return ret;
}

WORD_TYPE helper_stcond_name(CPUArchState *env, target_ulong addr,
                             DATA_TYPE val, TCGMemOpIdx oi,
                             uintptr_t retaddr)
{
    WORD_TYPE ret;
    unsigned mmu_idx = get_mmuidx(oi);

    /* We set it preventively to true to distinguish the following legacy
     * access as one made by the store conditional wrapper. If the store
     * conditional does not succeed, the value will be set to 0.*/
    env->excl_succeeded = 1;
    helper_st_legacy(env, addr, val, mmu_idx, retaddr);

    if (env->excl_succeeded) {
        env->excl_succeeded = 0;
        ret = 0;
    } else {
        ret = 1;
    }

    return ret;
}

#undef helper_ldlink_name
#undef helper_stcond_name
#undef helper_ld_legacy
#undef helper_st_legacy
