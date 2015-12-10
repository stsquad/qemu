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
#define helper_ld glue(glue(helper_be_ld, USUFFIX), MMUSUFFIX)
#define helper_st glue(glue(helper_be_st, SUFFIX), MMUSUFFIX)

#else /* LE helpers + 8bit helpers (generated only once for both LE end BE) */

#if DATA_SIZE > 1
#define helper_ldlink_name  glue(glue(helper_le_ldlink, USUFFIX), MMUSUFFIX)
#define helper_stcond_name  glue(glue(helper_le_stcond, SUFFIX), MMUSUFFIX)
#define helper_ld glue(glue(helper_le_ld, USUFFIX), MMUSUFFIX)
#define helper_st glue(glue(helper_le_st, SUFFIX), MMUSUFFIX)
#else /* DATA_SIZE <= 1 */
#define helper_ldlink_name  glue(glue(helper_ret_ldlink, USUFFIX), MMUSUFFIX)
#define helper_stcond_name  glue(glue(helper_ret_stcond, SUFFIX), MMUSUFFIX)
#define helper_ld glue(glue(helper_ret_ld, USUFFIX), MMUSUFFIX)
#define helper_st glue(glue(helper_ret_st, SUFFIX), MMUSUFFIX)
#endif

#endif

WORD_TYPE helper_ldlink_name(CPUArchState *env, target_ulong addr,
                                TCGMemOpIdx oi, uintptr_t retaddr)
{
    WORD_TYPE ret;
    int index;
    CPUState *this_cpu = ENV_GET_CPU(env);
    CPUClass *cc = CPU_GET_CLASS(this_cpu);
    hwaddr hw_addr;
    unsigned mmu_idx = get_mmuidx(oi);

    /* Use the proper load helper from cpu_ldst.h */
    ret = helper_ld(env, addr, oi, retaddr);

    index = (addr >> TARGET_PAGE_BITS) & (CPU_TLB_SIZE - 1);

    /* hw_addr = hwaddr of the page (i.e. section->mr->ram_addr + xlat)
     * plus the offset (i.e. addr & ~TARGET_PAGE_MASK) */
    hw_addr = (env->iotlb[mmu_idx][index].addr & TARGET_PAGE_MASK) + addr;
    if (likely(!(env->tlb_table[mmu_idx][index].addr_read & TLB_MMIO))) {
        /* If all the vCPUs have the EXCL bit set for this page there is no need
         * to request any flush. */
        if (cpu_physical_memory_set_excl(hw_addr)) {
            CPUState *cpu;

            excl_history_put_addr(hw_addr);
            CPU_FOREACH(cpu) {
                if (this_cpu != cpu) {
                    tlb_flush(cpu, 1);
                }
            }
        }
        /* For this vCPU, just update the TLB entry, no need to flush. */
        env->tlb_table[mmu_idx][index].addr_write |= TLB_EXCL;
    } else {
        /* Set a pending exclusive access in the MemoryRegion */
        MemoryRegion *mr = iotlb_to_region(this_cpu,
                                           env->iotlb[mmu_idx][index].addr,
                                           env->iotlb[mmu_idx][index].attrs);
        mr->pending_excl_access = true;
    }

    cc->cpu_set_excl_protected_range(this_cpu, hw_addr, DATA_SIZE);

    /* From now on we are in LL/SC context */
    this_cpu->ll_sc_context = true;

    return ret;
}

WORD_TYPE helper_stcond_name(CPUArchState *env, target_ulong addr,
                             DATA_TYPE val, TCGMemOpIdx oi,
                             uintptr_t retaddr)
{
    WORD_TYPE ret = 1;
    CPUState *cpu = ENV_GET_CPU(env);
    CPUClass *cc = CPU_GET_CLASS(cpu);

    if (cpu->ll_sc_context) {
        /* We set it preventively to true to distinguish the following legacy
         * access as one made by the store conditional wrapper. If the store
         * conditional does not succeed, the value will be set to 0.*/
        cpu->excl_succeeded = true;
        helper_st(env, addr, val, oi, retaddr);

        if (cpu->excl_succeeded) {
            ret = 0;
        }
    }

    /* Unset LL/SC context */
    cc->cpu_reset_excl_context(cpu);

    return ret;
}

#undef helper_ldlink_name
#undef helper_stcond_name
#undef helper_ld
#undef helper_st
