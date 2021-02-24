/*
 * ARM TLB (Translation lookaside buffer) helpers.
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include "qemu/osdep.h"
#include "cpu.h"
#include "internals.h"
#include "exec/exec-all.h"
#include "cpu-mmu.h"
#include "tcg/tlb_helper.h"

/*
 * arm_cpu_do_transaction_failed: handle a memory system error response
 * (eg "no device/memory present at address") by raising an external abort
 * exception
 */
void arm_cpu_do_transaction_failed(CPUState *cs, hwaddr physaddr,
                                   vaddr addr, unsigned size,
                                   MMUAccessType access_type,
                                   int mmu_idx, MemTxAttrs attrs,
                                   MemTxResult response, uintptr_t retaddr)
{
    ARMCPU *cpu = ARM_CPU(cs);
    ARMMMUFaultInfo fi = {};

    /* now we have a real cpu fault */
    cpu_restore_state(cs, retaddr, true);

    fi.ea = arm_extabort_type(response);
    fi.type = ARMFault_SyncExternal;
    arm_deliver_fault(cpu, addr, access_type, mmu_idx, &fi);
}

bool arm_cpu_tlb_fill(CPUState *cs, vaddr address, int size,
                      MMUAccessType access_type, int mmu_idx,
                      bool probe, uintptr_t retaddr)
{
    ARMCPU *cpu = ARM_CPU(cs);
    ARMMMUFaultInfo fi = {};
    hwaddr phys_addr;
    target_ulong page_size;
    int prot, ret;
    MemTxAttrs attrs = {};
    ARMCacheAttrs cacheattrs = {};

    /*
     * Walk the page table and (if the mapping exists) add the page
     * to the TLB.  On success, return true.  Otherwise, if probing,
     * return false.  Otherwise populate fsr with ARM DFSR/IFSR fault
     * register format, and signal the fault.
     */
    ret = get_phys_addr(&cpu->env, address, access_type,
                        core_to_arm_mmu_idx(&cpu->env, mmu_idx),
                        &phys_addr, &attrs, &prot, &page_size,
                        &fi, &cacheattrs);
    if (likely(!ret)) {
        /*
         * Map a single [sub]page. Regions smaller than our declared
         * target page size are handled specially, so for those we
         * pass in the exact addresses.
         */
        if (page_size >= TARGET_PAGE_SIZE) {
            phys_addr &= TARGET_PAGE_MASK;
            address &= TARGET_PAGE_MASK;
        }
        /* Notice and record tagged memory. */
        if (cpu_isar_feature(aa64_mte, cpu) && cacheattrs.attrs == 0xf0) {
            arm_tlb_mte_tagged(&attrs) = true;
        }

        tlb_set_page_with_attrs(cs, address, phys_addr, attrs,
                                prot, mmu_idx, page_size);
        return true;
    } else if (probe) {
        return false;
    } else {
        /* now we have a real cpu fault */
        cpu_restore_state(cs, retaddr, true);
        arm_deliver_fault(cpu, address, access_type, mmu_idx, &fi);
    }
}
