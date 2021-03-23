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
#include "tcg/tlb_helper.h"

bool arm_cpu_tlb_fill(CPUState *cs, vaddr address, int size,
                      MMUAccessType access_type, int mmu_idx,
                      bool probe, uintptr_t retaddr)
{
    ARMCPU *cpu = ARM_CPU(cs);
    ARMMMUFaultInfo fi = {};

    int flags = page_get_flags(useronly_clean_ptr(address));
    if (flags & PAGE_VALID) {
        fi.type = ARMFault_Permission;
    } else {
        fi.type = ARMFault_Translation;
    }

    /* now we have a real cpu fault */
    cpu_restore_state(cs, retaddr, true);
    arm_deliver_fault(cpu, address, access_type, mmu_idx, &fi);
}
