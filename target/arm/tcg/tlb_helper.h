/*
 * ARM TLB (Translation lookaside buffer) helpers.
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#ifndef TLB_HELPER_H
#define TLB_HELPER_H

#include "cpu.h"

void QEMU_NORETURN arm_deliver_fault(ARMCPU *cpu, vaddr addr,
                                     MMUAccessType access_type,
                                     int mmu_idx, ARMMMUFaultInfo *fi);

#endif /* TLB_HELPER_H */
