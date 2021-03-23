/*
 * QEMU ARM CPU
 *
 * Copyright (c) 2012 SUSE LINUX Products GmbH
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see
 * <http://www.gnu.org/licenses/gpl-2.0.html>
 */

#include "qemu/osdep.h"
#include "qapi/error.h"
#include "cpu.h"
#include "sysemu/kvm.h"
#include "kvm/kvm_arm.h"
#include "kvm/cpu-sve.h"

void kvm_cpu_sve_get_supported_lens(ARMCPU *cpu, unsigned long *kvm_supported)
{
    /* Collect the set of vector lengths supported by KVM. */
    bitmap_zero(kvm_supported, ARM_MAX_VQ);

    if (kvm_arm_sve_supported()) {
        kvm_arm_sve_get_vls(CPU(cpu), kvm_supported);
    } else {
        assert(!cpu_isar_feature(aa64_sve, cpu));
    }
}

void kvm_cpu_sve_enable_lens(unsigned long *sve_vq_map,
                             unsigned long *sve_vq_init, uint32_t max_vq,
                             unsigned long *kvm_supported)
{
    /*
     * For KVM we have to automatically enable all supported unitialized
     * lengths, even when the smaller lengths are not all powers-of-two.
     */
    DECLARE_BITMAP(tmp, ARM_MAX_VQ);

    bitmap_andnot(tmp, kvm_supported, sve_vq_init, max_vq);
    bitmap_or(sve_vq_map, sve_vq_map, tmp, max_vq);
}

uint32_t kvm_cpu_sve_disable_lens(unsigned long *sve_vq_map,
                                  unsigned long *sve_vq_init,
                                  unsigned long *kvm_supported, Error **errp)
{
    uint32_t max_vq, vq;

    /* Disabling a supported length disables all larger lengths. */
    for (vq = 1; vq <= ARM_MAX_VQ; ++vq) {
        if (test_bit(vq - 1, sve_vq_init) &&
            test_bit(vq - 1, kvm_supported)) {
            break;
        }
    }

    max_vq = vq <= ARM_MAX_VQ ? vq - 1 : ARM_MAX_VQ;
    bitmap_andnot(sve_vq_map, kvm_supported, sve_vq_init, max_vq);

    if (max_vq == 0 || bitmap_empty(sve_vq_map, max_vq)) {
        error_setg(errp, "cannot disable sve%d", vq * 128);
        error_append_hint(errp, "Disabling sve%d results in all "
                          "vector lengths being disabled.\n",
                          vq * 128);
        error_append_hint(errp, "With SVE enabled, at least one "
                          "vector length must be enabled.\n");
        return 0;
    }

    return max_vq;
}

bool kvm_cpu_sve_validate_lens(unsigned long *sve_vq_map, uint32_t max_vq,
                               unsigned long *kvm_supported, Error **errp,
                               uint32_t sve_max_vq)
{
    /* Ensure the set of lengths matches what KVM supports. */
    DECLARE_BITMAP(tmp, ARM_MAX_VQ);
    uint32_t vq;

    bitmap_xor(tmp, sve_vq_map, kvm_supported, max_vq);
    if (bitmap_empty(tmp, max_vq)) {
        return true;
    }

    vq = find_last_bit(tmp, max_vq) + 1;
    if (test_bit(vq - 1, sve_vq_map)) {
        if (sve_max_vq) {
            error_setg(errp, "cannot set sve-max-vq=%d", sve_max_vq);
            error_append_hint(errp, "This KVM host does not support "
                              "the vector length %d-bits.\n", vq * 128);
            error_append_hint(errp, "It may not be possible to use "
                              "sve-max-vq with this KVM host. Try "
                              "using only sve<N> properties.\n");
        } else {
            error_setg(errp, "cannot enable sve%d", vq * 128);
            error_append_hint(errp, "This KVM host does not support "
                              "the vector length %d-bits.\n", vq * 128);
        }
    } else {
        error_setg(errp, "cannot disable sve%d", vq * 128);
        error_append_hint(errp, "The KVM host requires all "
                          "supported vector lengths smaller "
                          "than %d bits to also be enabled.\n", max_vq * 128);
    }
    return false;
}
