/*
 * QEMU AArch64 CPU SVE KVM interface
 *
 * Copyright 2021 SUSE LLC
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#ifndef KVM_CPU_SVE_H
#define KVM_CPU_SVE_H

/* note: SVE is an AARCH64-only option, only include this for TARGET_AARCH64 */

void kvm_cpu_sve_get_supported_lens(ARMCPU *cpu,
                                    unsigned long *kvm_supported);

void kvm_cpu_sve_enable_lens(unsigned long *sve_vq_map,
                             unsigned long *sve_vq_init, uint32_t max_vq,
                             unsigned long *kvm_supported);

uint32_t kvm_cpu_sve_disable_lens(unsigned long *sve_vq_map,
                                  unsigned long *sve_vq_init,
                                  unsigned long *kvm_supported, Error **errp);

bool kvm_cpu_sve_validate_lens(unsigned long *sve_vq_map, uint32_t max_vq,
                               unsigned long *kvm_supported, Error **errp,
                               uint32_t sve_max_vq);

#endif /* KVM_CPU_SVE_H */
