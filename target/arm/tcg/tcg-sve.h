/*
 * QEMU AArch64 CPU SVE TCG interface
 *
 * Copyright 2021 SUSE LLC
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#ifndef TCG_SVE_H
#define TCG_SVE_H

/* note: SVE is an AARCH64-only option, only include this for TARGET_AARCH64 */

void tcg_sve_enable_lens(unsigned long *sve_vq_map,
                         unsigned long *sve_vq_init, uint32_t max_vq);

uint32_t tcg_sve_disable_lens(unsigned long *sve_vq_map,
                              unsigned long *sve_vq_init, Error **errp);

bool tcg_sve_validate_lens(unsigned long *sve_vq_map, uint32_t max_vq,
                           Error **errp);

void tcg_sve_narrow_vq(CPUARMState *env, unsigned vq);

void tcg_sve_change_el(CPUARMState *env, int old_el,
                       int new_el, bool el0_a64);

#endif /* TCG_SVE_H */
