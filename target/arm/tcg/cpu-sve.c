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
#include "sysemu/tcg.h"
#include "tcg/cpu-sve.h"

void tcg_cpu_sve_enable_lens(unsigned long *sve_vq_map,
                             unsigned long *sve_vq_init, uint32_t max_vq)
{
    /* Propagate enabled bits down through required powers-of-two. */
    uint32_t vq;

    for (vq = pow2floor(max_vq); vq >= 1; vq >>= 1) {
        if (!test_bit(vq - 1, sve_vq_init)) {
            set_bit(vq - 1, sve_vq_map);
        }
    }
}

uint32_t tcg_cpu_sve_disable_lens(unsigned long *sve_vq_map,
                                  unsigned long *sve_vq_init, Error **errp)
{
    /* Disabling a power-of-two disables all larger lengths. */
    uint32_t max_vq, vq;

    if (test_bit(0, sve_vq_init)) {
        error_setg(errp, "cannot disable sve128");
        error_append_hint(errp, "Disabling sve128 results in all "
                          "vector lengths being disabled.\n");
        error_append_hint(errp, "With SVE enabled, at least one "
                          "vector length must be enabled.\n");
        return 0;
    }
    for (vq = 2; vq <= ARM_MAX_VQ; vq <<= 1) {
        if (test_bit(vq - 1, sve_vq_init)) {
            break;
        }
    }
    max_vq = vq <= ARM_MAX_VQ ? vq - 1 : ARM_MAX_VQ;
    bitmap_complement(sve_vq_map, sve_vq_init, max_vq);
    return max_vq;
}

bool tcg_cpu_sve_validate_lens(unsigned long *sve_vq_map, uint32_t max_vq,
                               Error **errp)
{
    /* Ensure all required powers-of-two are enabled. */
    uint32_t vq;

    for (vq = pow2floor(max_vq); vq >= 1; vq >>= 1) {
        if (!test_bit(vq - 1, sve_vq_map)) {
            error_setg(errp, "cannot disable sve%d", vq * 128);
            error_append_hint(errp, "sve%d is required as it "
                              "is a power-of-two length smaller than "
                              "the maximum, sve%d\n", vq * 128, max_vq * 128);
            return false;
        }
    }
    return true;
}
