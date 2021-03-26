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
#include "cpu-sve.h"
#include "tcg-sve.h"
#include "cpu-exceptions-aa64.h"

void tcg_sve_enable_lens(unsigned long *sve_vq_map,
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

uint32_t tcg_sve_disable_lens(unsigned long *sve_vq_map,
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

bool tcg_sve_validate_lens(unsigned long *sve_vq_map, uint32_t max_vq,
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

/*
 * The manual says that when SVE is enabled and VQ is widened the
 * implementation is allowed to zero the previously inaccessible
 * portion of the registers.  The corollary to that is that when
 * SVE is enabled and VQ is narrowed we are also allowed to zero
 * the now inaccessible portion of the registers.
 *
 * The intent of this is that no predicate bit beyond VQ is ever set.
 * Which means that some operations on predicate registers themselves
 * may operate on full uint64_t or even unrolled across the maximum
 * uint64_t[4].  Performing 4 bits of host arithmetic unconditionally
 * may well be cheaper than conditionals to restrict the operation
 * to the relevant portion of a uint16_t[16].
 */
void tcg_sve_narrow_vq(CPUARMState *env, unsigned vq)
{
    int i, j;
    uint64_t pmask;

    assert(vq >= 1 && vq <= ARM_MAX_VQ);
    assert(vq <= env_archcpu(env)->sve_max_vq);

    /* Zap the high bits of the zregs.  */
    for (i = 0; i < 32; i++) {
        memset(&env->vfp.zregs[i].d[2 * vq], 0, 16 * (ARM_MAX_VQ - vq));
    }

    /* Zap the high bits of the pregs and ffr.  */
    pmask = 0;
    if (vq & 3) {
        pmask = ~(-1ULL << (16 * (vq & 3)));
    }
    for (j = vq / 4; j < ARM_MAX_VQ / 4; j++) {
        for (i = 0; i < 17; ++i) {
            env->vfp.pregs[i].p[j] &= pmask;
        }
        pmask = 0;
    }
}

/*
 * Notice a change in SVE vector size when changing EL.
 */
void tcg_sve_change_el(CPUARMState *env, int old_el,
                           int new_el, bool el0_a64)
{
    ARMCPU *cpu = env_archcpu(env);
    int old_len, new_len;
    bool old_a64, new_a64;

    /* Nothing to do if no SVE.  */
    if (!cpu_isar_feature(aa64_sve, cpu)) {
        return;
    }

    /* Nothing to do if FP is disabled in either EL.  */
    if (fp_exception_el(env, old_el) || fp_exception_el(env, new_el)) {
        return;
    }

    /*
     * DDI0584A.d sec 3.2: "If SVE instructions are disabled or trapped
     * at ELx, or not available because the EL is in AArch32 state, then
     * for all purposes other than a direct read, the ZCR_ELx.LEN field
     * has an effective value of 0".
     *
     * Consider EL2 (aa64, vq=4) -> EL0 (aa32) -> EL1 (aa64, vq=0).
     * If we ignore aa32 state, we would fail to see the vq4->vq0 transition
     * from EL2->EL1.  Thus we go ahead and narrow when entering aa32 so that
     * we already have the correct register contents when encountering the
     * vq0->vq0 transition between EL0->EL1.
     */
    old_a64 = old_el ? arm_el_is_aa64(env, old_el) : el0_a64;
    old_len = (old_a64 && !sve_exception_el(env, old_el)
               ? cpu_sve_get_zcr_len_for_el(env, old_el) : 0);
    new_a64 = new_el ? arm_el_is_aa64(env, new_el) : el0_a64;
    new_len = (new_a64 && !sve_exception_el(env, new_el)
               ? cpu_sve_get_zcr_len_for_el(env, new_el) : 0);

    /* When changing vector length, clear inaccessible state.  */
    if (new_len < old_len) {
        tcg_sve_narrow_vq(env, new_len + 1);
    }
}
