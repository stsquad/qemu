/*
 * ARM VFP floating-point operations
 *
 *  Copyright (c) 2003 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "cpu.h"
#include "cpu-vfp.h"
#include "sysemu/tcg.h"

uint32_t vfp_get_fpscr(CPUARMState *env)
{
    uint32_t i, fpscr;

    fpscr = env->vfp.xregs[ARM_VFP_FPSCR]
            | (env->vfp.vec_len << 16)
            | (env->vfp.vec_stride << 20);

    /*
     * M-profile LTPSIZE overlaps A-profile Stride; whichever of the
     * two is not applicable to this CPU will always be zero.
     */
    fpscr |= env->v7m.ltpsize << 16;

    if (tcg_enabled()) {
        fpscr |= vfp_get_fpscr_from_host(env);
    }

    i = env->vfp.qc[0] | env->vfp.qc[1] | env->vfp.qc[2] | env->vfp.qc[3];
    fpscr |= i ? FPCR_QC : 0;

    return fpscr;
}

void vfp_set_fpscr(CPUARMState *env, uint32_t val)
{
    /* When ARMv8.2-FP16 is not supported, FZ16 is RES0.  */
    if (!cpu_isar_feature(any_fp16, env_archcpu(env))) {
        val &= ~FPCR_FZ16;
    }

    if (tcg_enabled()) {
        vfp_set_fpscr_to_host(env, val);
    }

    if (!arm_feature(env, ARM_FEATURE_M)) {
        /*
         * Short-vector length and stride; on M-profile these bits
         * are used for different purposes.
         * We can't make this conditional be "if MVFR0.FPShVec != 0",
         * because in v7A no-short-vector-support cores still had to
         * allow Stride/Len to be written with the only effect that
         * some insns are required to UNDEF if the guest sets them.
         *
         * TODO: if M-profile MVE implemented, set LTPSIZE.
         */
        env->vfp.vec_len = extract32(val, 16, 3);
        env->vfp.vec_stride = extract32(val, 20, 2);
    }

    if (arm_feature(env, ARM_FEATURE_NEON)) {
        /*
         * The bit we set within fpscr_q is arbitrary; the register as a
         * whole being zero/non-zero is what counts.
         * TODO: M-profile MVE also has a QC bit.
         */
        env->vfp.qc[0] = val & FPCR_QC;
        env->vfp.qc[1] = 0;
        env->vfp.qc[2] = 0;
        env->vfp.qc[3] = 0;
    }

    /*
     * We don't implement trapped exception handling, so the
     * trap enable bits, IDE|IXE|UFE|OFE|DZE|IOE are all RAZ/WI (not RES0!)
     *
     * The exception flags IOC|DZC|OFC|UFC|IXC|IDC are stored in
     * fp_status; QC, Len and Stride are stored separately earlier.
     * Clear out all of those and the RES0 bits: only NZCV, AHP, DN,
     * FZ, RMode and FZ16 are kept in vfp.xregs[FPSCR].
     */
    env->vfp.xregs[ARM_VFP_FPSCR] = val & 0xf7c80000;
}
