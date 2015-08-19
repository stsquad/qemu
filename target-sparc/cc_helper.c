/*
 * Helpers for lazy condition code handling
 *
 *  Copyright (c) 2003-2005 Fabrice Bellard
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

#include "cpu.h"
#include "exec/helper-proto.h"

uint32_t cpu_get_icc(CPUSPARCState *env)
{
    uint32_t res, n, z, v, c;

    n = env->cc_n;
    z = env->cc_z;
    v = env->cc_v;
    c = env->cc_ic;

    res = 0;
    res |= ((int32_t)n < 0 ? PSR_NEG : 0);
    res |= (z == 0 ? PSR_ZERO : 0);
    res |= ((int32_t)v < 0 ? PSR_OVF : 0);
    res |= c * PSR_CARRY;
    return res;
}

void cpu_put_icc(CPUSPARCState *env, uint32_t icc)
{
    uint32_t in, iz, iv, ic;

    in = (icc & PSR_NEG ? -1 : 0);
    iz = (icc & PSR_ZERO ? 0 : 1);
    iv = (icc & PSR_OVF ? -1 : 0);
    ic = (icc & PSR_CARRY ? 1 : 0);

#ifdef TARGET_SPARC64
    /* Don't clobber XCC.  */
    env->cc_n = deposit64(env->cc_n, 0, 32, in);
    env->cc_z = deposit64(env->cc_z, 0, 32, iz);
    env->cc_v = deposit64(env->cc_v, 0, 32, iv);
#else
    env->cc_n = in;
    env->cc_z = iz;
    env->cc_v = iv;
#endif
    env->cc_ic = ic;
}

#ifdef TARGET_SPARC64
static uint32_t cpu_get_xcc(CPUSPARCState *env)
{
    target_ulong n, z, v, c;
    uint32_t res;

    n = env->cc_n;
    z = env->cc_z;
    v = env->cc_v;
    c = env->cc_xc;

    res = 0;
    res |= ((target_long)n < 0 ? PSR_NEG : 0);
    res |= (z == 0 ? PSR_ZERO : 0);
    res |= ((target_long)v < 0 ? PSR_OVF : 0);
    res |= c * PSR_CARRY;
    return res;
}

uint64_t cpu_get_ccr(CPUSPARCState *env)
{
    uint32_t icc = cpu_get_icc(env);
    uint32_t xcc = cpu_get_xcc(env);
    return ((xcc >> 20) << 4) | (icc >> 20);
}

void cpu_put_ccr(CPUSPARCState *env, uint32_t val)
{
    uint32_t xcc, xn, xz, xv, xc;
    uint32_t icc, in, iz, iv, ic;

    xcc = val << (20 - 4);
    xn = (xcc & PSR_NEG ? -1 : 0);
    xz = (xcc & PSR_ZERO ? 0 : 1);
    xv = (xcc & PSR_OVF ? -1 : 0);
    xc = (xcc & PSR_CARRY ? 1 : 0);

    icc = val << 20;
    in = (icc & PSR_NEG ? -1 : 0);
    iz = (icc & PSR_ZERO ? 0 : 1);
    iv = (icc & PSR_OVF ? -1 : 0);
    ic = (icc & PSR_CARRY ? 1 : 0);

    env->cc_n = ((uint64_t)xn << 32) | in;
    env->cc_z = ((uint64_t)xz << 32) | iz;
    env->cc_v = ((uint64_t)xv << 32) | iv;
    env->cc_ic = ic;
    env->cc_xc = xc;
}
#endif
