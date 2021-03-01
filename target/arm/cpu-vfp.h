/*
 * ARM VFP floating-point operations internals
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

#ifndef CPU_VFP_H
#define CPU_VFP_H

#include "qemu/osdep.h"
#include "cpu.h"

uint32_t vfp_get_fpscr_from_host(CPUARMState *env);
void vfp_set_fpscr_to_host(CPUARMState *env, uint32_t val);

#endif /* CPU_VFP_H */
