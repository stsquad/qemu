/*
 * QEMU AArch64 Pointer Authentication Extensions
 *
 * Copyright (c) 2013 Linaro Ltd
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

#ifndef CPU_PAUTH_H
#define CPU_PAUTH_H

/* ARMv8.3 pauth is an AARCH64 option, only include this for TARGET_AARCH64 */

#include "cpu.h"

/* called by arm_cpu_finalize_features in realizefn */
bool cpu_pauth_finalize(ARMCPU *cpu, Error **errp);

/* add the CPU Pointer Authentication properties */
void cpu_pauth_add_props(Object *obj);

#endif /* CPU_PAUTH_H */
