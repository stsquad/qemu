/*
 * QEMU ARM CPU models (32bit)
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

#ifndef ARM_CPU32_H
#define ARM_CPU32_H

void arm_cpu_dump_state(CPUState *cs, FILE *f, int flags);
void arm32_cpu_class_init(ObjectClass *oc, void *data);
void arm32_cpu_register(const ARMCPUInfo *info);

#endif /* ARM_CPU32_H */
