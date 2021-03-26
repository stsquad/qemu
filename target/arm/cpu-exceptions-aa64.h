/*
 * QEMU AArch64 CPU Exceptions Sysemu code
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

#ifndef CPU_EXCEPTIONS_AA64_H
#define CPU_EXCEPTIONS_AA64_H

#include "cpu.h"

int sve_exception_el(CPUARMState *env, int el);
void aarch64_sync_64_to_32(CPUARMState *env);
void aarch64_sync_32_to_64(CPUARMState *env);
void arm_cpu_do_interrupt_aarch64(CPUState *cs);

#endif /* CPU_EXCEPTIONS_AA64_H */

