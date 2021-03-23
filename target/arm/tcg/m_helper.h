/*
 * ARM v7m generic helpers.
 *
 * This code is licensed under the GNU GPL v2 or later.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#ifndef M_HELPER_H
#define M_HELPER_H

#include "cpu.h"

void v7m_msr_xpsr(CPUARMState *env, uint32_t mask,
                  uint32_t reg, uint32_t val);

uint32_t v7m_mrs_xpsr(CPUARMState *env, uint32_t reg, unsigned el);

uint32_t v7m_mrs_control(CPUARMState *env, uint32_t secure);

#endif /* M_HELPER_H */
