/*
 * ARM generic timer definitions for Arm A-class CPU
 *
 *  Copyright (c) 2003 Fabrice Bellard
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 */

#ifndef TARGET_ARM_GTIMER_H
#define TARGET_ARM_GTIMER_H

enum {
    GTIMER_PHYS     = 0, /* EL1 physical timer */
    GTIMER_VIRT     = 1, /* EL1 virtual timer */
    GTIMER_HYP      = 2, /* EL2 physical timer */
    GTIMER_SEC      = 3, /* EL3 physical timer */
    GTIMER_HYPVIRT  = 4, /* EL2 virtual timer */
#define NUM_GTIMERS   5
};

#endif
