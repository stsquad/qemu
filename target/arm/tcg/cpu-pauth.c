/*
 * QEMU AArch64 Pointer Authentication Extensions
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
#include "tcg/cpu-pauth.h"
#include "hw/qdev-properties.h"

void arm_cpu_pauth_finalize(ARMCPU *cpu, Error **errp)
{
    int arch_val = 0, impdef_val = 0;
    uint64_t t;

    /* TODO: Handle HaveEnhancedPAC, HaveEnhancedPAC2, HaveFPAC. */
    if (cpu->prop_pauth) {
        if (cpu->prop_pauth_impdef) {
            impdef_val = 1;
        } else {
            arch_val = 1;
        }
    } else if (cpu->prop_pauth_impdef) {
        error_setg(errp, "cannot enable pauth-impdef without pauth");
        error_append_hint(errp, "Add pauth=on to the CPU property list.\n");
    }

    t = cpu->isar.id_aa64isar1;
    t = FIELD_DP64(t, ID_AA64ISAR1, APA, arch_val);
    t = FIELD_DP64(t, ID_AA64ISAR1, GPA, arch_val);
    t = FIELD_DP64(t, ID_AA64ISAR1, API, impdef_val);
    t = FIELD_DP64(t, ID_AA64ISAR1, GPI, impdef_val);
    cpu->isar.id_aa64isar1 = t;
}

static Property arm_cpu_pauth_property =
    DEFINE_PROP_BOOL("pauth", ARMCPU, prop_pauth, true);
static Property arm_cpu_pauth_impdef_property =
    DEFINE_PROP_BOOL("pauth-impdef", ARMCPU, prop_pauth_impdef, false);

void cpu_pauth_add_props(Object *obj)
{
    /* Default to PAUTH on, with the architected algorithm. */
    qdev_property_add_static(DEVICE(obj), &arm_cpu_pauth_property);
    qdev_property_add_static(DEVICE(obj), &arm_cpu_pauth_impdef_property);
}
