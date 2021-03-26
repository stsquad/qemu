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
#include "sysemu/kvm.h"
#include "kvm/kvm_arm.h"
#include "qapi/visitor.h"
#include "cpu-sve.h"

#include "tcg/tcg-sve.h"
#include "kvm/kvm-sve.h"

static bool apply_max_vq(unsigned long *sve_vq_map, unsigned long *sve_vq_init,
                         uint32_t max_vq, Error **errp)
{
    DECLARE_BITMAP(tmp, ARM_MAX_VQ);

    if (!test_bit(max_vq - 1, sve_vq_map) &&
        test_bit(max_vq - 1, sve_vq_init)) {
        error_setg(errp, "cannot disable sve%d", max_vq * 128);
        error_append_hint(errp, "The maximum vector length must be "
                          "enabled, sve-max-vq=%d (%d bits)\n",
                          max_vq, max_vq * 128);
        return false;
    }
    /* Set all bits not explicitly set within sve-max-vq. */
    bitmap_complement(tmp, sve_vq_init, max_vq);
    bitmap_or(sve_vq_map, sve_vq_map, tmp, max_vq);
    return true;
}

bool cpu_sve_finalize_features(ARMCPU *cpu, Error **errp)
{
    /*
     * If any vector lengths are explicitly enabled with sve<N> properties,
     * then all other lengths are implicitly disabled.  If sve-max-vq is
     * specified then it is the same as explicitly enabling all lengths
     * up to and including the specified maximum, which means all larger
     * lengths will be implicitly disabled.  If no sve<N> properties
     * are enabled and sve-max-vq is not specified, then all lengths not
     * explicitly disabled will be enabled.  Additionally, all power-of-two
     * vector lengths less than the maximum enabled length will be
     * automatically enabled and all vector lengths larger than the largest
     * disabled power-of-two vector length will be automatically disabled.
     * Errors are generated if the user provided input that interferes with
     * any of the above.  Finally, if SVE is not disabled, then at least one
     * vector length must be enabled.
     */
    DECLARE_BITMAP(kvm_supported, ARM_MAX_VQ);
    uint32_t max_vq = 0;

    if (kvm_enabled()) {
        kvm_sve_get_supported_lens(cpu, kvm_supported);
    }
    /*
     * Process explicit sve<N> properties.
     * From the properties, sve_vq_map<N> implies sve_vq_init<N>.
     * Check first for any sve<N> enabled.
     */
    if (!bitmap_empty(cpu->sve_vq_map, ARM_MAX_VQ)) {
        max_vq = find_last_bit(cpu->sve_vq_map, ARM_MAX_VQ) + 1;

        if (cpu->sve_max_vq && max_vq > cpu->sve_max_vq) {
            error_setg(errp, "cannot enable sve%d", max_vq * 128);
            error_append_hint(errp, "sve%d is larger than the maximum vector "
                              "length, sve-max-vq=%d (%d bits)\n",
                              max_vq * 128, cpu->sve_max_vq,
                              cpu->sve_max_vq * 128);
            return false;
        }
        if (kvm_enabled()) {
            kvm_sve_enable_lens(cpu->sve_vq_map, cpu->sve_vq_init, max_vq,
                                kvm_supported);
        } else if (tcg_enabled()) {
            tcg_sve_enable_lens(cpu->sve_vq_map, cpu->sve_vq_init, max_vq);
        }
    } else if (cpu->sve_max_vq == 0) {
        /* No explicit bits enabled, and no implicit bits from sve-max-vq. */
        if (!cpu_isar_feature(aa64_sve, cpu)) {
            /* SVE is disabled and so are all vector lengths.  Good. */
            return true;
        }
        if (kvm_enabled()) {
            max_vq = kvm_sve_disable_lens(cpu->sve_vq_map, cpu->sve_vq_init,
                                          kvm_supported, errp);
        } else if (tcg_enabled()) {
            max_vq = tcg_sve_disable_lens(cpu->sve_vq_map, cpu->sve_vq_init,
                                          errp);
        }
        if (!max_vq) {
            return false;
        }
        max_vq = find_last_bit(cpu->sve_vq_map, max_vq) + 1;
    }

    /*
     * Process the sve-max-vq property.
     * Note that we know from the above that no bit above
     * sve-max-vq is currently set.
     */
    if (cpu->sve_max_vq != 0) {
        max_vq = cpu->sve_max_vq;
        if (!apply_max_vq(cpu->sve_vq_map, cpu->sve_vq_init, max_vq,
                          errp)) {
            return false;
        }
    }
    /*
     * We should know what max-vq is now.  Also, as we're done
     * manipulating sve-vq-map, we ensure any bits above max-vq
     * are clear, just in case anybody looks.
     */
    assert(max_vq != 0);
    bitmap_clear(cpu->sve_vq_map, max_vq, ARM_MAX_VQ - max_vq);

    if (kvm_enabled()) {
        if (!kvm_sve_validate_lens(cpu->sve_vq_map, max_vq, kvm_supported,
                                   errp, cpu->sve_max_vq)) {
            return false;
        }
    } else if (tcg_enabled()) {
        if (!tcg_sve_validate_lens(cpu->sve_vq_map, max_vq, errp)) {
            return false;
        }
    }

    /*
     * Now that we validated all our vector lengths, the only question
     * left to answer is if we even want SVE at all.
     */
    if (!cpu_isar_feature(aa64_sve, cpu)) {
        error_setg(errp, "cannot enable sve%d", max_vq * 128);
        error_append_hint(errp, "SVE must be enabled to enable vector "
                          "lengths.\n");
        error_append_hint(errp, "Add sve=on to the CPU property list.\n");
        return false;
    }

    /* From now on sve_max_vq is the actual maximum supported length. */
    cpu->sve_max_vq = max_vq;
    return true;
}

static void get_prop_max_vq(Object *obj, Visitor *v, const char *name,
                            void *opaque, Error **errp)
{
    ARMCPU *cpu = ARM_CPU(obj);
    uint32_t value;

    /* All vector lengths are disabled when SVE is off. */
    if (!cpu_isar_feature(aa64_sve, cpu)) {
        value = 0;
    } else {
        value = cpu->sve_max_vq;
    }
    visit_type_uint32(v, name, &value, errp);
}

static void set_prop_max_vq(Object *obj, Visitor *v, const char *name,
                            void *opaque, Error **errp)
{
    ARMCPU *cpu = ARM_CPU(obj);
    uint32_t max_vq;

    if (!visit_type_uint32(v, name, &max_vq, errp)) {
        return;
    }

    if (kvm_enabled() && !kvm_arm_sve_supported()) {
        error_setg(errp, "cannot set sve-max-vq");
        error_append_hint(errp, "SVE not supported by KVM on this host\n");
        return;
    }

    if (max_vq == 0 || max_vq > ARM_MAX_VQ) {
        error_setg(errp, "unsupported SVE vector length");
        error_append_hint(errp, "Valid sve-max-vq in range [1-%d]\n",
                          ARM_MAX_VQ);
        return;
    }

    cpu->sve_max_vq = max_vq;
}

/*
 * Note that cpu_arm_get/set_sve_vq cannot use the simpler
 * object_property_add_bool interface because they make use
 * of the contents of "name" to determine which bit on which
 * to operate.
 */
static void get_prop_vq(Object *obj, Visitor *v, const char *name,
                        void *opaque, Error **errp)
{
    ARMCPU *cpu = ARM_CPU(obj);
    uint32_t vq = atoi(&name[3]) / 128;
    bool value;

    /* All vector lengths are disabled when SVE is off. */
    if (!cpu_isar_feature(aa64_sve, cpu)) {
        value = false;
    } else {
        value = test_bit(vq - 1, cpu->sve_vq_map);
    }
    visit_type_bool(v, name, &value, errp);
}

static void set_prop_vq(Object *obj, Visitor *v, const char *name,
                        void *opaque, Error **errp)
{
    ARMCPU *cpu = ARM_CPU(obj);
    uint32_t vq = atoi(&name[3]) / 128;
    bool value;

    if (!visit_type_bool(v, name, &value, errp)) {
        return;
    }

    if (value && kvm_enabled() && !kvm_arm_sve_supported()) {
        error_setg(errp, "cannot enable %s", name);
        error_append_hint(errp, "SVE not supported by KVM on this host\n");
        return;
    }

    if (value) {
        set_bit(vq - 1, cpu->sve_vq_map);
    } else {
        clear_bit(vq - 1, cpu->sve_vq_map);
    }
    set_bit(vq - 1, cpu->sve_vq_init);
}

static bool get_prop_sve(Object *obj, Error **errp)
{
    ARMCPU *cpu = ARM_CPU(obj);
    return cpu_isar_feature(aa64_sve, cpu);
}

static void set_prop_sve(Object *obj, bool value, Error **errp)
{
    ARMCPU *cpu = ARM_CPU(obj);
    uint64_t t;

    if (value && kvm_enabled() && !kvm_arm_sve_supported()) {
        error_setg(errp, "'sve' feature not supported by KVM on this host");
        return;
    }

    t = cpu->isar.id_aa64pfr0;
    t = FIELD_DP64(t, ID_AA64PFR0, SVE, value);
    cpu->isar.id_aa64pfr0 = t;
}

void cpu_sve_add_props(Object *obj)
{
    uint32_t vq;

    object_property_add_bool(obj, "sve", get_prop_sve, set_prop_sve);

    for (vq = 1; vq <= ARM_MAX_VQ; ++vq) {
        char name[8];
        sprintf(name, "sve%d", vq * 128);
        object_property_add(obj, name, "bool", get_prop_vq, set_prop_vq, NULL, NULL);
    }
}

/* properties added for MAX CPU */
void cpu_sve_add_props_max(Object *obj)
{
    object_property_add(obj, "sve-max-vq", "uint32", get_prop_max_vq, set_prop_max_vq, NULL, NULL);
}

static uint32_t sve_zcr_get_valid_len(ARMCPU *cpu, uint32_t start_len)
{
    uint32_t end_len;

    end_len = start_len &= 0xf;
    if (!test_bit(start_len, cpu->sve_vq_map)) {
        end_len = find_last_bit(cpu->sve_vq_map, start_len);
        assert(end_len < start_len);
    }
    return end_len;
}

/*
 * Given that SVE is enabled, return the vector length for EL.
 */
uint32_t sve_zcr_len_for_el(CPUARMState *env, int el)
{
    ARMCPU *cpu = env_archcpu(env);
    uint32_t zcr_len = cpu->sve_max_vq - 1;

    if (el <= 1) {
        zcr_len = MIN(zcr_len, 0xf & (uint32_t)env->vfp.zcr_el[1]);
    }
    if (el <= 2 && arm_feature(env, ARM_FEATURE_EL2)) {
        zcr_len = MIN(zcr_len, 0xf & (uint32_t)env->vfp.zcr_el[2]);
    }
    if (arm_feature(env, ARM_FEATURE_EL3)) {
        zcr_len = MIN(zcr_len, 0xf & (uint32_t)env->vfp.zcr_el[3]);
    }

    return sve_zcr_get_valid_len(cpu, zcr_len);
}
