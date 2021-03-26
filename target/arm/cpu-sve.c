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

void cpu_sve_finalize_features(ARMCPU *cpu, Error **errp)
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
    DECLARE_BITMAP(tmp, ARM_MAX_VQ);
    uint32_t vq, max_vq = 0;

    /* Collect the set of vector lengths supported by KVM. */
    bitmap_zero(kvm_supported, ARM_MAX_VQ);
    if (kvm_enabled() && kvm_arm_sve_supported()) {
        kvm_arm_sve_get_vls(CPU(cpu), kvm_supported);
    } else if (kvm_enabled()) {
        assert(!cpu_isar_feature(aa64_sve, cpu));
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
            return;
        }

        if (kvm_enabled()) {
            /*
             * For KVM we have to automatically enable all supported unitialized
             * lengths, even when the smaller lengths are not all powers-of-two.
             */
            bitmap_andnot(tmp, kvm_supported, cpu->sve_vq_init, max_vq);
            bitmap_or(cpu->sve_vq_map, cpu->sve_vq_map, tmp, max_vq);
        } else if (tcg_enabled()) {
            /* Propagate enabled bits down through required powers-of-two. */
            for (vq = pow2floor(max_vq); vq >= 1; vq >>= 1) {
                if (!test_bit(vq - 1, cpu->sve_vq_init)) {
                    set_bit(vq - 1, cpu->sve_vq_map);
                }
            }
        }
    } else if (cpu->sve_max_vq == 0) {
        /*
         * No explicit bits enabled, and no implicit bits from sve-max-vq.
         */
        if (!cpu_isar_feature(aa64_sve, cpu)) {
            /* SVE is disabled and so are all vector lengths.  Good. */
            return;
        }

        if (kvm_enabled()) {
            /* Disabling a supported length disables all larger lengths. */
            for (vq = 1; vq <= ARM_MAX_VQ; ++vq) {
                if (test_bit(vq - 1, cpu->sve_vq_init) &&
                    test_bit(vq - 1, kvm_supported)) {
                    break;
                }
            }
            max_vq = vq <= ARM_MAX_VQ ? vq - 1 : ARM_MAX_VQ;
            bitmap_andnot(cpu->sve_vq_map, kvm_supported,
                          cpu->sve_vq_init, max_vq);
            if (max_vq == 0 || bitmap_empty(cpu->sve_vq_map, max_vq)) {
                error_setg(errp, "cannot disable sve%d", vq * 128);
                error_append_hint(errp, "Disabling sve%d results in all "
                                  "vector lengths being disabled.\n",
                                  vq * 128);
                error_append_hint(errp, "With SVE enabled, at least one "
                                  "vector length must be enabled.\n");
                return;
            }
        } else if (tcg_enabled()) {
            /* Disabling a power-of-two disables all larger lengths. */
            if (test_bit(0, cpu->sve_vq_init)) {
                error_setg(errp, "cannot disable sve128");
                error_append_hint(errp, "Disabling sve128 results in all "
                                  "vector lengths being disabled.\n");
                error_append_hint(errp, "With SVE enabled, at least one "
                                  "vector length must be enabled.\n");
                return;
            }
            for (vq = 2; vq <= ARM_MAX_VQ; vq <<= 1) {
                if (test_bit(vq - 1, cpu->sve_vq_init)) {
                    break;
                }
            }
            max_vq = vq <= ARM_MAX_VQ ? vq - 1 : ARM_MAX_VQ;
            bitmap_complement(cpu->sve_vq_map, cpu->sve_vq_init, max_vq);
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

        if (!test_bit(max_vq - 1, cpu->sve_vq_map) &&
            test_bit(max_vq - 1, cpu->sve_vq_init)) {
            error_setg(errp, "cannot disable sve%d", max_vq * 128);
            error_append_hint(errp, "The maximum vector length must be "
                              "enabled, sve-max-vq=%d (%d bits)\n",
                              max_vq, max_vq * 128);
            return;
        }

        /* Set all bits not explicitly set within sve-max-vq. */
        bitmap_complement(tmp, cpu->sve_vq_init, max_vq);
        bitmap_or(cpu->sve_vq_map, cpu->sve_vq_map, tmp, max_vq);
    }

    /*
     * We should know what max-vq is now.  Also, as we're done
     * manipulating sve-vq-map, we ensure any bits above max-vq
     * are clear, just in case anybody looks.
     */
    assert(max_vq != 0);
    bitmap_clear(cpu->sve_vq_map, max_vq, ARM_MAX_VQ - max_vq);

    if (kvm_enabled()) {
        /* Ensure the set of lengths matches what KVM supports. */
        bitmap_xor(tmp, cpu->sve_vq_map, kvm_supported, max_vq);
        if (!bitmap_empty(tmp, max_vq)) {
            vq = find_last_bit(tmp, max_vq) + 1;
            if (test_bit(vq - 1, cpu->sve_vq_map)) {
                if (cpu->sve_max_vq) {
                    error_setg(errp, "cannot set sve-max-vq=%d",
                               cpu->sve_max_vq);
                    error_append_hint(errp, "This KVM host does not support "
                                      "the vector length %d-bits.\n",
                                      vq * 128);
                    error_append_hint(errp, "It may not be possible to use "
                                      "sve-max-vq with this KVM host. Try "
                                      "using only sve<N> properties.\n");
                } else {
                    error_setg(errp, "cannot enable sve%d", vq * 128);
                    error_append_hint(errp, "This KVM host does not support "
                                      "the vector length %d-bits.\n",
                                      vq * 128);
                }
            } else {
                error_setg(errp, "cannot disable sve%d", vq * 128);
                error_append_hint(errp, "The KVM host requires all "
                                  "supported vector lengths smaller "
                                  "than %d bits to also be enabled.\n",
                                  max_vq * 128);
            }
            return;
        }
    } else if (tcg_enabled()) {
        /* Ensure all required powers-of-two are enabled. */
        for (vq = pow2floor(max_vq); vq >= 1; vq >>= 1) {
            if (!test_bit(vq - 1, cpu->sve_vq_map)) {
                error_setg(errp, "cannot disable sve%d", vq * 128);
                error_append_hint(errp, "sve%d is required as it "
                                  "is a power-of-two length smaller than "
                                  "the maximum, sve%d\n",
                                  vq * 128, max_vq * 128);
                return;
            }
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
        return;
    }

    /* From now on sve_max_vq is the actual maximum supported length. */
    cpu->sve_max_vq = max_vq;
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
