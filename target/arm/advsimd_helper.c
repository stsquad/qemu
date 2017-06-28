/*
 * ARM AdvancedSIMD helper functions
 *
 * Copyright (c) 2017 Linaro.
 * Author: Alex Bennée <alex.bennee@linaro.org>
 *
 * This code is licensed under the GNU GPL v2.
 *
 * This code is specifically for AdvancedSIMD helpers rather than
 * shared NEON helpers which are re-purposed for ARMv8. In practice
 * these are helpers for newer features not found in older ARMs,
 * currently half-precision float support.
 *
 * As such these helpers use the SoftFloat3c code. As we currently use
 * both SoftFloat cores we copy the SoftFloat2a status bits to 3c
 * before the operation and copy back at the end.
 */
#include "qemu/osdep.h"

#include "cpu.h"
#include "exec/exec-all.h"
#include "exec/helper-proto.h"

#include "fpu/softfloat3c/platform.h"
#include "fpu/softfloat3c/softfloat.h"
#include "fpu/softfloat3c/softfloat-internals.h"

typedef struct softfloat_flags {
    uint8_t float2a_flag;
    uint8_t float3c_flag;
} softfloat_flags;

static softfloat_flags softfloat_mapping_table[] = {
    { float_flag_inexact  , softfloat_flag_inexact },
    { float_flag_underflow, softfloat_flag_underflow },
    { float_flag_overflow , softfloat_flag_overflow },
    { float_flag_invalid  , softfloat_flag_invalid },
};
/* FIXME: 2a has no infinite flag */

static uint8_t sync_softfloat_flags_from_2a(float_status *flags2a)
{
    uint8_t flags = flags2a->float_exception_flags;
    int i;
    if (flags) {
        for (i = 0; i < ARRAY_SIZE(softfloat_mapping_table); i++) {
            struct softfloat_flags *map = &softfloat_mapping_table[i];
            if (flags & map->float2a_flag) {
                softfloat_raiseFlags(map->float3c_flag);
            }
        }
    } else {
        softfloat_exceptionFlags = 0;
    }

    return softfloat_exceptionFlags;
}

static void sync_softfloat_flags_to_2a(float_status *flags2a)
{
    int i;
    if (softfloat_exceptionFlags) {
        for (i = 0; i < ARRAY_SIZE(softfloat_mapping_table); i++) {
            struct softfloat_flags *map = &softfloat_mapping_table[i];
            if (softfloat_exceptionFlags & map->float3c_flag) {
                float_raise(map->float2a_flag, flags2a);
            }
        }
    } else {
        flags2a->float_exception_flags = 0;
    }
}


uint32_t HELPER(advsimd_acgt_f16)(uint32_t a, uint32_t b, void *fpstp)
{
    union ui16_f16 uA = { .ui = (a & 0x7fff) };
    union ui16_f16 uB = { .ui = (b & 0x7fff) };
    uint32_t r;

    sync_softfloat_flags_from_2a(fpstp);
    r = -f16_lt(uB.f, uA.f);
    sync_softfloat_flags_to_2a(fpstp);
    return r;
}


/* Data processing - scalar floating-point and advanced SIMD */

uint32_t HELPER(advsimd_addh)(uint32_t a, uint32_t b, void *fpstp)
{
    union ui16_f16 uA = { .ui = a };
    union ui16_f16 uB = { .ui = b };
    float16_t r;

    sync_softfloat_flags_from_2a(fpstp);
    r = f16_add(uA.f, uB.f);
    sync_softfloat_flags_to_2a(fpstp);
    return r.v;
}
