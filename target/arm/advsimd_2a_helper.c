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
 * These particular helpers use the existing SoftFloat2a code
 */
#include "qemu/osdep.h"

#include "cpu.h"
#include "exec/exec-all.h"
#include "exec/helper-proto.h"

#define ADVSIMD_HELPER(name, suffix) HELPER(glue(glue(advsimd_, name), suffix))

#define ADVSIMD_HALFOP(name) \
float16 ADVSIMD_HELPER(name, h)(float16 a, float16 b, void *fpstp) \
{ \
    float_status *fpst = fpstp; \
    return float16_ ## name(a, b, fpst);    \
}

ADVSIMD_HALFOP(min)
ADVSIMD_HALFOP(max)
ADVSIMD_HALFOP(minnum)
ADVSIMD_HALFOP(maxnum)
