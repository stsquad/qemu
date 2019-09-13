/*
 * Fused Multiply Add (Single)
 *
 * Copyright (c) 2019 Linaro
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <fenv.h>

#include "float_helpers.h"

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

typedef struct {
    int flag;
    char *desc;
} float_mapping;

float_mapping round_flags[] = {
    { FE_TONEAREST, "to nearest" },
#ifdef FE_UPWARD
    { FE_UPWARD, "upwards" },
#endif
#ifdef FE_DOWNWARD
    { FE_DOWNWARD, "downwards" },
#endif
    { FE_TOWARDZERO, "to zero" }
};

void print_result(float a, float b, float c, float r)
{
    char *a_fmt, *b_fmt, *c_fmt, *r_fmt, *flag_fmt;

    a_fmt = fmt_f32(a);
    b_fmt = fmt_f32(b);
    c_fmt = fmt_f32(c);
    r_fmt = fmt_f32(r);
    flag_fmt = fmt_flags();

    printf("op : %s * %s + %s\n", a_fmt, b_fmt, c_fmt);
    printf("res: %s flags=%s\n", r_fmt, flag_fmt);

    free(a_fmt);
    free(b_fmt);
    free(c_fmt);
    free(r_fmt);
    free(flag_fmt);
}


int main(int argc, char *argv[argc])
{
    int i, j, k;
    float a, b, c, r;

    for (i = 0; i < ARRAY_SIZE(round_flags); ++i) {
        fesetround(round_flags[i].flag);
        printf("### Rounding %s\n", round_flags[i].desc);
        for (j = 0; j < num_f32; j++) {
            for (k = 0; k < 3; k++) {
                a = get_f32(j + ((k)%3));
                b = get_f32(j + ((k+1)%3));
                c = get_f32(j + ((k+2)%3));
                feclearexcept(FE_ALL_EXCEPT);

                /* must be built with -O2 to generate fused op */
                r = a * b + c;

                print_result(a, b, c, r);
            }
        }
    }

    return 0;
}
