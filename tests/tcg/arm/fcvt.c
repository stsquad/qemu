/*
 * Test Floating Point Conversion
 */

/* we want additional float type definitions */
#define __STDC_WANT_IEC_60559_BFP_EXT__
#define __STDC_WANT_IEC_60559_TYPES_EXT__

#include <stdio.h>
#include <inttypes.h>
#include <math.h>
#include <float.h>
#include <fenv.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

static char flag_str[256];

static char *get_flag_state(int flags)
{
    if (flags) {
        snprintf(flag_str, sizeof(flag_str), "%s %s %s %s %s",
                 flags & FE_OVERFLOW ? "OVERFLOW" : "",
                 flags & FE_UNDERFLOW ? "UNDERFLOW" : "",
                 flags & FE_DIVBYZERO ? "DIV0" : "",
                 flags & FE_INEXACT ? "INEXACT" : "",
                 flags & FE_INVALID ? "INVALID" : "");
    } else {
        snprintf(flag_str, sizeof(flag_str), "OK");
    }

    return flag_str;
}

static void print_double_number(int i, double num)
{
    uint64_t double_as_hex = *(uint64_t *) &num;
    int flags = fetestexcept(FE_ALL_EXCEPT);
    char *fstr = get_flag_state(flags);

    printf("%02d DOUBLE: %02.20e / %#020" PRIx64 " (%#x => %s)\n",
           i, num, double_as_hex, flags, fstr);
}

static void print_single_number(int i, float num)
{
    uint32_t single_as_hex = *(uint32_t *) &num;
    int flags = fetestexcept(FE_ALL_EXCEPT);
    char *fstr = get_flag_state(flags);

    printf("%02d SINGLE: %02.20e / %#010x  (%#x => %s)\n",
           i, num, single_as_hex, flags, fstr);
}

static void print_half_number(int i, uint16_t num)
{
    int flags = fetestexcept(FE_ALL_EXCEPT);
    char *fstr = get_flag_state(flags);

    printf("%02d   HALF: %#04x  (%#x => %s)\n",
           i, num, flags, fstr);
}

static void print_int64(int i, int64_t num)
{
    uint64_t int64_as_hex = *(uint64_t *) &num;
    int flags = fetestexcept(FE_ALL_EXCEPT);
    char *fstr = get_flag_state(flags);

    printf("%02d  INT64: %20" PRId64 "/%#020" PRIx64 " (%#x => %s)\n",
           i, num, int64_as_hex, flags, fstr);
}

static void print_int32(int i, int32_t num)
{
    uint32_t int32_as_hex = *(uint32_t *) &num;
    int flags = fetestexcept(FE_ALL_EXCEPT);
    char *fstr = get_flag_state(flags);

    printf("%02d  INT32: %6" PRId32 "/%#10" PRIx32 " (%#x => %s)\n",
           i, num, int32_as_hex, flags, fstr);
}

#ifndef SNANF
/* Signaling NaN macros, if supported.  */
# if __GNUC_PREREQ(3, 3)
#  define SNANF (__builtin_nansf (""))
#  define SNAN (__builtin_nans (""))
#  define SNANL (__builtin_nansl (""))
# endif
#endif

float single_numbers[] = { -SNANF,
                           -NAN,
                           -INFINITY,
                           -FLT_MAX,
                           -1.111E+31,
                           -1.111E+30,
                           -1.08700982e-12,
                           -1.78051176e-20,
                           -FLT_MIN,
                           0.0,
                           FLT_MIN,
                           2.98023224e-08,
                           5.96046E-8, /* min positive FP16 subnormal */
                           6.09756E-5, /* max subnormal FP16 */
                           6.10352E-5, /* min positive normal FP16 */
                           1.0,
                           1.0009765625, /* smallest float after 1.0 FP16 */
                           2.0,
                           M_E, M_PI,
                           65503.0,
                           65504.0, /* max FP16 */
                           65505.0,
                           131007.0,
                           131008.0, /* max AFP */
                           131009.0,
                           1.111E+30,
                           FLT_MAX,
                           INFINITY,
                           NAN,
                           SNANF };

static void convert_single_to_half(void)
{
    int i;

    printf("Converting single-precision to half-precision\n");

    for (i = 0; i < ARRAY_SIZE(single_numbers); ++i) {
        float input = single_numbers[i];

        feclearexcept(FE_ALL_EXCEPT);

        print_single_number(i, input);
#if defined(__arm__)
        uint32_t output;
        asm("vcvtb.f16.f32 %0, %1" : "=t" (output) : "x" (input));
#else
        uint16_t output;
        asm("fcvt %h0, %s1" : "=w" (output) : "x" (input));
#endif
        print_half_number(i, output);
    }
}

static void convert_single_to_double(void)
{
    int i;

    printf("Converting single-precision to double-precision\n");

    for (i = 0; i < ARRAY_SIZE(single_numbers); ++i) {
        float input = single_numbers[i];
        /* uint64_t output; */
        double output;

        feclearexcept(FE_ALL_EXCEPT);

        print_single_number(i, input);
#if defined(__arm__)
        asm("vcvt.f64.f32 %P0, %1" : "=w" (output) : "t" (input));
#else
        asm("fcvt %d0, %s1" : "=w" (output) : "x" (input));
#endif
        print_double_number(i, output);
    }
}

static void convert_single_to_integer(void)
{
    int i;

    printf("Converting single-precision to integer\n");

    for (i = 0; i < ARRAY_SIZE(single_numbers); ++i) {
        float input = single_numbers[i];
        int64_t output;

        feclearexcept(FE_ALL_EXCEPT);

        print_single_number(i, input);
#if defined(__arm__)
        /* asm("vcvt.s32.f32 %s0, %s1" : "=t" (output) : "t" (input)); */
        output = input;
#else
        asm("fcvtzs %0, %s1" : "=r" (output) : "w" (input));
#endif
        print_int64(i, output);
    }
}

/* This allows us to initialise some doubles as pure hex */
typedef union {
    double d;
    uint64_t h;
} test_doubles;

test_doubles double_numbers[] = {
    {SNAN},
    {-NAN},
    {-INFINITY},
    {-DBL_MAX},
    {.h = 0xffc00bffffffffff },
    {-FLT_MAX-1.0},
    {-FLT_MAX},
    {-1.111E+31},
    {-1.111E+30}, /* half prec */
    {-2.0}, {-1.0},
    {-DBL_MIN},
    {-FLT_MIN},
    {0.0},
    {FLT_MIN},
    {2.98023224e-08},
    {5.96046E-8}, /* min positive FP16 subnormal */
    {6.09756E-5}, /* max subnormal FP16 */
    {6.10352E-5}, /* min positive normal FP16 */
    {1.0},
    {1.0009765625}, /* smallest float after 1.0 FP16 */
    {DBL_MIN},
    {1.3789972848607228e-308},
    {1.4914738736681624e-308},
    {1.0}, {2.0},
    {M_E}, {M_PI},
    {65503.0},
    {65504.0}, /* max FP16 */
    {65505.0},
    {131007.0},
    {131008.0}, /* max AFP */
    {131009.0},
    { .h = 0x41dfffffffc00000 }, /* to int = 0x7fffffff */
    {FLT_MAX},
    {FLT_MAX + 1.0},
    { .h = 0x3ff40000000001 },
    {DBL_MAX},
    {INFINITY},
    {NAN},
    {.h = 0x7ff0000000000001}, /* SNAN */
    {SNAN},
};

typedef struct {
    void (*convert_double) (double input, int i);
    char *description;
} double_test_t;


static void convert_double_to_half(double input, int i)
{
    uint16_t output;
    /* as we don't have _Float16 support */
#if defined(__arm__)
    /* asm("vcvtb.f16.f64 %0, %P1" : "=t" (output) : "x" (input)); */
    output = input;
#else
    asm("fcvt %h0, %d1" : "=w" (output) : "x" (input));
#endif
    print_half_number(i, output);
}

static void convert_double_to_single(double input, int i)
{
    float output;
#if defined(__arm__)
    asm("vcvt.f32.f64 %0, %P1" : "=w" (output) : "x" (input));
#else
    asm("fcvt %s0, %d1" : "=w" (output) : "x" (input));
#endif
    print_single_number(i, output);
}

#if defined(__arm__)
#define conv_double_scaled32(scale) \
    asm("vcvt.s32.f32 %0, %1, #%2" : "=r" (output) : "t" (input), "i" (scale));
#else
#define conv_double_scaled32(scale) \
    asm("fcvtzs %w0, %s1, #%2" : "=r" (output) : "x" (input), "i" (scale));
#endif

static void convert_double_to_fixed32(double input, int i)
{
    int32_t output;

    conv_double_scaled32(1);
    print_int32(i, output);
    conv_double_scaled32(2);
    print_int32(i, output);
    conv_double_scaled32(4);
    print_int32(i, output);
    conv_double_scaled32(8);
    print_int32(i, output);
    conv_double_scaled32(16);
    print_int32(i, output);
    conv_double_scaled32(16);
    print_int32(i, output);
    conv_double_scaled32(32);
    print_int32(i, output);
}

#if defined(__arm__)
#define conv_double_scaled64(scale) \
    asm("vcvt.s64.f32 %0, %1, #%2" : "=r" (output) : "t" (input), "i" (scale));
#else
#define conv_double_scaled64(scale) \
    asm("fcvtzs %x0, %d1, #%2" : "=r" (output) : "x" (input), "i" (scale));
#endif

static void convert_double_to_fixed64(double input, int i)
{
    int64_t output;

    conv_double_scaled64(1);
    print_int64(i, output);
    conv_double_scaled64(2);
    print_int64(i, output);
    conv_double_scaled64(4);
    print_int64(i, output);
    conv_double_scaled64(8);
    print_int64(i, output);
    conv_double_scaled64(16);
    print_int64(i, output);
    conv_double_scaled64(32);
    print_int64(i, output);
}

static void convert_double_to_int32(double input, int i)
{
    int32_t output;
#if defined(__arm__)
    /* asm("vcvt.s32.f32 %s0, %s1" : "=t" (output) : "t" (input)); */
    output = input;
#else
    asm("fcvtzs %w0, %d1" : "=r" (output) : "w" (input));
#endif
    print_int32(i, output);
}

static void convert_double_to_int64(double input, int i)
{
    int64_t output;
#if defined(__arm__)
    /* asm("vcvt.s32.f32 %s0, %s1" : "=t" (output) : "t" (input)); */
    output = input;
#else
    asm("fcvtzs %x0, %d1" : "=r" (output) : "w" (input));
#endif
    print_int64(i, output);
}

double_test_t double_tests[] = {
    { convert_double_to_half, "double-precision to half-precision" },
    { convert_double_to_single, "double-precision to single-precision" },
    { convert_double_to_fixed32, "double-precision to fixed point 32 bit int" },
    { convert_double_to_fixed64, "double-precision to fixed point 64 bit int" },
    { convert_double_to_int32, "double-precision to 32 bit int" },
    { convert_double_to_int64, "double-precision to 64 bit int" },
};

static void convert_doubles(void)
{
    int i,j;

    for (i = 0; i < ARRAY_SIZE(double_tests); ++i) {
        double_test_t *test = &double_tests[i];
        printf("Converting %s\n", test->description);
        for (j = 0; j < ARRAY_SIZE(double_numbers); ++j) {
            double input = double_numbers[j].d;
            feclearexcept(FE_ALL_EXCEPT);
            print_double_number(j, input);
            test->convert_double(input, j);
        }
    }
}

/* no handy defines for these numbers */
uint16_t half_numbers[] = {
    0xffff, /* -NaN / AHP -Max */
    0xfcff, /* -NaN / AHP */
    0xfc01, /* -NaN / AHP */
    0xfc00, /* -Inf */
    0xfbff, /* -Max */
    0xfbfe,
    0xc000, /* -2 */
    0xbc00, /* -1 */
    0x8001, /* -MIN subnormal */
    0x8000, /* -0 */
    0x0000, /* +0 */
    0x0001, /* MIN subnormal */
    0x3c00, /* 1 */
    0x7bfe,
    0x7bff, /* Max */
    0x7c00, /* Inf */
    0x7c01, /* NaN / AHP */
    0x7cff, /* NaN / AHP */
    0x7fff, /* NaN / AHP +Max*/
};

typedef struct {
    void (*convert_half) (uint16_t input, int i);
    char *description;
} half_test_t;

static void convert_half_to_double(uint16_t input, int i)
{
    double output;
#if defined(__arm__)
    /* asm("vcvtb.f64.f16 %P0, %1" : "=w" (output) : "t" (input)); */
    output = input;
#else
    asm("fcvt %d0, %h1" : "=w" (output) : "x" (input));
#endif
    print_double_number(i, output);
}

static void convert_half_to_single(uint16_t input, int i)
{
    float output;
#if defined(__arm__)
    asm("vcvtb.f32.f16 %0, %1" : "=w" (output) : "x" ((uint32_t)input));
#else
    asm("fcvt %s0, %h1" : "=w" (output) : "x" (input));
#endif
    print_single_number(i, output);
}

#if defined(__arm__)
#define conv_half_scaled64(scale) \
    asm("vcvt.s64.f32 %0, %1, #%2" : "=r" (output) : "t" (input), "i" (scale));
#else
#define conv_half_scaled64(scale) \
    asm("fcvtzs %x0, %h1, #%2" : "=r" (output) : "w" (input), "i" (scale));
#endif

static void convert_half_to_fixed64(uint16_t input, int i)
{
    int64_t output;

    conv_half_scaled64(1);
    print_int64(i, output);
    conv_half_scaled64(2);
    print_int64(i, output);
    conv_half_scaled64(4);
    print_int64(i, output);
    conv_half_scaled64(8);
    print_int64(i, output);
    conv_half_scaled64(16);
    print_int64(i, output);
    conv_half_scaled64(32);
    print_int64(i, output);
}

#if defined(__arm__)
#define conv_half_scaled32(scale) \
    asm("vcvt.s32.f16 %0, %1, %2" : "=r" (output) : "t" (input), "i" (scale));
#else
#define conv_half_scaled32(scale) \
    asm("fcvtzs %w0, %s1, #%2" : "=r" (output) : "x" (input), "i" (scale));
#endif

static void convert_half_to_fixed32(uint16_t input, int i)
{
    int32_t output;
    conv_half_scaled32(1);
    print_int32(i, output);
    conv_half_scaled32(2);
    print_int32(i, output);
    conv_half_scaled32(4);
    print_int32(i, output);
    conv_half_scaled32(8);
    print_int32(i, output);
    conv_half_scaled32(16);
    print_int32(i, output);
    conv_half_scaled32(16);
    print_int32(i, output);
    conv_half_scaled32(32);
    print_int32(i, output);
}

static void convert_half_to_int64(uint16_t input, int i)
{
    int64_t output;
#if defined(__arm__)
    /* asm("vcvt.s32.f16 %0, %1" : "=t" (output) : "t" (input)); v8.2*/
    output = input;
#else
    asm("fcvtzs %x0, %h1" : "=r" (output) : "x" (input));
#endif
    print_int64(i, output);
}

static void convert_half_to_int32(uint16_t input, int i)
{
    int32_t output;
#if defined(__arm__)
    /* asm("vcvt.s32.f16 %0, %1" : "=t" (output) : "t" (input)); v8.2*/
    output = input;
#else
    asm("fcvtzs %w0, %h1" : "=r" (output) : "x" (input));
#endif
    print_int32(i, output);
}

static void convert_half_to_uint64(uint16_t input, int i)
{
    uint64_t output;
#if defined(__arm__)
    /* asm("vcvt.s32.f16 %0, %1" : "=t" (output) : "t" (input)); v8.2*/
    output = input;
#else
    asm("fcvtzu %x0, %h1" : "=r" (output) : "x" (input));
#endif
    print_int64(i, output);
}

static void convert_half_to_uint32(uint16_t input, int i)
{
    uint32_t output;
#if defined(__arm__)
    /* asm("vcvt.s32.f16 %0, %1" : "=t" (output) : "t" (input)); v8.2*/
    output = input;
#else
    asm("fcvtzu %w0, %h1" : "=r" (output) : "x" (input));
#endif
    print_int32(i, output);
}

static void convert_half_to_uint16(uint16_t input, int i)
{
    uint16_t output;
#if defined(__arm__)
    /* asm("vcvt.s32.f16 %0, %1" : "=t" (output) : "t" (input)); v8.2*/
    output = input;
#else
    asm("fcvtzu %h0, %h1" : "=w" (output) : "x" (input));
#endif
    print_half_number(i, output);
}

half_test_t half_tests[] = {
    {convert_half_to_double, "half-precision to double-precision"},
    {convert_half_to_single, "half-precision to single-precision"},
    {convert_half_to_fixed64, "half-precision to fixed point 64 bit int"},
    {convert_half_to_fixed32, "half-precision to fixed point 32 bit int"},
    {convert_half_to_int64, "half-precision to 64 bit int"},
    {convert_half_to_int32, "half-precision to 32 bit int"},
    {convert_half_to_uint64, "half-precision to 64 bit uint"},
    {convert_half_to_uint32, "half-precision to 32 bit uint"},
    {convert_half_to_uint16, "half-precision to 16 bit uint"},
};

static void convert_halves(void)
{
    int i,j;

    for (i = 0; i < ARRAY_SIZE(half_tests); ++i) {
        half_test_t *test = &half_tests[i];
        printf("Converting %s\n", test->description);
        for (j = 0; j < ARRAY_SIZE(half_numbers); ++j) {
            uint16_t input = half_numbers[j];
            feclearexcept(FE_ALL_EXCEPT);
            print_half_number(j, input);
            test->convert_half(input, j);
        }
    }
}

typedef struct {
    int flag;
    char *desc;
} float_mapping;

float_mapping round_flags[] = {
    { FE_TONEAREST, "to nearest" },
    { FE_UPWARD, "upwards" },
    { FE_DOWNWARD, "downwards" },
    { FE_TOWARDZERO, "to zero" }
};


void run_tests(void) {
    int i;

    for (i = 0; i < ARRAY_SIZE(round_flags); ++i) {
        fesetround(round_flags[i].flag);
        printf("### Rounding %s\n", round_flags[i].desc);
        convert_single_to_half();
        convert_single_to_double();
        convert_doubles();
        convert_halves();
    }

    /* convert to integer */
    convert_single_to_integer();
}

int main(int argc, char *argv[argc])
{

    printf("#### Enabling IEEE Half Precision\n");

    run_tests();

    /* And now with ARM alternative FP16 */
#if defined(__arm__)
    /* See glibc sysdeps/arm/fpu_control.h */
    asm("mrc p10, 7, r1, cr1, cr0, 0\n\t"
        "orr r1, r1, %[flags]\n\t"
        "mcr p10, 7, r1, cr1, cr0, 0\n\t"
        : /* no output */ : [flags] "n" (1 << 26) : "r1" );
#else
    asm("mrs x1, fpcr\n\t"
        "orr x1, x1, %[flags]\n\t"
        "msr fpcr, x1\n\t"
        : /* no output */ : [flags] "n" (1 << 26) : "x1" );
#endif

    printf("#### Enabling ARM Alternative Half Precision\n");

    run_tests();

    return 0;
}
