/*
 * TCG Common Helpers
 *
 * Copyright (c) 2014
 * Written by Alex Bennée <alex.bennee@linaro.org>
 *
 * This code is licensed under the GNU GPL v2.
 *
 * WARNING: intended to be included in $ARCH/helper.h
 */

DEF_HELPER_2(dump_u32, i32, i32, ptr)
DEF_HELPER_2(dump_u64, i64, i64, ptr)


/*
 * Formatting macros
 *
 * To be useful we would like to format a string with a message to be
 * associated with this dump. As these strings have to live the
 * lifetime of the generated code we are essentially leaking memory so
 * the more times a given dump call is generated the more memory is
 * consumed (not so for each time the TCG code itself is called)
 */

/* This limits the number of malloc'ed strings *per call site* */
#define TCG_DEBUG_DUMP_MAX_STRINGS 10

/* String macro magic */
#define STRINGIFY_DETAIL(x) #x
#define STRINGIFY(x) STRINGIFY_DETAIL(x)

#define tcg_debug_dump_i32(tcg32, fmt, ...)                               \
do {                                                                      \
    static int tcg_debug_alloc_strings = 0;                               \
    gchar *debug_string = (gchar *) __FILE__ ":" STRINGIFY(__LINE__) ":"; \
    TCGv_ptr tcg_string;                                                  \
    if (tcg_debug_alloc_strings++ < TCG_DEBUG_DUMP_MAX_STRINGS) {         \
        debug_string = g_strdup_printf(fmt ":", ## __VA_ARGS__);          \
    }                                                                     \
    tcg_string = tcg_const_ptr(debug_string);                             \
    gen_helper_dump_u32(tcg32, tcg32, tcg_string);                        \
    tcg_temp_free_ptr(tcg_string);                                        \
} while (0);

#define tcg_debug_dump_i64(tcg64, fmt, ...)                               \
do {                                                                      \
    static int tcg_debug_alloc_strings = 0;                               \
    gchar *debug_string = (gchar *) __FILE__ ":" STRINGIFY(__LINE__) ":"; \
    TCGv_ptr tcg_string;                                                  \
    if (tcg_debug_alloc_strings++ < TCG_DEBUG_DUMP_MAX_STRINGS) {         \
        debug_string = g_strdup_printf(fmt ":", ## __VA_ARGS__);          \
    }                                                                     \
    tcg_string = tcg_const_ptr(debug_string);                             \
    gen_helper_dump_u64(tcg64, tcg64, tcg_string);                        \
    tcg_temp_free_ptr(tcg_string);                                        \
} while (0);
