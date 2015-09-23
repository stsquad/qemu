#if TARGET_LONG_BITS == 32
#define TYPE i32
#else
#define TYPE i64
#endif

DEF_HELPER_3(ldlink_i8, i32, env, TYPE, i32)
DEF_HELPER_3(ldlink_i16_be, i32, env, TYPE, i32)
DEF_HELPER_3(ldlink_i32_be, i32, env, TYPE, i32)
DEF_HELPER_3(ldlink_i64_be, i64, env, TYPE, i32)
DEF_HELPER_3(ldlink_i16_le, i32, env, TYPE, i32)
DEF_HELPER_3(ldlink_i32_le, i32, env, TYPE, i32)
DEF_HELPER_3(ldlink_i64_le, i64, env, TYPE, i32)

DEF_HELPER_4(stcond_i8, TYPE, env, TYPE, i32, i32)
DEF_HELPER_4(stcond_i16_be, TYPE, env, TYPE, i32, i32)
DEF_HELPER_4(stcond_i32_be, TYPE, env, TYPE, i32, i32)
DEF_HELPER_4(stcond_i64_be, TYPE, env, TYPE, i64, i32)
DEF_HELPER_4(stcond_i16_le, TYPE, env, TYPE, i32, i32)
DEF_HELPER_4(stcond_i32_le, TYPE, env, TYPE, i32, i32)
DEF_HELPER_4(stcond_i64_le, TYPE, env, TYPE, i64, i32)

/* Aligned versions */
DEF_HELPER_3(ldlink_i16_bea, i32, env, TYPE, i32)
DEF_HELPER_3(ldlink_i32_bea, i32, env, TYPE, i32)
DEF_HELPER_3(ldlink_i64_bea, i64, env, TYPE, i32)
DEF_HELPER_3(ldlink_i16_lea, i32, env, TYPE, i32)
DEF_HELPER_3(ldlink_i32_lea, i32, env, TYPE, i32)
DEF_HELPER_3(ldlink_i64_lea, i64, env, TYPE, i32)

DEF_HELPER_4(stcond_i16_bea, TYPE, env, TYPE, i32, i32)
DEF_HELPER_4(stcond_i32_bea, TYPE, env, TYPE, i32, i32)
DEF_HELPER_4(stcond_i64_bea, TYPE, env, TYPE, i64, i32)
DEF_HELPER_4(stcond_i16_lea, TYPE, env, TYPE, i32, i32)
DEF_HELPER_4(stcond_i32_lea, TYPE, env, TYPE, i32, i32)
DEF_HELPER_4(stcond_i64_lea, TYPE, env, TYPE, i64, i32)

/* Convenient aliases */
#ifdef TARGET_WORDS_BIGENDIAN
#define gen_helper_stcond_i16 gen_helper_stcond_i16_be
#define gen_helper_stcond_i32 gen_helper_stcond_i32_be
#define gen_helper_stcond_i64 gen_helper_stcond_i64_be
#define gen_helper_ldlink_i16 gen_helper_ldlink_i16_be
#define gen_helper_ldlink_i32 gen_helper_ldlink_i32_be
#define gen_helper_ldlink_i64 gen_helper_ldlink_i64_be
#define gen_helper_stcond_i16a gen_helper_stcond_i16_bea
#define gen_helper_stcond_i32a gen_helper_stcond_i32_bea
#define gen_helper_stcond_i64a gen_helper_stcond_i64_bea
#define gen_helper_ldlink_i16a gen_helper_ldlink_i16_bea
#define gen_helper_ldlink_i32a gen_helper_ldlink_i32_bea
#define gen_helper_ldlink_i64a gen_helper_ldlink_i64_bea
#else
#define gen_helper_stcond_i16 gen_helper_stcond_i16_le
#define gen_helper_stcond_i32 gen_helper_stcond_i32_le
#define gen_helper_stcond_i64 gen_helper_stcond_i64_le
#define gen_helper_ldlink_i16 gen_helper_ldlink_i16_le
#define gen_helper_ldlink_i32 gen_helper_ldlink_i32_le
#define gen_helper_ldlink_i64 gen_helper_ldlink_i64_le
#define gen_helper_stcond_i16a gen_helper_stcond_i16_lea
#define gen_helper_stcond_i32a gen_helper_stcond_i32_lea
#define gen_helper_stcond_i64a gen_helper_stcond_i64_lea
#define gen_helper_ldlink_i16a gen_helper_ldlink_i16_lea
#define gen_helper_ldlink_i32a gen_helper_ldlink_i32_lea
#define gen_helper_ldlink_i64a gen_helper_ldlink_i64_lea
#endif

#undef TYPE
