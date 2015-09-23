#ifndef HELPER_LLSC_HEAD_H
#define HELPER_LLSC_HEAD_H 1

uint32_t HELPER(ldlink_i8)(CPUArchState *env, target_ulong addr,
                           uint32_t index);
uint32_t HELPER(ldlink_i16_be)(CPUArchState *env, target_ulong addr,
                               uint32_t index);
uint32_t HELPER(ldlink_i32_be)(CPUArchState *env, target_ulong addr,
                               uint32_t index);
uint64_t HELPER(ldlink_i64_be)(CPUArchState *env, target_ulong addr,
                               uint32_t index);
uint32_t HELPER(ldlink_i16_le)(CPUArchState *env, target_ulong addr,
                               uint32_t index);
uint32_t HELPER(ldlink_i32_le)(CPUArchState *env, target_ulong addr,
                               uint32_t index);
uint64_t HELPER(ldlink_i64_le)(CPUArchState *env, target_ulong addr,
                               uint32_t index);

target_ulong HELPER(stcond_i8)(CPUArchState *env, target_ulong addr,
                               uint32_t val, uint32_t index);
target_ulong HELPER(stcond_i16_be)(CPUArchState *env, target_ulong addr,
                                   uint32_t val, uint32_t index);
target_ulong HELPER(stcond_i32_be)(CPUArchState *env, target_ulong addr,
                                   uint32_t val, uint32_t index);
target_ulong HELPER(stcond_i64_be)(CPUArchState *env, target_ulong addr,
                                   uint64_t val, uint32_t index);
target_ulong HELPER(stcond_i16_le)(CPUArchState *env, target_ulong addr,
                                   uint32_t val, uint32_t index);
target_ulong HELPER(stcond_i32_le)(CPUArchState *env, target_ulong addr,
                                   uint32_t val, uint32_t index);
target_ulong HELPER(stcond_i64_le)(CPUArchState *env, target_ulong addr,
                                   uint64_t val, uint32_t index);

/* Aligned versions */
uint32_t HELPER(ldlink_i16_bea)(CPUArchState *env, target_ulong addr,
                                uint32_t index);
uint32_t HELPER(ldlink_i32_bea)(CPUArchState *env, target_ulong addr,
                                uint32_t index);
uint64_t HELPER(ldlink_i64_bea)(CPUArchState *env, target_ulong addr,
                                uint32_t index);
uint32_t HELPER(ldlink_i16_lea)(CPUArchState *env, target_ulong addr,
                                uint32_t index);
uint32_t HELPER(ldlink_i32_lea)(CPUArchState *env, target_ulong addr,
                                uint32_t index);
uint64_t HELPER(ldlink_i64_lea)(CPUArchState *env, target_ulong addr,
                                uint32_t index);

target_ulong HELPER(stcond_i16_bea)(CPUArchState *env, target_ulong addr,
                                    uint32_t val, uint32_t index);
target_ulong HELPER(stcond_i32_bea)(CPUArchState *env, target_ulong addr,
                                    uint32_t val, uint32_t index);
target_ulong HELPER(stcond_i64_bea)(CPUArchState *env, target_ulong addr,
                                    uint64_t val, uint32_t index);
target_ulong HELPER(stcond_i16_lea)(CPUArchState *env, target_ulong addr,
                                    uint32_t val, uint32_t index);
target_ulong HELPER(stcond_i32_lea)(CPUArchState *env, target_ulong addr,
                                    uint32_t val, uint32_t index);
target_ulong HELPER(stcond_i64_lea)(CPUArchState *env, target_ulong addr,
                                    uint64_t val, uint32_t index);

#endif
