/*
 * To be included directly from the target's helper.c
 */
#include "qemu/aie.h"

#ifdef CONFIG_USER_ONLY
static inline
hwaddr h_get_ld_phys(CPUArchState *env, target_ulong vaddr, uintptr_t retaddr)
{
    return vaddr;
}

static inline
hwaddr h_get_st_phys(CPUArchState *env, target_ulong vaddr, uintptr_t retaddr)
{
    return vaddr;
}
#else
static inline
hwaddr h_get_ld_phys(CPUArchState *env, target_ulong vaddr, uintptr_t retaddr)
{
    return helper_ret_get_ld_phys(env, vaddr, cpu_mmu_index(env), retaddr);
}

static inline
hwaddr h_get_st_phys(CPUArchState *env, target_ulong vaddr, uintptr_t retaddr)
{
    return helper_ret_get_st_phys(env, vaddr, cpu_mmu_index(env), retaddr);
}
#endif /* CONFIG_USER_ONLY */

static inline void h_aie_lock(CPUArchState *env, hwaddr paddr)
{
    AIEEntry *entry = aie_entry_get_lock(paddr);

    env->aie_entry = entry;
    env->aie_locked = true;
}

static inline void h_aie_unlock(CPUArchState *env)
{
    assert(env->aie_entry && env->aie_locked);
    qemu_spin_unlock(&env->aie_entry->lock);
    env->aie_locked = false;
}

static inline void h_aie_unlock__done(CPUArchState *env)
{
    h_aie_unlock(env);
    env->aie_entry = NULL;
}

static inline
void aie_ld_lock_ret(CPUArchState *env, target_ulong vaddr, uintptr_t retaddr)
{
    hwaddr paddr;

    assert(!env->aie_locked);
    paddr = h_get_ld_phys(env, vaddr, retaddr);
    h_aie_lock(env, paddr);
}

void HELPER(aie_ld_lock)(CPUArchState *env, target_ulong vaddr)
{
    aie_ld_lock_ret(env, vaddr, GETRA());
}

static inline
void aie_st_lock_ret(CPUArchState *env, target_ulong vaddr, uintptr_t retaddr)
{
    hwaddr paddr;

    assert(!env->aie_locked);
    paddr = h_get_st_phys(env, vaddr, retaddr);
    h_aie_lock(env, paddr);
}

void HELPER(aie_unlock__done)(CPUArchState *env)
{
    h_aie_unlock__done(env);
}

void HELPER(aie_ld_pre)(CPUArchState *env, target_ulong vaddr)
{
    assert(env->aie_lock_enabled);
    if (env->aie_locked) {
        return;
    }
    aie_ld_lock_ret(env, vaddr, GETRA());
}

void HELPER(aie_st_pre)(CPUArchState *env, target_ulong vaddr)
{
    if (unlikely(env->aie_lock_enabled)) {
        if (env->aie_locked) {
            return;
        }
        aie_st_lock_ret(env, vaddr, GETRA());
    } else {
        hwaddr paddr = h_get_st_phys(env, vaddr, GETRA());

        if (unlikely(aie_entry_exists(paddr))) {
            h_aie_lock(env, paddr);
        }
    }
}

void HELPER(aie_st_post)(CPUArchState *env, target_ulong vaddr)
{
    if (unlikely(!env->aie_lock_enabled && env->aie_locked)) {
        h_aie_unlock__done(env);
    }
}
