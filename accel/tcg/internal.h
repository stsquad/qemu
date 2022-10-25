/*
 * Internal execution defines for qemu
 *
 *  Copyright (c) 2003 Fabrice Bellard
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 */

#ifndef ACCEL_TCG_INTERNAL_H
#define ACCEL_TCG_INTERNAL_H

#include "exec/exec-all.h"

/*
 * Access to the various translations structures need to be serialised
 * via locks for consistency.  In user-mode emulation access to the
 * memory related structures are protected with mmap_lock.
 * In !user-mode we use per-page locks.
 */
#ifdef CONFIG_SOFTMMU
#define assert_memory_lock()
#else
#define assert_memory_lock() tcg_debug_assert(have_mmap_lock())
#endif

#if defined(CONFIG_SOFTMMU) && defined(CONFIG_DEBUG_TCG)
void assert_no_pages_locked(void);
#else
static inline void assert_no_pages_locked(void) { }
#endif

#ifdef CONFIG_USER_ONLY
static inline void page_table_config_init(void) { }
#else
void page_table_config_init(void);
#endif

#ifdef CONFIG_SOFTMMU
struct page_collection;
void tb_invalidate_phys_page_fast(struct page_collection *pages,
                                  tb_page_addr_t start, int len,
                                  uintptr_t retaddr);
struct page_collection *page_collection_lock(tb_page_addr_t start,
                                             tb_page_addr_t end);
void page_collection_unlock(struct page_collection *set);
#endif /* CONFIG_SOFTMMU */

TranslationBlock *tb_gen_code(CPUState *cpu, target_ulong pc,
                              target_ulong cs_base, uint32_t flags,
                              int cflags);
G_NORETURN void cpu_io_recompile(CPUState *cpu, uintptr_t retaddr);
void page_init(void);
void tb_htable_init(void);
void tb_reset_jump(TranslationBlock *tb, int n);
TranslationBlock *tb_link_page(TranslationBlock *tb, tb_page_addr_t phys_pc,
                               tb_page_addr_t phys_page2);
bool tb_invalidate_phys_page_unwind(tb_page_addr_t addr, uintptr_t pc);
int cpu_restore_state_from_tb(CPUState *cpu, TranslationBlock *tb,
                              uintptr_t searched_pc, bool reset_icount);

/* Return the current PC from CPU, which may be cached in TB. */
static inline target_ulong log_pc(CPUState *cpu, const TranslationBlock *tb)
{
#if TARGET_TB_PCREL
    return cpu->cc->get_pc(cpu);
#else
    return tb_pc(tb);
#endif
}

#endif /* ACCEL_TCG_INTERNAL_H */
