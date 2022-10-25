/*
 * Translation Block Maintaince
 *
 *  Copyright (c) 2003 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "qemu/interval-tree.h"
#include "exec/cputlb.h"
#include "exec/log.h"
#include "exec/exec-all.h"
#include "exec/translate-all.h"
#include "sysemu/tcg.h"
#include "tcg/tcg.h"
#include "tb-hash.h"
#include "tb-context.h"
#include "internal.h"


static bool tb_cmp(const void *ap, const void *bp)
{
    const TranslationBlock *a = ap;
    const TranslationBlock *b = bp;

    return ((TARGET_TB_PCREL || tb_pc(a) == tb_pc(b)) &&
            a->cs_base == b->cs_base &&
            a->flags == b->flags &&
            (tb_cflags(a) & ~CF_INVALID) == (tb_cflags(b) & ~CF_INVALID) &&
            a->trace_vcpu_dstate == b->trace_vcpu_dstate &&
            tb_page_addr0(a) == tb_page_addr0(b) &&
            tb_page_addr1(a) == tb_page_addr1(b));
}

void tb_htable_init(void)
{
    unsigned int mode = QHT_MODE_AUTO_RESIZE;

    qht_init(&tb_ctx.htable, tb_cmp, CODE_GEN_HTABLE_SIZE, mode);
}

#ifdef CONFIG_USER_ONLY
/*
 * For user-only, since we are protecting all of memory with a single lock,
 * and because the two pages of a TranslationBlock are always contiguous,
 * use a single data structure to record all TranslationBlocks.
 */
static IntervalTreeRoot tb_root;

static void page_flush_tb(void)
{
    assert_memory_lock();
    memset(&tb_root, 0, sizeof(tb_root));
}

/* Call with mmap_lock held. */
static void tb_page_add(TranslationBlock *tb, PageDesc *p1, PageDesc *p2)
{
    target_ulong addr;
    int flags;

    assert_memory_lock();
    tb->itree.last = tb->itree.start + tb->size - 1;

    /* translator_loop() must have made all TB pages non-writable */
    addr = tb_page_addr0(tb);
    flags = page_get_flags(addr);
    assert(!(flags & PAGE_WRITE));

    addr = tb_page_addr1(tb);
    if (addr != -1) {
        flags = page_get_flags(addr);
        assert(!(flags & PAGE_WRITE));
    }

    interval_tree_insert(&tb->itree, &tb_root);
}

/* Call with mmap_lock held. */
static void tb_page_remove(TranslationBlock *tb)
{
    assert_memory_lock();
    interval_tree_remove(&tb->itree, &tb_root);
}

/* TODO: For now, still shared with translate-all.c for system mode. */
#define PAGE_FOR_EACH_TB(start, end, pagedesc, T, N)    \
    for (T = foreach_tb_first(start, end),              \
         N = foreach_tb_next(T, start, end);            \
         T != NULL;                                     \
         T = N, N = foreach_tb_next(N, start, end))

typedef TranslationBlock *PageForEachNext;

static PageForEachNext foreach_tb_first(tb_page_addr_t start,
                                        tb_page_addr_t end)
{
    IntervalTreeNode *n = interval_tree_iter_first(&tb_root, start, end - 1);
    return n ? container_of(n, TranslationBlock, itree) : NULL;
}

static PageForEachNext foreach_tb_next(PageForEachNext tb,
                                       tb_page_addr_t start,
                                       tb_page_addr_t end)
{
    IntervalTreeNode *n;

    if (tb) {
        n = interval_tree_iter_next(&tb->itree, start, end - 1);
        if (n) {
            return container_of(n, TranslationBlock, itree);
        }
    }
    return NULL;
}

#else

/* Set to NULL all the 'first_tb' fields in all PageDescs. */
static void page_flush_tb_1(int level, void **lp)
{
    int i;

    if (*lp == NULL) {
        return;
    }
    if (level == 0) {
        PageDesc *pd = *lp;

        for (i = 0; i < V_L2_SIZE; ++i) {
            page_lock(&pd[i]);
            pd[i].first_tb = (uintptr_t)NULL;
            page_unlock(&pd[i]);
        }
    } else {
        void **pp = *lp;

        for (i = 0; i < V_L2_SIZE; ++i) {
            page_flush_tb_1(level - 1, pp + i);
        }
    }
}

static void page_flush_tb(void)
{
    int i, l1_sz = v_l1_size;

    for (i = 0; i < l1_sz; i++) {
        page_flush_tb_1(v_l2_levels, l1_map + i);
    }
}

/*
 * Add the tb in the target page and protect it if necessary.
 * Called with @p->lock held.
 */
static void tb_page_add(TranslationBlock *tb, PageDesc *p1, PageDesc *p2)
{
    /*
     * If some code is already present, then the pages are already
     * protected. So we handle the case where only the first TB is
     * allocated in a physical page.
     */
    assert_page_locked(p1);
    if (p1->first_tb) {
        tb->page_next[0] = p1->first_tb;
    } else {
        tlb_protect_code(tb->page_addr[0] & TARGET_PAGE_MASK);
        tb->page_next[0] = 0;
    }
    p1->first_tb = (uintptr_t)tb | 0;

    if (unlikely(p2)) {
        assert_page_locked(p2);
        if (p2->first_tb) {
            tb->page_next[1] = p2->first_tb;
        } else {
            tlb_protect_code(tb->page_addr[1] & TARGET_PAGE_MASK);
            tb->page_next[1] = 0;
        }
        p2->first_tb = (uintptr_t)tb | 1;
    }
}

static void tb_page_remove1(TranslationBlock *tb, PageDesc *pd)
{
    TranslationBlock *i;
    PageForEachNext n;
    uintptr_t *pprev;

    assert_page_locked(pd);
    pprev = &pd->first_tb;
    PAGE_FOR_EACH_TB(unused, unused, pd, i, n) {
        if (i == tb) {
            *pprev = i->page_next[n];
            return;
        }
        pprev = &i->page_next[n];
    }
    g_assert_not_reached();
}

static void tb_page_remove(TranslationBlock *tb)
{
    PageDesc *pd;

    pd = page_find(tb->page_addr[0] >> TARGET_PAGE_BITS);
    tb_page_remove1(tb, pd);
    if (unlikely(tb->page_addr[1] != -1)) {
        pd = page_find(tb->page_addr[1] >> TARGET_PAGE_BITS);
        tb_page_remove1(tb, pd);
    }
}

#endif

/* flush all the translation blocks */
static void do_tb_flush(CPUState *cpu, run_on_cpu_data tb_flush_count)
{
    bool did_flush = false;

    mmap_lock();
    /* If it is already been done on request of another CPU, just retry. */
    if (tb_ctx.tb_flush_count != tb_flush_count.host_int) {
        goto done;
    }
    did_flush = true;

    CPU_FOREACH(cpu) {
        tcg_flush_jmp_cache(cpu);
    }

    qht_reset_size(&tb_ctx.htable, CODE_GEN_HTABLE_SIZE);
    page_flush_tb();

    tcg_region_reset_all();
    /* XXX: flush processor icache at this point if cache flush is expensive */
    qatomic_mb_set(&tb_ctx.tb_flush_count, tb_ctx.tb_flush_count + 1);

done:
    mmap_unlock();
    if (did_flush) {
        qemu_plugin_flush_cb();
    }
}

void tb_flush(CPUState *cpu)
{
    if (tcg_enabled()) {
        unsigned tb_flush_count = qatomic_mb_read(&tb_ctx.tb_flush_count);

        if (cpu_in_exclusive_context(cpu)) {
            do_tb_flush(cpu, RUN_ON_CPU_HOST_INT(tb_flush_count));
        } else {
            async_safe_run_on_cpu(cpu, do_tb_flush,
                                  RUN_ON_CPU_HOST_INT(tb_flush_count));
        }
    }
}

/* remove @orig from its @n_orig-th jump list */
static inline void tb_remove_from_jmp_list(TranslationBlock *orig, int n_orig)
{
    uintptr_t ptr, ptr_locked;
    TranslationBlock *dest;
    TranslationBlock *tb;
    uintptr_t *pprev;
    int n;

    /* mark the LSB of jmp_dest[] so that no further jumps can be inserted */
    ptr = qatomic_or_fetch(&orig->jmp_dest[n_orig], 1);
    dest = (TranslationBlock *)(ptr & ~1);
    if (dest == NULL) {
        return;
    }

    qemu_spin_lock(&dest->jmp_lock);
    /*
     * While acquiring the lock, the jump might have been removed if the
     * destination TB was invalidated; check again.
     */
    ptr_locked = qatomic_read(&orig->jmp_dest[n_orig]);
    if (ptr_locked != ptr) {
        qemu_spin_unlock(&dest->jmp_lock);
        /*
         * The only possibility is that the jump was unlinked via
         * tb_jump_unlink(dest). Seeing here another destination would be a bug,
         * because we set the LSB above.
         */
        g_assert(ptr_locked == 1 && dest->cflags & CF_INVALID);
        return;
    }
    /*
     * We first acquired the lock, and since the destination pointer matches,
     * we know for sure that @orig is in the jmp list.
     */
    pprev = &dest->jmp_list_head;
    TB_FOR_EACH_JMP(dest, tb, n) {
        if (tb == orig && n == n_orig) {
            *pprev = tb->jmp_list_next[n];
            /* no need to set orig->jmp_dest[n]; setting the LSB was enough */
            qemu_spin_unlock(&dest->jmp_lock);
            return;
        }
        pprev = &tb->jmp_list_next[n];
    }
    g_assert_not_reached();
}

/*
 * Reset the jump entry 'n' of a TB so that it is not chained to another TB.
 */
void tb_reset_jump(TranslationBlock *tb, int n)
{
    uintptr_t addr = (uintptr_t)(tb->tc.ptr + tb->jmp_reset_offset[n]);
    tb_set_jmp_target(tb, n, addr);
}

/* remove any jumps to the TB */
static inline void tb_jmp_unlink(TranslationBlock *dest)
{
    TranslationBlock *tb;
    int n;

    qemu_spin_lock(&dest->jmp_lock);

    TB_FOR_EACH_JMP(dest, tb, n) {
        tb_reset_jump(tb, n);
        qatomic_and(&tb->jmp_dest[n], (uintptr_t)NULL | 1);
        /* No need to clear the list entry; setting the dest ptr is enough */
    }
    dest->jmp_list_head = (uintptr_t)NULL;

    qemu_spin_unlock(&dest->jmp_lock);
}

static void tb_jmp_cache_inval_tb(TranslationBlock *tb)
{
    CPUState *cpu;

    if (TARGET_TB_PCREL) {
        /* A TB may be at any virtual address */
        CPU_FOREACH(cpu) {
            tcg_flush_jmp_cache(cpu);
        }
    } else {
        uint32_t h = tb_jmp_cache_hash_func(tb_pc(tb));

        CPU_FOREACH(cpu) {
            CPUJumpCache *jc = cpu->tb_jmp_cache;

            if (qatomic_read(&jc->array[h].tb) == tb) {
                qatomic_set(&jc->array[h].tb, NULL);
            }
        }
    }
}

/*
 * In user-mode, call with mmap_lock held.
 * In !user-mode, if @rm_from_page_list is set, call with the TB's pages'
 * locks held.
 */
static void do_tb_phys_invalidate(TranslationBlock *tb, bool rm_from_page_list)
{
    uint32_t h;
    tb_page_addr_t phys_pc;
    uint32_t orig_cflags = tb_cflags(tb);

    assert_memory_lock();

    /* make sure no further incoming jumps will be chained to this TB */
    qemu_spin_lock(&tb->jmp_lock);
    qatomic_set(&tb->cflags, tb->cflags | CF_INVALID);
    qemu_spin_unlock(&tb->jmp_lock);

    /* remove the TB from the hash list */
    phys_pc = tb_page_addr0(tb);
    h = tb_hash_func(phys_pc, (TARGET_TB_PCREL ? 0 : tb_pc(tb)),
                     tb->flags, orig_cflags, tb->trace_vcpu_dstate);
    if (!qht_remove(&tb_ctx.htable, tb, h)) {
        return;
    }

    /* remove the TB from the page list */
    if (rm_from_page_list) {
        tb_page_remove(tb);
    }

    /* remove the TB from the hash list */
    tb_jmp_cache_inval_tb(tb);

    /* suppress this TB from the two jump lists */
    tb_remove_from_jmp_list(tb, 0);
    tb_remove_from_jmp_list(tb, 1);

    /* suppress any remaining jumps to this TB */
    tb_jmp_unlink(tb);

    qatomic_set(&tb_ctx.tb_phys_invalidate_count,
                tb_ctx.tb_phys_invalidate_count + 1);
}

static void tb_phys_invalidate__locked(TranslationBlock *tb)
{
    qemu_thread_jit_write();
    do_tb_phys_invalidate(tb, true);
    qemu_thread_jit_execute();
}

static void page_lock_pair(PageDesc **ret_p1, tb_page_addr_t phys1,
                           PageDesc **ret_p2, tb_page_addr_t phys2, bool alloc)
{
    PageDesc *p1, *p2;
    tb_page_addr_t page1;
    tb_page_addr_t page2;

    assert_memory_lock();
    g_assert(phys1 != -1);

    page1 = phys1 >> TARGET_PAGE_BITS;
    page2 = phys2 >> TARGET_PAGE_BITS;

    p1 = page_find_alloc(page1, alloc);
    if (ret_p1) {
        *ret_p1 = p1;
    }
    if (likely(phys2 == -1)) {
        page_lock(p1);
        return;
    } else if (page1 == page2) {
        page_lock(p1);
        if (ret_p2) {
            *ret_p2 = p1;
        }
        return;
    }
    p2 = page_find_alloc(page2, alloc);
    if (ret_p2) {
        *ret_p2 = p2;
    }
    if (page1 < page2) {
        page_lock(p1);
        page_lock(p2);
    } else {
        page_lock(p2);
        page_lock(p1);
    }
}

#ifdef CONFIG_USER_ONLY
static inline void page_lock_tb(const TranslationBlock *tb) { }
static inline void page_unlock_tb(const TranslationBlock *tb) { }
#else
/* lock the page(s) of a TB in the correct acquisition order */
static void page_lock_tb(const TranslationBlock *tb)
{
    page_lock_pair(NULL, tb_page_addr0(tb), NULL, tb_page_addr1(tb), false);
}

static void page_unlock_tb(const TranslationBlock *tb)
{
    PageDesc *p1 = page_find(tb_page_addr0(tb) >> TARGET_PAGE_BITS);

    page_unlock(p1);
    if (unlikely(tb_page_addr1(tb) != -1)) {
        PageDesc *p2 = page_find(tb_page_addr1(tb) >> TARGET_PAGE_BITS);

        if (p2 != p1) {
            page_unlock(p2);
        }
    }
}
#endif

/*
 * Invalidate one TB.
 * Called with mmap_lock held in user-mode.
 */
void tb_phys_invalidate(TranslationBlock *tb, tb_page_addr_t page_addr)
{
    if (page_addr == -1 && tb_page_addr0(tb) != -1) {
        page_lock_tb(tb);
        do_tb_phys_invalidate(tb, true);
        page_unlock_tb(tb);
    } else {
        do_tb_phys_invalidate(tb, false);
    }
}

/*
 * Add a new TB and link it to the physical page tables. phys_page2 is
 * (-1) to indicate that only one page contains the TB.
 *
 * Called with mmap_lock held for user-mode emulation.
 *
 * Returns a pointer @tb, or a pointer to an existing TB that matches @tb.
 * Note that in !user-mode, another thread might have already added a TB
 * for the same block of guest code that @tb corresponds to. In that case,
 * the caller should discard the original @tb, and use instead the returned TB.
 */
TranslationBlock *tb_link_page(TranslationBlock *tb, tb_page_addr_t phys_pc,
                               tb_page_addr_t phys_page2)
{
    PageDesc *p;
    PageDesc *p2 = NULL;
    void *existing_tb = NULL;
    uint32_t h;

    assert_memory_lock();
    tcg_debug_assert(!(tb->cflags & CF_INVALID));

    /*
     * Add the TB to the page list, acquiring first the pages's locks.
     * We keep the locks held until after inserting the TB in the hash table,
     * so that if the insertion fails we know for sure that the TBs are still
     * in the page descriptors.
     * Note that inserting into the hash table first isn't an option, since
     * we can only insert TBs that are fully initialized.
     */
    page_lock_pair(&p, phys_pc, &p2, phys_page2, true);
    tb_page_add(tb, p, p2);

    /* add in the hash table */
    h = tb_hash_func(phys_pc, (TARGET_TB_PCREL ? 0 : tb_pc(tb)),
                     tb->flags, tb->cflags, tb->trace_vcpu_dstate);
    qht_insert(&tb_ctx.htable, tb, h, &existing_tb);

    /* remove TB from the page(s) if we couldn't insert it */
    if (unlikely(existing_tb)) {
        tb_page_remove(tb);
        tb = existing_tb;
    }

    if (p2 && p2 != p) {
        page_unlock(p2);
    }
    page_unlock(p);
    return tb;
}

#ifdef CONFIG_USER_ONLY
/*
 * Invalidate all TBs which intersect with the target address range.
 * Called with mmap_lock held for user-mode emulation.
 * NOTE: this function must not be called while a TB is running.
 */
void tb_invalidate_phys_range(tb_page_addr_t start, tb_page_addr_t end)
{
    TranslationBlock *tb;
    PageForEachNext n;

    assert_memory_lock();

    PAGE_FOR_EACH_TB(start, end, unused, tb, n) {
        tb_phys_invalidate__locked(tb);
    }
}

/*
 * Invalidate all TBs which intersect with the target address page @addr.
 * Called with mmap_lock held for user-mode emulation
 * NOTE: this function must not be called while a TB is running.
 */
void tb_invalidate_phys_page(tb_page_addr_t addr)
{
    tb_page_addr_t start, end;

    start = addr & TARGET_PAGE_MASK;
    end = start + TARGET_PAGE_SIZE;
    tb_invalidate_phys_range(start, end);
}

/*
 * Called with mmap_lock held. If pc is not 0 then it indicates the
 * host PC of the faulting store instruction that caused this invalidate.
 * Returns true if the caller needs to abort execution of the current
 * TB (because it was modified by this store and the guest CPU has
 * precise-SMC semantics).
 */
bool tb_invalidate_phys_page_unwind(tb_page_addr_t addr, uintptr_t pc)
{
    assert(pc != 0);
#ifdef TARGET_HAS_PRECISE_SMC
    assert_memory_lock();
    {
        TranslationBlock *current_tb = tcg_tb_lookup(pc);
        bool current_tb_modified = false;
        TranslationBlock *tb;
        PageForEachNext n;

        addr &= TARGET_PAGE_MASK;

        PAGE_FOR_EACH_TB(addr, addr + TARGET_PAGE_SIZE, unused, tb, n) {
            if (current_tb == tb &&
                (tb_cflags(current_tb) & CF_COUNT_MASK) != 1) {
                /*
                 * If we are modifying the current TB, we must stop its
                 * execution. We could be more precise by checking that
                 * the modification is after the current PC, but it would
                 * require a specialized function to partially restore
                 * the CPU state.
                 */
                current_tb_modified = true;
                cpu_restore_state_from_tb(current_cpu, current_tb, pc, true);
            }
            tb_phys_invalidate__locked(tb);
        }

        if (current_tb_modified) {
            /* Force execution of one insn next time.  */
            CPUState *cpu = current_cpu;
            cpu->cflags_next_tb = 1 | CF_NOIRQ | curr_cflags(current_cpu);
            return true;
        }
    }
#else
    tb_invalidate_phys_page(addr);
#endif /* TARGET_HAS_PRECISE_SMC */
    return false;
}
#else
/*
 * @p must be non-NULL.
 * user-mode: call with mmap_lock held.
 * !user-mode: call with all @pages locked.
 */
static void
tb_invalidate_phys_page_range__locked(struct page_collection *pages,
                                      PageDesc *p, tb_page_addr_t start,
                                      tb_page_addr_t end,
                                      uintptr_t retaddr)
{
    TranslationBlock *tb;
    tb_page_addr_t tb_start, tb_end;
    PageForEachNext n;
#ifdef TARGET_HAS_PRECISE_SMC
    bool current_tb_modified = false;
    TranslationBlock *current_tb = retaddr ? tcg_tb_lookup(retaddr) : NULL;
#endif /* TARGET_HAS_PRECISE_SMC */

    /*
     * We remove all the TBs in the range [start, end[.
     * XXX: see if in some cases it could be faster to invalidate all the code
     */
    PAGE_FOR_EACH_TB(start, end, p, tb, n) {
        /* NOTE: this is subtle as a TB may span two physical pages */
        if (n == 0) {
            /* NOTE: tb_end may be after the end of the page, but
               it is not a problem */
            tb_start = tb_page_addr0(tb);
            tb_end = tb_start + tb->size;
        } else {
            tb_start = tb_page_addr1(tb);
            tb_end = tb_start + ((tb_page_addr0(tb) + tb->size)
                                 & ~TARGET_PAGE_MASK);
        }
        if (!(tb_end <= start || tb_start >= end)) {
#ifdef TARGET_HAS_PRECISE_SMC
            if (current_tb == tb &&
                (tb_cflags(current_tb) & CF_COUNT_MASK) != 1) {
                /*
                 * If we are modifying the current TB, we must stop
                 * its execution. We could be more precise by checking
                 * that the modification is after the current PC, but it
                 * would require a specialized function to partially
                 * restore the CPU state.
                 */
                current_tb_modified = true;
                cpu_restore_state_from_tb(current_cpu, current_tb,
                                          retaddr, true);
            }
#endif /* TARGET_HAS_PRECISE_SMC */
            tb_phys_invalidate__locked(tb);
        }
    }

    /* if no code remaining, no need to continue to use slow writes */
    if (!p->first_tb) {
        tlb_unprotect_code(start);
    }

#ifdef TARGET_HAS_PRECISE_SMC
    if (current_tb_modified) {
        page_collection_unlock(pages);
        /* Force execution of one insn next time.  */
        current_cpu->cflags_next_tb = 1 | CF_NOIRQ | curr_cflags(current_cpu);
        mmap_unlock();
        cpu_loop_exit_noexc(current_cpu);
    }
#endif
}

/*
 * Invalidate all TBs which intersect with the target physical
 * address page @addr.
 *
 * Called with mmap_lock held for user-mode emulation
 */
void tb_invalidate_phys_page(tb_page_addr_t addr)
{
    struct page_collection *pages;
    tb_page_addr_t start, end;
    PageDesc *p;

    p = page_find(addr >> TARGET_PAGE_BITS);
    if (p == NULL) {
        return;
    }

    start = addr & TARGET_PAGE_MASK;
    end = start + TARGET_PAGE_SIZE;
    pages = page_collection_lock(start, end);
    tb_invalidate_phys_page_range__locked(pages, p, start, end, 0);
    page_collection_unlock(pages);
}

/*
 * Invalidate all TBs which intersect with the target physical address range
 * [start;end[. NOTE: start and end may refer to *different* physical pages.
 * 'is_cpu_write_access' should be true if called from a real cpu write
 * access: the virtual CPU will exit the current TB if code is modified inside
 * this TB.
 *
 * Called with mmap_lock held for user-mode emulation.
 */
void tb_invalidate_phys_range(tb_page_addr_t start, tb_page_addr_t end)
{
    struct page_collection *pages;
    tb_page_addr_t next;

    pages = page_collection_lock(start, end);
    for (next = (start & TARGET_PAGE_MASK) + TARGET_PAGE_SIZE;
         start < end;
         start = next, next += TARGET_PAGE_SIZE) {
        PageDesc *pd = page_find(start >> TARGET_PAGE_BITS);
        tb_page_addr_t bound = MIN(next, end);

        if (pd == NULL) {
            continue;
        }
        assert_page_locked(pd);
        tb_invalidate_phys_page_range__locked(pages, pd, start, bound, 0);
    }
    page_collection_unlock(pages);
}

/*
 * len must be <= 8 and start must be a multiple of len.
 * Called via softmmu_template.h when code areas are written to with
 * iothread mutex not held.
 *
 * Call with all @pages in the range [@start, @start + len[ locked.
 */
void tb_invalidate_phys_page_fast(struct page_collection *pages,
                                  tb_page_addr_t start, int len,
                                  uintptr_t retaddr)
{
    PageDesc *p;

    p = page_find(start >> TARGET_PAGE_BITS);
    if (!p) {
        return;
    }

    assert_page_locked(p);
    tb_invalidate_phys_page_range__locked(pages, p, start, start + len,
                                          retaddr);
}
#endif /* CONFIG_USER_ONLY */
