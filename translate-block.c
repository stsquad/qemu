/*
 * Generic intermediate code generation.
 *
 * Copyright (C) 2016-2017 Lluís Vilanova <vilanova@ac.upc.edu>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#include "qemu/osdep.h"
#include "qemu-common.h"
#include "qemu/error-report.h"
#include "cpu.h"
#include "tcg/tcg.h"
#include "tcg/tcg-op.h"
#include "exec/exec-all.h"
#include "exec/gen-icount.h"
#include "exec/log.h"
#include "exec/translate-block.h"


static inline void translate_block_tcg_check(const DisasContextBase *db)
{
    if (tcg_check_temp_count()) {
        error_report("warning: TCG temporary leaks before "TARGET_FMT_lx,
                     db->pc_next);
    }
}

void translate_block(const TranslatorOps *ops, DisasContextBase *db,
                     CPUState *cpu, TCGv_env *tcg_cpu, TranslationBlock *tb)
{
    int max_insns;

    /* Sanity-check ops */
    if (ops->disas_insn == NULL) {
        error_report("Missing ops->disas_insn");
        abort();
    }

    /* Initialize DisasContext */
    db->tb = tb;
    db->pc_first = tb->pc;
    db->pc_next = db->pc_first;
    db->is_jmp = DJ_NEXT;
    db->num_insns = 0;
    db->singlestep_enabled = cpu->singlestep_enabled;
    if (ops->init_disas_context) {
        ops->init_disas_context(db, cpu);
    }

    /* Initialize globals */
    if (ops->init_globals) {
        ops->init_globals(db, cpu);
    }
    tcg_clear_temp_count();

    /* Instruction counting */
    max_insns = db->tb->cflags & CF_COUNT_MASK;
    if (max_insns == 0) {
        max_insns = CF_COUNT_MASK;
    }
    if (max_insns > TCG_MAX_INSNS) {
        max_insns = TCG_MAX_INSNS;
    }
    if (db->singlestep_enabled || singlestep) {
        max_insns = 1;
    }

    /* Start translating */
    gen_tb_start(db->tb, *tcg_cpu);
    if (ops->tb_start) {
        ops->tb_start(db, cpu);
    }

    while (true) {
        CPUBreakpoint *bp;

        db->num_insns++;
        if (ops->insn_start) {
            ops->insn_start(db, cpu);
        }

        /* Early exit before breakpoint checks */
        if (unlikely(db->is_jmp != DJ_NEXT)) {
            break;
        }

        /* Pass breakpoint hits to target for further processing */
        bp = NULL;
        do {
            bp = cpu_breakpoint_get(cpu, db->pc_next, bp);
            if (unlikely(bp) && ops->breakpoint_check) {
                BreakpointCheckType bp_check = ops->breakpoint_check(
                    db, cpu, bp);
                if (bp_check == BC_HIT_INSN) {
                    /* Hit, keep translating */
                    /*
                     * TODO: if we're never going to have more than one BP in a
                     *       single address, we can simply use a bool here.
                     */
                    break;
                } else if (bp_check == BC_HIT_TB) {
                    goto done_generating;
                } else {
                    error_report("Unexpected BreakpointCheckType %d", bp_check);
                    abort();
                }
            }
        } while (bp != NULL);

        /* Accept I/O on last instruction */
        if (db->num_insns == max_insns && (db->tb->cflags & CF_LAST_IO)) {
            gen_io_start(*tcg_cpu);
        }

        /* Disassemble one instruction */
        db->pc_next = ops->disas_insn(db, cpu);

        /**************************************************/
        /* Conditions to stop translation                 */
        /**************************************************/

        /* Target-specific conditions set by disassembly */
        if (db->is_jmp != DJ_NEXT) {
            break;
        }

        /* Too many instructions */
        if (tcg_op_buf_full() || db->num_insns >= max_insns) {
            db->is_jmp = DJ_TOO_MANY;
            break;
        }

        /*
         * Check if next instruction is on next page, which can cause an
         * exception.
         *
         * NOTE: Target-specific code must check a single instruction does not
         *       cross page boundaries; the first in the TB is always allowed to
         *       cross pages (never goes through this check).
         */
        if ((db->pc_first & TARGET_PAGE_MASK)
            != (db->pc_next & TARGET_PAGE_MASK)) {
            db->is_jmp = DJ_TOO_MANY;
            break;
        }

        translate_block_tcg_check(db);
    }

    if (ops->tb_stop) {
        ops->tb_stop(db, cpu);
    }

    if (db->tb->cflags & CF_LAST_IO) {
        gen_io_end(*tcg_cpu);
    }

done_generating:
    gen_tb_end(db->tb, db->num_insns);

    translate_block_tcg_check(db);

#ifdef DEBUG_DISAS
    if (qemu_loglevel_mask(CPU_LOG_TB_IN_ASM) &&
        qemu_log_in_addr_range(db->pc_first)) {
        int flags;
        if (ops->disas_flags) {
            flags = ops->disas_flags(db);
        } else {
            flags = 0;
        }
        qemu_log_lock();
        qemu_log("----------------\n");
        qemu_log("IN: %s\n", lookup_symbol(db->pc_first));
        log_target_disas(cpu, db->pc_first, db->pc_next - db->pc_first, flags);
        qemu_log("\n");
        qemu_log_unlock();
    }
#endif

    db->tb->size = db->pc_next - db->pc_first;
    db->tb->icount = db->num_insns;
}
