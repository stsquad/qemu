/*
 * Generic intermediate code generation.
 *
 * Copyright (C) 2016-2017 Lluís Vilanova <vilanova@ac.upc.edu>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#ifndef EXEC__TRANSLATE_BLOCK_H
#define EXEC__TRANSLATE_BLOCK_H

/*
 * Include this header from a target-specific file, and add a
 *
 *     DisasContextBase base;
 *
 * member in your target-specific DisasContext.
 */


#include "exec/exec-all.h"
#include "tcg/tcg.h"


/**
 * BreakpointCheckType:
 * @BC_MISS: No hit
 * @BC_HIT_INSN: Hit, but continue translating TB
 * @BC_HIT_TB: Hit, stop translating TB
 *
 * How to react to a breakpoint. A hit means no more breakpoints will be checked
 * for the current instruction.
 *
 * Not all breakpoints associated to an address are necessarily raised by
 * targets (e.g., due to conditions encoded in their flags), so tey can decide
 * that a breakpoint missed the address (@BP_MISS).
 */
typedef enum BreakpointCheckType {
    BC_MISS,
    BC_HIT_INSN,
    BC_HIT_TB,
} BreakpointCheckType;

/**
 * DisasJumpType:
 * @DJ_NEXT: Next instruction in program order.
 * @DJ_TOO_MANY: Too many instructions translated.
 * @DJ_TARGET: Start of target-specific conditions.
 *
 * What instruction to disassemble next.
 */
typedef enum DisasJumpType {
    DJ_NEXT,
    DJ_TOO_MANY,
    DJ_TARGET,
} DisasJumpType;

/**
 * DisasContextBase:
 * @tb: Translation block for this disassembly.
 * @pc_first: Address of first guest instruction in this TB.
 * @pc_next: Address of next guest instruction in this TB (current during
 *           disassembly).
 * @is_jmp: What instruction to disassemble next.
 * @num_insns: Number of translated instructions (including current).
 * @singlestep_enabled: "Hardware" single stepping enabled.
 *
 * Architecture-agnostic disassembly context.
 */
typedef struct DisasContextBase {
    TranslationBlock *tb;
    target_ulong pc_first;
    target_ulong pc_next;
    DisasJumpType is_jmp;
    unsigned int num_insns;
    bool singlestep_enabled;
} DisasContextBase;

/**
 * TranslatorOps:
 * @init_disas_context: Initialize a DisasContext struct (DisasContextBase has
 *                      already been initialized).
 * @init_globals: Initialize global variables.
 * @tb_start: Start translating a new TB.
 * @insn_start: Start translating a new instruction.
 * @breakpoint_check: Check if a breakpoint did hit. When called, the breakpoint
 *                    has already been checked to match the PC.
 * @disas_insn: Disassemble one instruction an return the PC for the next
 *              one. Can set db->is_jmp to DJ_TARGET or above to stop
 *              translation.
 * @tb_stop: Stop translating a TB.
 * @disas_flags: Get flags argument for log_target_disas().
 *
 * Target-specific operations for the generic translator loop.
 *
 * All operations but disas_insn() are optional, and ignored when not set.
 * A missing breakpoint_check() will ignore breakpoints. A missing disas_flags()
 * will pass no flags.
 */
typedef struct TranslatorOps {
    void (*init_disas_context)(DisasContextBase *db, CPUState *cpu);
    void (*init_globals)(DisasContextBase *db, CPUState *cpu);
    void (*tb_start)(DisasContextBase *db, CPUState *cpu);
    void (*insn_start)(DisasContextBase *db, CPUState *cpu);
    BreakpointCheckType (*breakpoint_check)(DisasContextBase *db, CPUState *cpu,
                                            const CPUBreakpoint *bp);
    target_ulong (*disas_insn)(DisasContextBase *db, CPUState *cpu);
    void (*tb_stop)(DisasContextBase *db, CPUState *cpu);
    int (*disas_flags)(const DisasContextBase *db);
} TranslatorOps;

/**
 * translate_block:
 * @ops: Target-specific operations.
 * @db:
 * @cpu:
 * @tb:
 *
 * Generic translator loop.
 */
void translate_block(const TranslatorOps *ops, DisasContextBase *db,
                     CPUState *cpu, TCGv_env *tcg_cpu, TranslationBlock *tb);

#endif  /* EXEC__TRANSLATE_BLOCK_H */
