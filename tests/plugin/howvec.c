/*
 * Copyright (C) 2019, Alex Bennée <alex.bennee@linaro.org>
 *
 * How vectorised is this code?
 *
 * Attempt to measure the amount of vectorisation that has been done
 * on some code by counting classes of instruction. This is very much
 * ARM specific.
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 */
#include <inttypes.h>
#include <assert.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <glib.h>

#include <qemu-plugin.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

static bool collect_all = true;
static int limit = 50;
static int stdout_fd;
static bool do_inline;

typedef struct {
    char *insn;
    uint32_t opcode;
    uint64_t count;
} InsnExecCount;

static GMutex lock;
static GHashTable *insns;

typedef struct {
    const char *class;
    uint32_t mask;
    uint32_t pattern;
    uint64_t count;
} InsnClassExecCount;

/*
 * Matchers for classes of instructions, order is important.
 *
 * You most precise match must be before looser matches. If no match
 * is found in the table a new entry is created.
 */
InsnClassExecCount insn_classes[] = {
    /* Top Level Encoding */
    { "Data Proc Imm", 0x1c000000, 0x10000000, 0},
    { "Branches",      0x1c000000, 0x14000000, 0},
    { "Scalar FP ",    0x0e000000, 0x0e000000, 0},
};

static gint cmp_exec_count(gconstpointer a, gconstpointer b)
{
    InsnExecCount *ea = (InsnExecCount *) a;
    InsnExecCount *eb = (InsnExecCount *) b;
    return ea->count > eb->count ? -1 : 1;
}

static void plugin_exit(qemu_plugin_id_t id, void *p)
{
    GString *report = g_string_new("Instruction Classes:\n");
    int i;

    for (i = 0; i < ARRAY_SIZE(insn_classes); i++) {
        g_string_append_printf(report,"Class: %-16s %ld hits\n",
                               insn_classes[i].class, insn_classes[i].count);

    }

    if (collect_all) {
        GList *counts, *it;

        g_string_append_printf(report,"Individual Instructions:\n");

        counts = g_hash_table_get_values(insns);
        it = g_list_sort(counts, cmp_exec_count);

        for (i = 0; i < limit && it->next; i++, it = it->next) {
            InsnExecCount *rec = (InsnExecCount *) it->data;
            g_string_append_printf(report, "Instr: %-16s %ld hits\n",
                                   rec->insn,
                                   rec->count);
        }
        g_list_free(it);
    }

    dprintf(stdout_fd, "%s", report->str);
    g_string_free(report, true);
}

static void plugin_init(void)
{
    insns = g_hash_table_new(NULL, g_direct_equal);
}

static void vcpu_insn_exec_before(unsigned int cpu_index, void *udata)
{
    uint64_t *count = (uint64_t *) udata;
    (*count)++;
}

static uint64_t * find_counter(uint32_t insn)
{
    int i;
    uint64_t *cnt = NULL;

    for (i = 0; !cnt && i < ARRAY_SIZE(insn_classes); i++) {
        uint32_t masked_bits = insn & insn_classes[i].mask;
        if (masked_bits == insn_classes[i].pattern) {
            cnt = &insn_classes[i].count;
        }
    }

    /*
     * If we have failed to find an instruction class then find or create an
     * entry for the instruction.
     */
    if (!cnt && collect_all) {
        InsnExecCount *icount;

        g_mutex_lock(&lock);
        icount = (InsnExecCount *) g_hash_table_lookup(insns, GUINT_TO_POINTER(insn));

        if (!icount) {
            icount = g_new0(InsnExecCount, 1);
            icount->opcode = insn;
            icount->insn = g_strdup_printf("insn:%#08x", insn);
            g_hash_table_insert(insns, GUINT_TO_POINTER(insn), (gpointer) icount);
        }
        g_mutex_unlock(&lock);

        cnt = &icount->count;
    }

    return cnt;
}

static void vcpu_tb_trans(qemu_plugin_id_t id, unsigned int cpu_index,
                          struct qemu_plugin_tb *tb)
{
    size_t n = qemu_plugin_tb_n_insns(tb);
    size_t i;

    for (i = 0; i < n; i++) {
        uint32_t code;
        uint64_t *cnt;
        struct qemu_plugin_insn *insn = qemu_plugin_tb_get_insn(tb, i);
        /* we expect all instructions to by 32 bits for ARM */
        g_assert(qemu_plugin_insn_size(insn) == 4);
        code = *((uint32_t *)qemu_plugin_insn_data(insn));
        cnt = find_counter(code);

        if (do_inline) {
            qemu_plugin_register_vcpu_insn_exec_inline(
                insn, QEMU_PLUGIN_INLINE_ADD_U64, cnt, 1);
        } else {
            qemu_plugin_register_vcpu_insn_exec_cb(
                insn, vcpu_insn_exec_before, QEMU_PLUGIN_CB_NO_REGS, cnt);
        }
    }
}

QEMU_PLUGIN_EXPORT int qemu_plugin_install(qemu_plugin_id_t id, int argc,
                                           char **argv)
{
    if (argc && !strcmp(argv[0], "inline")) {
        do_inline = true;
    }

    /* to be used when in the exit hook */
    stdout_fd = dup(STDOUT_FILENO);
    assert(stdout_fd);

    plugin_init();

    qemu_plugin_register_vcpu_tb_trans_cb(id, vcpu_tb_trans);
    qemu_plugin_register_atexit_cb(id, plugin_exit, NULL);
    return 0;
}
