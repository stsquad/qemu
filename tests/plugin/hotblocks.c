/*
 * Copyright (C) 2019, Alex Bennée <alex.bennee@linaro.org>
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

static bool do_inline;
static int stdout_fd;

/* Plugins need to take care of their own locking */
static GMutex lock;
static GHashTable *hotblocks;
static guint64 limit = 20;

/*
 * Counting Structure
 *
 * The internals of the TCG are not exposed to plugins so we can only
 * get the starting PC for each block. As a result we will see
 * multiple translations at the same PC, we aggregate them here.
 */
typedef struct {
    uint64_t start_addr;
    uint64_t exec_count;
    int      trans_count;
    unsigned long insns;
} ExecCount;

static gint cmp_exec_count(gconstpointer a, gconstpointer b)
{
    ExecCount *ea = (ExecCount *) a;
    ExecCount *eb = (ExecCount *) b;
    return ea->exec_count > eb->exec_count ? -1 : 1;
}

static void plugin_exit(qemu_plugin_id_t id, void *p)
{
    GString *report = g_string_new("collected ");
    GList *counts, *it;
    int i;

    g_mutex_lock(&lock);
    g_string_append_printf(report, "%d entries in the hash table\n",
                           g_hash_table_size(hotblocks));
    counts = g_hash_table_get_values(hotblocks);
    it = g_list_sort(counts, cmp_exec_count);

    for (i = 0; i < limit && it->next; i++, it = it->next) {
        ExecCount *rec = (ExecCount *) it->data;
        g_string_append_printf(report, "  pc: %#" PRIx64 " (%d block%s, %ld insns, %" PRId64" total hits)\n",
                               rec->start_addr,
                               rec->trans_count, rec->trans_count < 2 ? "" : "s",
                               rec->insns, rec->exec_count);
    }

    g_mutex_unlock(&lock);
    g_list_free(it);

    dprintf(stdout_fd, "%s", report->str);
    g_string_free(report, true);
}

static void plugin_init(void)
{
    hotblocks = g_hash_table_new(NULL, g_direct_equal);
}

static void vcpu_tb_exec(unsigned int cpu_index, void *udata)
{
    ExecCount *cnt;
    uint64_t cheap_hash = (uint64_t) udata;

    g_mutex_lock(&lock);
    cnt = (ExecCount *) g_hash_table_lookup(hotblocks, (gconstpointer) cheap_hash);
    /* should always succeed */
    g_assert(cnt);
    cnt->exec_count++;
    g_mutex_unlock(&lock);
}

/*
 * When do_inline we ask the plugin to increment the counter for us.
 * Otherwise a helper is inserted which calls the vcpu_tb_exec
 * callback.
 */
static void vcpu_tb_trans(qemu_plugin_id_t id, unsigned int cpu_index,
                          struct qemu_plugin_tb *tb)
{
    ExecCount *cnt;
    uint64_t pc = qemu_plugin_tb_vaddr(tb);
    unsigned long insns = qemu_plugin_tb_n_insns(tb);
    uint64_t cheap_hash = pc ^ insns;

    g_mutex_lock(&lock);
    cnt = (ExecCount *) g_hash_table_lookup(hotblocks, (gconstpointer) cheap_hash);
    if (cnt) {
        cnt->trans_count++;
    } else {
        cnt = g_new0(ExecCount, 1);
        cnt->start_addr = pc;
        cnt->trans_count = 1;
        cnt->insns = insns;
        g_hash_table_insert(hotblocks, (gpointer) cheap_hash, (gpointer) cnt);
    }

    g_mutex_unlock(&lock);

    if (do_inline) {
        qemu_plugin_register_vcpu_tb_exec_inline(tb, QEMU_PLUGIN_INLINE_ADD_U64,
                                                 &cnt->exec_count, 1);
    } else {
        qemu_plugin_register_vcpu_tb_exec_cb(tb, vcpu_tb_exec,
                                             QEMU_PLUGIN_CB_NO_REGS,
                                             (void *)cheap_hash);
    }
}

QEMU_PLUGIN_EXPORT int qemu_plugin_install(qemu_plugin_id_t id, int argc,
                                           char **argv)
{
    if (argc && strcmp(argv[0], "inline") == 0) {
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
