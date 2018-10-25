#include <inttypes.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>

#include <qemu-plugin.h>

static uint64_t bb_count;
static uint64_t insn_count;
const char *filename;
static int stdout_fd;

static void plugin_exit(qemu_plugin_id_t id, void *p)
{
    dprintf(stdout_fd, "insns: %" PRIu64", bb: %" PRIu64 ", "
            "avg insns/bb: %.2f\n",
            insn_count, bb_count, (double)insn_count / bb_count);
}

static void vcpu_tb_exec(unsigned int cpu_index, void *udata)
{
    unsigned long n_insns = (unsigned long)udata;

    insn_count += n_insns;
    bb_count++;
}

static void vcpu_tb_trans(qemu_plugin_id_t id, unsigned int cpu_index,
                          struct qemu_plugin_tb *tb)
{
    unsigned long n_insns = qemu_plugin_tb_n_insns(tb);

    qemu_plugin_register_vcpu_tb_exec_cb(tb, vcpu_tb_exec,
                                         QEMU_PLUGIN_CB_NO_REGS,
                                         (void *)n_insns);
}

QEMU_PLUGIN_EXPORT int qemu_plugin_install(qemu_plugin_id_t id, int argc,
                                           char **argv)
{
    /* plugin_exit might write to stdout after stdout has been closed */
    stdout_fd = dup(STDOUT_FILENO);
    assert(stdout_fd);

    qemu_plugin_register_vcpu_tb_trans_cb(id, vcpu_tb_trans);
    qemu_plugin_register_atexit_cb(id, plugin_exit, NULL);
    return 0;
}
