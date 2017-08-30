/*
 * Copyright (C) 2017, Emilio G. Cota <cota@braap.org>
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 */
#ifndef QEMU_PLUGIN_H
#define QEMU_PLUGIN_H

#include "qemu/config-file.h"
#include "qemu/qemu-plugin.h"
#include "qemu/error-report.h"
#include "qemu/queue.h"
#include "qemu/option.h"

/*
 * Option parsing/processing.
 * Note that we can load an arbitrary number of plugins.
 */
struct qemu_plugin_desc;
typedef QTAILQ_HEAD(, qemu_plugin_desc) QemuPluginList;

#ifdef CONFIG_PLUGIN
extern QemuOptsList qemu_plugin_opts;

static inline void qemu_plugin_add_opts(void)
{
    qemu_add_opts(&qemu_plugin_opts);
}

void qemu_plugin_opt_parse(const char *optarg, QemuPluginList *head);
int qemu_plugin_load_list(QemuPluginList *head);
#else /* !CONFIG_PLUGIN */
static inline void qemu_plugin_add_opts(void)
{ }

static inline void qemu_plugin_opt_parse(const char *optarg,
                                         QemuPluginList *head)
{
    error_report("plugin interface not enabled in this build");
    exit(1);
}

static inline int qemu_plugin_load_list(QemuPluginList *head)
{
    return 0;
}
#endif /* !CONFIG_PLUGIN */

/*
 * Events that plugins can subscribe to.
 */
enum qemu_plugin_event {
    QEMU_PLUGIN_EV_VCPU_INIT,
    QEMU_PLUGIN_EV_VCPU_EXIT,
    QEMU_PLUGIN_EV_VCPU_TB_TRANS,
    QEMU_PLUGIN_EV_VCPU_IDLE,
    QEMU_PLUGIN_EV_VCPU_RESUME,
    QEMU_PLUGIN_EV_VCPU_SYSCALL,
    QEMU_PLUGIN_EV_VCPU_SYSCALL_RET,
    QEMU_PLUGIN_EV_FLUSH,
    QEMU_PLUGIN_EV_ATEXIT,
    QEMU_PLUGIN_EV_MAX,
};

union qemu_plugin_cb_sig {
    qemu_plugin_simple_cb_t          simple;
    qemu_plugin_udata_cb_t           udata;
    qemu_plugin_vcpu_simple_cb_t     vcpu_simple;
    qemu_plugin_vcpu_udata_cb_t      vcpu_udata;
    qemu_plugin_vcpu_tb_trans_cb_t   vcpu_tb_trans;
    qemu_plugin_vcpu_mem_cb_t        vcpu_mem;
    qemu_plugin_vcpu_mem_haddr_cb_t  vcpu_mem_haddr;
    qemu_plugin_vcpu_syscall_cb_t    vcpu_syscall;
    qemu_plugin_vcpu_syscall_ret_cb_t vcpu_syscall_ret;
    void *generic;
};

enum plugin_dyn_cb_type {
    PLUGIN_CB_INSN,
    PLUGIN_CB_MEM,
    PLUGIN_CB_HADDR,
    PLUGIN_N_CB_TYPES,
};

enum plugin_dyn_cb_subtype {
    PLUGIN_CB_REGULAR,
    PLUGIN_CB_INLINE,
    PLUGIN_N_CB_SUBTYPES,
};

/*
 * A dynamic callback has an insertion point that is determined at run-time.
 * Usually the insertion point is somewhere in the code cache; think for
 * instance of a callback to be called upon the execution of a particular TB.
 */
struct qemu_plugin_dyn_cb {
    union qemu_plugin_cb_sig f;
    void *userp;
    unsigned tcg_flags;
    enum plugin_dyn_cb_type type;
    /* @rw applies to mem callbacks only (both regular and inline) */
    enum qemu_plugin_mem_rw rw;
    /* fields specific to each dyn_cb type go here */
    union {
        struct {
            bool haddr;
        } mem;
        struct {
            enum qemu_plugin_op op;
            uint64_t imm;
        } inline_insn;
    };
};

struct qemu_plugin_dyn_cb_arr {
    struct qemu_plugin_dyn_cb *data;
    size_t n;
    size_t capacity;
};

struct qemu_plugin_insn {
    void *data;
    size_t size;
    size_t capacity;
    uint64_t vaddr;
    void *haddr;
    struct qemu_plugin_dyn_cb_arr cbs[PLUGIN_N_CB_TYPES][PLUGIN_N_CB_SUBTYPES];
    bool calls_helpers;
    bool mem_helper;
};

struct qemu_plugin_tb {
    struct qemu_plugin_insn *insns;
    size_t n;
    size_t capacity;
    uint64_t vaddr;
    uint64_t vaddr2;
    void *haddr1;
    void *haddr2;
    struct qemu_plugin_dyn_cb_arr cbs[PLUGIN_N_CB_SUBTYPES];
};

static inline
void qemu_plugin_dyn_cb_arr_init(struct qemu_plugin_dyn_cb_arr *arr)
{
    arr->data = NULL;
    arr->capacity = 0;
}

static inline
struct qemu_plugin_insn *qemu_plugin_tb_insn_get(struct qemu_plugin_tb *tb)
{
    struct qemu_plugin_insn *insn;
    int i, j;

    if (unlikely(tb->n == tb->capacity)) {
        tb->insns = g_renew(struct qemu_plugin_insn, tb->insns, ++tb->capacity);
        insn = &tb->insns[tb->capacity - 1];
        insn->data = NULL;
        insn->capacity = 0;
        for (i = 0; i < PLUGIN_N_CB_TYPES; i++) {
            for (j = 0; j < PLUGIN_N_CB_SUBTYPES; j++) {
                qemu_plugin_dyn_cb_arr_init(&insn->cbs[i][j]);
            }
        }
    }
    insn = &tb->insns[tb->n++];
    insn->size = 0;
    insn->calls_helpers = false;
    insn->mem_helper = false;

    for (i = 0; i < PLUGIN_N_CB_TYPES; i++) {
        for (j = 0; j < PLUGIN_N_CB_SUBTYPES; j++) {
            insn->cbs[i][j].n = 0;
        }
    }

    return insn;
}

#ifdef CONFIG_PLUGIN

void qemu_plugin_vcpu_init_hook(CPUState *cpu);
void qemu_plugin_vcpu_exit_hook(CPUState *cpu);
void qemu_plugin_tb_trans_cb(CPUState *cpu, struct qemu_plugin_tb *tb);
void qemu_plugin_vcpu_idle_cb(CPUState *cpu);
void qemu_plugin_vcpu_resume_cb(CPUState *cpu);
void
qemu_plugin_vcpu_syscall(CPUState *cpu, int64_t num, uint64_t a1,
                         uint64_t a2, uint64_t a3, uint64_t a4, uint64_t a5,
                         uint64_t a6, uint64_t a7, uint64_t a8);
void qemu_plugin_vcpu_syscall_ret(CPUState *cpu, int64_t num, int64_t ret);

void qemu_plugin_vcpu_mem_cb(CPUState *cpu, uint64_t vaddr, void *haddr,
                             uint32_t meminfo);

void qemu_plugin_flush_cb(void);

void qemu_plugin_atexit_cb(void);

void qemu_plugin_add_dyn_cb_arr(struct qemu_plugin_dyn_cb_arr *arr);

void qemu_plugin_disable_mem_helpers(CPUState *cpu);

#else /* !CONFIG_PLUGIN */

static inline void qemu_plugin_vcpu_init_hook(CPUState *cpu)
{ }

static inline void qemu_plugin_vcpu_exit_hook(CPUState *cpu)
{ }

static inline void qemu_plugin_tb_trans_cb(CPUState *cpu,
                                           struct qemu_plugin_tb *tb)
{ }

static inline void qemu_plugin_vcpu_idle_cb(CPUState *cpu)
{ }

static inline void qemu_plugin_vcpu_resume_cb(CPUState *cpu)
{ }

static inline void
qemu_plugin_vcpu_syscall(CPUState *cpu, int64_t num, uint64_t a1, uint64_t a2,
                         uint64_t a3, uint64_t a4, uint64_t a5, uint64_t a6,
                         uint64_t a7, uint64_t a8)
{ }

static inline
void qemu_plugin_vcpu_syscall_ret(CPUState *cpu, int64_t num, int64_t ret)
{ }

static inline void qemu_plugin_vcpu_mem_cb(CPUState *cpu, uint64_t vaddr,
                                           void *haddr, uint32_t meminfo)
{ }

static inline void qemu_plugin_flush_cb(void)
{ }

static inline void qemu_plugin_atexit_cb(void)
{ }

static inline
void qemu_plugin_add_dyn_cb_arr(struct qemu_plugin_dyn_cb_arr *arr)
{ }

static inline void qemu_plugin_disable_mem_helpers(CPUState *cpu)
{ }

#endif /* !CONFIG_PLUGIN */

#endif /* QEMU_PLUGIN_H */
