/*
 * Copyright (C) 2017, Emilio G. Cota <cota@braap.org>
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 */
#ifndef QEMU_PLUGIN_H
#define QEMU_PLUGIN_H

#include "qemu/config-file.h"
#include "qemu/plugin-api.h"
#include "qemu/error-report.h"
#include "qemu/queue.h"
#include "qemu/option.h"

/*
 * Option parsing/processing.
 * Note that we can load an arbitrary number of plugins.
 */
struct qemu_plugin_desc;
QTAILQ_HEAD(qemu_plugin_list, qemu_plugin_desc);

#ifdef CONFIG_PLUGINS
extern QemuOptsList qemu_plugin_opts;

static inline void qemu_plugin_add_opts(void)
{
    qemu_add_opts(&qemu_plugin_opts);
}

void qemu_plugin_opt_parse(const char *optarg, struct qemu_plugin_list *head);
int qemu_plugin_load_list(struct qemu_plugin_list *head);
#else /* !CONFIG_PLUGINS */
static inline void qemu_plugin_add_opts(void)
{ }

static inline void qemu_plugin_opt_parse(const char *optarg,
                                         struct qemu_plugin_list *head)
{
    error_report("plugin interface not enabled in this build");
    exit(1);
}

static inline int qemu_plugin_load_list(struct qemu_plugin_list *head)
{
    return 0;
}
#endif /* !CONFIG_PLUGINS */

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
    QEMU_PLUGIN_EV_LOCKSTEP,
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

enum qemu_plugin_dyn_cb_type {
    QEMU_PLUGIN_DYN_CB_TYPE_REGULAR,
    QEMU_PLUGIN_DYN_CB_TYPE_INLINE,
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
    enum qemu_plugin_dyn_cb_type type;
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
    struct qemu_plugin_dyn_cb_arr exec_cbs;
    struct qemu_plugin_dyn_cb_arr mem_cbs;
    bool calls_helpers;
};

struct qemu_plugin_tb {
    struct qemu_plugin_insn *insns;
    size_t n;
    size_t capacity;
    uint64_t vaddr;
    struct qemu_plugin_dyn_cb_arr cbs;
};

extern bool use_plugin_clock;

static inline void qemu_plugin_insn_append(struct qemu_plugin_insn *insn,
                                           const void *from, size_t size)
{
    if (insn == NULL) {
        return;
    }
    if (unlikely(insn->size + size > insn->capacity)) {
        insn->data = g_realloc(insn->data, insn->size + size);
        insn->capacity = insn->size + size;
    }
    memcpy(insn->data + insn->size, from, size);
    insn->size += size;
}

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

    if (unlikely(tb->n == tb->capacity)) {
        tb->insns = g_renew(struct qemu_plugin_insn, tb->insns, ++tb->capacity);
        insn = &tb->insns[tb->capacity - 1];
        insn->data = NULL;
        insn->capacity = 0;
        qemu_plugin_dyn_cb_arr_init(&insn->exec_cbs);
        qemu_plugin_dyn_cb_arr_init(&insn->mem_cbs);
    }
    insn = &tb->insns[tb->n++];
    insn->size = 0;
    insn->exec_cbs.n = 0;
    insn->mem_cbs.n = 0;
    insn->calls_helpers = false;

    return insn;
}

#ifdef CONFIG_PLUGINS

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
int64_t plugin_get_clock(void);
void plugin_lockstep_cb(void);

#else /* !CONFIG_PLUGINS */

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

static inline void plugin_lockstep_cb(void)
{ }

int64_t plugin_get_clock(void);

#endif /* !CONFIG_PLUGINS */

#endif /* QEMU_PLUGIN_H */
