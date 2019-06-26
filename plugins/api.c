/*
 * QEMU Plugin API
 *
 * This provides the API that is available to the plugins to interact
 * with QEMU. We have to be careful not to expose internal details of
 * how QEMU works so we abstract out things like translation and
 * instructions to anonymous data types:
 *
 *  qemu_plugin_tb
 *  qemu_plugin_insn
 *
 * Which can then be passed back into the API to do additional things.
 * As such all the public functions in here are exported in
 * qemu-plugin.h.
 *
 * The general life-cycle of a plugin is:
 *
 *  - plugin is loaded, public qemu_plugin_install called
 *    - the install func registers callbacks for events
 *    - usually an atexit_cb is registered to dump info at the end
 *  - when a registered event occurs the plugin is called
 *     - some events pass additional info
 *     - during translation the plugin can decide to instrument any
 *       instruction
 *  - when QEMU exits all the registered atexit callbacks are called
 *
 * Copyright (C) 2017, Emilio G. Cota <cota@braap.org>
 * Copyright (C) 2019, Linaro
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 *
 */

#include "qemu/osdep.h"
#include "qemu/plugin.h"
#include "cpu.h"
#include "sysemu/sysemu.h"
#include "tcg/tcg.h"
#include "trace/mem-internal.h" /* mem_info macros */
#include "disas/disas.h"
#include "plugin.h"

/* Uninstall and Reset handlers */

void qemu_plugin_uninstall(qemu_plugin_id_t id, qemu_plugin_simple_cb_t cb)
{
    plugin_reset_uninstall(id, cb, false);
}

void qemu_plugin_reset(qemu_plugin_id_t id, qemu_plugin_simple_cb_t cb)
{
    plugin_reset_uninstall(id, cb, true);
}

/*
 * Plugin Register Functions
 *
 * This allows the plugin to register callbacks for various events
 * during the translation.
 */

void qemu_plugin_register_vcpu_init_cb(qemu_plugin_id_t id,
                                       qemu_plugin_vcpu_simple_cb_t cb)
{
    plugin_register_cb(id, QEMU_PLUGIN_EV_VCPU_INIT, cb);
}

void qemu_plugin_register_vcpu_exit_cb(qemu_plugin_id_t id,
                                       qemu_plugin_vcpu_simple_cb_t cb)
{
    plugin_register_cb(id, QEMU_PLUGIN_EV_VCPU_EXIT, cb);
}

void qemu_plugin_register_vcpu_tb_exec_cb(struct qemu_plugin_tb *tb,
                                          qemu_plugin_vcpu_udata_cb_t cb,
                                          enum qemu_plugin_cb_flags flags,
                                          void *udata)
{
    plugin_register_dyn_cb__udata(&tb->cbs[PLUGIN_CB_REGULAR],
                                  cb, flags, udata);
}

void qemu_plugin_register_vcpu_tb_exec_inline(struct qemu_plugin_tb *tb,
                                              enum qemu_plugin_op op,
                                              void *ptr, uint64_t imm)
{
    plugin_register_inline_op(&tb->cbs[PLUGIN_CB_INLINE], 0, op, ptr, imm);
}

void qemu_plugin_register_vcpu_insn_exec_cb(struct qemu_plugin_insn *insn,
                                            qemu_plugin_vcpu_udata_cb_t cb,
                                            enum qemu_plugin_cb_flags flags,
                                            void *udata)
{
    plugin_register_dyn_cb__udata(&insn->cbs[PLUGIN_CB_INSN][PLUGIN_CB_REGULAR],
        cb, flags, udata);
}

void qemu_plugin_register_vcpu_insn_exec_inline(struct qemu_plugin_insn *insn,
                                                enum qemu_plugin_op op,
                                                void *ptr, uint64_t imm)
{
    plugin_register_inline_op(&insn->cbs[PLUGIN_CB_INSN][PLUGIN_CB_INLINE],
                              0, op, ptr, imm);
}



void qemu_plugin_register_vcpu_mem_cb(struct qemu_plugin_insn *insn,
                                      qemu_plugin_vcpu_mem_cb_t cb,
                                      enum qemu_plugin_cb_flags flags,
                                      enum qemu_plugin_mem_rw rw,
                                      void *udata)
{
    plugin_register_vcpu_mem_cb(&insn->cbs[PLUGIN_CB_MEM][PLUGIN_CB_REGULAR],
        cb, flags, rw, udata, false);
}

void qemu_plugin_register_vcpu_mem_haddr_cb(struct qemu_plugin_insn *insn,
                                            qemu_plugin_vcpu_mem_haddr_cb_t cb,
                                            enum qemu_plugin_cb_flags flags,
                                            enum qemu_plugin_mem_rw rw,
                                            void *udata)
{
    plugin_register_vcpu_mem_cb(&insn->cbs[PLUGIN_CB_HADDR][PLUGIN_CB_REGULAR],
        cb, flags, rw, udata, true);
}

void qemu_plugin_register_vcpu_mem_inline(struct qemu_plugin_insn *insn,
                                          enum qemu_plugin_mem_rw rw,
                                          enum qemu_plugin_op op, void *ptr,
                                          uint64_t imm)
{
    plugin_register_inline_op(&insn->cbs[PLUGIN_CB_MEM][PLUGIN_CB_INLINE],
        rw, op, ptr, imm);
}

void qemu_plugin_register_vcpu_tb_trans_cb(qemu_plugin_id_t id,
                                           qemu_plugin_vcpu_tb_trans_cb_t cb)
{
    plugin_register_cb(id, QEMU_PLUGIN_EV_VCPU_TB_TRANS, cb);
}

void qemu_plugin_register_vcpu_syscall_cb(qemu_plugin_id_t id,
                                          qemu_plugin_vcpu_syscall_cb_t cb)
{
    plugin_register_cb(id, QEMU_PLUGIN_EV_VCPU_SYSCALL, cb);
}

void
qemu_plugin_register_vcpu_syscall_ret_cb(qemu_plugin_id_t id,
                                         qemu_plugin_vcpu_syscall_ret_cb_t cb)
{
    plugin_register_cb(id, QEMU_PLUGIN_EV_VCPU_SYSCALL_RET, cb);
}

/*
 * Plugin Queries
 *
 * These are queries that the plugin can make to gauge information
 * from our opaque data types. We do not want to leak internal details
 * here just information useful to the plugin.
 */

/*
 * Translation block information:
 *
 * A plugin can query the virtual address of the start of the block
 * and the number of instructions in it. It can also get access to
 * each translated instruction.
 */

size_t qemu_plugin_tb_n_insns(const struct qemu_plugin_tb *tb)
{
    return tb->n;
}

uint64_t qemu_plugin_tb_vaddr(const struct qemu_plugin_tb *tb)
{
    return tb->vaddr;
}

struct qemu_plugin_insn *
qemu_plugin_tb_get_insn(const struct qemu_plugin_tb *tb, size_t idx)
{
    if (unlikely(idx >= tb->n)) {
        return NULL;
    }
    return g_ptr_array_index(tb->insns, idx);
}

/*
 * Instruction information
 *
 * These queries allow the plugin to retrieve information about each
 * instruction being translated.
 */

const void *qemu_plugin_insn_data(const struct qemu_plugin_insn *insn)
{
    return insn->data->data;
}

size_t qemu_plugin_insn_size(const struct qemu_plugin_insn *insn)
{
    return insn->data->len;
}

uint64_t qemu_plugin_insn_vaddr(const struct qemu_plugin_insn *insn)
{
    return insn->vaddr;
}

void *qemu_plugin_insn_haddr(const struct qemu_plugin_insn *insn)
{
    return insn->haddr;
}

char *qemu_plugin_insn_disas(const struct qemu_plugin_insn *insn)
{
    CPUState *cpu = current_cpu;
    return plugin_disas(cpu, insn->vaddr, insn->data->len);
}

/*
 * The memory queries allow the plugin to query information about a
 * memory access.
 */

unsigned qemu_plugin_mem_size_shift(qemu_plugin_meminfo_t info)
{
    return info & TRACE_MEM_SZ_SHIFT_MASK;
}

bool qemu_plugin_mem_is_sign_extended(qemu_plugin_meminfo_t info)
{
    return !!(info & TRACE_MEM_SE);
}

bool qemu_plugin_mem_is_big_endian(qemu_plugin_meminfo_t info)
{
    return !!(info & TRACE_MEM_BE);
}

bool qemu_plugin_mem_is_store(qemu_plugin_meminfo_t info)
{
    return !!(info & TRACE_MEM_ST);
}

/*
 * Memory transformations.
 * Queries about the hwaddr
 */

static GArray *hwaddr_refs;

struct qemu_plugin_hwaddr *qemu_plugin_get_hwaddr(uint64_t vaddr)
{
    CPUState *cpu = current_cpu;

    if (!hwaddr_refs) {
        hwaddr_refs = g_array_new()
    }


}

bool qemu_plugin_hwaddr_spans_pages(struct qemu_plugin_hwaddr *hwaddr)
{
    return false;
}

bool qemu_plugin_hwaddr_is_io(struct qemu_plugin_hwaddr *hwaddr)
{
        return false;
}
uint64_t qemu_plugin_raddr_from_hwaddr(const struct qemu_plugin_hwaddr *haddr);

uint64_t qemu_plugin_hwaddr_page(const struct qemu_plugin_hwaddr *haddr, bool second_page)
{
#ifdef CONFIG_SOFTMMU
    return second_page ? haddr->haddr2 : haddr->haddr1;
#else
    return 0;
#endif
}

uint64_t qemu_plugin_ram_addr_from_host(const struct qemu_plugin_hwaddr *haddr)
{
#ifdef CONFIG_SOFTMMU
    ram_addr_t ram_addr;

    g_assert(haddr);
    ram_addr = qemu_ram_addr_from_host((void *) haddr->haddr1);
    if (ram_addr == RAM_ADDR_INVALID) {
        error_report("Bad ram pointer %p", haddr);
        abort();
    }
    return ram_addr;
#else
    return 0;
#endif
}

/*
 * Queries to the number and potential maximum number of vCPUs there
 * will be. This helps the plugin dimension per-vcpu arrays.
 */

int qemu_plugin_n_vcpus(void)
{
#ifdef CONFIG_USER_ONLY
    return -1;
#else
    return smp_cpus;
#endif
}

int qemu_plugin_n_max_vcpus(void)
{
#ifdef CONFIG_USER_ONLY
    return -1;
#else
    return max_cpus;
#endif
}
