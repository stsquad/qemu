/*
 * Plugin Shared Internal Functions
 *
 * Copyright (C) 2019, Linaro
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#ifndef _PLUGIN_INTERNAL_H_
#define _PLUGIN_INTERNAL_H_

void plugin_register_inline_op(struct qemu_plugin_dyn_cb_arr *arr,
                               enum qemu_plugin_mem_rw rw,
                               enum qemu_plugin_op op, void *ptr,
                               uint64_t imm);

void plugin_reset_uninstall(qemu_plugin_id_t id,
                            qemu_plugin_simple_cb_t cb,
                            bool reset);

void plugin_register_cb(qemu_plugin_id_t id, enum qemu_plugin_event ev,
                        void *func);

void
plugin_register_cb_udata(qemu_plugin_id_t id, enum qemu_plugin_event ev,
                         void *func, void *udata);

void
plugin_register_dyn_cb__udata(struct qemu_plugin_dyn_cb_arr *arr,
                              qemu_plugin_vcpu_udata_cb_t cb,
                              enum qemu_plugin_cb_flags flags, void *udata);

struct qemu_plugin_dyn_cb *
plugin_get_dyn_cb(struct qemu_plugin_dyn_cb_arr *arr);

void plugin_register_vcpu_mem_cb(struct qemu_plugin_dyn_cb_arr *arr,
                                 void *cb,
                                 enum qemu_plugin_cb_flags flags,
                                 enum qemu_plugin_mem_rw rw,
                                 void *udata, bool haddr);


void exec_inline_op(struct qemu_plugin_dyn_cb *cb);

#endif /* _PLUGIN_INTERNAL_H_ */
