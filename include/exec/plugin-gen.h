/*
 * Copyright (C) 2017, Emilio G. Cota <cota@braap.org>
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 *
 * plugin-gen.h - TCG-dependent definitions for generating plugin code
 *
 * This header should be included only from plugin.c and C files that emit
 * TCG code.
 */
#ifndef QEMU_PLUGIN_GEN_H
#define QEMU_PLUGIN_GEN_H

#include "qemu/plugin.h"
#include "tcg/tcg.h"

#ifdef CONFIG_PLUGINS

void qemu_plugin_gen_vcpu_mem_callbacks(struct qemu_plugin_dyn_cb_arr *arr,
                                        TCGv vaddr, uint8_t info);

void qemu_plugin_gen_vcpu_udata_callbacks(struct qemu_plugin_dyn_cb_arr *arr);

void qemu_plugin_gen_disable_mem_helpers(void);

void
qemu_plugin_gen_enable_mem_helpers(const struct qemu_plugin_dyn_cb_arr *orig);

#else /* !CONFIG_PLUGINS */

static inline void
qemu_plugin_gen_vcpu_mem_callbacks(struct qemu_plugin_dyn_cb_arr *arr,
                                   TCGv vaddr, uint8_t info)
{ }

static inline void
qemu_plugin_gen_vcpu_udata_callbacks(struct qemu_plugin_dyn_cb_arr *arr)
{ }

static inline void qemu_plugin_gen_disable_mem_helpers(void)
{ }

static inline void
qemu_plugin_gen_enable_mem_helpers(const struct qemu_plugin_dyn_cb_arr *orig)
{ }

#endif /* CONFIG_PLUGINS */

#endif /* QEMU_PLUGIN_GEN_H */

