/*
 * Register Definition API
 *
 * Copyright (c) 2016 Xilinx Inc.
 * Copyright (c) 2013 Peter Crosthwaite <peter.crosthwaite@xilinx.com>
 *
 * This work is licensed under the terms of the GNU GPL, version 2.  See
 * the COPYING file in the top-level directory.
 */

#include "qemu/osdep.h"
#include "hw/register.h"
#include "hw/qdev.h"
#include "qemu/log.h"

static inline void register_write_val(RegisterInfo *reg, uint64_t val)
{
    g_assert(reg->data);

    switch (reg->data_size) {
    case 1:
        *(uint8_t *)reg->data = val;
        break;
    case 2:
        *(uint16_t *)reg->data = val;
        break;
    case 4:
        *(uint32_t *)reg->data = val;
        break;
    case 8:
        *(uint64_t *)reg->data = val;
        break;
    default:
        g_assert_not_reached();
    }
}

static inline uint64_t register_read_val(RegisterInfo *reg)
{
    switch (reg->data_size) {
    case 1:
        return *(uint8_t *)reg->data;
    case 2:
        return *(uint16_t *)reg->data;
    case 4:
        return *(uint32_t *)reg->data;
    case 8:
        return *(uint64_t *)reg->data;
    default:
        g_assert_not_reached();
    }
    return 0; /* unreachable */
}

void register_write(RegisterInfo *reg, uint64_t val, uint64_t we)
{
    uint64_t old_val, new_val, test, no_w_mask;
    const RegisterAccessInfo *ac;

    assert(reg);

    ac = reg->access;

    if (!ac || !ac->name) {
        qemu_log_mask(LOG_GUEST_ERROR, "%s: write to undefined device state "
                      "(written value: %#" PRIx64 ")\n", reg->prefix, val);
        return;
    }

    old_val = reg->data ? register_read_val(reg) : ac->reset;

    test = (old_val ^ val) & ac->rsvd;
    if (test) {
        qemu_log_mask(LOG_GUEST_ERROR, "%s: change of value in reserved bit"
                      "fields: %#" PRIx64 ")\n", reg->prefix, test);
    }

    test = val & ac->unimp;
    if (test) {
        qemu_log_mask(LOG_UNIMP,
                      "%s:%s writing %#" PRIx64 " to unimplemented bits:" \
                      " %#" PRIx64 "",
                      reg->prefix, reg->access->name, val, ac->unimp);
    }

    /* Create the no write mask based on the read only, write to clear and
     * reserved bit masks.
     */
    no_w_mask = ac->ro | ac->w1c | ac->rsvd | ~we;
    new_val = (val & ~no_w_mask) | (old_val & no_w_mask);
    new_val &= ~(val & ac->w1c);

    if (ac->pre_write) {
        new_val = ac->pre_write(reg, new_val);
    }

    if (reg->debug) {
        qemu_log("%s:%s: write of value %#" PRIx64 "\n", reg->prefix, ac->name,
                 new_val);
    }

    register_write_val(reg, new_val);

    if (ac->post_write) {
        ac->post_write(reg, new_val);
    }
}

uint64_t register_read(RegisterInfo *reg)
{
    uint64_t ret;
    const RegisterAccessInfo *ac;

    assert(reg);

    ac = reg->access;
    if (!ac || !ac->name) {
        qemu_log_mask(LOG_GUEST_ERROR, "%s: read from undefined device state\n",
                      reg->prefix);
        return 0;
    }

    ret = reg->data ? register_read_val(reg) : ac->reset;

    register_write_val(reg, ret & ~ac->cor);

    if (ac->post_read) {
        ret = ac->post_read(reg, ret);
    }

    if (reg->debug) {
        qemu_log("%s:%s: read of value %#" PRIx64 "\n", reg->prefix,
                 ac->name, ret);
    }

    return ret;
}

void register_reset(RegisterInfo *reg)
{
    g_assert(reg);

    if (!reg->data || !reg->access) {
        return;
    }

    register_write_val(reg, reg->access->reset);
}
