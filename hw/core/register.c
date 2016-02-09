/*
 * Register Definition API
 *
 * Copyright (c) 2013 Xilinx Inc.
 * Copyright (c) 2013 Peter Crosthwaite <peter.crosthwaite@xilinx.com>
 *
 * This work is licensed under the terms of the GNU GPL, version 2.  See
 * the COPYING file in the top-level directory.
 */

#include "hw/register.h"
#include "hw/qdev.h"
#include "qemu/log.h"

static inline void register_write_log(int mask, RegisterInfo *reg, int dir,
                                      uint64_t val, const char *msg,
                                      const char *reason)
{
    qemu_log_mask(mask, "%s:%s bits %#" PRIx64 " %s write of %d%s%s\n",
                  reg->prefix, reg->access->name, val, msg, dir,
                  reason ? ": " : "", reason ? reason : "");
}

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
    register_refresh_gpios(reg, old_val);

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
    g_assert(reg->data);
    g_assert(reg->access);
    uint64_t old_val;

    old_val = register_read_val(reg);

    register_write_val(reg, reg->access->reset);
    register_refresh_gpios(reg, old_val);
}

void register_refresh_gpios(RegisterInfo *reg, uint64_t old_value)
{
    const RegisterAccessInfo *ac;
    const RegisterGPIOMapping *gpio;

    ac = reg->access;
    for (gpio = ac->gpios; gpio && gpio->name; gpio++) {
        int i;

        if (gpio->input) {
            continue;
        }

        for (i = 0; i < gpio->num; ++i) {
            uint64_t gpio_value, gpio_value_old;

            qemu_irq gpo = qdev_get_gpio_out_named(DEVICE(reg), gpio->name, i);
            gpio_value_old = extract64(old_value,
                                       gpio->bit_pos + i * gpio->width,
                                       gpio->width) ^ gpio->polarity;
            gpio_value = extract64(register_read_val(reg),
                                   gpio->bit_pos + i * gpio->width,
                                   gpio->width) ^ gpio->polarity;
            if (!(gpio_value_old ^ gpio_value)) {
                continue;
            }
            if (reg->debug && gpo) {
                qemu_log("refreshing gpio out %s to %" PRIx64 "\n",
                         gpio->name, gpio_value);
            }
            qemu_set_irq(gpo, gpio_value);
        }
    }
}

typedef struct DeviceNamedGPIOHandlerOpaque {
    DeviceState *dev;
    const char *name;
} DeviceNamedGPIOHandlerOpaque;

static void register_gpio_handler(void *opaque, int n, int level)
{
    DeviceNamedGPIOHandlerOpaque *gho = opaque;
    RegisterInfo *reg = REGISTER(gho->dev);

    const RegisterAccessInfo *ac;
    const RegisterGPIOMapping *gpio;

    ac = reg->access;
    for (gpio = ac->gpios; gpio && gpio->name; gpio++) {
        if (gpio->input && !strcmp(gho->name, gpio->name)) {
            register_write_val(reg, deposit64(register_read_val(reg),
                                              gpio->bit_pos + n * gpio->width,
                                              gpio->width,
                                              level ^ gpio->polarity));
            return;
        }
    }

    abort();
}

void register_init(RegisterInfo *reg)
{
    assert(reg);
    const RegisterAccessInfo *ac;
    const RegisterGPIOMapping *gpio;

    if (!reg->data || !reg->access) {
        return;
    }

    object_initialize((void *)reg, sizeof(*reg), TYPE_REGISTER);

    ac = reg->access;
    for (gpio = ac->gpios; gpio && gpio->name; gpio++) {
        if (!gpio->num) {
            ((RegisterGPIOMapping *)gpio)->num = 1;
        }
        if (!gpio->width) {
            ((RegisterGPIOMapping *)gpio)->width = 1;
        }
        if (gpio->input) {
            DeviceNamedGPIOHandlerOpaque gho = {
                .name = gpio->name,
                .dev = DEVICE(reg),
            };
            qemu_irq irq;

            qdev_init_gpio_in_named(DEVICE(reg), register_gpio_handler,
                                    gpio->name, gpio->num);
            irq = qdev_get_gpio_in_named(DEVICE(reg), gpio->name, gpio->num);
            qemu_irq_set_opaque(irq, g_memdup(&gho, sizeof(gho)));
        } else {
            qemu_irq *gpos = g_new0(qemu_irq, gpio->num);

            qdev_init_gpio_out_named(DEVICE(reg), gpos, gpio->name, gpio->num);
        }
    }
}

static inline void register_write_memory(void *opaque, hwaddr addr,
                                         uint64_t value, unsigned size, bool be)
{
    RegisterInfo *reg = opaque;
    uint64_t we = ~0;
    int shift = 0;

    if (reg->data_size != size) {
        we = (size == 8) ? ~0ull : (1ull << size * 8) - 1;
        shift = 8 * (be ? reg->data_size - size - addr : addr);
    }

    assert(size + addr <= reg->data_size);
    register_write(reg, value << shift, we << shift);
}

void register_write_memory_be(void *opaque, hwaddr addr, uint64_t value,
                              unsigned size)
{
    register_write_memory(opaque, addr, value, size, true);
}


void register_write_memory_le(void *opaque, hwaddr addr, uint64_t value,
                              unsigned size)
{
    register_write_memory(opaque, addr, value, size, false);
}

static inline uint64_t register_read_memory(void *opaque, hwaddr addr,
                                            unsigned size, bool be)
{
    RegisterInfo *reg = opaque;
    int shift = 8 * (be ? reg->data_size - size - addr : addr);

    return register_read(reg) >> shift;
}

uint64_t register_read_memory_be(void *opaque, hwaddr addr, unsigned size)
{
    return register_read_memory(opaque, addr, size, true);
}

uint64_t register_read_memory_le(void *opaque, hwaddr addr, unsigned size)
{
    return register_read_memory(opaque, addr, size, false);
}

void register_init_block32(DeviceState *owner, const RegisterAccessInfo *rae,
                           int num, RegisterInfo *ri, uint32_t *data,
                           MemoryRegion *container, const MemoryRegionOps *ops,
                           bool debug_enabled)
{
    const char *debug_prefix = object_get_typename(OBJECT(owner));
    int i;

    for (i = 0; i < num; i++) {
        int index = rae[i].decode.addr / 4;
        RegisterInfo *r = &ri[index];

        *r = (RegisterInfo) {
            .data = &data[index],
            .data_size = sizeof(uint32_t),
            .access = &rae[i],
            .debug = debug_enabled,
            .prefix = debug_prefix,
            .opaque = owner,
        };
        register_init(r);
        qdev_pass_all_gpios(DEVICE(r), owner);

        memory_region_init_io(&r->mem, OBJECT(owner), ops, r, r->access->name,
                              sizeof(uint32_t));
        memory_region_add_subregion_no_print(container,
                                             r->access->decode.addr, &r->mem);
    }
}

static const TypeInfo register_info = {
    .name  = TYPE_REGISTER,
    .parent = TYPE_DEVICE,
};

static void register_register_types(void)
{
    type_register_static(&register_info);
}

type_init(register_register_types)
