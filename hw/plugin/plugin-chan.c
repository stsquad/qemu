#include "qemu/osdep.h"
#include "hw/pci/pci.h"
#include "qemu/plugin.h"
#include "qapi/error.h"
#include "qemu/error-report.h"

#define PLUGIN_CHAN_CFG_BAR    0
#define PLUGIN_CHAN_CMD_BAR    1
#define PLUGIN_CHAN_DATA_BAR   2

struct plugin_chan_cfg {
    uint32_t data_max_len;
};

struct plugin_chan {
    PCIDevice dev;
    MemoryRegion cfg_region;
    MemoryRegion cmd_region;
    MemoryRegion data_region;
    void *data;
    size_t data_size;
    struct plugin_chan_cfg cfg;
};

static uint64_t chan_cfg_read(void *obj, hwaddr addr, unsigned size)
{
    struct plugin_chan *s = obj;

    g_assert(size == 4);
    switch (addr) {
    case 0:
        return s->cfg.data_max_len;
        break;
    }
    g_assert_not_reached();
    return 0;
}

static void chan_cmd_write(void *obj, hwaddr addr, uint64_t val, unsigned size)
{
    struct plugin_chan *s = obj;

    g_assert(size == 4);
    switch (addr) {
    case 0:
        plugin_chan_xmit(val, s->data_size ? s->data : NULL, s->data_size);
        break;
    case 4:
        s->data_size = val;
        break;
    default:
        g_assert_not_reached();
    }
}

static const MemoryRegionOps chan_cfg_ops = {
    .read = chan_cfg_read,
    .endianness = DEVICE_LITTLE_ENDIAN,
    .impl = {
        .min_access_size = 4,
        .max_access_size = 4,
    },
};

static const MemoryRegionOps chan_cmd_ops = {
    .write = chan_cmd_write,
    .endianness = DEVICE_LITTLE_ENDIAN,
    .impl = {
        .min_access_size = 4,
        .max_access_size = 4,
    },
};

static void plugin_chan_realize(PCIDevice *pci_dev, Error **errp)
{
    struct plugin_chan *s = DO_UPCAST(struct plugin_chan, dev, pci_dev);
    Error *err = NULL;

    s->cfg.data_max_len = 4096;

    pci_set_word(s->dev.config + PCI_COMMAND,
                 PCI_COMMAND_IO | PCI_COMMAND_MEMORY);

    /* data */
    memory_region_init_ram(&s->data_region, OBJECT(s), "plugin_chan.data",
                           pow2ceil(s->cfg.data_max_len), &err);
    if (err) {
        error_propagate(errp, err);
        return;
    }
    pci_register_bar(&s->dev, PLUGIN_CHAN_DATA_BAR,
                     PCI_BASE_ADDRESS_SPACE_MEMORY, &s->data_region);
    s->data = qemu_map_ram_ptr(s->data_region.ram_block, 0);

    /* config */
    memory_region_init_io(&s->cfg_region, OBJECT(s), &chan_cfg_ops, s,
                          "plugin_chan.cfg", sizeof(struct plugin_chan_cfg));
    pci_register_bar(&s->dev, PLUGIN_CHAN_CFG_BAR,
                     PCI_BASE_ADDRESS_SPACE_MEMORY, &s->cfg_region);

    /* cmd */
    memory_region_init_io(&s->cmd_region, OBJECT(s), &chan_cmd_ops, s,
                          "plugin_chan.cmd", 8);
    pci_register_bar(&s->dev, PLUGIN_CHAN_CMD_BAR,
                     PCI_BASE_ADDRESS_SPACE_MEMORY, &s->cmd_region);
}

static void plugin_chan_class_init(ObjectClass *class, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(class);
    PCIDeviceClass *p = PCI_DEVICE_CLASS(class);

    p->realize = plugin_chan_realize;
    p->vendor_id = PCI_VENDOR_ID_REDHAT_QUMRANET;
    p->device_id = PCI_DEVICE_ID_QEMU_PLUGIN_CHAN;
    p->class_id = PCI_CLASS_MEMORY_RAM;
    dc->desc = "Plugin communication channel between guest and host";
}

static TypeInfo plugin_chan_info = {
    .name = "qemu-plugin-chan",
    .parent = TYPE_PCI_DEVICE,
    .instance_size = sizeof(struct plugin_chan),
    .class_init = plugin_chan_class_init,
    .interfaces = (InterfaceInfo[]) {
        { INTERFACE_CONVENTIONAL_PCI_DEVICE },
        { }
    },
};

static void plugin_chan_register_types(void)
{
    type_register_static(&plugin_chan_info);
}

type_init(plugin_chan_register_types)
