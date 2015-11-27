/*
 * Greensocs logger.
 *
 * This code is licensed under the GPL V2.
 */

#include "hw/sysbus.h"
#include "sysemu/char.h"

#define TYPE_GLOGGER "glogger"
#define GLOGGER(obj) OBJECT_CHECK(GLOGGERState, (obj), TYPE_GLOGGER)

typedef struct GLOGGERState {
    SysBusDevice parent_obj;

    MemoryRegion iomem;
} GLOGGERState;

static const unsigned char glogger_major = 0;
static const unsigned char glogger_minor = 1;
static const unsigned char glogger_id[8] =
  { 'G', 'L', 'O', 'G', 'G', 'E', 'R', 0x00};

static uint64_t glogger_read(void *opaque, hwaddr offset,
                             unsigned size)
{
//    GLOGGERState *s = (GLOGGERState *)opaque;
    if (offset >= 0x8 && offset <= 0x15) {
        return glogger_id[offset - 0x08];
    }
    switch (offset) {
    case 0x16:
        return glogger_major;
    break;
    case 0x17:
        return glogger_minor;
    default:
        return 0;
    }
}

static void glogger_write(void *opaque, hwaddr offset,
                          uint64_t value, unsigned size)
{
//    GLOGGERState *s = (GLOGGERState *)opaque;

    switch (offset) {
    case 0x04:
        if (value == 0) {
            printf("greensocs-logger: test failed!\n");
            exit(-1);
	}
	else
	{
            printf("greensocs-logger: test passed!\n");
            exit(0);
	}
    break;
    default:
        qemu_log_mask(LOG_GUEST_ERROR,
                      "glogger_write: Bad offset %x\n", (int)offset);
    }
}

static const MemoryRegionOps glogger_ops = {
    .read = glogger_read,
    .write = glogger_write,
    .endianness = DEVICE_NATIVE_ENDIAN,
};

static void glogger_init(Object *obj)
{
    SysBusDevice *sbd = SYS_BUS_DEVICE(obj);
    GLOGGERState *s = GLOGGER(obj);

    memory_region_init_io(&s->iomem, OBJECT(s), &glogger_ops, s, "glogger", 0x100);
    sysbus_init_mmio(sbd, &s->iomem);
}

static void glogger_realize(DeviceState *dev, Error **errp)
{

}

static void glogger_class_init(ObjectClass *oc, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->realize = glogger_realize;
}

static const TypeInfo glogger_info = {
    .name          = TYPE_GLOGGER,
    .parent        = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(GLOGGERState),
    .instance_init = glogger_init,
    .class_init    = glogger_class_init,
};

static void glogger_register_types(void)
{
    type_register_static(&glogger_info);
}

type_init(glogger_register_types)
