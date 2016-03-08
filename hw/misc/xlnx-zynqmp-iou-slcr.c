/*
 * Xilinx ZynqMP IOU System Level Control Registers (SLCR)
 *
 * Copyright (c) 2013 Xilinx Inc
 * Copyright (c) 2013 Peter Crosthwaite <peter.crosthwaite@xilinx.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "qemu/osdep.h"
#include "hw/misc/xlnx-zynqmp-iou-slcr.h"

#ifndef XLNX_ZYNQMP_IOU_SLCR_ERR_DEBUG
#define XLNX_ZYNQMP_IOU_SLCR_ERR_DEBUG 0
#endif

REG32(SD_SLOTTYPE, 0x310)
    #define R_SD_SLOTTYPE_RSVD       0xffff7ffe

static const RegisterAccessInfo xlnx_zynqmp_iou_slcr_regs_info[] = {
    {   .name = "SD Slot TYPE",             .decode.addr = A_SD_SLOTTYPE,
            .rsvd = R_SD_SLOTTYPE_RSVD,
            .gpios = (RegisterGPIOMapping []) {
                { .name = "SD0_SLOTTYPE",   .bit_pos = 0  },
                { .name = "SD1_SLOTTYPE",   .bit_pos = 15 },
                {},
            }
    }
    /* FIXME: Complete device model */
};

static void xlnx_zynqmp_iou_slcr_reset(DeviceState *dev)
{
    XlnxZynqMPIOUSLCR *s = XLNX_ZYNQMP_IOU_SLCR(dev);
    int i;

    for (i = 0; i < XLNX_ZYNQ_MP_IOU_SLCR_R_MAX; ++i) {
        register_reset(&s->regs_info[i]);
    }
}

static const MemoryRegionOps xlnx_zynqmp_iou_slcr_ops = {
    .read = register_read_memory_le,
    .write = register_write_memory_le,
    .endianness = DEVICE_LITTLE_ENDIAN,
    .valid = {
        .min_access_size = 4,
        .max_access_size = 4,
    }
};

static void xlnx_zynqmp_iou_slcr_init(Object *obj)
{
    XlnxZynqMPIOUSLCR *s = XLNX_ZYNQMP_IOU_SLCR(obj);

    memory_region_init(&s->iomem, obj, "MMIO", XLNX_ZYNQ_MP_IOU_SLCR_R_MAX * 4);
    register_init_block32(DEVICE(obj), xlnx_zynqmp_iou_slcr_regs_info,
                          ARRAY_SIZE(xlnx_zynqmp_iou_slcr_regs_info),
                          s->regs_info, s->regs, &s->iomem,
                          &xlnx_zynqmp_iou_slcr_ops,
                          XLNX_ZYNQMP_IOU_SLCR_ERR_DEBUG,
                          XLNX_ZYNQ_MP_IOU_SLCR_R_MAX);
    sysbus_init_mmio(SYS_BUS_DEVICE(obj), &s->iomem);
}

static const VMStateDescription vmstate_xlnx_zynqmp_iou_slcr = {
    .name = "xlnx_zynqmp_iou_slcr",
    .version_id = 1,
    .minimum_version_id = 1,
    .minimum_version_id_old = 1,
    .fields = (VMStateField[]) {
        VMSTATE_UINT32_ARRAY(regs, XlnxZynqMPIOUSLCR,
                             XLNX_ZYNQ_MP_IOU_SLCR_R_MAX),
        VMSTATE_END_OF_LIST(),
    }
};

static void xlnx_zynqmp_iou_slcr_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);

    dc->reset = xlnx_zynqmp_iou_slcr_reset;
    dc->vmsd = &vmstate_xlnx_zynqmp_iou_slcr;
}

static const TypeInfo xlnx_zynqmp_iou_slcr_info = {
    .name          = TYPE_XLNX_ZYNQMP_IOU_SLCR,
    .parent        = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(XlnxZynqMPIOUSLCR),
    .class_init    = xlnx_zynqmp_iou_slcr_class_init,
    .instance_init = xlnx_zynqmp_iou_slcr_init,
};

static void xlnx_zynqmp_iou_slcr_register_types(void)
{
    type_register_static(&xlnx_zynqmp_iou_slcr_info);
}

type_init(xlnx_zynqmp_iou_slcr_register_types)
