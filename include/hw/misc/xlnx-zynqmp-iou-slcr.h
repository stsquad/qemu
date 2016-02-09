/*
 * Xilinx ZynqMP IOU system level control registers (SLCR)
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

#include "hw/sysbus.h"
#include "hw/register.h"

#define TYPE_XLNX_ZYNQMP_IOU_SLCR "xlnx_zynqmp-iou-slcr"

#define XLNX_ZYNQMP_IOU_SLCR(obj) \
     OBJECT_CHECK(XlnxZynqMPIOUSLCR, (obj), TYPE_XLNX_ZYNQMP_IOU_SLCR)

#define XLNX_ZYNQ_MP_IOU_SLCR_R_MAX (0x314/4)

typedef struct XlnxZynqMPIOUSLCR XlnxZynqMPIOUSLCR;

struct XlnxZynqMPIOUSLCR {
    /*< private >*/
    SysBusDevice busdev;

    /*< public >*/
    MemoryRegion iomem;

    uint32_t regs[XLNX_ZYNQ_MP_IOU_SLCR_R_MAX];
    RegisterInfo regs_info[XLNX_ZYNQ_MP_IOU_SLCR_R_MAX];
};
