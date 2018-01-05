/*
 * Arasan SDHCI Controller emulation
 *
 * Copyright (C) 2018 Philippe Mathieu-Daudé <f4bug@amsat.org>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or
 * (at your option) any later version.  See the COPYING file in the
 * top-level directory.
 */
#include "qemu/osdep.h"
#include "hw/sd/sdhci.h"
#include "qapi/error.h"

/* Compatible with:
 * - SD Host Controller Specification Version 2.0 Part A2
 * - SDIO Specification Version 2.0
 * - MMC Specification Version 3.31
 *
 * - SDMA (single operation DMA)
 * - ADMA1 (4 KB boundary limited DMA)
 * - ADMA2
 *
 * - up to seven functions in SD1, SD4, but does not support SPI mode
 * - SD high-speed (SDHS) card
 * - SD High Capacity (SDHC) card
 *
 * - Low-speed, 1 KHz to 400 KHz
 * - Full-speed, 1 MHz to 50 MHz (25 MB/sec)
 */
static void arasan4_9a_sdhci_realize(DeviceState *dev, Error **errp)
{
    SDHCICommonClass *cc = SYSBUS_SDHCI_COMMON_GET_CLASS(dev);
    Object *obj = OBJECT(dev);
    Error *local_err = NULL;

    object_property_set_uint(obj, 2, "sd-spec-version", &local_err);
    object_property_set_bool(obj, true, "adma1", &local_err);
    object_property_set_bool(obj, true, "high-speed", &local_err);
    object_property_set_uint(obj, 1024, "max-block-length", &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        return;
    }

    cc->parent_realize(dev, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        return;
    }
}

static void arasan4_9a_sdhci_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    SDHCICommonClass *cc = SYSBUS_SDHCI_COMMON_CLASS(klass);

    cc->parent_realize = dc->realize;
    dc->realize = arasan4_9a_sdhci_realize;
}

static const TypeInfo arasan4_9a_sdhci_info = {
    .name = "arasan,sdhci-4.9a",
    .parent = TYPE_SYSBUS_SDHCI,
    .class_init = arasan4_9a_sdhci_class_init,
};

static void arasan_sdhci_register_types(void)
{
    type_register_static(&arasan4_9a_sdhci_info);
}

type_init(arasan_sdhci_register_types)
