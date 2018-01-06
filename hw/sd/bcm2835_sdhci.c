/*
 * Broadcom BCM2835 SDHCI controller emulation
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
 * - SD Host Controller Specification Version 3.0 Draft 1.0
 * - SDIO Specification Version 3.0
 * - MMC Specification Version 4.4
 *
 * - 32-bit access only
 * - default clocks
 * - no DMA
 * - SD high-speed (SDHS) card
 * - maximum block size: 1kB
 *
 * For the exact details please refer to the Arasan documentation:
 *   SD3.0_Host_AHB_eMMC4.4_Usersguide_ver5.9_jan11_10.pdf   ¯\_(ツ)_/¯
 */
static void bcm2835_sdhci_realize(DeviceState *dev, Error **errp)
{
    SDHCICommonClass *cc = SYSBUS_SDHCI_COMMON_GET_CLASS(dev);
    Object *obj = OBJECT(dev);
    Error *local_err = NULL;

    object_property_set_uint(obj, 3, "sd-spec-version", &local_err);
    object_property_set_uint(obj, 52, "timeout-freq", &local_err);
    object_property_set_uint(obj, 52, "max-frequency", &local_err);
    object_property_set_bool(obj, false, "sdma", &local_err);
    object_property_set_bool(obj, false, "adma1", &local_err);
    object_property_set_bool(obj, false, "adma2", &local_err);
    object_property_set_bool(obj, true, "1v8", &local_err);
    /* FIXME verify/validate with someone from Broadcom?
    object_property_set_uint(obj, 1024, "max-block-length", &local_err);
    */
    object_property_set_bool(obj, true, "pending-insert-quirk",  &local_err);
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

static void bcm2835_sdhci_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    SDHCICommonClass *cc = SYSBUS_SDHCI_COMMON_CLASS(klass);

    cc->parent_realize = dc->realize;
    dc->realize = bcm2835_sdhci_realize;
}

static const TypeInfo bcm2835_sdhci_info = {
    .name = "brcm,bcm2835-sdhci",
    .parent = TYPE_SYSBUS_SDHCI,
    .class_init = bcm2835_sdhci_class_init,
};

static void brcm_sdhci_register_types(void)
{
    type_register_static(&bcm2835_sdhci_info);
}

type_init(brcm_sdhci_register_types)
