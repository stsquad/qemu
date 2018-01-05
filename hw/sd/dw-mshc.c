/*
 * Synopsys Designware Mobile Storage Host Controller emulation
 * (and Samsung Exynos specific extensions)
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
 * - SD Host Controller Specification Version 2.0
 * - SDIO Specification Version 2.0
 * - MMC Specification Version 4.3
 *
 * - SDMA
 * - ADMA
 */
static void exynos4210_dw_mshc_realize(DeviceState *dev, Error **errp)
{
    SDHCICommonClass *cc = SYSBUS_SDHCI_COMMON_GET_CLASS(dev);
    Object *obj = OBJECT(dev);
    Error *local_err = NULL;

    object_property_set_uint(obj, 2, "sd-spec-version", &local_err);
    object_property_set_bool(obj, true, "suspend", &local_err);
    object_property_set_bool(obj, true, "1v8", &local_err);
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

static void exynos4210_dw_mshc_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    SDHCICommonClass *cc = SYSBUS_SDHCI_COMMON_CLASS(klass);

    cc->parent_realize = dc->realize;
    dc->realize = exynos4210_dw_mshc_realize;
}

static const TypeInfo exynos4210_dw_mshc_info = {
    .name = "samsung,exynos4210-dw-mshc",
    .parent = TYPE_SYSBUS_SDHCI,
    .class_init = exynos4210_dw_mshc_class_init,
};

static void dw_mshc_sdhc_register_types(void)
{
    type_register_static(&exynos4210_dw_mshc_info);
}

type_init(dw_mshc_sdhc_register_types)
