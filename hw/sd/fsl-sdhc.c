/*
 * Freescale SD Host Controller for i.MX emulation
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

/* UHS-I SDIO3.0 SDR104 1.8V ADMA */
static void fsl_imx6q_usdhc_realize(DeviceState *dev, Error **errp)
{
    SDHCICommonClass *cc = SYSBUS_SDHCI_COMMON_GET_CLASS(dev);
    Object *obj = OBJECT(dev);
    Error *local_err = NULL;

    object_property_set_uint(obj, 3, "sd-spec-version", &local_err);
    object_property_set_uint(obj, 52, "timeout-freq", &local_err);
    object_property_set_uint(obj, 52, "max-frequency", &local_err);
    object_property_set_bool(obj, true, "adma1", &local_err);
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

static void fsl_imx6q_usdhc_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    SDHCICommonClass *cc = SYSBUS_SDHCI_COMMON_CLASS(klass);

    cc->parent_realize = dc->realize;
    dc->realize = fsl_imx6q_usdhc_realize;
}

static const TypeInfo fsl_imx6q_usdhc_info = {
    .name = "fsl,imx6q-usdhc",
    .parent = TYPE_SYSBUS_SDHCI,
    .class_init = fsl_imx6q_usdhc_class_init,
};

static void fsl_imx_sdhc_register_types(void)
{
    type_register_static(&fsl_imx6q_usdhc_info);
}

type_init(fsl_imx_sdhc_register_types)
