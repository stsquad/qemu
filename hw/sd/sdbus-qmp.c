/*
 * SD card bus QMP debugging interface (for QTesting).
 *
 * Copyright (C) 2018 Philippe Mathieu-Daudé <f4bug@amsat.org>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or
 * (at your option) any later version.  See the COPYING file in the
 * top-level directory.
 */
#include "qemu/osdep.h"
#include "hw/sd/sd.h"
#include "qmp-commands.h"

SDBusCommandResponse *qmp_x_debug_sdbus_command(const char *qom_path,
                                                uint8_t command,
                                                bool has_arg, uint64_t arg,
                                                bool has_crc, uint16_t crc,
                                                Error **errp)
{
    uint8_t response[16 + 1];
    SDBusCommandResponse *res;
    bool ambiguous = false;
    Object *obj;
    SDBus *sdbus;
    int sz;

    obj = object_resolve_path(qom_path, &ambiguous);
    if (!obj) {
        if (ambiguous) {
            error_setg(errp, "Path '%s' is ambiguous", qom_path);
        } else {
            error_set(errp, ERROR_CLASS_DEVICE_NOT_FOUND,
                      "Device '%s' not found", qom_path);
        }
        return NULL;
    }
    sdbus = (SDBus *)object_dynamic_cast(obj, TYPE_SD_BUS);
    if (!sdbus) {
        error_setg(errp, "Device '%s' not a sd-bus", qom_path);
        return NULL;
    }

    res = g_new0(SDBusCommandResponse, 1);
    sz = sdbus_do_command(sdbus,
                          &(SDRequest){.cmd = command, .arg = arg,
                                       .crc = has_crc ? crc : -1 },
                          response);
    res->base64 = sz > 0 ? g_base64_encode(response, sz) : g_strdup("");

    return res;
}
