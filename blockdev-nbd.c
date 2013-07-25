/*
 * Serving QEMU block devices via NBD
 *
 * Copyright (c) 2012 Red Hat, Inc.
 *
 * Author: Paolo Bonzini <pbonzini@redhat.com>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or
 * later.  See the COPYING file in the top-level directory.
 */

#include "sysemu/blockdev.h"
#include "hw/block/block.h"
#include "monitor/monitor.h"
#include "qapi/qmp/qerror.h"
#include "sysemu/sysemu.h"
#include "qmp-commands.h"
#include "trace.h"
#include "block/nbd.h"
#include "block/block_int.h"
#include "block/block.h"
#include "qemu/sockets.h"

static int server_fd = -1;

static void nbd_accept(void *opaque)
{
    struct sockaddr_in addr;
    socklen_t addr_len = sizeof(addr);

    int fd = accept(server_fd, (struct sockaddr *)&addr, &addr_len);
    if (fd >= 0) {
        nbd_client_new(NULL, fd, nbd_client_put);
    }
}

void qmp_nbd_server_start(SocketAddress *addr, Error **errp)
{
    if (server_fd != -1) {
        error_setg(errp, "NBD server already running");
        return;
    }

    server_fd = socket_listen(addr, errp);
    if (server_fd != -1) {
        qemu_set_fd_handler2(server_fd, NULL, nbd_accept, NULL, NULL);
    }
}

/* Hook into the BlockDriverState notifiers to close the export when
 * the file is closed.
 */
typedef struct NBDCloseNotifier {
    Notifier n;
    NBDExport *exp;
    QTAILQ_ENTRY(NBDCloseNotifier) next;
} NBDCloseNotifier;

static QTAILQ_HEAD(, NBDCloseNotifier) close_notifiers =
    QTAILQ_HEAD_INITIALIZER(close_notifiers);

static void nbd_close_notifier(Notifier *n, void *data)
{
    NBDCloseNotifier *cn = DO_UPCAST(NBDCloseNotifier, n, n);

    notifier_remove(&cn->n);
    QTAILQ_REMOVE(&close_notifiers, cn, next);

    nbd_export_close(cn->exp);
    nbd_export_put(cn->exp);
    g_free(cn);
}

static void snapshot_drive_backup_cb(void *opaque, int ret)
{
}

/* create a point-in-time snapshot BDS from an existing BDS */
static BlockDriverState *bdrv_create_snapshot(BlockDriverState *orig_bs)
{
    int ret;
    char filename[1024];
    BlockDriver *drv;
    BlockDriverState *bs;
    QEMUOptionParameter *options;
    Error *local_err = NULL;

    bs = bdrv_new("");
    ret = get_tmp_filename(filename, sizeof(filename));
    if (ret < 0) {
        goto err;
    }
    drv = bdrv_find_format("qcow2");
    if (drv < 0) {
        goto err;
    }
    options = parse_option_parameters("", drv->create_options, NULL);
    set_option_parameter_int(options, BLOCK_OPT_SIZE, bdrv_getlength(orig_bs));

    ret = bdrv_create(drv, filename, options);
    if (ret < 0) {
        goto err;
    }
    ret = bdrv_open(bs, filename, NULL, BDRV_O_RDWR, drv);
    if (ret < 0) {
        goto err;
    }
    bs->backing_hd = orig_bs;
    bdrv_ref(orig_bs);
    /* TODO: use sync mode none */
    backup_start(orig_bs, bs, 1,
            BLOCKDEV_ON_ERROR_REPORT,
            BLOCKDEV_ON_ERROR_REPORT,
            snapshot_drive_backup_cb, bs, &local_err);
    if (error_is_set(&local_err)) {
        goto err;
    }
    return bs;

err:
    bdrv_unref(bs);
    unlink(filename);
    return NULL;
}

void qmp_nbd_server_add(const char *device, bool has_writable, bool writable,
                        bool has_snapshot, bool snapshot, Error **errp)
{
    BlockDriverState *bs;
    NBDExport *exp;
    NBDCloseNotifier *n;

    if (server_fd == -1) {
        error_setg(errp, "NBD server not running");
        return;
    }

    if (nbd_export_find(device)) {
        error_setg(errp, "NBD server already exporting device '%s'", device);
        return;
    }

    bs = bdrv_find(device);
    if (!bs) {
        error_set(errp, QERR_DEVICE_NOT_FOUND, device);
        return;
    }

    if (!has_writable) {
        writable = false;
    }

    if (!has_snapshot) {
        snapshot = false;
    }

    if (bdrv_is_read_only(bs)) {
        writable = false;
    }

    if (snapshot) {
        bs = bdrv_create_snapshot(bs);
        if (!bs) {
            error_setg(errp, "Can't create snapshot for device");
            return;
        }
    }

    exp = nbd_export_new(bs, 0, -1, writable ? 0 : NBD_FLAG_READ_ONLY, NULL);

    nbd_export_set_name(exp, device);

    n = g_malloc0(sizeof(NBDCloseNotifier));
    n->n.notify = nbd_close_notifier;
    n->exp = exp;
    bdrv_add_close_notifier(bs, &n->n);
    QTAILQ_INSERT_TAIL(&close_notifiers, n, next);
    return;
}

void qmp_nbd_server_stop(Error **errp)
{
    while (!QTAILQ_EMPTY(&close_notifiers)) {
        NBDCloseNotifier *cn = QTAILQ_FIRST(&close_notifiers);
        nbd_close_notifier(&cn->n, nbd_export_get_blockdev(cn->exp));
    }

    if (server_fd != -1) {
        qemu_set_fd_handler2(server_fd, NULL, NULL, NULL, NULL);
        close(server_fd);
        server_fd = -1;
    }
}
