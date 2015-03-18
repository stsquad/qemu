/*
 * g_poll implementation for QEMU Poll API
 *
 * Copyright Red Hat, Inc. 2014
 *
 * Authors:
 *   Fam Zheng <famz@redhat.com>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 *
 */

#include "qemu-common.h"
#include "qemu/timer.h"
#include "qemu/poll.h"
#if CONFIG_PPOLL
#include <poll.h>
#endif

struct QEMUPoll {
    /* Array of GPollFD for g_poll() */
    GArray *gpollfds;

    /* Array of opaque pointers, should be in sync with gpollfds */
    GArray *opaque;
};

QEMUPoll *qemu_poll_new(void)
{
    QEMUPoll *qpoll = g_new0(QEMUPoll, 1);
    qpoll->gpollfds = g_array_new(FALSE, FALSE, sizeof(GPollFD));
    qpoll->opaque = g_array_new(FALSE, FALSE, sizeof(void *));
    return qpoll;
}


void qemu_poll_free(QEMUPoll *qpoll)
{
    g_array_unref(qpoll->gpollfds);
    g_array_unref(qpoll->opaque);
    g_free(qpoll);
}

int qemu_poll(QEMUPoll *qpoll, int64_t timeout_ns)
{
    int i;
    void *fds = qpoll->gpollfds->data;
    int nfds = qpoll->gpollfds->len;

    for (i = 0; i < qpoll->gpollfds->len; i++) {
        GPollFD *p = &g_array_index(qpoll->gpollfds, GPollFD, i);
        p->revents = 0;
    }
#if CONFIG_PPOLL
    if (timeout_ns < 0) {
        return ppoll(fds, nfds, NULL, NULL);
    } else {
        struct timespec ts;
        int64_t tvsec = timeout_ns / 1000000000LL;
        /* Avoid possibly overflowing and specifying a negative number of
         * seconds, which would turn a very long timeout into a busy-wait.
         */
        if (tvsec > (int64_t)INT32_MAX) {
            tvsec = INT32_MAX;
        }
        ts.tv_sec = tvsec;
        ts.tv_nsec = timeout_ns % 1000000000LL;
        return ppoll(fds, nfds, &ts, NULL);
    }

#else
    return g_poll(data, nfds, qemu_timeout_ns_to_ms(timeout_ns));
#endif
}

/* Add an fd to poll. Return -EEXIST if fd already registered. */
int qemu_poll_add(QEMUPoll *qpoll, int fd, int gio_events, void *opaque)
{
    int i;
    GPollFD pfd = (GPollFD) {
        .fd = fd,
        .revents = 0,
        .events = gio_events,
    };

    for (i = 0; i < qpoll->gpollfds->len; i++) {
        GPollFD *p = &g_array_index(qpoll->gpollfds, GPollFD, i);
        if (p->fd == fd) {
            return -EEXIST;
        }
    }
    g_array_append_val(qpoll->gpollfds, pfd);
    g_array_append_val(qpoll->opaque, opaque);
    assert(qpoll->gpollfds->len == qpoll->opaque->len);
    return 0;
}

/* Delete a previously added fd. Return -ENOENT if fd not registered. */
int qemu_poll_del(QEMUPoll *qpoll, int fd)
{
    int i;
    int found = -1;

    for (i = 0; i < qpoll->gpollfds->len; i++) {
        GPollFD *p = &g_array_index(qpoll->gpollfds, GPollFD, i);
        if (p->fd == fd) {
            found = i;
            break;
        }
    }
    if (found >= 0) {
        g_array_remove_index(qpoll->gpollfds, found);
        g_array_remove_index(qpoll->opaque, found);
        assert(qpoll->gpollfds->len == qpoll->opaque->len);
        return 0;
    } else {
        return -ENOENT;
    }
}

int qemu_poll_set_fds(QEMUPoll *qpoll, GPollFD *fds, int nfds)
{
    int i;
    g_array_set_size(qpoll->gpollfds, 0);
    g_array_set_size(qpoll->opaque, 0);
    for (i = 0; i < nfds; i++) {
        void *opaque = &fds[i];
        g_array_append_val(qpoll->gpollfds, fds[i]);
        g_array_append_val(qpoll->opaque, opaque);
        assert(qpoll->gpollfds->len == qpoll->opaque->len);
    }
    return nfds;
}

int qemu_poll_get_events(QEMUPoll *qpoll,
                         QEMUPollEvent *events,
                         int max_events)
{
    int i;
    int r = 0;
    for (i = 0; i < qpoll->gpollfds->len && r < max_events; i++) {
        GPollFD *fd = &g_array_index(qpoll->gpollfds, GPollFD, i);
        if (fd->revents & fd->events) {
            events[r].fd = fd->fd;
            events[r].revents = fd->revents;
            events[r].events = fd->events;
            events[r].opaque = g_array_index(qpoll->opaque, void *, i);
            r++;
        }
    }
    return r;
}
