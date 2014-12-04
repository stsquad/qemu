/*
 * epoll implementation for QEMU Poll API
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

#include <sys/epoll.h>

#define USE_TIMERFD CONFIG_TIMERFD

#ifdef USE_TIMERFD
#include <sys/timerfd.h>
#endif

#include <glib.h>
#include <poll.h>
#include "qemu-common.h"
#include "qemu/timer.h"
#include "qemu/poll.h"


struct QEMUPoll {
    int epollfd;
#if USE_TIMERFD
    int timerfd;
#endif
    struct epoll_event *events;
    int max_events;
    int nevents;
    GHashTable *fds;
};

static void qemu_poll_init_timerfd(QEMUPoll *qpoll)
{
#if USE_TIMERFD
    int r;
    struct epoll_event ev;
    qpoll->timerfd = timerfd_create(CLOCK_MONOTONIC,
                                    TFD_NONBLOCK | TFD_CLOEXEC);
    if (qpoll->timerfd < 0) {
        perror("timerfd_create");
        abort();
    }
    ev.events = EPOLLIN | EPOLLOUT | EPOLLERR | EPOLLHUP;
    ev.data.fd = qpoll->timerfd;
    r = epoll_ctl(qpoll->epollfd, EPOLL_CTL_ADD, qpoll->timerfd, &ev);
    if (r) {
        perror("epoll_ctl add timerfd");
        abort();
    }
#endif
}

QEMUPoll *qemu_poll_new(void)
{
    int epollfd;
    QEMUPoll *qpoll = g_new0(QEMUPoll, 1);
    epollfd = epoll_create1(EPOLL_CLOEXEC);
    if (epollfd < 0) {
        perror("epoll_create1:");
        abort();
    }
    qpoll->epollfd = epollfd;
    qpoll->max_events = 1;
    qpoll->events = g_new(struct epoll_event, 1);
    qpoll->fds = g_hash_table_new_full(g_int_hash, g_int_equal,
                                       NULL, g_free);
    qemu_poll_init_timerfd(qpoll);
    return qpoll;
}

void qemu_poll_free(QEMUPoll *qpoll)
{
    g_free(qpoll->events);
    g_hash_table_destroy(qpoll->fds);
    close(qpoll->epollfd);
#if USE_TIMERFD
    close(qpoll->timerfd);
#endif
    g_free(qpoll);
}

static int qemu_poll_ppoll(QEMUPoll *qpoll, int64_t timeout_ns)
{
    int r;
    struct pollfd fd = {
        .fd = qpoll->epollfd,
        .events = POLLIN | POLLOUT | POLLERR | POLLHUP,
    };

    if (timeout_ns >= 0) {
        struct timespec ts;
        ts.tv_sec = timeout_ns / 1000000000LL;
        ts.tv_nsec = timeout_ns % 1000000000LL;
        r = ppoll(&fd, 1, &ts, NULL);
    } else {
        r = ppoll(&fd, 1, NULL, NULL);
    }

    if (r > 0) {
        r = epoll_wait(qpoll->epollfd,
                       qpoll->events,
                       qpoll->max_events,
                       0);
        qpoll->nevents = r;
    }
    return r;
}

#if USE_TIMERFD
static int qemu_poll_timerfd(QEMUPoll *qpoll, int64_t timeout_ns)
{
    int r;
    struct itimerspec its = { { 0 } };

    if (timeout_ns > 0) {
        its.it_value.tv_sec = timeout_ns / 1000000000LL;
        its.it_value.tv_nsec = timeout_ns % 1000000000LL;
    }

    /* The timer must be set even when there is no timeout so the readable
     * timerfd is cleared (we never call read(2) on it).
     */
    r = timerfd_settime(qpoll->timerfd, 0, &its, NULL);

    assert(r == 0);
    if (r < 0) {
        return r;
    }
    r = epoll_wait(qpoll->epollfd,
                   qpoll->events,
                   qpoll->max_events,
                   timeout_ns > 0 ? -1 : timeout_ns);
    qpoll->nevents = r;
    return r;
}
#endif

int qemu_poll(QEMUPoll *qpoll, int64_t timeout_ns)
{
#if USE_TIMERFD
    return qemu_poll_timerfd(qpoll, timeout_ns);
#endif
    return qemu_poll_ppoll(qpoll, timeout_ns);
}

static inline uint32_t epoll_event_from_gio_events(int gio_events)
{

    return (gio_events & G_IO_IN  ? EPOLLIN : 0) |
           (gio_events & G_IO_OUT ? EPOLLOUT : 0) |
           (gio_events & G_IO_ERR ? EPOLLERR : 0) |
           (gio_events & G_IO_HUP ? EPOLLHUP : 0);
}


/* Add an fd to poll. Return -EEXIST if fd already registered. */
int qemu_poll_add(QEMUPoll *qpoll, int fd, int gio_events, void *opaque)
{
    int ret;
    struct epoll_event ev;
    QEMUPollEvent *e;

    ev.events = epoll_event_from_gio_events(gio_events);
    ev.data.fd = fd;
    ret = epoll_ctl(qpoll->epollfd, EPOLL_CTL_ADD, fd, &ev);
    if (ret) {
        ret = -errno;
        return ret;
    }
    qpoll->max_events++;
    qpoll->events = g_renew(struct epoll_event,
                            qpoll->events,
                            qpoll->max_events);
    /* Shouldn't get here if the fd is already added since epoll_ctl would
     * return -EEXIST, assert to be sure */
    assert(g_hash_table_lookup(qpoll->fds, &fd) == NULL);
    e = g_new0(QEMUPollEvent, 1);
    e->fd = fd;
    e->events = gio_events;
    e->opaque = opaque;
    g_hash_table_insert(qpoll->fds, &e->fd, e);
    return ret;
}

/* Delete a previously added fd. Return -ENOENT if fd not registered. */
int qemu_poll_del(QEMUPoll *qpoll, int fd)
{
    int ret;

    if (!g_hash_table_lookup(qpoll->fds, &fd)) {
        ret = -ENOENT;
        goto out;
    }
    ret = epoll_ctl(qpoll->epollfd, EPOLL_CTL_DEL, fd, NULL);
    if (ret) {
        ret = -errno;
        goto out;
    }
    qpoll->max_events--;
    qpoll->events = g_renew(struct epoll_event,
                            qpoll->events,
                            qpoll->max_events);
out:
    g_hash_table_remove(qpoll->fds, &fd);
    return ret;
}

static void qemu_poll_copy_fd(gpointer key, gpointer value, gpointer user_data)
{
    GHashTable *dst = user_data;
    QEMUPollEvent *event, *copy;

    event = value;
    copy = g_new(QEMUPollEvent, 1);
    *copy = *event;
    g_hash_table_insert(dst, &copy->fd, copy);
}

static void qemu_poll_del_fd(gpointer key, gpointer value, gpointer user_data)
{
    QEMUPoll *qpoll = user_data;
    QEMUPollEvent *event = value;

    qemu_poll_del(qpoll, event->fd);
}

int qemu_poll_set_fds(QEMUPoll *qpoll, GPollFD *fds, int nfds)
{
    int i;
    int updated = 0;
    int ret = nfds;
    int old_size = g_hash_table_size(qpoll->fds);

    for (i = 0; i < nfds; i++) {
        int r;
        GPollFD *fd = &fds[i];
        QEMUPollEvent *e = g_hash_table_lookup(qpoll->fds, &fd->fd);
        if (e) {
            updated++;
            assert(e->fd == fd->fd);
            if (e->events == fd->events) {
                e->opaque = fd;
                continue;
            }
            r = qemu_poll_del(qpoll, fd->fd);
            if (r < 0) {
                ret = r;
                break;
            }
        }
        r = qemu_poll_add(qpoll, fd->fd, fd->events, fd);
        if (r < 0) {
            ret = r;
            break;
        }
    }

    if (updated < old_size) {
        GHashTable *fds_copy;

        fds_copy = g_hash_table_new_full(g_int_hash, g_int_equal, NULL, g_free);
        g_hash_table_foreach(qpoll->fds, qemu_poll_copy_fd, fds_copy);

        /* Some fds need to be removed, as they are not seen in new fds */
        for (i = 0; i < nfds; i++) {
            GPollFD *fd = &fds[i];
            g_hash_table_remove(fds_copy, &fd->fd);
        }

        g_hash_table_foreach(fds_copy, qemu_poll_del_fd, qpoll);
        g_hash_table_destroy(fds_copy);
    }
    return ret;
}

int qemu_poll_get_events(QEMUPoll *qpoll,
                         QEMUPollEvent *events,
                         int max_events)
{
    int i;
    QEMUPollEvent *p;
    struct epoll_event *ev;
    int fd;

    for (i = 0; i < MIN(qpoll->nevents, max_events); i++) {
        ev = &qpoll->events[i];
        fd = ev->data.fd;
#if USE_TIMERFD
        if (fd == qpoll->timerfd) {
            continue;
        }
#endif
        p = g_hash_table_lookup(qpoll->fds, &fd);
        assert(p);

        events[i].revents = ev->events;
        events[i].opaque = p->opaque;
        events[i].fd = fd;
        events[i].events = p->events;
    }
    return i;
}
