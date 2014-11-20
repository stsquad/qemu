#ifndef QEMU_POLL_H
#define QEMU_POLL_H

#include "qemu/typedefs.h"
#include "qemu-common.h"

struct QEMUPollEvent {
    int fd;
    int events;
    int revents;
    void *opaque;
};

QEMUPoll *qemu_poll_new(void);
void qemu_poll_free(QEMUPoll *qpoll);
int qemu_poll(QEMUPoll *qpoll, int64_t timeout_ns);

/* Add an fd to poll. Return -EEXIST if fd already registered. */
int qemu_poll_add(QEMUPoll *qpoll, int fd, int gio_events, void *opaque);

/* Delete a previously added fd. Return -ENOENT if fd not registered. */
int qemu_poll_del(QEMUPoll *qpoll, int fd);

/**
 * A shortcut to:
 * 1) remove all the existing fds;
 * 2) add all in @fds, while setting each fd's opaque pointer to the pollfd
 * struct itself.
 */
int qemu_poll_set_fds(QEMUPoll *qpoll, GPollFD *fds, int nfds);

/**
 * Query the events from last qemu_poll. ONLY revent and opaque are valid in
 * @events after return.
 */
int qemu_poll_get_events(QEMUPoll *qpoll,
                         QEMUPollEvent *events,
                         int max_events);

#endif
