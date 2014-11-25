/*
 * QTest testcase for QEMU poll
 *
 * Copyright Red Hat, Inc. 2014
 *
 * Authors:
 *  Fam Zheng <famz@redhat.com>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#include <glib.h>
#include "qemu/poll.h"
#include "qemu/event_notifier.h"

static EventNotifier *poll_add_one(QEMUPoll *qpoll)
{
    EventNotifier *e = g_new(EventNotifier, 1);

    event_notifier_init(e, false);
    qemu_poll_add(qpoll, event_notifier_get_fd(e),
                  G_IO_IN,
                  NULL);
    return e;
}

static int poll_del_one(QEMUPoll *qpoll, EventNotifier *e)
{
    int r = qemu_poll_del(qpoll, event_notifier_get_fd(e));
    event_notifier_cleanup(e);
    g_free(e);
    return r;
}

static void test_poll_single(void)
{
    QEMUPoll *qpoll;
    EventNotifier *e;
    int r, i;

    qpoll = qemu_poll_new();
    g_assert(qpoll);

    e = poll_add_one(qpoll);

    /* Write some data and poll */
    event_notifier_set(e);
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, 1);

    /* Clear data and poll */
    r = event_notifier_test_and_clear(e);
    g_assert(r);
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, 0);

    /* Write a lot of data and poll */
    for (i = 0; i < 10000; i++) {
        event_notifier_set(e);
    }
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, 1);

    /* Clear data and poll again */
    r = event_notifier_test_and_clear(e);
    g_assert(r);
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, 0);

    /* Clean up */
    poll_del_one(qpoll, e);
    qemu_poll_free(qpoll);
}

static void test_poll_multiple(void)
{
    QEMUPoll *qpoll;
    const int N = 32;
    EventNotifier *e[N];
    QEMUPollEvent events[N];
    int r, s, i;

    qpoll = qemu_poll_new();
    g_assert(qpoll);

    for (i = 0; i < N; i++) {
        e[i] = poll_add_one(qpoll);
    }

    /* Write some data for each and poll */
    for (i = 0; i < N; i++) {
        event_notifier_set(e[i]);
    }
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, N);

    /* Clear data and poll */
    for (i = 0; i < N; i++) {
        r = event_notifier_test_and_clear(e[i]);
        g_assert(r);
    }
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, 0);

    /* Write some data for first half and poll */
    for (i = 0; i < N / 2; i++) {
        event_notifier_set(e[i]);
    }
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, N / 2);
    s = qemu_poll_get_events(qpoll, events, N);
    g_assert_cmpint(s, ==, r);
    for (i = 0; i < s; i++) {
        g_assert(events[i].revents & G_IO_IN);
    }

    /* Clean up */
    for (i = 0; i < N; i++) {
        poll_del_one(qpoll, e[i]);
    }
    qemu_poll_free(qpoll);
}

static void test_poll_add_del(void)
{
    QEMUPoll *qpoll;
    EventNotifier *e1, *e2;
    QEMUPollEvent events[2];
    int r;

    qpoll = qemu_poll_new();
    g_assert(qpoll);

    e1 = poll_add_one(qpoll);
    e2 = poll_add_one(qpoll);

    /* Write some data for each and poll */
    event_notifier_set(e1);
    event_notifier_set(e2);
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, 2);

    /* Clear e1 and poll */
    r = event_notifier_test_and_clear(e1);
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, 1);
    r = qemu_poll_get_events(qpoll, events, 2);
    g_assert_cmpint(r, ==, 1);
    g_assert_cmpint(events[0].fd, ==, event_notifier_get_fd(e2));

    /* Write to both but remove one and poll the other */
    event_notifier_set(e1);
    event_notifier_set(e2);
    qemu_poll_del(qpoll, event_notifier_get_fd(e2));
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, 1);
    r = qemu_poll_get_events(qpoll, events, 2);
    g_assert_cmpint(r, ==, 1);
    g_assert_cmpint(events[0].fd, ==, event_notifier_get_fd(e1));

    r = qemu_poll_del(qpoll, event_notifier_get_fd(e2));
    g_assert_cmpint(r, ==, -ENOENT);

    /* Add it back and poll both */
    qemu_poll_add(qpoll, event_notifier_get_fd(e2),
                  G_IO_IN, NULL);
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, 2);

    event_notifier_test_and_clear(e1);
    event_notifier_test_and_clear(e2);

    r = qemu_poll_add(qpoll, event_notifier_get_fd(e2),
                      G_IO_IN, NULL);
    g_assert_cmpint(r, ==, -EEXIST);

    /* Clean up */
    poll_del_one(qpoll, e1);
    poll_del_one(qpoll, e2);
    qemu_poll_free(qpoll);
}

static void gpollfd_fill(GPollFD *fds, int start, EventNotifier **e, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        fds[i + start] = (GPollFD) {
            .fd = event_notifier_get_fd(e[i]),
            .events = G_IO_IN,
        };
    }
}

static void test_poll_set_fds(void)
{
    QEMUPoll *qpoll;
    const int M = 20;
    const int N = 10;
    EventNotifier *em[M], *en[N];
    GPollFD fds[M + N];
    int i, r;

    qpoll = qemu_poll_new();
    g_assert(qpoll);

    for (i = 0; i < M; i++) {
        en[i] = poll_add_one(qpoll);
    }

    for (i = 0; i < M; i++) {
        em[i] = poll_add_one(qpoll);
    }

    gpollfd_fill(fds, 0, em, M);
    gpollfd_fill(fds, M, en, N);

    /* Set N */
    for (i = 0; i < N; i++) {
        event_notifier_set(en[i]);
    }

    /* Poll M + N should get N */
    r = qemu_poll_set_fds(qpoll, fds, M + N);
    g_assert_cmpint(r, ==, M + N);
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, N);

    /* Poll M should get 0 */
    r = qemu_poll_set_fds(qpoll, fds, M);
    g_assert_cmpint(r, ==, M);
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, 0);

    /* Poll N should get N */
    r = qemu_poll_set_fds(qpoll, fds + M, N);
    g_assert_cmpint(r, ==, N);
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, N);

    /* Poll M + N / 2 should get N / 2 */
    r = qemu_poll_set_fds(qpoll, fds, M + N / 2);
    g_assert_cmpint(r, ==, M + N / 2);
    r = qemu_poll(qpoll, 0);
    g_assert_cmpint(r, ==, N / 2);

    /* Clean up */
    for (i = 0; i < M; i++) {
        poll_del_one(qpoll, em[i]);
    }

    for (i = 0; i < N; i++) {
        poll_del_one(qpoll, en[i]);
    }
    qemu_poll_free(qpoll);
}

int main(int argc, char **argv)
{
    int ret;

    g_test_init(&argc, &argv, NULL);

    g_test_add_func("/poll/single", test_poll_single);
    g_test_add_func("/poll/multiple", test_poll_multiple);
    g_test_add_func("/poll/add-del", test_poll_add_del);
    g_test_add_func("/poll/set-fds", test_poll_set_fds);

    ret = g_test_run();

    return ret;
}
