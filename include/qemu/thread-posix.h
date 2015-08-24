#ifndef __QEMU_THREAD_POSIX_H
#define __QEMU_THREAD_POSIX_H 1
#include "pthread.h"
#include <semaphore.h>

struct QemuMutex {
    pthread_mutex_t lock;
};

struct QemuSpin {
    pthread_spinlock_t lock;
};

struct QemuCond {
    pthread_cond_t cond;
};

struct QemuSemaphore {
#if defined(__APPLE__) || defined(__NetBSD__)
    pthread_mutex_t lock;
    pthread_cond_t cond;
    unsigned int count;
#else
    sem_t sem;
#endif
};

struct QemuEvent {
#ifndef __linux__
    pthread_mutex_t lock;
    pthread_cond_t cond;
#endif
    unsigned value;
};

struct QemuThread {
    pthread_t thread;
};

void qemu_spin_error_exit(int err, const char *msg);

static inline void qemu_spin_init(QemuSpin *spin)
{
    int err;

    err = pthread_spin_init(&spin->lock, 0);
    if (err) {
        qemu_spin_error_exit(err, __func__);
    }
}

static inline void qemu_spin_destroy(QemuSpin *spin)
{
    int err;

    err = pthread_spin_destroy(&spin->lock);
    if (err) {
        qemu_spin_error_exit(err, __func__);
    }
}

static inline void qemu_spin_lock(QemuSpin *spin)
{
    int err;

    err = pthread_spin_lock(&spin->lock);
    if (err) {
        qemu_spin_error_exit(err, __func__);
    }
}

static inline int qemu_spin_trylock(QemuSpin *spin)
{
    return pthread_spin_trylock(&spin->lock);
}

static inline void qemu_spin_unlock(QemuSpin *spin)
{
    int err;

    err = pthread_spin_unlock(&spin->lock);
    if (err) {
        qemu_spin_error_exit(err, __func__);
    }
}

#endif
