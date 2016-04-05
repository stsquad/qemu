#ifndef QEMU_SPINLOCK_POSIX_H
#define QEMU_SPINLOCK_POSIX_H

#include <qemu/thread.h>
#include <qemu/osdep.h>

typedef pthread_spinlock_t QemuSpinLock;

static inline void qemu_spinlock_error_exit(int err, const char *msg)
{
    fprintf(stderr, "qemu: %s: %s\n", msg, strerror(err));
    abort();
}

static inline void qemu_spinlock_init(QemuSpinLock *lock)
{
    int rc;

    rc = pthread_spin_init(lock, PTHREAD_PROCESS_SHARED);
    if (unlikely(rc)) {
        qemu_spinlock_error_exit(rc, __func__);
    }
}

static inline void qemu_spinlock_destroy(QemuSpinLock *lock)
{
    int rc;

    rc = pthread_spin_destroy(lock);
    if (unlikely(rc)) {
        qemu_spinlock_error_exit(rc, __func__);
    }
}

static inline void qemu_spinlock_lock(QemuSpinLock *lock)
{
    int rc;

    rc = pthread_spin_lock(lock);
    if (unlikely(rc)) {
        qemu_spinlock_error_exit(rc, __func__);
    }
}

static inline int qemu_spinlock_trylock(QemuSpinLock *lock)
{
    return pthread_spin_trylock(lock);
}

static inline void qemu_spinlock_unlock(QemuSpinLock *lock)
{
    int rc;

    rc = pthread_spin_unlock(lock);
    if (unlikely(rc)) {
        qemu_spinlock_error_exit(rc, __func__);
    }
}

#endif /* QEMU_SPINLOCK_POSIX_H */
