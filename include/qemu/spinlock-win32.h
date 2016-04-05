#ifndef QEMU_SPINLOCK_WIN32_H
#define QEMU_SPINLOCK_WIN32_H

#include <qemu/thread.h>

typedef QemuMutex QemuSpinLock;

static inline void qemu_spinlock_init(QemuSpinLock *lock)
{
    qemu_mutex_init(lock);
}

static inline void qemu_spinlock_destroy(QemuSpinLock *lock)
{
    qemu_mutex_destroy(lock);
}

static inline void qemu_spinlock_lock(QemuSpinLock *lock)
{
    qemu_mutex_lock(lock);
}

static inline int qemu_spinlock_trylock(QemuSpinLock *lock)
{
    return qemu_mutex_trylock(lock);
}

static inline void qemu_spinlock_unlock(QemuSpinLock *lock)
{
    qemu_mutex_unlock(lock);
}

#endif /* QEMU_SPINLOCK_WIN32_H */
