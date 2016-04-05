#ifndef QEMU_SPINLOCK_H
#define QEMU_SPINLOCK_H

#ifdef _WIN32
#include "spinlock-win32.h"
#else
#include "spinlock-posix.h"
#endif

#endif /* QEMU_SPINLOCK_H */
