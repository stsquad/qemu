/*
 * Copyright (C) 2016, Emilio G. Cota <cota@braap.org>
 *
 * Host architectural helper functions
 *
 * License: GNU GPL, version 2.
 *   See the COPYING file in the top-level directory.
 */
#ifndef QEMU_PROCESSOR_H
#define QEMU_PROCESSOR_H

#include "qemu/atomic.h"

#if defined(__i386__) || defined(__x86_64__)
# define cpu_relax() asm volatile("rep; nop" ::: "memory")

static inline void flush_icache_range(uintptr_t start, uintptr_t stop)
{
}

#elif defined(__aarch64__)
# define cpu_relax() asm volatile("yield" ::: "memory")

static inline void flush_icache_range(uintptr_t start, uintptr_t stop)
{
    __builtin___clear_cache((char *) start, (char *) stop);
}

#elif defined(__arm__)
# define cpu_relax() barrier()

static inline void flush_icache_range(uintptr_t start, uintptr_t stop)
{
    __builtin___clear_cache((char *) start, (char *) stop);
}

#elif defined(__powerpc64__)
/* set Hardware Multi-Threading (HMT) priority to low; then back to medium */
# define cpu_relax() asm volatile("or 1, 1, 1;" \
                                  "or 2, 2, 2;" ::: "memory")

void flush_icache_range(uintptr_t start, uintptr_t stop)
{
    uintptr_t p, start1, stop1;
    size_t dsize = qemu_dcache_linesize;
    size_t isize = qemu_icache_linesize;

    start1 = start & ~(dsize - 1);
    stop1 = (stop + dsize - 1) & ~(dsize - 1);
    for (p = start1; p < stop1; p += dsize) {
        asm volatile ("dcbst 0,%0" : : "r"(p) : "memory");
    }
    asm volatile ("sync" : : : "memory");

    start &= start & ~(isize - 1);
    stop1 = (stop + isize - 1) & ~(isize - 1);
    for (p = start1; p < stop1; p += isize) {
        asm volatile ("icbi 0,%0" : : "r"(p) : "memory");
    }
    asm volatile ("sync" : : : "memory");
    asm volatile ("isync" : : : "memory");
}

#elif defined(__mips__)
# define cpu_relax() barrier()

#ifdef __OpenBSD__
#include <machine/sysarch.h>
#else
#include <sys/cachectl.h>
#endif

static inline void flush_icache_range(uintptr_t start, uintptr_t stop)
{
    cacheflush ((void *)start, stop-start, ICACHE);
}

#elif defined(__riscv)

static inline void flush_icache_range(uintptr_t start, uintptr_t stop)
{
    __builtin___clear_cache((char *)start, (char *)stop);
}

#elif defined(__sparc__)

static inline void flush_icache_range(uintptr_t start, uintptr_t stop)
{
    uintptr_t p;
    for (p = start & -8; p < ((stop + 7) & -8); p += 8) {
        __asm__ __volatile__("flush\t%0" : : "r" (p));
    }
}

#else
# define cpu_relax() barrier()

static inline void flush_icache_range(uintptr_t start, uintptr_t stop)
{
}
#endif

#endif /* QEMU_PROCESSOR_H */
