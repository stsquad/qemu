/*
 * Routines common to user and system emulation of load/store.
 *
 *  Copyright (c) 2022 Linaro, Ltd.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#ifdef CONFIG_ATOMIC64
# define HAVE_al8          true
#else
# define HAVE_al8          false
#endif
#define HAVE_al8_fast      (ATOMIC_REG_SIZE >= 8)

/*
 * If __alignof(unsigned __int128) < 16, GCC may refuse to inline atomics
 * that are supported by the host, e.g. s390x.  We can force the pointer to
 * have our known alignment with __builtin_assume_aligned, however prior to
 * GCC 13 that was only reliable with optimization enabled.  See
 *   https://gcc.gnu.org/bugzilla/show_bug.cgi?id=107389
 */
#if defined(CONFIG_ATOMIC128_OPT)
# if !defined(__OPTIMIZE__)
#  define ATTRIBUTE_ATOMIC128_OPT  __attribute__((optimize("O1")))
# endif
# define CONFIG_ATOMIC128
#endif
#ifndef ATTRIBUTE_ATOMIC128_OPT
# define ATTRIBUTE_ATOMIC128_OPT
#endif

#if defined(CONFIG_ATOMIC128)
# define HAVE_al16_fast    true
#elif defined(CONFIG_TCG_INTERPRETER)
/*
 * FIXME: host specific detection this is in tcg/$host/,
 * but we're using tcg/tci/ instead.
 */
# define HAVE_al16_fast    false
#elif defined(__aarch64__)
# define HAVE_al16_fast    likely(have_lse2)
#elif defined(__x86_64__)
# define HAVE_al16_fast    likely(have_atomic16)
#else
# define HAVE_al16_fast    false
#endif
#if defined(CONFIG_ATOMIC128) || defined(CONFIG_CMPXCHG128)
# define HAVE_al16         true
#elif defined(__aarch64__)
# define HAVE_al16         true
#else
# define HAVE_al16         false
#endif


/**
 * required_atomicity:
 *
 * Return the lg2 bytes of atomicity required by @memop for @p.
 * If the operation must be split into two operations to be
 * examined separately for atomicity, return -lg2.
 */
static int required_atomicity(CPUArchState *env, uintptr_t p, MemOp memop)
{
    int atmax = memop & MO_ATMAX_MASK;
    int size = memop & MO_SIZE;
    unsigned tmp;

    if (atmax == MO_ATMAX_SIZE) {
        atmax = size;
    } else {
        atmax >>= MO_ATMAX_SHIFT;
    }

    switch (memop & MO_ATOM_MASK) {
    case MO_ATOM_IFALIGN:
        tmp = (1 << atmax) - 1;
        if (p & tmp) {
            return MO_8;
        }
        break;
    case MO_ATOM_NONE:
        return MO_8;
    case MO_ATOM_SUBALIGN:
        tmp = p & -p;
        if (tmp != 0 && tmp < atmax) {
            atmax = tmp;
        }
        break;
    case MO_ATOM_WITHIN16:
        tmp = p & 15;
        if (tmp + (1 << size) <= 16) {
            atmax = size;
        } else if (atmax == size) {
            return MO_8;
        } else if (tmp + (1 << atmax) != 16) {
            /*
             * Paired load/store, where the pairs aren't aligned.
             * One of the two must still be handled atomically.
             */
            atmax = -atmax;
        }
        break;
    default:
        g_assert_not_reached();
    }

    /*
     * Here we have the architectural atomicity of the operation.
     * However, when executing in a serial context, we need no extra
     * host atomicity in order to avoid racing.  This reduction
     * avoids looping with cpu_loop_exit_atomic.
     */
    if (cpu_in_serial_context(env_cpu(env))) {
        return MO_8;
    }
    return atmax;
}

/**
 * load_atomic2:
 * @pv: host address
 *
 * Atomically load 2 aligned bytes from @pv.
 */
static inline uint16_t load_atomic2(void *pv)
{
    uint16_t *p = __builtin_assume_aligned(pv, 2);
    return qatomic_read(p);
}

/**
 * load_atomic4:
 * @pv: host address
 *
 * Atomically load 4 aligned bytes from @pv.
 */
static inline uint32_t load_atomic4(void *pv)
{
    uint32_t *p = __builtin_assume_aligned(pv, 4);
    return qatomic_read(p);
}

/**
 * load_atomic8:
 * @pv: host address
 *
 * Atomically load 8 aligned bytes from @pv.
 */
static inline uint64_t load_atomic8(void *pv)
{
    uint64_t *p = __builtin_assume_aligned(pv, 8);

    qemu_build_assert(HAVE_al8);
    return qatomic_read__nocheck(p);
}

/**
 * load_atomic16:
 * @pv: host address
 *
 * Atomically load 16 aligned bytes from @pv.
 */
static inline Int128 ATTRIBUTE_ATOMIC128_OPT
load_atomic16(void *pv)
{
#ifdef CONFIG_ATOMIC128
    __uint128_t *p = __builtin_assume_aligned(pv, 16);
    Int128Alias r;

    r.u = qatomic_read__nocheck(p);
    return r.s;
#elif defined(__aarch64__)
    uint64_t l, h;

    /* Via HAVE_al16_fast, FEAT_LSE2 is present: LDP becomes atomic. */
    asm("ldp %0, %1, %2" : "=r"(l), "=r"(h) : "m"(*(__uint128_t *)pv));

    qemu_build_assert(!HOST_BIG_ENDIAN);
    return int128_make128(l, h);
#elif defined(__x86_64__)
    Int128Alias r;

    /* Via HAVE_al16_fast, have_atomic16 is true. */
    asm("vmovdqa %1, %0" : "=x" (r.u) : "m" (*(Int128 *)pv));
    return r.s;
#else
    qemu_build_not_reached();
#endif
}

/**
 * load_atomic8_or_exit:
 * @env: cpu context
 * @ra: host unwind address
 * @pv: host address
 *
 * Atomically load 8 aligned bytes from @pv.
 * If this is not possible, longjmp out to restart serially.
 */
static uint64_t load_atomic8_or_exit(CPUArchState *env, uintptr_t ra, void *pv)
{
    if (HAVE_al8) {
        return load_atomic8(pv);
    }

#ifdef CONFIG_USER_ONLY
    /*
     * If the page is not writable, then assume the value is immutable
     * and requires no locking.  This ignores the case of MAP_SHARED with
     * another process, because the fallback start_exclusive solution
     * provides no protection across processes.
     */
    if (!page_check_range(h2g(pv), 8, PAGE_WRITE)) {
        uint64_t *p = __builtin_assume_aligned(pv, 8);
        return *p;
    }
#endif

    /* Ultimate fallback: re-execute in serial context. */
    cpu_loop_exit_atomic(env_cpu(env), ra);
}

/**
 * load_atomic16_or_exit:
 * @env: cpu context
 * @ra: host unwind address
 * @pv: host address
 *
 * Atomically load 16 aligned bytes from @pv.
 * If this is not possible, longjmp out to restart serially.
 */
static Int128 load_atomic16_or_exit(CPUArchState *env, uintptr_t ra, void *pv)
{
    Int128 *p = __builtin_assume_aligned(pv, 16);

    if (HAVE_al16_fast) {
        return load_atomic16(p);
    }

#ifdef CONFIG_USER_ONLY
    /*
     * We can only use cmpxchg to emulate a load if the page is writable.
     * If the page is not writable, then assume the value is immutable
     * and requires no locking.  This ignores the case of MAP_SHARED with
     * another process, because the fallback start_exclusive solution
     * provides no protection across processes.
     */
    if (!page_check_range(h2g(p), 16, PAGE_WRITE)) {
        return *p;
    }
#endif

    /*
     * In system mode all guest pages are writable, and for user-only
     * we have just checked writability.  Try cmpxchg.
     */
#if defined(__aarch64__)
    /* We can do better than cmpxchg for AArch64.  */
    {
        uint64_t l, h;
        uint32_t fail;

        /* The load must be paired with the store to guarantee not tearing. */
        asm("0: ldxp %0, %1, %3\n\t"
            "stxp %w2, %0, %1, %3\n\t"
            "cbnz %w2, 0b"
            : "=&r"(l), "=&r"(h), "=&r"(fail) : "Q"(*p));

        qemu_build_assert(!HOST_BIG_ENDIAN);
        return int128_make128(l, h);
    }
#elif defined(CONFIG_CMPXCHG128)
    /* Swap 0 with 0, with the side-effect of returning the old value. */
    {
        Int128Alias r;
        r.u = __sync_val_compare_and_swap_16((__uint128_t *)p, 0, 0);
        return r.s;
    }
#endif

    /* Ultimate fallback: re-execute in serial context. */
    cpu_loop_exit_atomic(env_cpu(env), ra);
}

/**
 * load_atom_extract_al4x2:
 * @pv: host address
 *
 * Load 4 bytes from @p, from two sequential atomic 4-byte loads.
 */
static uint32_t load_atom_extract_al4x2(void *pv)
{
    uintptr_t pi = (uintptr_t)pv;
    int sh = (pi & 3) * 8;
    uint32_t a, b;

    pv = (void *)(pi & ~3);
    a = load_atomic4(pv);
    b = load_atomic4(pv + 4);

    if (HOST_BIG_ENDIAN) {
        return (a << sh) | (b >> (-sh & 31));
    } else {
        return (a >> sh) | (b << (-sh & 31));
    }
}

/**
 * load_atom_extract_al8x2:
 * @pv: host address
 *
 * Load 8 bytes from @p, from two sequential atomic 8-byte loads.
 */
static uint64_t load_atom_extract_al8x2(void *pv)
{
    uintptr_t pi = (uintptr_t)pv;
    int sh = (pi & 7) * 8;
    uint64_t a, b;

    pv = (void *)(pi & ~7);
    a = load_atomic8(pv);
    b = load_atomic8(pv + 8);

    if (HOST_BIG_ENDIAN) {
        return (a << sh) | (b >> (-sh & 63));
    } else {
        return (a >> sh) | (b << (-sh & 63));
    }
}

/**
 * load_atom_extract_al8:
 * @pv: host address
 * @s: object size in bytes, @s <= 4.
 *
 * Atomically load @s bytes from @p, when p % s != 0, and [p, p+s-1] does
 * not cross an 8-byte boundary.  This means that we can perform an atomic
 * 8-byte load and extract.
 * The value is returned in the low bits of a uint32_t.
 */
static uint32_t load_atom_extract_al8(void *pv, int s)
{
    uintptr_t pi = (uintptr_t)pv;
    int o = pi & 7;
    int shr = (HOST_BIG_ENDIAN ? 8 - s - o : o) * 8;

    pv = (void *)(pi & ~7);
    return load_atomic8(pv) >> shr;
}

/**
 * load_atom_extract_al16_or_exit:
 * @env: cpu context
 * @ra: host unwind address
 * @p: host address
 * @s: object size in bytes, @s <= 8.
 *
 * Atomically load @s bytes from @p, when p % 16 < 8
 * and p % 16 + s > 8.  I.e. does not cross a 16-byte
 * boundary, but *does* cross an 8-byte boundary.
 * This is the slow version, so we must have eliminated
 * any faster load_atom_extract_al8 case.
 *
 * If this is not possible, longjmp out to restart serially.
 */
static uint64_t load_atom_extract_al16_or_exit(CPUArchState *env, uintptr_t ra,
                                               void *pv, int s)
{
    uintptr_t pi = (uintptr_t)pv;
    int o = pi & 7;
    int shr = (HOST_BIG_ENDIAN ? 16 - s - o : o) * 8;
    Int128 r;

    /*
     * Note constraints above: p & 8 must be clear.
     * Provoke SIGBUS if possible otherwise.
     */
    pv = (void *)(pi & ~7);
    r = load_atomic16_or_exit(env, ra, pv);

    r = int128_urshift(r, shr);
    return int128_getlo(r);
}

/**
 * load_atom_extract_al16_or_al8:
 * @p: host address
 * @s: object size in bytes, @s <= 8.
 *
 * Load @s bytes from @p, when p % s != 0.  If [p, p+s-1] does not
 * cross an 16-byte boundary then the access must be 16-byte atomic,
 * otherwise the access must be 8-byte atomic.
 */
static inline uint64_t ATTRIBUTE_ATOMIC128_OPT
load_atom_extract_al16_or_al8(void *pv, int s)
{
#if defined(CONFIG_ATOMIC128)
    uintptr_t pi = (uintptr_t)pv;
    int o = pi & 7;
    int shr = (HOST_BIG_ENDIAN ? 16 - s - o : o) * 8;
    __uint128_t r;

    pv = (void *)(pi & ~7);
    if (pi & 8) {
        uint64_t *p8 = __builtin_assume_aligned(pv, 16, 8);
        uint64_t a = qatomic_read__nocheck(p8);
        uint64_t b = qatomic_read__nocheck(p8 + 1);

        if (HOST_BIG_ENDIAN) {
            r = ((__uint128_t)a << 64) | b;
        } else {
            r = ((__uint128_t)b << 64) | a;
        }
    } else {
        __uint128_t *p16 = __builtin_assume_aligned(pv, 16, 0);
        r = qatomic_read__nocheck(p16);
    }
    return r >> shr;
#elif defined(__aarch64__)
    /*
     * Via HAVE_al16_fast, FEAT_LSE2 is present.
     * LDP becomes single-copy atomic if 16-byte aligned, and
     * single-copy atomic on the parts if 8-byte aligned.
     */
    uintptr_t pi = (uintptr_t)pv;
    int shr = (pi & 7) * 8;
    uint64_t l, h;

    asm("ldp %0, %1, %2" : "=r"(l), "=r"(h) : "m"(*(__uint128_t *)(pi & ~7)));
    return (l >> shr) | (h << (-shr & 63));
#elif defined(__x86_64__)
    uintptr_t pi = (uintptr_t)pv;
    int shr = (pi & 7) * 8;
    uint64_t a, b;

    /* Via HAVE_al16_fast, have_atomic16 is true. */
    pv = (void *)(pi & ~7);
    if (pi & 8) {
        uint64_t *p8 = __builtin_assume_aligned(pv, 16, 8);
        a = qatomic_read__nocheck(p8);
        b = qatomic_read__nocheck(p8 + 1);
    } else {
        asm("vmovdqa %2, %0\n\tvpextrq $1, %0, %1"
            : "=x"(a), "=r"(b) : "m" (*(__uint128_t *)pv));
    }
    asm("shrd %b2, %1, %0" : "+r"(a) : "r"(b), "c"(shr));

    return a;
#else
    qemu_build_not_reached();
#endif
}

/**
 * load_atom_4_by_2:
 * @pv: host address
 *
 * Load 4 bytes from @pv, with two 2-byte atomic loads.
 */
static inline uint32_t load_atom_4_by_2(void *pv)
{
    uint32_t a = load_atomic2(pv);
    uint32_t b = load_atomic2(pv + 2);

    if (HOST_BIG_ENDIAN) {
        return (a << 16) | b;
    } else {
        return (b << 16) | a;
    }
}

/**
 * load_atom_8_by_2:
 * @pv: host address
 *
 * Load 8 bytes from @pv, with four 2-byte atomic loads.
 */
static inline uint64_t load_atom_8_by_2(void *pv)
{
    uint32_t a = load_atom_4_by_2(pv);
    uint32_t b = load_atom_4_by_2(pv + 4);

    if (HOST_BIG_ENDIAN) {
        return ((uint64_t)a << 32) | b;
    } else {
        return ((uint64_t)b << 32) | a;
    }
}

/**
 * load_atom_8_by_4:
 * @pv: host address
 *
 * Load 8 bytes from @pv, with two 4-byte atomic loads.
 */
static inline uint64_t load_atom_8_by_4(void *pv)
{
    uint32_t a = load_atomic4(pv);
    uint32_t b = load_atomic4(pv + 4);

    if (HOST_BIG_ENDIAN) {
        return ((uint64_t)a << 32) | b;
    } else {
        return ((uint64_t)b << 32) | a;
    }
}

/**
 * load_atom_8_by_8_or_4:
 * @pv: host address
 *
 * Load 8 bytes from aligned @pv, with at least 4-byte atomicity.
 */
static inline uint64_t load_atom_8_by_8_or_4(void *pv)
{
    if (HAVE_al8_fast) {
        return load_atomic8(pv);
    } else {
        return load_atom_8_by_4(pv);
    }
}

/**
 * load_atom_2:
 * @p: host address
 * @memop: the full memory op
 *
 * Load 2 bytes from @p, honoring the atomicity of @memop.
 */
static uint16_t load_atom_2(CPUArchState *env, uintptr_t ra,
                            void *pv, MemOp memop)
{
    uintptr_t pi = (uintptr_t)pv;
    int atmax;

    if (likely((pi & 1) == 0)) {
        return load_atomic2(pv);
    }
    if (HAVE_al16_fast) {
        return load_atom_extract_al16_or_al8(pv, 2);
    }

    atmax = required_atomicity(env, pi, memop);
    switch (atmax) {
    case MO_8:
        return lduw_he_p(pv);
    case MO_16:
        /* The only case remaining is MO_ATOM_WITHIN16. */
        if (!HAVE_al8_fast && (pi & 3) == 1) {
            /* Big or little endian, we want the middle two bytes. */
            return load_atomic4(pv - 1) >> 8;
        }
        if (unlikely((pi & 15) != 7)) {
            return load_atom_extract_al8(pv, 2);
        }
        return load_atom_extract_al16_or_exit(env, ra, pv, 2);
    default:
        g_assert_not_reached();
    }
}

/**
 * load_atom_4:
 * @p: host address
 * @memop: the full memory op
 *
 * Load 4 bytes from @p, honoring the atomicity of @memop.
 */
static uint32_t load_atom_4(CPUArchState *env, uintptr_t ra,
                            void *pv, MemOp memop)
{
    uintptr_t pi = (uintptr_t)pv;
    int atmax;

    if (likely((pi & 3) == 0)) {
        return load_atomic4(pv);
    }
    if (HAVE_al16_fast) {
        return load_atom_extract_al16_or_al8(pv, 4);
    }

    atmax = required_atomicity(env, pi, memop);
    switch (atmax) {
    case MO_8:
    case MO_16:
    case -MO_16:
        /*
         * For MO_ATOM_IFALIGN, this is more atomicity than required,
         * but it's trivially supported on all hosts, better than 4
         * individual byte loads (when the host requires alignment),
         * and overlaps with the MO_ATOM_SUBALIGN case of p % 2 == 0.
         */
        return load_atom_extract_al4x2(pv);
    case MO_32:
        if (!(pi & 4)) {
            return load_atom_extract_al8(pv, 4);
        }
        return load_atom_extract_al16_or_exit(env, ra, pv, 4);
    default:
        g_assert_not_reached();
    }
}

/**
 * load_atom_8:
 * @p: host address
 * @memop: the full memory op
 *
 * Load 8 bytes from @p, honoring the atomicity of @memop.
 */
static uint64_t load_atom_8(CPUArchState *env, uintptr_t ra,
                            void *pv, MemOp memop)
{
    uintptr_t pi = (uintptr_t)pv;
    int atmax;

    /*
     * If the host does not support 8-byte atomics, wait until we have
     * examined the atomicity parameters below.
     */
    if (HAVE_al8 && likely((pi & 7) == 0)) {
        return load_atomic8(pv);
    }
    if (HAVE_al16_fast) {
        return load_atom_extract_al16_or_al8(pv, 8);
    }

    atmax = required_atomicity(env, pi, memop);
    if (atmax == MO_64) {
        if (!HAVE_al8 && (pi & 7) == 0) {
            load_atomic8_or_exit(env, ra, pv);
        }
        return load_atom_extract_al16_or_exit(env, ra, pv, 8);
    }
    if (HAVE_al8_fast) {
        return load_atom_extract_al8x2(pv);
    }
    switch (atmax) {
    case MO_8:
        return ldq_he_p(pv);
    case MO_16:
        return load_atom_8_by_2(pv);
    case MO_32:
        return load_atom_8_by_4(pv);
    case -MO_32:
        if (HAVE_al8) {
            return load_atom_extract_al8x2(pv);
        }
        cpu_loop_exit_atomic(env_cpu(env), ra);
    default:
        g_assert_not_reached();
    }
}

/**
 * load_atom_16:
 * @p: host address
 * @memop: the full memory op
 *
 * Load 16 bytes from @p, honoring the atomicity of @memop.
 */
static Int128 load_atom_16(CPUArchState *env, uintptr_t ra,
                           void *pv, MemOp memop)
{
    uintptr_t pi = (uintptr_t)pv;
    int atmax;
    Int128 r;
    uint64_t a, b;

    /*
     * If the host does not support 8-byte atomics, wait until we have
     * examined the atomicity parameters below.
     */
    if (HAVE_al16_fast && likely((pi & 15) == 0)) {
        return load_atomic16(pv);
    }

    atmax = required_atomicity(env, pi, memop);
    switch (atmax) {
    case MO_8:
        memcpy(&r, pv, 16);
        return r;
    case MO_16:
        a = load_atom_8_by_2(pv);
        b = load_atom_8_by_2(pv + 8);
        break;
    case MO_32:
        a = load_atom_8_by_4(pv);
        b = load_atom_8_by_4(pv + 8);
        break;
    case MO_64:
        if (!HAVE_al8) {
            cpu_loop_exit_atomic(env_cpu(env), ra);
        }
        a = load_atomic8(pv);
        b = load_atomic8(pv + 8);
        break;
    case -MO_64:
        if (!HAVE_al8) {
            cpu_loop_exit_atomic(env_cpu(env), ra);
        }
        a = load_atom_extract_al8x2(pv);
        b = load_atom_extract_al8x2(pv + 8);
        break;
    case MO_128:
        return load_atomic16_or_exit(env, ra, pv);
    default:
        g_assert_not_reached();
    }
    return int128_make128(HOST_BIG_ENDIAN ? b : a, HOST_BIG_ENDIAN ? a : b);
}

/**
 * store_atomic2:
 * @pv: host address
 * @val: value to store
 *
 * Atomically store 2 aligned bytes to @pv.
 */
static inline void store_atomic2(void *pv, uint16_t val)
{
    uint16_t *p = __builtin_assume_aligned(pv, 2);
    qatomic_set(p, val);
}

/**
 * store_atomic4:
 * @pv: host address
 * @val: value to store
 *
 * Atomically store 4 aligned bytes to @pv.
 */
static inline void store_atomic4(void *pv, uint32_t val)
{
    uint32_t *p = __builtin_assume_aligned(pv, 4);
    qatomic_set(p, val);
}

/**
 * store_atomic8:
 * @pv: host address
 * @val: value to store
 *
 * Atomically store 8 aligned bytes to @pv.
 */
static inline void store_atomic8(void *pv, uint64_t val)
{
    uint64_t *p = __builtin_assume_aligned(pv, 8);

    qemu_build_assert(HAVE_al8);
    qatomic_set__nocheck(p, val);
}

/**
 * store_atomic16:
 * @pv: host address
 * @val: value to store
 *
 * Atomically store 16 aligned bytes to @pv.
 */
static inline void ATTRIBUTE_ATOMIC128_OPT
store_atomic16(void *pv, Int128Alias val)
{
#if defined(CONFIG_ATOMIC128)
    {
        __uint128_t *pu = __builtin_assume_aligned(pv, 16);
        qatomic_set__nocheck(pu, val.u);
        return;
    }
#endif
#if defined(__x86_64__)
    if (HAVE_al16_fast) {
        asm("vmovdqa %1, %0" : "=m"(*(__uint128_t *)pv) : "x" (val.u));
        return;
    }
#endif
#if defined(__aarch64__)
    /* We can do better than cmpxchg for AArch64.  */
    {
        uint64_t l, h, t;

        qemu_build_assert(!HOST_BIG_ENDIAN);
        l = int128_getlo(val.s);
        h = int128_gethi(val.s);

        if (HAVE_al16_fast) {
            /* Via HAVE_al16_fast, FEAT_LSE2 is present: STP becomes atomic. */
            asm("stp %1, %2, %0" : "=Q"(*(__uint128_t *)pv) : "r"(l), "r"(h));
        } else {
            asm("0: ldxp %0, xzr, %1\n\t"
                "stxp %w0, %2, %3, %1\n\t"
                "cbnz %w0, 0b"
                : "=&r"(t), "=Q"(*(__uint128_t *)pv) : "r"(l), "r"(h));
        }
        return;
    }
#elif defined(CONFIG_CMPXCHG128)
    {
        __uint128_t *pu = __builtin_assume_aligned(pv, 16);
        __uint128_t o;

        /*
         * Without CONFIG_ATOMIC128, __atomic_compare_exchange_n will always
         * defer to libatomic, so we must use __sync_val_compare_and_swap_16
         * and accept the sequential consistency that comes with it.
         */
        do {
            o = *pu;
        } while (!__sync_bool_compare_and_swap_16(pu, o, val.u));
        return;
    }
#endif
    qemu_build_not_reached();
}

/**
 * store_atom_4x2
 */
static inline void store_atom_4_by_2(void *pv, uint32_t val)
{
    store_atomic2(pv, val >> (HOST_BIG_ENDIAN ? 16 : 0));
    store_atomic2(pv + 2, val >> (HOST_BIG_ENDIAN ? 0 : 16));
}

/**
 * store_atom_8_by_2
 */
static inline void store_atom_8_by_2(void *pv, uint64_t val)
{
    store_atom_4_by_2(pv, val >> (HOST_BIG_ENDIAN ? 32 : 0));
    store_atom_4_by_2(pv + 4, val >> (HOST_BIG_ENDIAN ? 0 : 32));
}

/**
 * store_atom_8_by_4
 */
static inline void store_atom_8_by_4(void *pv, uint64_t val)
{
    store_atomic4(pv, val >> (HOST_BIG_ENDIAN ? 32 : 0));
    store_atomic4(pv + 4, val >> (HOST_BIG_ENDIAN ? 0 : 32));
}

/**
 * store_atom_insert_al4:
 * @p: host address
 * @val: shifted value to store
 * @msk: mask for value to store
 *
 * Atomically store @val to @p, masked by @msk.
 */
static void store_atom_insert_al4(uint32_t *p, uint32_t val, uint32_t msk)
{
    uint32_t old, new;

    p = __builtin_assume_aligned(p, 4);
    old = qatomic_read(p);
    do {
        new = (old & ~msk) | val;
    } while (!__atomic_compare_exchange_n(p, &old, new, true,
                                          __ATOMIC_RELAXED, __ATOMIC_RELAXED));
}

/**
 * store_atom_insert_al8:
 * @p: host address
 * @val: shifted value to store
 * @msk: mask for value to store
 *
 * Atomically store @val to @p masked by @msk.
 */
static void store_atom_insert_al8(uint64_t *p, uint64_t val, uint64_t msk)
{
    uint64_t old, new;

    qemu_build_assert(HAVE_al8);
    p = __builtin_assume_aligned(p, 8);
    old = qatomic_read__nocheck(p);
    do {
        new = (old & ~msk) | val;
    } while (!__atomic_compare_exchange_n(p, &old, new, true,
                                          __ATOMIC_RELAXED, __ATOMIC_RELAXED));
}

/**
 * store_atom_insert_al16:
 * @p: host address
 * @val: shifted value to store
 * @msk: mask for value to store
 *
 * Atomically store @val to @p masked by @msk.
 */
static void ATTRIBUTE_ATOMIC128_OPT
store_atom_insert_al16(Int128 *ps, Int128Alias val, Int128Alias msk)
{
#if defined(__aarch64__)
    /*
     * GCC only implements __sync* primitives for int128 on aarch64.
     * We can do better without the barriers, and integrating the
     * arithmetic into the load-exclusive/store-conditional pair.
     */
    uint64_t tl, th, vl, vh, ml, mh;
    uint32_t fail;

    qemu_build_assert(!HOST_BIG_ENDIAN);
    vl = int128_getlo(val.s);
    vh = int128_gethi(val.s);
    ml = int128_getlo(msk.s);
    mh = int128_gethi(msk.s);

    asm("0: ldxp %[l], %[h], %[mem]\n\t"
        "bic %[l], %[l], %[ml]\n\t"
        "bic %[h], %[h], %[mh]\n\t"
        "orr %[l], %[l], %[vl]\n\t"
        "orr %[h], %[h], %[vh]\n\t"
        "stxp %w[f], %[l], %[h], %[mem]\n\t"
        "cbnz %w[f], 0b\n"
        : [mem] "+Q"(*ps), [f] "=&r"(fail), [l] "=&r"(tl), [h] "=&r"(th)
        : [vl] "r"(vl), [vh] "r"(vh), [ml] "r"(ml), [mh] "r"(mh));
#elif defined(CONFIG_ATOMIC128)
    __uint128_t *pu, old, new;

    /* With CONFIG_ATOMIC128, we can avoid the memory barriers. */
    pu = __builtin_assume_aligned(ps, 16);
    old = *pu;
    do {
        new = (old & ~msk.u) | val.u;
    } while (!__atomic_compare_exchange_n(pu, &old, new, true,
                                          __ATOMIC_RELAXED, __ATOMIC_RELAXED));
#elif defined(CONFIG_CMPXCHG128)
    __uint128_t *pu, old, new;

    /*
     * Without CONFIG_ATOMIC128, __atomic_compare_exchange_n will always
     * defer to libatomic, so we must use __sync_val_compare_and_swap_16
     * and accept the sequential consistency that comes with it.
     */
    pu = __builtin_assume_aligned(ps, 16);
    do {
        old = *pu;
        new = (old & ~msk.u) | val.u;
    } while (!__sync_bool_compare_and_swap_16(pu, old, new));
#else
    qemu_build_not_reached();
#endif
}

/**
 * store_bytes_leN:
 * @pv: host address
 * @size: number of bytes to store
 * @val_le: data to store
 *
 * Store @size bytes at @p.  The bytes to store are extracted in little-endian order
 * from @val_le; return the bytes of @val_le beyond @size that have not been stored.
 */
static uint64_t store_bytes_leN(void *pv, int size, uint64_t val_le)
{
    uint8_t *p = pv;
    for (int i = 0; i < size; i++, val_le >>= 8) {
        p[i] = val_le;
    }
    return val_le;
}

/**
 * store_parts_leN
 * @pv: host address
 * @size: number of bytes to store
 * @val_le: data to store
 *
 * As store_bytes_leN, but atomically on each aligned part.
 */
G_GNUC_UNUSED
static uint64_t store_parts_leN(void *pv, int size, uint64_t val_le)
{
    do {
        int n;

        /* Find minimum of alignment and size */
        switch (((uintptr_t)pv | size) & 7) {
        case 4:
            store_atomic4(pv, le32_to_cpu(val_le));
            val_le >>= 32;
            n = 4;
            break;
        case 2:
        case 6:
            store_atomic2(pv, le16_to_cpu(val_le));
            val_le >>= 16;
            n = 2;
            break;
        default:
            *(uint8_t *)pv = val_le;
            val_le >>= 8;
            n = 1;
            break;
        case 0:
            g_assert_not_reached();
        }
        pv += n;
        size -= n;
    } while (size != 0);

    return val_le;
}

/**
 * store_whole_le4
 * @pv: host address
 * @size: number of bytes to store
 * @val_le: data to store
 *
 * As store_bytes_leN, but atomically as a whole.
 * Four aligned bytes are guaranteed to cover the store.
 */
static uint64_t store_whole_le4(void *pv, int size, uint64_t val_le)
{
    int sz = size * 8;
    int o = (uintptr_t)pv & 3;
    int sh = o * 8;
    uint32_t m = MAKE_64BIT_MASK(0, sz);
    uint32_t v;

    if (HOST_BIG_ENDIAN) {
        v = bswap32(val_le) >> sh;
        m = bswap32(m) >> sh;
    } else {
        v = val_le << sh;
        m <<= sh;
    }
    store_atom_insert_al4(pv - o, v, m);
    return val_le >> sz;
}

/**
 * store_whole_le8
 * @pv: host address
 * @size: number of bytes to store
 * @val_le: data to store
 *
 * As store_bytes_leN, but atomically as a whole.
 * Eight aligned bytes are guaranteed to cover the store.
 */
static uint64_t store_whole_le8(void *pv, int size, uint64_t val_le)
{
    int sz = size * 8;
    int o = (uintptr_t)pv & 7;
    int sh = o * 8;
    uint64_t m = MAKE_64BIT_MASK(0, sz);
    uint64_t v;

    qemu_build_assert(HAVE_al8);
    if (HOST_BIG_ENDIAN) {
        v = bswap64(val_le) >> sh;
        m = bswap64(m) >> sh;
    } else {
        v = val_le << sh;
        m <<= sh;
    }
    store_atom_insert_al8(pv - o, v, m);
    return val_le >> sz;
}

/**
 * store_whole_le16
 * @pv: host address
 * @size: number of bytes to store
 * @val_le: data to store
 *
 * As store_bytes_leN, but atomically as a whole.
 * 16 aligned bytes are guaranteed to cover the store.
 */
static uint64_t store_whole_le16(void *pv, int size, Int128 val_le)
{
    int sz = size * 8;
    int o = (uintptr_t)pv & 15;
    int sh = o * 8;
    Int128 m, v;

    qemu_build_assert(HAVE_al16);

    /* Like MAKE_64BIT_MASK(0, sz), but larger. */
    if (sz <= 64) {
        m = int128_make64(MAKE_64BIT_MASK(0, sz));
    } else {
        m = int128_make128(-1, MAKE_64BIT_MASK(0, sz - 64));
    }

    if (HOST_BIG_ENDIAN) {
        v = int128_urshift(bswap128(val_le), sh);
        m = int128_urshift(bswap128(m), sh);
    } else {
        v = int128_lshift(val_le, sh);
        m = int128_lshift(m, sh);
    }
    store_atom_insert_al16(pv - o, v, m);

    /* Unused if sz <= 64. */
    return int128_gethi(val_le) >> (sz - 64);
}

/**
 * store_atom_2:
 * @p: host address
 * @val: the value to store
 * @memop: the full memory op
 *
 * Store 2 bytes to @p, honoring the atomicity of @memop.
 */
static void store_atom_2(CPUArchState *env, uintptr_t ra,
                         void *pv, MemOp memop, uint16_t val)
{
    uintptr_t pi = (uintptr_t)pv;
    int atmax;

    if (likely((pi & 1) == 0)) {
        store_atomic2(pv, val);
        return;
    }

    atmax = required_atomicity(env, pi, memop);
    if (atmax == MO_8) {
        stw_he_p(pv, val);
        return;
    }

    /*
     * The only case remaining is MO_ATOM_WITHIN16.
     * Big or little endian, we want the middle two bytes in each test.
     */
    if ((pi & 3) == 1) {
        store_atom_insert_al4(pv - 1, (uint32_t)val << 8, MAKE_64BIT_MASK(8, 16));
        return;
    } else if ((pi & 7) == 3) {
        if (HAVE_al8) {
            store_atom_insert_al8(pv - 3, (uint64_t)val << 24, MAKE_64BIT_MASK(24, 16));
            return;
        }
    } else if ((pi & 15) == 7) {
        if (HAVE_al16) {
            Int128 v = int128_lshift(int128_make64(val), 56);
            Int128 m = int128_lshift(int128_make64(0xffff), 56);
            store_atom_insert_al16(pv - 7, v, m);
            return;
        }
    } else {
        g_assert_not_reached();
    }

    cpu_loop_exit_atomic(env_cpu(env), ra);
}

/**
 * store_atom_4:
 * @p: host address
 * @val: the value to store
 * @memop: the full memory op
 *
 * Store 4 bytes to @p, honoring the atomicity of @memop.
 */
static void store_atom_4(CPUArchState *env, uintptr_t ra,
                         void *pv, MemOp memop, uint32_t val)
{
    uintptr_t pi = (uintptr_t)pv;
    int atmax;

    if (likely((pi & 3) == 0)) {
        store_atomic4(pv, val);
        return;
    }

    atmax = required_atomicity(env, pi, memop);
    switch (atmax) {
    case MO_8:
        stl_he_p(pv, val);
        return;
    case MO_16:
        store_atom_4_by_2(pv, val);
        return;
    case -MO_16:
        {
            uint32_t val_le = cpu_to_le32(val);
            int s2 = pi & 3;
            int s1 = 4 - s2;

            switch (s2) {
            case 1:
                val_le = store_whole_le4(pv, s1, val_le);
                *(uint8_t *)(pv + 3) = val_le;
                break;
            case 3:
                *(uint8_t *)pv = val_le;
                store_whole_le4(pv + 1, s2, val_le >> 8);
                break;
            case 0: /* aligned */
            case 2: /* atmax MO_16 */
            default:
                g_assert_not_reached();
            }
        }
        return;
    case MO_32:
        if ((pi & 7) < 4) {
            if (HAVE_al8) {
                store_whole_le8(pv, 4, cpu_to_le32(val));
                return;
            }
        } else {
            if (HAVE_al16) {
                store_whole_le16(pv, 4, int128_make64(cpu_to_le32(val)));
                return;
            }
        }
        cpu_loop_exit_atomic(env_cpu(env), ra);
    default:
        g_assert_not_reached();
    }
}

/**
 * store_atom_8:
 * @p: host address
 * @val: the value to store
 * @memop: the full memory op
 *
 * Store 8 bytes to @p, honoring the atomicity of @memop.
 */
static void store_atom_8(CPUArchState *env, uintptr_t ra,
                         void *pv, MemOp memop, uint64_t val)
{
    uintptr_t pi = (uintptr_t)pv;
    int atmax;

    if (HAVE_al8 && likely((pi & 7) == 0)) {
        store_atomic8(pv, val);
        return;
    }

    atmax = required_atomicity(env, pi, memop);
    switch (atmax) {
    case MO_8:
        stq_he_p(pv, val);
        return;
    case MO_16:
        store_atom_8_by_2(pv, val);
        return;
    case MO_32:
        store_atom_8_by_4(pv, val);
        return;
    case -MO_32:
        if (HAVE_al8) {
            uint64_t val_le = cpu_to_le64(val);
            int s2 = pi & 7;
            int s1 = 8 - s2;

            switch (s2) {
            case 1 ... 3:
                val_le = store_whole_le8(pv, s1, val_le);
                store_bytes_leN(pv + s1, s2, val_le);
                break;
            case 5 ... 7:
                val_le = store_bytes_leN(pv, s1, val_le);
                store_whole_le8(pv + s1, s2, val_le);
                break;
            case 0: /* aligned */
            case 4: /* atmax MO_32 */
            default:
                g_assert_not_reached();
            }
            return;
        }
        break;
    case MO_64:
        if (HAVE_al16) {
            store_whole_le16(pv, 8, int128_make64(cpu_to_le64(val)));
            return;
        }
        break;
    default:
        g_assert_not_reached();
    }
    cpu_loop_exit_atomic(env_cpu(env), ra);
}

/**
 * store_atom_16:
 * @p: host address
 * @val: the value to store
 * @memop: the full memory op
 *
 * Store 16 bytes to @p, honoring the atomicity of @memop.
 */
static void store_atom_16(CPUArchState *env, uintptr_t ra,
                          void *pv, MemOp memop, Int128 val)
{
    uintptr_t pi = (uintptr_t)pv;
    uint64_t a, b;
    int atmax;

    if (HAVE_al16_fast && likely((pi & 15) == 0)) {
        store_atomic16(pv, val);
        return;
    }

    atmax = required_atomicity(env, pi, memop);

    a = HOST_BIG_ENDIAN ? int128_gethi(val) : int128_getlo(val);
    b = HOST_BIG_ENDIAN ? int128_getlo(val) : int128_gethi(val);
    switch (atmax) {
    case MO_8:
        memcpy(pv, &val, 16);
        return;
    case MO_16:
        store_atom_8_by_2(pv, a);
        store_atom_8_by_2(pv + 8, b);
        return;
    case MO_32:
        store_atom_8_by_4(pv, a);
        store_atom_8_by_4(pv + 8, b);
        return;
    case MO_64:
        if (HAVE_al8) {
            store_atomic8(pv, a);
            store_atomic8(pv + 8, b);
            return;
        }
        break;
    case -MO_64:
        if (HAVE_al16) {
            uint64_t val_le;
            int s2 = pi & 15;
            int s1 = 16 - s2;

            if (HOST_BIG_ENDIAN) {
                val = bswap128(val);
            }
            switch (s2) {
            case 1 ... 7:
                val_le = store_whole_le16(pv, s1, val);
                store_bytes_leN(pv + s1, s2, val_le);
                break;
            case 9 ... 15:
                store_bytes_leN(pv, s1, int128_getlo(val));
                val = int128_urshift(val, s1 * 8);
                store_whole_le16(pv + s1, s2, val);
                break;
            case 0: /* aligned */
            case 8: /* atmax MO_64 */
            default:
                g_assert_not_reached();
            }
            return;
        }
        break;
    case MO_128:
        if (HAVE_al16) {
            store_atomic16(pv, val);
            return;
        }
        break;
    default:
        g_assert_not_reached();
    }
    cpu_loop_exit_atomic(env_cpu(env), ra);
}