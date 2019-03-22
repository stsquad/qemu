/*
 * Simple LSE atomic tests
 */

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/syscall.h>

#define VALUES  8192

int32_t  sdata_32[VALUES];
int64_t  sdata_64[VALUES];
uint32_t udata_32[VALUES];
uint64_t udata_64[VALUES];


uint64_t seed;

int32_t int32_val(int index)
{
    int32_t value = index ^ seed;
    return index % 2 == 0 ? value : -value;
}

uint32_t uint32_val(int index)
{
    return index ^ seed;
}

int64_t int64_val(int index)
{
    int64_t value = index ^ seed;
    return index % 2 == 0 ? value : -value;
}


uint64_t uint64_val(int index)
{
    return index ^ seed;
}

void reset_data(uint64_t new_seed)
{
    int i;
    seed = new_seed;

    for (i = 0; i < VALUES; i++) {
        sdata_32[i] = int32_val(i);
        sdata_64[i] = int64_val(i);
        udata_32[i] = uint32_val(i);
        udata_64[i] = uint64_val(i);
    }
}

int do_cmpxchg_s32(int32_t *ptr, int32_t swapval)
{
    int i;

    printf("%s: with swapval = %x\n", __func__, swapval);

    for (i = 0; i < VALUES; i++) {
        int32_t *ptr = &sdata_32[i];
        int32_t check = int32_val(i);
        int32_t expected = int32_val(i);

        __atomic_compare_exchange(ptr, &expected, &swapval, true,
                                  __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
        if (expected != check) {
            printf("%s: %" PRId32 " != %" PRId32 " and current = %" PRId32 "\n",
                   __func__, expected, check, sdata_32[i]);
            return -1;
        }
    }

    for (i = 0; i < VALUES; i++) {
        if (sdata_32[i] != swapval) {
            printf("%s: data[i]=%" PRIx32 " != %" PRIx32 "\n", __func__, sdata_32[i], swapval);
            return -1;
        }
    }

    return 0;
}

int do_cmpxchg_u32(uint32_t *ptr, uint32_t swapval)
{
    int i;

    printf("%s: with swapval = %x\n", __func__, swapval);

    for (i = 0; i < VALUES; i++) {
        uint32_t *ptr = &udata_32[i];
        uint32_t check = uint32_val(i);
        uint32_t expected = uint32_val(i);

        __atomic_compare_exchange(ptr, &expected, &swapval, true,
                                  __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
        if (expected != check) {
            printf("%s: %" PRIu32 " != %" PRIu32 " and current = %" PRIu32 "\n",
                   __func__, expected, check, udata_32[i]);
            return -1;
        }
    }

    for (i = 0; i < VALUES; i++) {
        if (udata_32[i] != swapval) {
            printf("%s: data[i]=%" PRIx32 " != %" PRIx32 "\n", __func__, udata_32[i], swapval);
            return -1;
        }
    }

    return 0;
}

int do_cmpxchg_s64(int64_t *ptr, int64_t swapval)
{
    int i;

    printf("%s: with swapval = %lx\n", __func__, swapval);

    for (i = 0; i < VALUES; i++) {
        int64_t *ptr = &sdata_64[i];
        int64_t check = int64_val(i);
        int64_t expected = int64_val(i);

        __atomic_compare_exchange(ptr, &expected, &swapval, true,
                                  __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
        if (expected != check) {
            printf("%s: %" PRId64 " != %" PRId64 " and current = %" PRId64 "\n",
                   __func__, expected, check, sdata_64[i]);
            return -1;
        }
    }

    for (i = 0; i < VALUES; i++) {
        if (sdata_64[i] != swapval) {
            printf("%s: data[i]=%" PRIx64 " != %" PRIx64 "\n", __func__, sdata_64[i], swapval);
            return -1;
        }
    }

    return 0;
}

int do_cmpxchg_u64(uint64_t *ptr, uint64_t swapval)
{
    int i;

    printf("%s: with swapval = %lx\n", __func__, swapval);

    for (i = 0; i < VALUES; i++) {
        uint64_t *ptr = &udata_64[i];
        uint64_t check = uint64_val(i);
        uint64_t expected = uint64_val(i);

        __atomic_compare_exchange(ptr, &expected, &swapval, true,
                                  __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
        if (expected != check) {
            printf("%s: %" PRIu64 " != %" PRIu64" and current = %" PRIu64 "\n",
                   __func__, expected, check, udata_64[i]);
            return -1;
        }
    }

    for (i = 0; i < VALUES; i++) {
        if (udata_64[i] != swapval) {
            printf("%s: data[i]=%" PRIx64 " != %" PRIx64 "\n", __func__, udata_64[i], swapval);
            return -1;
        }
    }

    return 0;
}

int do_cmpxchg_u128(uint64_t *ptr, uint64_t swapin)
{
    int i;
    __uint128_t swapval = ((__uint128_t) swapin << 64) | swapin;

    printf("%s: with swapval = %lx:%lx\n", __func__, swapin, swapin);

    for (i = 0; i < VALUES; i = i + 2) {
        __uint128_t *ptr = (__uint128_t *) &udata_64[i];
        __uint128_t check = ((__uint128_t)uint64_val(i) << 64) | uint64_val(i+1);
        __uint128_t expected = check;

        __atomic_compare_exchange(ptr, &expected, &swapval, true,
                                  __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
        if (expected != check) {
            printf("%s: expected != check and current = %" PRIx64 ":%" PRIx64"\n",
                   __func__, udata_64[i], udata_64[i+1]);
            return -1;
        }
    }

    for (i = 0; i < VALUES; i++) {
        if (udata_64[i] != swapin) {
            printf("%s: data[i]=%" PRIx64 " != %" PRIx64 "\n", __func__, udata_64[i], swapin);
            return -1;
        }
        if (udata_64[i+1] != swapin) {
            printf("%s: data[i]=%" PRIx64 " != %" PRIx64 "\n", __func__, udata_64[i+1], swapin);
            return -1;
        }
    }

    return 0;
}


int do_swap_u32(uint32_t *ptr, uint32_t swapval)
{
    int i;

    printf("%s: with swapval = %x\n", __func__, swapval);

    for (i = 0; i < VALUES; i++) {
        uint32_t *ptr = &udata_32[i];
        uint32_t expected = uint32_val(i);
        uint32_t ret;

        __atomic_exchange(ptr, &swapval, &ret, __ATOMIC_SEQ_CST);

        if (ret != expected) {
            printf("%s: %" PRIu32 " != %" PRIu32 " and current = %" PRIu32 "\n",
                   __func__, ret, expected, udata_32[i]);
            return -1;
        }
    }

    for (i = 0; i < VALUES; i++) {
        if (udata_32[i] != swapval) {
            printf("%s: data[i]=%" PRIx32 " != %" PRIx32 "\n", __func__, udata_32[i], swapval);
            return -1;
        }
    }

    return 0;
}

int do_swap_u64(uint64_t *ptr, uint64_t swapval)
{
    int i;

    printf("%s: with swapval = %lx\n", __func__, swapval);

    for (i = 0; i < VALUES; i++) {
        uint64_t *ptr = &udata_64[i];
        uint64_t expected = uint64_val(i);
        uint64_t ret;

        __atomic_exchange(ptr, &swapval, &ret, __ATOMIC_SEQ_CST);

        if (ret != expected) {
            printf("%s: %" PRIu64 " != %" PRIu64 " and current = %" PRIu64 "\n",
                   __func__, ret, expected, udata_64[i]);
            return -1;
        }
    }

    for (i = 0; i < VALUES; i++) {
        if (udata_64[i] != swapval) {
            printf("%s: data[i]=%" PRIx64 " != %" PRIx64 "\n", __func__, udata_64[i], swapval);
            return -1;
        }
    }

    return 0;
}

int do_sub_u32(uint32_t *ptr, uint32_t subval)
{
    int i;

    printf("%s: with subval = %x\n", __func__, subval);

    for (i = 0; i < VALUES; i++) {
        uint32_t *ptr = &udata_32[i];
        uint32_t expected = uint32_val(i) - subval;
        uint32_t ret;

        ret = __atomic_sub_fetch(ptr, subval, __ATOMIC_SEQ_CST);

        if (ret != expected) {
            printf("%s: %" PRIu32 " != %" PRIu32 " and current = %" PRIu32 "\n",
                   __func__, ret, expected, udata_32[i]);
            return -1;
        }
    }

    return 0;
}

int do_sub_u64(uint64_t *ptr, uint64_t subval)
{
    int i;

    printf("%s: with subval = %lx\n", __func__, subval);

    for (i = 0; i < VALUES; i++) {
        uint64_t *ptr = &udata_64[i];
        uint64_t expected = uint64_val(i) - subval;
        uint64_t ret;

        ret = __atomic_sub_fetch(ptr, subval, __ATOMIC_SEQ_CST);

        if (ret != expected) {
            printf("%s: %" PRIu64 " != %" PRIu64 " and current = %" PRIu64 "\n",
                   __func__, ret, expected, udata_64[i]);
            return -1;
        }
    }

    return 0;
}


int main(int argc, char **argv)
{
    int i, r = 0;
    uint64_t swap, seed = 0;

    printf("Starting LSE atomic test\n");

    for (i = 0; i < 64 && !r; i++) {
        seed = seed ^ (1 << i);
        swap = (1ULL << (64 - i));
        printf("iteration: %d, data_seed = %#lx\n", i, seed);

        reset_data(seed);
        r  = do_cmpxchg_u32(udata_32, swap)
            || do_cmpxchg_s32(sdata_32, swap)
            || do_cmpxchg_u64(udata_64, swap)
            || do_cmpxchg_s64(sdata_64, swap);

        if (r) {
            break;
        }
        reset_data(seed);

        r  = do_cmpxchg_u128(udata_64, swap);
        if (r) {
            break;
        }

        reset_data(seed);
        r = do_swap_u32(udata_32, swap)
            || do_swap_u64(udata_64, swap);

        if (r) {
            break;
        }

        reset_data(seed);
        r = do_sub_u32(udata_32, swap)
            || do_sub_u64(udata_64, swap);

        if (r) {
            break;
        }

    }

    printf("Finished %d, r = %d\n", i, r);
    return r;
}
