#include "qemu-common.h"
#include "qemu/main-loop.h"

void __qemu_mutex_lock_iothread(const char *func, int line)
{
    fprintf(stderr,"%s: what?\n", __func__);
}

void qemu_mutex_unlock_iothread(void)
{
    fprintf(stderr,"%s: wait, how did we get here?\n", __func__);
}
