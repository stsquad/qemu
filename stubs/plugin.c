#include "qemu/osdep.h"
#include "qemu/plugin.h"

bool use_plugin_clock;

int64_t plugin_get_clock(void)
{
    abort();
}
