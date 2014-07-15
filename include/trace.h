#ifndef TRACE_H
#define TRACE_H

#include "trace/generated-tracers.h"

static inline void trace_inc_counter(unsigned int *counter)
{
#ifndef CONFIG_TRACE_NOP
    (*counter)++;
#endif
}

#endif  /* TRACE_H */
