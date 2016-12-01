#include <asm/unistd.h>
#include <stddef.h>

static inline void _exit(int status)
{
  int __res;
  __asm__ volatile ("movl %%ecx,%%ebx\n"\
		    "int $0x80" \
		    :  "=a" (__res) : "0" (__NR_exit),"c" ((long)(status)));
}

static inline int _write(int fd, const char * buf, int len)
{
  int status;
  __asm__ volatile ("pushl %%ebx\n"\
		    "movl %%esi,%%ebx\n"\
		    "int $0x80\n" \
		    "popl %%ebx\n"\
		    : "=a" (status) \
		    : "0" (__NR_write),"S" ((long)(fd)),"c" ((long)(buf)),"d" ((long)(len)));
  return status;
}

void _start(void)
{
    _write(1, "Hello World\n", 12);
    _exit(0);
}
