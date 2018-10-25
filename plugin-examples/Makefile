NAMES :=
NAMES += bbcount_avgsize_racy
NAMES += mem_count_racy_both

SONAMES := $(addsuffix .so,$(addprefix lib,$(NAMES)))

# QEMU installed path, set by --prefix during configure
QEMU_PATH ?= /data/src/qemu-inst/plugin-test

CC := gcc
CFLAGS :=
CFLAGS += -O2 -Werror -Wall
CFLAGS += -Wundef -Wwrite-strings -Wmissing-prototypes
CFLAGS += -Wstrict-prototypes -Wredundant-decls
CFLAGS += -fno-strict-aliasing -fno-common -fwrapv
CFLAGS += -I$(QEMU_PATH)/include
LDLIBS := -lc

all: $(SONAMES)

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c $< -o $@

lib%.so: %.o
	$(CC) -shared -Wl,-soname,$@ -o $@ $^ $(LDLIBS)

clean:
	$(RM) -f *.o *.so
	$(RM) -Rf .libs

.PHONY: all clean
