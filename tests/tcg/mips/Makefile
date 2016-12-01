# -*- Mode: makefile -*-
#
# mips linux-user TCG tests
#
# The Make is expected to be called in the
# ${BUILD_DIR}/mips-linux-user/tests directory
#

BUILD_DIR=../..
include $(BUILD_DIR)/config-host.mak	# brings in SRC_PATH
include ../config-target.mak		# TARGET_NAME
include $(SRC_PATH)/rules.mak

$(call set-vpath, $(SRC_PATH)/tests/tcg/mips)

QEMU=$(BUILD_DIR)/mips-linux-user/qemu-mips

# Compiler set-up, default to system compiler if not set
CROSS_CC?=$(CC)

CFLAGS=-Wall -O2 -g -fno-strict-aliasing
LDFLAGS=

TESTS=hello-mips

all: $(patsubst %,run-%,$(TESTS))
test: all

# rules to run tests

run-%: %
	$(QEMU) ./$*

#.PHONY: $(patsubst %,run-%,$(TESTS))

run-hello-mips: hello-mips

# mips test
hello-mips: hello-mips.o
	$(CROSS_CC) -o $@ $<

hello-mips.o: hello-mips.c
	$(CROSS_CC) -Wall -g -O2 -c -o $@ $<

clean:
	rm -f *~ *.o $(TESTS)
