# -*- Mode: makefile -*-
#
# Alpha linux-user TCG tests
#
# The Make is expected to be called in the
# ${BUILD_DIR}/alpha-linux-user/tests directory
#

BUILD_DIR?=$(CURDIR)
include $(BUILD_DIR)/config-host.mak	# brings in SRC_PATH
include ../config-target.mak		# TARGET_NAME
include $(SRC_PATH)/rules.mak

$(call set-vpath, $(SRC_PATH)/tests/tcg/alpha)

QEMU=$(BUILD_DIR)/alpha-linux-user/qemu-alpha

# Compiler set-up, default to system compiler if not set
CROSS_CC?=$(CC)

CFLAGS=-Wall -O2 -g -fno-strict-aliasing -static
LDFLAGS=-nostdlib

TESTS=test-cond test-cmov

all: hello-alpha $(TESTS)

hello-alpha: hello-alpha.o crt.o
	$(CROSS_CC) -o $@ crt.o $< $(LDFLAGS)

test-cond: test-cond.o crt.o
	$(CROSS_CC) -o $@ crt.o $< $(LDFLAGS)

test-cmov.o: test-cond.c
	$(CROSS_CC) -c $(CFLAGS) -DTEST_CMOV -o $@ $<

test-cmov: test-cmov.o crt.o
	$(CROSS_CC) -o $@ crt.o $< $(LDFLAGS)

test-ovf: test-ovf.o crt.o
	$(CROSS_CC) -o $@ crt.o $< $(LDFLAGS)

check: $(TESTS)
	for f in $(TESTS); do $(QEMU) $$f || exit 1; done

clean:
	$(RM) *.o *~ hello-alpha $(TESTS)

.PHONY: clean all check
