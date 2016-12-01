# -*- Mode: makefile -*-
#
# Arm linux-user TCG tests
#
# The Make is expected to be called in the
# ${BUILD_DIR}/arm-linux-user/tests directory
#

BUILD_DIR=../..
include $(BUILD_DIR)/config-host.mak	# brings in SRC_PATH
include ../config-target.mak		# TARGET_NAME
include $(SRC_PATH)/rules.mak

$(call set-vpath, $(SRC_PATH)/tests/tcg/arm)

QEMU=$(BUILD_DIR)/arm-linux-user/qemu-arm

# Compiler set-up, default to system compiler if not set
CROSS_CC?=$(CC)

QEMU_INCLUDES += -I$(BUILD_DIR)
CFLAGS=-Wall -O2 -g -fno-strict-aliasing -nostdlib
LDFLAGS=-nostdlib

TESTS=hello-arm arm-iwmmxt

all: $(patsubst %,run-%,$(TESTS))
test: all

# rules to run tests

run-%: %
	$(QEMU) ./$*

#.PHONY: $(patsubst %,run-%,$(TESTS))

run-hello-arm: hello-arm
run-arm-iwmmxt: arm-iwmmxt

# arm test
hello-arm: hello-arm.o
	$(CROSS_CC) $(LDFLAGS) -o $@ $<

hello-arm.o: hello-arm.c
	$(CROSS_CC) $(CFLAGS) -c -o $@ $<

arm-iwmmxt: test-arm-iwmmxt.s
	cpp < $< | $(CROSS_CC) -Wall -static -march=iwmmxt -mabi=aapcs -x assembler - -o $@

clean:
	rm -f *~ *.o $(TESTS)
