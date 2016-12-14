# -*- Mode: makefile -*-
#
# Generic linux-user TCG tests
#
# These tests all use standard non-architecture specific APIs so can
# be built for all TCG targets. The Make is expected to be called in
# the ${BUILD_DIR}/$(TARGET_NAME_ARCH)-linux-user/tests directory
#
# To build for other architectures call make with CROSS_CC set
#

BUILD_DIR=../..
include $(BUILD_DIR)/config-host.mak	# brings in SRC_PATH
include ../config-target.mak		# TARGET_NAME
include $(SRC_PATH)/rules.mak

$(call set-vpath, $(SRC_PATH)/tests/tcg/misc)

QEMU=$(BUILD_DIR)/$(TARGET_NAME)-linux-user/qemu-$(TARGET_NAME)

# Compiler set-up, default to system compiler if not set
CROSS_CC?=$(CC)

CFLAGS=-Wall -O2 -g -fno-strict-aliasing -static
LDFLAGS=

TESTS=linux-test \
      testthread \
      sha1 \
      test-mmap

all: $(TESTS)
test: all

# rules to run tests

run-%: %
	-$(QEMU) ./$*

run-linux-test: linux-test
run-testthread: testthread
run-sha1: sha1
run-test_path: test_path

run-test-mmap: test-mmap
	-$(QEMU) ./test-mmap
	-$(QEMU) -p 8192 ./test-mmap 8192
	-$(QEMU) -p 16384 ./test-mmap 16384
	-$(QEMU) -p 32768 ./test-mmap 32768

run-test_path: test_path

# rules to compile tests

# These two need to be re-written as relying on glib (rather than just glibc)
# test_path: test_path.o
# 	$(CROSS_CC) $^ -o $@ -lglib-2.0

# test_path.o: test_path.c
# 	$(CROSS_CC) $^ -c -o $@ $(QEMU_INCLUDES) `pkg-config --cflags --libs glib-2.0`

testthread: testthread.c
	$(CROSS_CC) $(CFLAGS) $(LDFLAGS) -o $@ $< -lpthread

# generic Linux and CPU test
linux-test: linux-test.c
	$(CROSS_CC) ${QEMU_INCLUDES} $(CFLAGS) $(LDFLAGS) -o $@ $< -lm

test-mmap: test-mmap.c
	$(CROSS_CC) $(CFLAGS) -Wall -O2 $(LDFLAGS) -o $@ $< -static

# speed test
sha1: sha1.c
	$(CROSS_CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

speed: sha1
	time ./sha1
	time $(QEMU) ./sha1

clean:
	rm -f *~ *.o $(TESTS)
