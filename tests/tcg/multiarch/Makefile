# -*- Mode: makefile -*-
#
# Multi-arch linux-user TCG tests
#
# The Make is expected to be called while in:
#   ${BUILD_DIR}/FOO-linux-user/tests directory
#
# The makefile expects CC to be overridden by the calling Make
# machinary which can either use a cross compiler or docker based
# cross compile.

BUILD_DIR?=$(CURDIR)

include ../../config-host.mak		# brings in SRC_PATH
include ../config-target.mak		# TARGET_NAME

#include $(SRC_PATH)/rules.mak

TEST_SRCS=$(wildcard $(SRC_PATH)/tests/tcg/multiarch/*.c)

# Set search path for all sources
VPATH=$(SRC_PATH)/tests/tcg/multiarch

# We use what ever CC we have
CFLAGS=-Wall -O0 -g -fno-strict-aliasing -static
LDFLAGS=-nostdlib

SRCS=$(notdir ${TEST_SRCS})
TESTS=$(SRCS:.c=)

%$(EXESUF): %.c Makefile
	$(CC) $(CFLAGS) $< -o $@

all: $(TESTS)

check: $(TESTS)
	for f in $(TESTS); do $(QEMU) $$f || exit 1; done

clean:
	$(RM) $(TESTS)

.PHONY: clean all check
