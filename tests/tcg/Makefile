# -*- Mode: makefile -*-
#
# TCG tests
#
# These are complicated by the fact we want to build them for guest
# systems. This requires knowing what guests we are building and which
# ones we have cross-compilers for or docker images with
# cross-compilers.
#
# The tests themselves should be as minimal as possible as
# cross-compilers don't always have a large amount of libraries
# available.
#
# We don't reference the host build system and expect CC to have been
# set for us from the parent make. We also expect to be in the tests
# build dir for the FOO-linux-user.
#

-include ../../config-host.mak
-include $(SRC_PATH)/rules.mak

# Set search path for all sources
VPATH = $(SRC_PATH)/tests/tcg/multiarch
TEST_SRCS = $(wildcard $(SRC_PATH)/tests/tcg/multiarch/*.c)

VPATH     += $(SRC_PATH)/tests/tcg/$(ARCH)
TEST_SRCS += $(wildcard $(SRC_PATH)/tests/tcg/$(ARCH)/*.c)

# We use what ever CC we have
CFLAGS=-Wall -O0 -g -fno-strict-aliasing -static
LDFLAGS=-nostdlib

SRCS=$(notdir ${TEST_SRCS})
TESTS=$(SRCS:.c=)

%$(EXESUF): %.c Makefile
	$(CC) $(CFLAGS) $< -o $@

all: $(TESTS)
	@echo "CC is $(CC)"
	@echo "SRCS is $(SRCS)"
	@echo "ARCH is $(ARCH)"
