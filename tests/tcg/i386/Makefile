# -*- Mode: makefile -*-
#
# i386 linux-user TCG tests
#
# The Make is expected to be called in the
# ${BUILD_DIR}/i386-linux-user/tests directory
#

BUILD_DIR=../..
include $(BUILD_DIR)/config-host.mak	# brings in SRC_PATH
include ../config-target.mak		# TARGET_NAME
include $(SRC_PATH)/rules.mak

$(call set-vpath, $(SRC_PATH)/tests/tcg/i386)

QEMU=$(BUILD_DIR)/i386-linux-user/qemu-i386
QEMU_X86_64=$(BUILD_DIR)/x86_64-linux-user/qemu-x86_64
CC_X86_64=$(CC_I386) -m64

QEMU_INCLUDES += -I$(BUILD_DIR)
CFLAGS=-Wall -O2 -g -fno-strict-aliasing
#CFLAGS+=-msse2
LDFLAGS=

I386_TESTS=hello-i386 \
	   test-i386-fprem

# native i386 compilers sometimes are not biarch.  assume cross-compilers are
ifneq ($(ARCH),i386)
I386_TESTS+=test-i386 \
	    run-test-x86_64
endif

all: $(patsubst %,run-%,$(I386_TESTS))
test: all

# rules to run tests

.PHONY: $(patsubst %,run-%,$(I386_TESTS))

run-%: %
	-$(QEMU) ./$*

run-hello-i386: hello-i386
	$(QEMU) ./hello-i386

run-test-i386: test-i386
	./test-i386 > test-i386.ref
	-$(QEMU_X86_64) test-i386 > test-i386.out
	@if cmp -s test-i386.ref test-i386.out ; then echo "Auto Test OK"; fi

run-test-i386-fprem: test-i386-fprem
	./test-i386-fprem > test-i386-fprem.ref
	-$(QEMU_X86_64) test-i386-fprem > test-i386-fprem.out
	@if cmp -s test-i386-fprem.ref test-i386-fprem.out ; then echo "Auto Test OK"; fi

run-test-x86_64: test-x86_64
	./test-x86_64 > test-x86_64.ref
	-$(QEMU_X86_64) test-x86_64 > test-x86_64.out
	@if diff -u test-x86_64.ref test-x86_64.out ; then echo "Auto Test OK"; fi

# rules to compile tests

hello-i386: hello-i386.c
	$(CC_I386) -nostdlib $(CFLAGS) -static $(LDFLAGS) -o $@ $<
	strip $@

# i386/x86_64 emulation test (test various opcodes) */
test-i386: test-i386.c test-i386-code16.S test-i386-vm86.S \
           test-i386.h test-i386-shift.h test-i386-muldiv.h
	$(CC) $(QEMU_INCLUDES) $(CFLAGS) $(LDFLAGS) -o $@ \
              $(<D)/test-i386.c $(<D)/test-i386-code16.S $(<D)/test-i386-vm86.S -lm

test-i386-fprem: test-i386-fprem.c
	$(CC) $(QEMU_INCLUDES) $(CFLAGS) $(LDFLAGS) -o $@ $^ `pkg-config --cflags --libs glib-2.0`

test-x86_64: test-i386.c \
           test-i386.h test-i386-shift.h test-i386-muldiv.h
	$(CC_X86_64) $(QEMU_INCLUDES) $(CFLAGS) $(LDFLAGS) -o $@ $(<D)/test-i386.c -lm

# vm86 test
runcom: runcom.c
	$(CC_I386) $(CFLAGS) $(LDFLAGS) -o $@ $<

clean:
	rm -f *~ *.o test-i386.out test-i386.ref test-i386-fprem.out test-i386-fprem.ref \
           test-x86_64.log test-x86_64.ref qruncom $(I386_TESTS)
