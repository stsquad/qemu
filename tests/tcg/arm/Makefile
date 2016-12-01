BUILD_DIR?=$(CURDIR)
SRC_PATH=../../../
include $(BUILD_DIR)/config-host.mak
include $(SRC_PATH)/rules.mak

$(call set-vpath, $(SRC_PATH)/tests/tcg/arm)

QEMU=$(BUILD_DIR)/arm-linux-user/qemu-arm

QEMU_INCLUDES += -I$(BUILD_DIR)
CFLAGS=-Wall -O2 -g -fno-strict-aliasing
LDFLAGS=

# TODO: automatically detect ARM and MIPS compilers, and run those too

# runcom maps page 0, so it requires root privileges
# also, pi_10.com runs indefinitely

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
	arm-linux-gnueabi-ld -o $@ $<

hello-arm.o: hello-arm.c
	arm-linux-gnueabi-gcc -Wall -g -O2 -c -o $@ $<

arm-iwmmxt: test-arm-iwmmxt.s
	cpp < $< | arm-linux-gnueabi-gcc -Wall -static -march=iwmmxt -mabi=aapcs -x assembler - -o $@

clean:
	rm -f *~ *.o $(TESTS)
