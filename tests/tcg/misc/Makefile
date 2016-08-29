BUILD_DIR=../../../build/
SRC_PATH=../../../
include $(BUILD_DIR)/config-host.mak
include $(SRC_PATH)/rules.mak

$(call set-vpath, $(SRC_PATH)/tests/tcg/misc)

QEMU=$(BUILD_DIR)/$(ARCH)-linux-user/qemu-$(ARCH)

QEMU_INCLUDES += -I$(BUILD_DIR)
CFLAGS=-Wall -O2 -g -fno-strict-aliasing -I$(SRC_PATH)/include -I$(BUILD_DIR)
#CFLAGS+=-msse2
LDFLAGS=

# TODO: automatically detect ARM and MIPS compilers, and run those too

# runcom maps page 0, so it requires root privileges
# also, pi_10.com runs indefinitely

TESTS=linux-test \
      testthread \
      sha1 \
      test-mmap \
      test_path
      # runcom

all: $(patsubst %,run-%,$(TESTS))
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

test_path: test_path.o
	$(CC) $^ -o $@ -lglib-2.0

test_path.o: test_path.c
	$(CC) $^ -c -o $@ $(QEMU_INCLUDES) `pkg-config --cflags --libs glib-2.0`

testthread: testthread.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< -lpthread

# generic Linux and CPU test
linux-test: linux-test.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< -lm

test-mmap: test-mmap.c
	$(CC) $(CFLAGS) -Wall -O2 $(LDFLAGS) -o $@ $< -static

# speed test
sha1: sha1.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

speed: sha1
	time ./sha1
	time $(QEMU) ./sha1

clean:
	rm -f *~ *.o $(TESTS)
