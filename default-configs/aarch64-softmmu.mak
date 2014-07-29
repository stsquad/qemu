# Default configuration for aarch64-softmmu

# We support most of the 32 bit boards so need all their config
include arm-softmmu.mak

# we explicitly disable ones that require old ARMv5 support
CONFIG_ARMV5_BOARDS=n

# Currently no 64-bit specific config requirements
