#
# A version of the config that only supports 64bits and their devices.
# This doesn't quite eliminate all 32 bit devices as some boards like
# "virt" support both. The CONFIG_XLNX_ZYNQMP_ARM isn't included as it
# also requires 32 bit support for the R5s
#

CONFIG_ARM_VIRT=y
CONFIG_XLNX_VERSAL=y
CONFIG_SBSA_REF=y
