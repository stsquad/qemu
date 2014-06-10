#!/bin/bash
#
# Travis Build Script
#
set -ex

# Echo: our build environment
env

# Configure
./configure --target-list=${TARGETS} ${EXTRA_CONFIG}

# Build
make

# Test
${TEST_CMD}

exit 0
