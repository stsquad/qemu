#!/bin/bash -e
#
# Compile and check with clang.
#
# Copyright (c) 2016 Red Hat Inc.
#
# Authors:
#  Fam Zheng <famz@redhat.com>
#
# This work is licensed under the terms of the GNU GPL, version 2
# or (at your option) any later version. See the COPYING file in
# the top-level directory.

. common.rc

requires clang

cd $(mktemp -d)
build_qemu \
    --target-list=x86_64-softmmu,aarch64-softmmu \
    --enable-debug \
    --cxx=clang++ --cc=clang --host-cc=clang \
    --extra-cflags=-Werror --extra-cflags=-fsanitize=undefined \
    --extra-cflags=-Wno-deprecated-declarations \
    --extra-cflags=-fno-sanitize=float-divide-by-zero
make $MAKEFLAGS check
