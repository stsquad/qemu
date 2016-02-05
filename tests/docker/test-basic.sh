#!/bin/bash -e
#
# Basic compiling test that everyone already does. But why not automate it?
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

cd $(mktemp -d)
mkdir build
mkdir install
cd build
build_qemu --target-list=x86_64-softmmu --prefix="${pwd}/install"
make check $MAKEFLAGS
make install
