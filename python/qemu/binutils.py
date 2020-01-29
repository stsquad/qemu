"""
QEMU binary utility module:

The binary utility module provides helpers to query QEMU binary for
build-dependent configuration options at runtime.
"""
#
# Copyright (c) 2020 Red Hat, Inc.
#
# Author:
#  Philippe Mathieu-Daudé <philmd@redhat.com>
#
# This work is licensed under the terms of the GNU GPL, version 2 or later.
# See the COPYING file in the top-level directory.
#
# SPDX-License-Identifier: GPL-2.0-or-later

import logging

from .machine import QEMUMachine

LOG = logging.getLogger(__name__)


def binary_get_version(qemu_bin):
    '''
    Get QEMU binary version

    @param qemu_bin (str): path to the QEMU binary
    @return binary version (dictionary with major/minor/micro keys)
    '''
    with QEMUMachine(qemu_bin) as vm:
        vm.set_machine('none')
        vm.launch()
        res = vm.command('query-version')
        LOG.info(res)
        vm.shutdown()
        return res['qemu']

def binary_get_arch(qemu_bin):
    '''
    Get target architecture for a QEMU binary

    @param qemu_bin (str): path to the QEMU binary
    @return binary target architecture
    '''
    with QEMUMachine(qemu_bin) as vm:
        vm.set_machine('none')
        vm.launch()
        res = vm.command('query-target')
        LOG.info(res)
        vm.shutdown()
        return res['arch']
