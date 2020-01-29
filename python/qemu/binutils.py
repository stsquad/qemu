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

def binary_get_machines(qemu_bin):
    '''
    Get list of machines supported by a QEMU binary

    @param qemu_bin (str): path to the QEMU binary
    @return list of machines supported by the binary
    '''
    with QEMUMachine(qemu_bin) as vm:
        vm.set_machine('none')
        vm.launch()
        res = vm.command('query-machines')
        LOG.info(res)
        vm.shutdown()
        return [m['name'] for m in res]

def binary_get_qom_implementations(qemu_bin, type_name, include_abstract=False):
    '''
    Get list of QOM types implementing a particular interface

    @param qemu_bin (str): path to the QEMU binary
    @param type_name (str): QOM interface name
    @param include_abstract (bool): if True, abstract interfaces are also
                                    returned in the list
    @return list of QOM types implementing the interface @type_name
    '''
    with QEMUMachine(qemu_bin) as vm:
        vm.set_machine('none')
        vm.launch()
        res = vm.command('qom-list-types',
                         implements=type_name,
                         abstract=include_abstract)
        LOG.info(res)
        vm.shutdown()
        return [m['name'] for m in res]

def binary_get_accels(qemu_bin):
    '''
    Get list of accelerators supported by a QEMU binary

    @param qemu_bin (str): path to the QEMU binary
    @return list of accelerators supported by the binary
    '''
    accel_types = binary_get_qom_implementations(qemu_bin, "accel", False)
    return [a.strip("-accel") for a in accel_types]
