# Tests covering various python/qemu/ scripts
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

import sys
import os
import logging

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', 'python'))
from avocado_qemu import Test
from qemu.binutils import binary_get_arch
from qemu.binutils import binary_get_version


class PythonQemuCoreScripts(Test):

    def test_get_version(self):
        logger = logging.getLogger('core')
        version = binary_get_version(self.qemu_bin)
        logger.debug('version: {}'.format(version))
        # QMP 'query-version' introduced with QEMU v0.14
        self.assertGreaterEqual(version['major'], 0)
        if version['major'] == 0:
            self.assertGreaterEqual(version['minor'], 14)

    def test_get_arch_x86(self):
        """
        :avocado: tags=arch:i386
        :avocado: tags=arch:x86_64
        """
        logger = logging.getLogger('core')
        a = binary_get_arch(self.qemu_bin)
        logger.debug('arch: {}'.format(a))
        self.assertIn(a, ['i386', 'x86_64'])
