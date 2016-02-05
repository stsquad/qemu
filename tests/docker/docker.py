#!/usr/bin/env python2 -B
#
# Docker controlling module
#
# Copyright (c) 2016 Red Hat Inc.
#
# Authors:
#  Fam Zheng <famz@redhat.com>
#
# This work is licensed under the terms of the GNU GPL, version 2
# or (at your option) any later version. See the COPYING file in
# the top-level directory.

import os
import subprocess
import json
import hashlib
import atexit
import time
import uuid

class ContainerTerminated(Exception):
    pass

class Docker(object):
    def __init__(self):
        self._command = self._guess_command()
        self._instances = []
        atexit.register(self._kill_instances)

    def _do(self, cmd, quiet=True, **kwargs):
        if quiet:
            kwargs["stdout"] = subprocess.PIPE
        return subprocess.call(self._command + cmd, **kwargs)

    def _do_kill_instances(self, only_known, only_active=True):
        cmd = ["ps", "-q"]
        if not only_active:
            cmd.append("-a")
        for i in self._output(cmd).split():
            r = self._output(["inspect", i])
            labels = json.loads(r)[0]["Config"]["Labels"]
            active = json.loads(r)[0]["State"]["Running"]
            if not labels:
                continue
            u = labels.get("com.qemu.instance.uuid", None)
            if not u:
                continue
            if only_known and u not in self._instances:
                continue
            print "Terminating", i
            if active:
                self._do(["kill", i])
            self._do(["rm", i])

    def clean(self):
        self._do_kill_instances(False, False)
        return 0

    def _kill_instances(self):
        return self._do_kill_instances(True)

    def _output(self, cmd, **kwargs):
        return subprocess.check_output(self._command + cmd,
                                       stderr=subprocess.STDOUT,
                                       **kwargs)

    def _guess_command(self):
        for c in [["docker"], ["sudo", "-n", "docker"]]:
            if subprocess.call(c + ["images"],
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE) == 0:
                return c
        raise Exception("Cannot find working docker command")

    def get_image_dockerfile_checksum(self, tag):
        resp = self._output(["inspect", tag])
        t = json.loads(resp)[0]
        return t["Config"].get("Labels", {}).get("com.qemu.dockerfile-checksum", "")

    def checksum(self, text):
        return hashlib.sha1(text).hexdigest()

    def build_image(self, tag, dockerfile, df, quiet=True):
        tmp = dockerfile + "\n" + \
              "LABEL com.qemu.dockerfile-checksum=%s" % self.checksum(dockerfile)
        tmp_df = df + ".tmp"
        f = open(tmp_df, "wb")
        f.write(tmp)
        f.close()
        self._do(["build", "-t", tag, "-f", tmp_df, os.path.dirname(df)],
                 quiet=quiet)
        os.unlink(tmp_df)

    def image_matches_dockerfile(self, tag, dockerfile):
        try:
            a = self.get_image_dockerfile_checksum(tag)
        except:
            return False
        return a == self.checksum(dockerfile)

    def run(self, cmd, quiet, **kwargs):
        label = uuid.uuid1().hex
        self._instances.append(label)
        r = self._do(["run", "--label", "com.qemu.instance.uuid=" + label] + cmd, quiet=quiet)
        self._instances.remove(label)
        return r

