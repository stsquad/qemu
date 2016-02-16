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
import uuid

class ContainerTerminated(Exception):
    """ Raised if the container has already existed """
    pass

class Docker(object):
    """ Running Docker commands """
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
            resp = self._output(["inspect", i])
            labels = json.loads(resp)[0]["Config"]["Labels"]
            active = json.loads(resp)[0]["State"]["Running"]
            if not labels:
                continue
            instance_uuid = labels.get("com.qemu.instance.uuid", None)
            if not instance_uuid:
                continue
            if only_known and instance_uuid not in self._instances:
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
        commands = [["docker"], ["sudo", "-n", "docker"]]
        for cmd in commands:
            if subprocess.call(cmd + ["images"],
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE) == 0:
                return cmd
        commands_txt = "\n".join(["  " + " ".join(x) for x in commands])
        raise Exception("Cannot find working docker command. Tried:\n%s" % commands_txt)

    def get_image_dockerfile_checksum(self, tag):
        resp = self._output(["inspect", tag])
        labels = json.loads(resp)[0]["Config"].get("Labels", {})
        return labels.get("com.qemu.dockerfile-checksum", "")

    def checksum(self, text):
        return hashlib.sha1(text).hexdigest()

    def build_image(self, tag, dockerfile, df, quiet=True):
        tmp = dockerfile + "\n" + \
              "LABEL com.qemu.dockerfile-checksum=%s" % self.checksum(dockerfile)
        tmp_df = df + ".tmp"
        tmp_file = open(tmp_df, "wb")
        tmp_file.write(tmp)
        tmp_file.close()
        self._do(["build", "-t", tag, "-f", tmp_df, os.path.dirname(df)],
                 quiet=quiet)
        os.unlink(tmp_df)

    def image_matches_dockerfile(self, tag, dockerfile):
        try:
            checksum = self.get_image_dockerfile_checksum(tag)
        except:
            return False
        return checksum == self.checksum(dockerfile)

    def run(self, cmd, keep, quiet):
        label = uuid.uuid1().hex
        if not keep:
            self._instances.append(label)
        ret = self._do(["run", "--label", "com.qemu.instance.uuid=" + label] + cmd, quiet=quiet)
        if not keep:
            self._instances.remove(label)
        return ret

