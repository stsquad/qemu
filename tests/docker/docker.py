#!/usr/bin/env python2
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
import sys
import subprocess
import json
import hashlib
import atexit
import uuid
import argparse
import tempfile
import re
from shutil import copy

def _text_checksum(text):
    """Calculate a digest string unique to the text content"""
    return hashlib.sha1(text).hexdigest()

def _guess_docker_command():
    """ Guess a working docker command or raise exception if not found"""
    commands = [["docker"], ["sudo", "-n", "docker"]]
    for cmd in commands:
        if subprocess.call(cmd + ["images"],
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE) == 0:
            return cmd
    commands_txt = "\n".join(["  " + " ".join(x) for x in commands])
    raise Exception("Cannot find working docker command. Tried:\n%s" % \
                    commands_txt)

def _find_user_binary(binary_name):
    """ Find a binary in the QEMU source tree. Used for finding qemu-$arch."""
    top = os.path.abspath("%s/../../.." % sys.argv[0])
    linux_user = [ x for x in os.listdir(top) if x.endswith("-linux-user") ]
    for x in linux_user:
        check_path = "%s/%s/%s" % (top, x, binary_name)
        if os.path.isfile(check_path):
            return check_path
    return None

def _copy_with_mkdir(src, root_dir, sub_path):
    """Copy src into root_dir, creating sub_path as needed."""
    full_path = "%s/%s" % (root_dir, sub_path)
    try:
        os.makedirs(full_path)
    except OSError:
        print "skipping %s" % (full_path)

    copy(src, "%s/%s" % (full_path, os.path.basename(src)))

class Docker(object):
    """ Running Docker commands """
    def __init__(self):
        self._command = _guess_docker_command()
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

    def get_image_dockerfile_checksum(self, tag):
        resp = self._output(["inspect", tag])
        labels = json.loads(resp)[0]["Config"].get("Labels", {})
        return labels.get("com.qemu.dockerfile-checksum", "")

    def build_image(self, tag, dockerfile, quiet=True, qemu=None, argv=None):
        if argv == None:
            argv = []

        # Create a temporary docker context to build in
        tmp_dir = tempfile.mkdtemp(prefix="docker_build")

        # Copy the dockerfile into our work space
        # line by line, stripping and executing HOST_CMDs
        #
        tmp_df = tempfile.NamedTemporaryFile(dir=tmp_dir, suffix=".docker")

        for l in open(dockerfile).readlines():
            m = re.match("HOST_CMD ", l)
            if m:
                print "l=%s" % (l)
                cmd = l[m.end():]
                r = subprocess.check_call(cmd, cwd=tmp_dir, shell=True)
                tmp_df.write("# HOST_CMD %s# HOST_RES = %d\n" % (cmd, r))
            else:
                tmp_df.write(l)

        tmp_df.write("\n")
        tmp_df.write("LABEL com.qemu.dockerfile-checksum=%s" %
                     _text_checksum(dockerfile))
        tmp_df.flush()

        # Do we want to copy QEMU into here?
        if qemu:
            _copy_with_mkdir(qemu, tmp_dir, "/usr/bin")
            # do ldd bit here
            ldd_output = subprocess.check_output(["ldd", qemu])
            for l in ldd_output.split("\n"):
                s = re.search("(/.*/)(\S*)", l)
                if s and len(s.groups())==2:
                    so_path=s.groups()[0]
                    so_lib=s.groups()[1]
                    _copy_with_mkdir("%s/%s" % (so_path, so_lib),
                                     tmp_dir, so_path)

        self._do(["build", "-t", tag, "-f", tmp_df.name] + argv + \
                 [tmp_dir],
                 quiet=quiet)

    def image_matches_dockerfile(self, tag, dockerfile):
        try:
            checksum = self.get_image_dockerfile_checksum(tag)
        except Exception:
            return False
        return checksum == _text_checksum(dockerfile)

    def run(self, cmd, keep, quiet):
        label = uuid.uuid1().hex
        if not keep:
            self._instances.append(label)
        ret = self._do(["run", "--label",
                        "com.qemu.instance.uuid=" + label] + cmd,
                       quiet=quiet)
        if not keep:
            self._instances.remove(label)
        return ret

class SubCommand(object):
    """A SubCommand template base class"""
    name = None # Subcommand name
    def shared_args(self, parser):
        parser.add_argument("--quiet", action="store_true",
                            help="Run quietly unless an error occured")

    def args(self, parser):
        """Setup argument parser"""
        pass
    def run(self, args, argv):
        """Run command.
        args: parsed argument by argument parser.
        argv: remaining arguments from sys.argv.
        """
        pass

class RunCommand(SubCommand):
    """Invoke docker run and take care of cleaning up"""
    name = "run"
    def args(self, parser):
        parser.add_argument("--keep", action="store_true",
                            help="Don't remove image when command completes")
    def run(self, args, argv):
        return Docker().run(argv, args.keep, quiet=args.quiet)

class BuildCommand(SubCommand):
    """ Build docker image out of a dockerfile. Arguments: <tag> <dockerfile>"""
    name = "build"
    def args(self, parser):
        parser.add_argument("--qemu", help="Include qemu-user binaries")
        parser.add_argument("tag",
                            help="Image Tag")
        parser.add_argument("dockerfile",
                            help="Dockerfile name")

    def run(self, args, argv):
        dockerfile = open(args.dockerfile, "rb").read()
        tag = args.tag

        # find qemu binary
        qbin=None
        if args.qemu:
            qbin=_find_user_binary(args.qemu)

        dkr = Docker()
        if dkr.image_matches_dockerfile(tag, dockerfile):
            if not args.quiet:
                print "Image is up to date."
            return 0

        dkr.build_image(tag, args.dockerfile, quiet=args.quiet, qemu=qbin, argv=argv)
        return 0

class CleanCommand(SubCommand):
    """Clean up docker instances"""
    name = "clean"
    def run(self, args, argv):
        Docker().clean()
        return 0

def main():
    parser = argparse.ArgumentParser(description="A Docker helper",
            usage="%s <subcommand> ..." % os.path.basename(sys.argv[0]))
    subparsers = parser.add_subparsers(title="subcommands", help=None)
    for cls in SubCommand.__subclasses__():
        cmd = cls()
        subp = subparsers.add_parser(cmd.name, help=cmd.__doc__)
        cmd.shared_args(subp)
        cmd.args(subp)
        subp.set_defaults(cmdobj=cmd)
    args, argv = parser.parse_known_args()
    return args.cmdobj.run(args, argv)

if __name__ == "__main__":
    sys.exit(main())
