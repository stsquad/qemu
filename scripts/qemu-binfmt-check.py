#!/usr/bin/python
#
# binfmt check script
#
# Copyright 2014 Linaro
#
# Authors:
#  Alex Bennee <alex.bennee@linaro.org>
#
# This work is licensed under the terms of the GNU GPL, version 2.  See
# the COPYING file in the top-level directory.

import os
import re
import binascii
import logging

re_int = re.compile(r"interpreter (.+)$")
re_off = re.compile(r"offset (\d+)$")
re_magic = re.compile(r"magic ([\dabcdef]+)")
re_mask = re.compile(r"mask ([\dabcdef]+)")

# argparse is only available in Python >= 2.7
from optparse import OptionParser
parser = OptionParser(usage="""usage: %prog [file [file..]]

This script will check you binary files against helpers registered in the
systems binfmt_misc and report which helper should get run.""")

parser.add_option("-v", "--verbose", action="store_true", default=False,
                  help="Be more verbose about what it's doing")

# list of binfmts
binfmts = []


def read_binfmt_spec(f):
    bfmt = {}
    with open(f) as fd:
        content = fd.readlines()
    for l in content:
        m = re_int.match(l)
        if m:
            bfmt["interpreter"] = m.group(1)
        m = re_off.match(l)
        if m:
            bfmt["offset"] = int(m.group(1))
        m = re_magic.match(l)
        if m:
            bfmt["magic"] = binascii.unhexlify(m.group(1))
        m = re_mask.match(l)
        if m:
            bfmt["mask"] = binascii.unhexlify(m.group(1))
    logging.info("loaded: %s" % bfmt)
    binfmts.append(bfmt)


def load_binfmt_masks():
    binfmt_dir = "/proc/sys/fs/binfmt_misc"
    files = os.listdir(binfmt_dir)
    for f in files:
        if not f.startswith("status"):
            fp = "%s/%s" % (binfmt_dir, f)
            if os.access(fp, os.R_OK):
                read_binfmt_spec(fp)


def check_file_against_binfmt(fmt, f):
    """
    Check if a file will match a given binfmt mask
    """
    nbytes = len(fmt["magic"])

    fd = open(f, "rb")
    fd.seek(fmt["offset"])
    header = fd.read(nbytes)
    magic = fmt["magic"]
    try:
        mask = fmt["mask"]
    except:
        logging.warning("Failed to parse mask in %s" % (f))
        return

    values = zip(mask, magic, header)
    failed = False
    pos = 0
    for m, g, h in values:
        mask = ord(m)
        bits_to_check = ord(h) & mask
        magic = ord(g)
        if not bits_to_check == magic:
            logging.warning("failed at %d (%x, %x, %x)" %
                            (pos, mask, magic, bits_to_check))
            failed = True
            break
        pos += 1
    return not failed


def check_file_against_all_binfmts(f):
    """
    Check a file against the binfmt masks
    """
    path = os.path.abspath(f)
    logging.info("checking: %s" % (path))
    for b in binfmts:
        if check_file_against_binfmt(b, path):
            print "%s will use %s" % (path, b["interpreter"])
            break


if __name__ == "__main__":
    (opts, args) = parser.parse_args()

    if opts.verbose:
        level = logging.INFO
    else:
        level = logging.WARNING
    logging.basicConfig(level=level,
                        format="%s: %%(message)s"
                        % (os.path.basename(__file__)))

    load_binfmt_masks()

    for f in args:
        check_file_against_all_binfmts(f)
