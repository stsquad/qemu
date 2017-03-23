#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Dump the contents of a recorded execution stream
#
#  Copyright (c) 2017 Alex Bennée <alex.bennee@linaro.org>
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, see <http://www.gnu.org/licenses/>.

import argparse
import struct
from collections import namedtuple

# Simple read functions that mirror replay-internal.c
# The file-stream is big-endian and manually written out a byte at a time.

def read_byte(fin):
    "Read a single byte"
    return struct.unpack('>B', fin.read(1))

def read_word(fin):
    "Read a 16 bit word"
    return struct.unpack('>H', fin.read(2))

def read_dword(fin):
    "Read a 32 bit word"
    return struct.unpack('>I', fin.read(4))

def read_qword(fin):
    "Read a 64 bit word"
    return struct.unpack('>Q', fin.read(8))

# Generic decoder structure
Decoder = namedtuple("Decoder", "event name decode_function")

def call_decode(table, index, dumpfile):
    "Search decode table for next step"
    decoder = [d for d in table if d.event == index]
    if not decoder:
        print "Couldn't decode: %d" % (index)
        return False
    else:
        decoder.decode_function(decoder.name, dumpfile)
        return True

def decode_instruction(name, dumpfile):
    print "INS"

def decode_interrupt(name, dumpfile):
    print "IRQ"

def decode_clock(name, dumpfile):
    print "CLOCK"

def decode_checkpoint(name, dumpfile):
    checkpoint_event = dumpfile.read(1)



# See replay/replay-internal.h
event_decode_table = [ Decoder(0, "EVENT_INSTRUCTION", decode_instruction),
                       Decoder(1, "EVENT_INTERRUPT", decode_interrupt),
                       Decoder(10, "EVENT_CLOCK_HOST", decode_clock),
                       Decoder(11, "EVENT_CLOCK_VIRTUAL_RT", decode_clock),
                       Decoder(12, "EVENT_CHECKPOINT_CLOCK_WARP_START", decode_checkpoint)
]

def parse_arguments():
    "Grab arguments for script"
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--file", help='record/replay dump to read from', required=True)
    return parser.parse_args()

def decode_file(filename):
    "Decode a record/replay dump"
    dumpfile = open(filename, "rb")

    # read and throwaway the header
    version = read_dword(dumpfile)
    junk = read_word(dumpfile)

    print "HEADER: version 0x%x" % (version)

    try:
        event = read_byte(dumpfile)
        while event != "":
            if not call_decode(event_decode_table, event, dumpfile):
                break;
    finally:
        dumpfile.close()


if __name__ == "__main__":
    args = parse_arguments()
    decode_file(args.file)
