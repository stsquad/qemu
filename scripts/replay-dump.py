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

# This mirrors some of the global replay state which some of the
# stream loading refers to.

class ReplayState(object):
    def __init__(self):
        self.event = -1
    def set_event(self, ev):
        self.event = ev

replay_state = ReplayState()

# Simple read functions that mirror replay-internal.c
# The file-stream is big-endian and manually written out a byte at a time.

def read_byte(fin):
    "Read a single byte"
    return struct.unpack('>B', fin.read(1))[0]

def read_event(fin):
    "Read a single byte event, but save some state"
    replay_state.set_event(read_byte(fin))
    return replay_state.event

def read_word(fin):
    "Read a 16 bit word"
    return struct.unpack('>H', fin.read(2))[0]

def read_dword(fin):
    "Read a 32 bit word"
    return struct.unpack('>I', fin.read(4))[0]

def read_qword(fin):
    "Read a 64 bit word"
    return struct.unpack('>Q', fin.read(8))[0]

# Generic decoder structure
Decoder = namedtuple("Decoder", "eid name fn")

def call_decode(table, index, dumpfile):
    "Search decode table for next step"
    decoder = next((d for d in table if d.eid == index), None)
    if not decoder:
        print "Could not decode index: %d" % (index)
        print "Entry is: %s" % (decoder)
        print "Decode Table is:\n%s" % (table)
        return False
    else:
        return decoder.fn(decoder.name, dumpfile)

# Decoders for each event type

def decode_unimp(name, _unused_dumpfile):
    "Unimplimented decoder, will trigger exit"
    print "%s not handled - will now stop" % (name)
    return False

def decode_instruction(name, dumpfile):
    print "INS"

def decode_interrupt(name, dumpfile):
    print "IRQ"

def decode_clock(name, dumpfile):
    print "CLOCK"


# Checkpoint decoder
def swallow_async_qword(name, dumpfile):
    "Swallow a qword of data without looking at it"
    read_qword(dumpfile)
    print "%s" % (name)
    return True

async_decode_table = [ Decoder(0, "REPLAY_ASYNC_EVENT_BH", swallow_async_qword),
                       Decoder(1, "REPLAY_ASYNC_INPUT", decode_unimp),
                       Decoder(2, "REPLAY_ASYNC_INPUT_SYNC", decode_unimp),
                       Decoder(3, "REPLAY_ASYNC_CHAR_READ", decode_unimp),
                       Decoder(4, "REPLAY_ASYNC_EVENT_BLOCK", decode_unimp),
                       Decoder(5, "REPLAY_ASYNC_EVENT_NET", decode_unimp),
]

def decode_checkpoint(name, dumpfile):
    """Decode a checkpoint.

    Checkpoints contain a series of async events with their own specific data.
    """
    ev_async = read_byte(dumpfile)
    if ev_async != 3:
        print "Confused event %d not EVENT_ASYNC" % (ev_async)
        return False
    checkpoint = read_byte(dumpfile)
    async_event = read_byte(dumpfile)
    print "%s: %d, %d, %d" % (name, ev_async, checkpoint, async_event)
    return call_decode(async_decode_table, async_event, dumpfile)

def decode_checkpoint_init(name, dumpfile):
    print "%s" % (name)
    return True

# See replay/replay-internal.h
event_decode_table = [ Decoder(0, "EVENT_INSTRUCTION", decode_instruction),
                       Decoder(1, "EVENT_INTERRUPT", decode_interrupt),
                       Decoder(10, "EVENT_CLOCK_HOST", decode_clock),
                       Decoder(11, "EVENT_CLOCK_VIRTUAL_RT", decode_clock),
                       Decoder(12, "EVENT_CP_CLOCK_WARP_START", decode_checkpoint),
                       Decoder(13, "EVENT_CP_CLOCK_WARP_ACCOUNT", decode_checkpoint),
                       Decoder(14, "EVENT_CP_RESET_REQUESTED", decode_checkpoint),
                       Decoder(15, "EVENT_CP_SUSPEND_REQUESTED", decode_checkpoint),
                       Decoder(16, "EVENT_CP_CLOCK_VIRTUAL", decode_checkpoint),
                       Decoder(17, "EVENT_CP_CLOCK_HOST", decode_checkpoint),
                       Decoder(18, "EVENT_CP_CLOCK_VIRTUAL_RT", decode_checkpoint),
                       Decoder(19, "EVENT_CP_INIT", decode_checkpoint_init),
                       Decoder(20, "EVENT_CP_RESET", decode_checkpoint),
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
    junk = read_qword(dumpfile)

    print "HEADER: version 0x%x" % (version)

    try:
        decode_ok = True
        while decode_ok:
            event = read_event(dumpfile)
            decode_ok = call_decode(event_decode_table, event, dumpfile)
    finally:
        dumpfile.close()

if __name__ == "__main__":
    args = parse_arguments()
    decode_file(args.file)
