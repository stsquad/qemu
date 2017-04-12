#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Generate a simple graph of flushes over time
#
# Author: Alex Bennée <alex.bennee@linaro.org>
#

import os
import simpletrace
import argparse
import numpy as np
import matplotlib.pyplot as plt

class TCGAnalyser(simpletrace.Analyzer):
    "An analyser class for examining TCG execution."

    def __init__(self, pcfilter):
        self.pcfilter = pcfilter

        self.timestamps = []
        self.tbs = []
        self.pcs = []

    def exec_tb(self, event, tb, pc):
        "Build array of exec_tb data"
        # Track start and end of time
        timestamp, = event

        self.timestamps.append(timestamp)
        self.tbs.append(tb)
        if self.pcfilter:
            self.pcs.append(pcfilter(pc))
        else:
            self.pcs.append(pc)

#
# Jump Cache Hash
#
TB_JMP_CACHE_BITS = 12
TB_JMP_CACHE_SIZE = (1 << TB_JMP_CACHE_BITS)
TARGET_PAGE_BITS = 12  # this varies
TB_JMP_PAGE_BITS = 12 / 2 # TB_JMP_CACHE_BITS / 2
TB_JMP_PAGE_SIZE = (1 << TB_JMP_PAGE_BITS)
TB_JMP_ADDR_MASK = (TB_JMP_PAGE_SIZE - 1)
TB_JMP_PAGE_MASK = (TB_JMP_CACHE_SIZE - TB_JMP_PAGE_SIZE)

def tb_jmp_cache_hash_func(pc):
    "Return a hash based on PC, just like the same function in tb-hash.h"
    tmp = pc ^ (pc >> (TARGET_PAGE_BITS - TB_JMP_PAGE_BITS))
    return (((tmp >> (TARGET_PAGE_BITS - TB_JMP_PAGE_BITS)) & TB_JMP_PAGE_MASK)
           | (tmp & TB_JMP_ADDR_MASK));

#
# QHT tb_hash_func
# (only not quite, we just use PC without flags)
#
PRIME32_1 = 2654435761
PRIME32_2 = 2246822519
PRIME32_3 = 3266489917
PRIME32_4 = 668265263
PRIME32_5 = 374761393

TB_HASH_XX_SEED = 1

def uint32(x):
    """python does not have 32 bit integer so we need this hack to produce uint32 after bit operations"""
    return x & 0xffffffff

def rol32(x,k):
    return (((x)<<(k)) | (uint32(x)>>(32-(k))))

def tb_hash_func5(pc):
    a0 = pc # phys pc
    b0 = pc # virt pc
    e = 0   # flags

    v1 = uint32(TB_HASH_XX_SEED + PRIME32_1 + PRIME32_2)
    v2 = uint32(TB_HASH_XX_SEED + PRIME32_2)
    v3 = uint32(TB_HASH_XX_SEED + 0)
    v4 = uint32(TB_HASH_XX_SEED - PRIME32_1)
    a = uint32(a0 >> 32)
    b = uint32(a0)
    c = uint32(b0 >> 32)
    d = uint32(b0)
    h32 = 0

    v1 = uint32(v1 + a * PRIME32_2)
    v1 = rol32(v1, 13)
    v1 = uint32(v1 * PRIME32_1)

    v2 = uint32(v2 + b * PRIME32_2)
    v2 = rol32(v2, 13)
    v2 = uint32(v2 * PRIME32_1)

    v3 = uint32(v3 + c * PRIME32_2)
    v3 = rol32(v3, 13)
    v3 = uint32(v3 * PRIME32_1)

    v4 = uint32(v4 + (d * PRIME32_2))
    v4 = rol32(v4, 13)
    v4 = uint32(v4 *PRIME32_1)

    h32 = uint32(rol32(v1, 1) + rol32(v2, 7) + rol32(v3, 12) + rol32(v4, 18))
    h32 = uint32(h32 + 20)

    h32 = uint32(h32 + (e * PRIME32_3))
    h32 = rol32(h32, 17) * PRIME32_4

    h32 ^= h32 >> 15
    h32 = uint32(h32 * PRIME32_2)
    h32 ^= h32 >> 13
    h32 = (h32 * PRIME32_3)
    h32 ^= h32 >> 16

    return h32




def auto_int(x):
    "Let python automatically guess the base of the int"
    return int(x, 0)

def get_args():
    "Grab options"
    parser = argparse.ArgumentParser()
    parser.add_argument("--pcfilter", choices=['raw', 'jumpcache', 'qht'], help="Ignore addresses under this value")
    parser.add_argument("--limit", type=int, default=0, help="Limit plot to first N samples")
    parser.add_argument("--output", "-o", type=str, help="Render plot to file")
    parser.add_argument("--tbs", action="store_true", help="Plot TBs instead of PC addresses")
    parser.add_argument("events", type=str, help='trace file read from')
    parser.add_argument("tracefile", type=str, help='trace file read from')
    return parser.parse_args()

if __name__ == '__main__':
    args = get_args()

    # Gather data from the trace
    pcfilter = None
    if args.pcfilter == "raw":
        plt.ylabel("Execution Address")
    elif args.pcfilter == "jumpcache":
        plt.ylabel("TB Jump Hash")
        pcfilter = tb_jmp_cache_hash_func
    elif args.pcfilter == "qht":
        plt.ylabel("QHT Hash")
        pcfilter = tb_hash_func5


    analyser = TCGAnalyser(pcfilter)
    simpletrace.process(args.events, args.tracefile, analyser, limit=args.limit)

    plt.title("Execution Heatmap")
    plt.xlabel("Timestamp")

    if args.tbs:
        plt.hexbin(analyser.timestamps, analyser.tbs)
        plt.ylabel("TB Address")
    else:
        plt.hexbin(analyser.timestamps, analyser.pcs)

    if args.output:
        plt.savefig(args.output)
    else:
        plt.show()
