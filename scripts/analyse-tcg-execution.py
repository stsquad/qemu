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

    def __init__(self, base_address):
        self.base = base_address

        self.timestamps = []
        self.tbs = []
        self.pcs = []

    def exec_tb(self, event, tb, pc):
        "Build array of exec_tb data"
        # Track start and end of time
        timestamp, = event

        if self.base and pc < self.base:
            return
        else:
            self.timestamps.append(timestamp)
            self.tbs.append(tb)
            self.pcs.append(pc)

def auto_int(x):
    "Let python automatically guess the base of the int"
    return int(x, 0)

def get_args():
    "Grab options"
    parser = argparse.ArgumentParser()
    parser.add_argument("--base-address", type=auto_int, default=None, help="Ignore addresses under this value")
    parser.add_argument("--limit", type=int, default=0, help="Limit plot to first N samples")
    parser.add_argument("--output", "-o", type=str, help="Render plot to file")
    parser.add_argument("--tbs", action="store_true", help="Plot TBs instead of PC addresses")
    parser.add_argument("events", type=str, help='trace file read from')
    parser.add_argument("tracefile", type=str, help='trace file read from')
    return parser.parse_args()

if __name__ == '__main__':
    args = get_args()

    # Gather data from the trace
    analyser = TCGAnalyser(args.base_address)
    simpletrace.process(args.events, args.tracefile, analyser, limit=args.limit)

    plt.title("Execution Heatmap")
    plt.xlabel("Timestamp")

    if args.tbs:
        plt.hexbin(analyser.timestamps, analyser.tbs)
        plt.ylabel("TB Address")
    else:
        plt.hexbin(analyser.timestamps, analyser.pcs)
        plt.ylabel("Execution Address")

    if args.output:
        plt.savefig(args.output)
    else:
        plt.show()
