#!/usr/bin/env python2 -B
#
# Travis YAML config parser
#
# Copyright (c) 2016 Red Hat Inc.
#
# Authors:
#  Fam Zheng <famz@redhat.com>
#
# This work is licensed under the terms of the GNU GPL, version 2
# or (at your option) any later version. See the COPYING file in
# the top-level directory.

import sys
import yaml
import json
import itertools

def load_yaml(fname):
    return yaml.load(open(sys.argv[1], "r").read())

def conf_iter(conf):
    def env_list(e):
        return e if type(e) is list else [e]
    global_env = conf["env"]["global"]
    for c in conf["matrix"]["include"]:
        a = { "env": global_env + env_list(c["env"]),
              "compiler": c["compiler"]
            }
        yield a
    for c in itertools.product(conf["compiler"],
                               conf["env"]["matrix"]):
        a = { "env": global_env + env_list(c[1]),
              "compiler": c[0]
            }
        yield a

def main():
    if len(sys.argv) < 2:
        sys.stderr.write("Usage: %s <travis-file>\n" % sys.argv[0])
        return 1
    conf = load_yaml(sys.argv[1])
    for config in conf_iter(conf):
        print "("
        print "\n".join(config["env"])
        print "CC=" + config["compiler"]
        print "\n".join(conf["before_script"])
        print "\n".join(conf["script"])
        print ")"
    return 0

if __name__ == "__main__":
    sys.exit(main())
