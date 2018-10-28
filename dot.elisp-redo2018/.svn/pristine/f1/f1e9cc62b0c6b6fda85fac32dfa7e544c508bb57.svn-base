#!/usr/bin/env python

"""Clean up a gnumake .depend file for the msvc_function.el routines."""

import sys
import os
import re

seps = " |\t"

data = sys.stdin.read()

files = [x for x in re.split(seps, data) if not (x.endswith(".o:")
                                                 or x.endswith(".obj:")
                                                 or x.startswith("/usr/include")
                                                 or x.startswith("/usr/local/embedded-arm-cross"))]

all = {}
for f in files:
    f = f.strip()
    if not (f=='\\' or len(f)==0):
        all[f] = f

for f in all.keys():
    print f
    
