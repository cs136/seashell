#!/usr/bin/python3

import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument('-o', '--output')
known_args, unknown_args = parser.parse_known_args()

files_to_concat = []
if "--" in unknown_args:
    index = unknown_args.index("--")
    files_to_concat = unknown_args[(1+index):]
else:
    for arg in unknown_args:
        if ((not arg) or arg[0] == '-'):
            continue
        else:
            files_to_concat.append(arg)

print("Concatenating these %d files to %s:" % \
    (len(files_to_concat), known_args.output or "stdout"), file=sys.stderr)
for afile in files_to_concat:
    print("%s" % afile, file=sys.stderr)

if known_args.output:
    output_file = open(known_args.output, 'wb')
else:
    output_file = sys.stdout.buffer

first = True
for afile in files_to_concat:
    with open(afile, 'rb') as f:
        if not first:
            output_file.write(b"\n\n")
        output_file.write(f.read())
    first = False

if known_args.output:
    output_file.close()
