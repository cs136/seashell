#!/bin/bash
export PATH=/bin:/usr/bin
out="$1"
shift
sources="$@"

clang -Wall -o $out $sources

