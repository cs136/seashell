#!/bin/bash
out="$1"
shift
sources="$@"

clang -Wall -o $out $sources

