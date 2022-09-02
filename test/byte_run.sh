#!/usr/bin/env bash

echo "holaa"
filename="${@%.*}"
stack run -- -m "$@" ; 
# ./bvm/bvm "$filename.byte"
stack run -- -r "$filename.byte"