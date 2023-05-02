#!/usr/bin/env bash

filename="${@%.*}"
stack run -- -m "$@" ; 
# ./bvm/bvm "$filename.byte"
stack run -- -r "$filename.byte"