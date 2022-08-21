#!/usr/bin/env bash

filename="${@%.*}"
echo "${filename}"
stack run -- -m "$@" ; 
./bvm/bvm "$filename.byte"
