#!/usr/bin/env bash

filename="${@%.*}"
echo "aca llega con $filename"
stack run -- -m "$@" ; 
./bvm/bvm "$filename.byte"

