#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl

wm withdraw .
puts "got: [filename_dialog]"
exit
