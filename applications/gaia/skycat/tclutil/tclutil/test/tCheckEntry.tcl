#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl

set w [CheckEntry .ce -text "Test label:"]
pack $w -fill x -expand 1

