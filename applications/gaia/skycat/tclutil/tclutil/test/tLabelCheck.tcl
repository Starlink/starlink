#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl

set w [LabelCheck .lc \
		  -text {Test Label:} \
		  -rows 3 \
		  -choice {one two three four five six seven eight nine ten} \
		  -value {three six nine} \
		  -command puts]
pack $w -fill x -expand 1

