#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl
wm withdraw .
set w [InputDialog .d \
	   -text {Please type something...} \
	   -bitmap warning \
	   -modal 1 \
	   -buttons {OK Cancel Help} \
	  ]
puts "Dialog returned: [$w activate]"
exit
