#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"


source test.tcl

wm withdraw .
set w [ChoiceDialog .d \
	   -text {Please choose...} \
	   -cols 2 \
	   -messagewidth 3i \
	   -choice {One Two Three Four Five Six Seven Eight} \
	   -value {One}]
puts "Dialog returned: [$w activate]"
exit
