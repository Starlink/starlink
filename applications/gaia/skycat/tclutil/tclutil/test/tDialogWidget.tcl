#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl
wm withdraw .
set w [DialogWidget .d \
	   -text {This is a test message} \
	   -bitmap warning \
	   -default 1 \
	   -buttons {OK Cancel Help}]
#foreach i [$w config] {puts $i}
puts "Dialog returned: [$w activate]"
exit
