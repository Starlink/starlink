#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl
wm withdraw .
set w [TextDialog .d \
	   -text {This is a test message} \
	   -contents [exec cat ../library/TextDialog.tcl] \
	  ]
puts "Dialog returned: [$w activate]"
exit
