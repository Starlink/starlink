#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl

set w [LabelNumber .ln \
	   -text "Test label:" \
	   -value 42 \
	   -labelwidth 20 \
	   -labelfont -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
	   -valuefont 8x13 \
	   -relief sunken \
	   -borderwidth 3 \
	   -command puts \
	   -orient horizontal \
	  ]
pack $w -fill x -expand 1

