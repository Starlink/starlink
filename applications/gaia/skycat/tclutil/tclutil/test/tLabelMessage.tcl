#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl

set w [LabelMessage .lm \
	   -text "Test label:" \
	   -value "Some Message..." \
	   -labelwidth 10 \
	   -labelfont -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
	   -valuefont 8x13 \
	   -relief sunken \
	   -borderwidth 3 \
	   -aspect 1000 \
	   -orient horizontal \
	  ]
pack $w -fill x -expand 1

