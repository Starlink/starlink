#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl

set w [LabelWidget .le \
	   -text "Test label:" \
	   -labelwidth 20 \
	  ]
pack $w -fill x -expand 1


