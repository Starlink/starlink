#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl

set w [LabelValue .le \
	   -text "Test label:" \
	   -value "test value" \
	  ]
pack $w -anchor w -fill x

