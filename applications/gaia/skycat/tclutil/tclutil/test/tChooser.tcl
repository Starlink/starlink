#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl

set w [Chooser .c \
	   -title "The Chooser" \
	   -dir . \
	   -suffix .tcl \
	   -files [glob ./*.tcl] \
	   -command puts]
pack $w -fill both -expand 1

