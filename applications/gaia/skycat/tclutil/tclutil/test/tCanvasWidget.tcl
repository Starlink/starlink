#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl

set w [CanvasWidget .c]
pack $w -fill x -expand 1

[$w component canvas] create text 100 100 -text "CanvasWidget" 


