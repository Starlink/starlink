#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/tclutil.sh "$0" "$@"

source test.tcl

TopLevelWidget .t
.t add_menubar
.t add_menubutton File
.t add_menubutton View
.t add_menubutton Edit
.t make_short_help
