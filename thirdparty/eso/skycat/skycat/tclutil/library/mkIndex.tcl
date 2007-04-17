#! /bin/sh
# -*-tcl-*-
# The next line is executed by /bin/sh, but not Tcl \
exec tclsh $0 ${1+"$@"}

package require Itcl
auto_mkindex . *.tcl
exit 0
