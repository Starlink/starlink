#!/bin/sh
# The next line restarts using tcl \
exec tcl $0 ${1+"$@"}

# List here all packages used in this file
package require Tcl 8.0
package require Tclx 8.0

# Here the Tcl-script starts:
puts stdout "Hello, World"
