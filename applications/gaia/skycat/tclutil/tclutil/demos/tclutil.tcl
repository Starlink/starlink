# E.S.O. - VLT project
#
# "@(#) $Id: tclutil.tcl,v 1.4 1998/10/28 17:45:45 abrighto Exp $" 
#
# tclutil.tcl - example application script for tclutil package
#
# Usage: tclutil ?options? (using link to tclutil.sh script)
#    or: tclutil_wish tclutil.tcl ?options?
#
# This is just an example. There are currently no options.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 28 Nov 97   created


set tclutil_usage "
Usage: tclutil ?-option value ...?
"

# The startup script tclutil.sh(.in) sets these environment variables to
# point to the tcl/tk script dirs. We need to know this mainly when
# loading packages dynamically from shared libraries, since the
# auto_path tcl variable determines the search path for the pkgIndex.tcl
# files used to load the shared libraries at run time.
foreach pkg {BLT TCLUTIL} {
    if {[info exists env(${pkg}_LIBRARY)]} {
	lappend auto_path $env(${pkg}_LIBRARY)
    }
}

set tk_strictMotif 0
tk appname Tclutil

# load the Tclutil pkg if it is not already loaded
if {[catch {package require Tclutil} msg]} {
	puts "error loading Tclutil package: $msg"
	exit 1
}

utilPrintErrors

# Start the application class Tclutil:
# util::TopLevelWidget::start Tclutil "-file" "$tclutil_usage"

