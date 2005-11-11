# E.S.O. - VLT project
#
# "@(#) $Id: astrotcl.tcl,v 1.2 2005/02/02 01:43:04 brighton Exp $" 
#
# astrotcl.tcl - example application script for astrotcl package
#
# Usage: astrotcl ?options? (using link to astrotcl.sh script)
#    or: astrotcl_wish astrotcl.tcl ?options?
#
# This is just an example. There are currently no options.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 28 Nov 97   created


set astrotcl_usage "
Usage: astrotcl ?-option value ...?
"

# The startup script astrotcl.sh(.in) sets these environment variables to
# point to the tcl/tk script dirs. We need to know this mainly when
# loading packages dynamically from shared libraries, since the
# auto_path tcl variable determines the search path for the pkgIndex.tcl
# files used to load the shared libraries at run time.
foreach pkg {BLT TCLUTIL ASTROTCL} {
    if {[info exists env(${pkg}_LIBRARY)]} {
	lappend auto_path $env(${pkg}_LIBRARY)
    }
}

set tk_strictMotif 0
tk appname Astrotcl

# load the required packages, if it is not already loaded
foreach pkg {Tclutil Astrotcl} {
    if {[catch {package require $pkg} msg]} {
	puts "error loading $pkg package: $msg"
	exit 1
    }
}

utilPrintErrors

# Start the application class Astrotcl:
# util::TopLevelWidget::start Astrotcl "-file" "$astrotcl_usage"

