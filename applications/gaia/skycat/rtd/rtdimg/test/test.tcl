# E.S.O. - VLT project
#
# "@(#) $Id: test.tcl,v 1.4 1997/12/02 10:27:21 abrighto Exp $" 
#
# test.tcl - tcl defs to set up environment for test scripts
#
# Usage: source test.tcl
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 08 Apr 97   created


foreach pkg {RTD ASTROTCL TCLUTIL BLT} {
    if {[info exists env(${pkg}_LIBRARY)]} {
	lappend auto_path $env(${pkg}_LIBRARY)
    }
}

# add blt namespace to the import list in the current namespace context
import add ::blt

set tk_strictMotif 0
tk appname Rtd

# load the Rtd pkg if it is not already loaded
if {[catch {package require Rtd} msg]} {
	puts "error loading Rtd package: $msg"
	exit 1
}

utilPrintErrors
