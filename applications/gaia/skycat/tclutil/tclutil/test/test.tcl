# E.S.O. - VLT project
#
# "@(#) $Id: test.tcl,v 1.4 1998/10/28 17:47:28 abrighto Exp $" 
#
# test.tcl - tcl defs to set up environment for test scripts
#
# Usage: source test.tcl
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 08 Apr 97   created

foreach pkg {BLT} {
    if {[info exists env(${pkg}_LIBRARY)]} {
	lappend auto_path $env(${pkg}_LIBRARY)
    }
}

lappend auto_path ../library
package require Tclutil

set tk_strictMotif 1
tk appname Tclutil

utilPrintErrors

util::setXdefaults
