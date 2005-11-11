# E.S.O. - VLT project
#
# "@(#) $Id: test.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $" 
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

proc tkerror {msg} {
    global errorInfo
    puts stderr "$errorInfo"
    tkerror__ "error: $msg"
}

# for debugging: print all errors on stderr
catch {tkerror}
rename tkerror tkerror__

lappend auto_path ../library
package require Tclutil

set tk_strictMotif 1
tk appname Tclutil

utilPrintErrors

util::setXdefaults
