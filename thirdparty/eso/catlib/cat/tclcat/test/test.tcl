# E.S.O. - VLT project
#
# "@(#) $Id: test.tcl,v 1.2 2001/08/27 10:10:39 abrighto Exp $" 
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
package require Cat

set tk_strictMotif 1
tk appname Cat

utilPrintErrors

util::setXdefaults
