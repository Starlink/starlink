# E.S.O. - VLT project
#
# "@(#) $Id: test.tcl,v 1.1.1.1 2006/01/12 16:36:09 abrighto Exp $" 
#
# test.tcl - tcl defs to set up environment for test scripts
#
# Usage: source test.tcl
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 08 Apr 97   created


proc tkerror {msg} {
    global errorInfo
    puts stderr "$errorInfo"
    tkerror__ "error: $msg"
}

# for debugging: print all errors on stderr
catch {tkerror}
rename tkerror tkerror__

#lappend auto_path ../library
package require Cat

set tk_strictMotif 1
tk appname Tclutil

utilPrintErrors

util::setXdefaults
