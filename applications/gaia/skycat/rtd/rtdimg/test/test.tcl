# E.S.O. - VLT project
#
# "@(#) $Id: test.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $" 
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
proc tkerror {msg} {
    global errorInfo
    puts stderr "$errorInfo"
    tkerror__ "error: $msg"
}

# for debugging: print all errors on stderr
catch {tkerror}
rename tkerror tkerror__

# add blt namespace to the import list in the current namespace context
#import add ::blt

set tk_strictMotif 0
tk appname Rtd

# load the Rtd pkg if it is not already loaded
if {[catch {package require Rtd} msg]} {
	puts "error loading Rtd package: $msg"
	exit 1
}
rtd::setXdefaults

utilPrintErrors
