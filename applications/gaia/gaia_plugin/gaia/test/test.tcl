# E.S.O. - VLT project
#
# "@(#) $Id: test.tcl,v 1.1 1998/04/06 23:55:38 abrighto Exp $" 
#
# test.tcl - tcl defs to set up environment for test scripts
#
# Usage: source test.tcl
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 08 Apr 97   created

foreach pkg {BLT RTD CAT SKYCAT GAIA} {
    if {[info exists env(${pkg}_LIBRARY)]} {
	lappend auto_path $env(${pkg}_LIBRARY)
    }
}

# add blt namespace to the import list in the current namespace context
import add ::blt

set tk_strictMotif 0
tk appname Gaia

utilPrintErrors

util::setXdefaults
rtd::setXdefaults
cat::setXdefaults
skycat::setXdefaults
gaia::setXdefaults
