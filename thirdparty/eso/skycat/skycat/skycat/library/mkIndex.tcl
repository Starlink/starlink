#!../bin/skycat_wish
#
# mkIndex.tcl - generate a tclIndex file in the current directory
# "@(#) $Id: mkIndex.tcl,v 1.1.1.1 2002/04/04 20:11:54 brighton Exp $"

package require Itcl
auto_mkindex . *.tcl
exit 0
