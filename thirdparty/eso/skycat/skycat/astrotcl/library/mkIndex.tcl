#!../bin/astrotcl_wish
#
# mkIndex.tcl - generate a tclIndex file in the current directory
# "@(#) $Id: mkIndex.tcl,v 1.2 2005/02/02 01:43:04 brighton Exp $"

package require Itcl
auto_mkindex . *.tcl
exit 0
