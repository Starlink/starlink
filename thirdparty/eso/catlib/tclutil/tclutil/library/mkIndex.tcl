#!../bin/tclutil_wish
#
# mkIndex.tcl - generate a tclIndex file in the current directory
# "@(#) $Id: mkIndex.tcl,v 1.4 2001/08/27 10:11:12 abrighto Exp $"

package require Itcl
auto_mkindex . *.tcl
exit 0
