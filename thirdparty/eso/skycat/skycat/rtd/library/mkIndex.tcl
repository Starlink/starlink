#!../bin/rtdimage_wish
#
# mkIndex.tcl - generate a tclIndex file in the current directory
# "@(#) $Id: mkIndex.tcl,v 1.1.1.1 2006/01/12 16:38:24 abrighto Exp $"

package require Itcl
auto_mkindex . *.tcl
exit 0
