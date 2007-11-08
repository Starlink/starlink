#!../bin/gaia_wish
#
# mkIndex.tcl - generate a tclIndex file in the current directory
# "@(#) $Id: mkIndex.tcl,v 1.3 2007/05/01 16:39:34 pdraper Exp $"
package require Itcl
auto_mkindex . *.tcl *.itk
exit 0
