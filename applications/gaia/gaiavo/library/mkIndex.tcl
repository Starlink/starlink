#!../bin/gaia_wish
#
# mkIndex.tcl - generate a tclIndex file in the current directory
# "@(#) $Id: mkIndex.tcl 24161 2007-05-01 16:39:34Z pdraper $"
package require Itcl
auto_mkindex . *.tcl *.itk
exit 0
