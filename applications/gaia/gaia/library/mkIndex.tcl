#!../bin/gaia_wish
#
# mkIndex.tcl - generate a tclIndex file in the current directory
# "@(#) $Id$"
package require Itcl
auto_mkindex . *.tcl *.itk
exit 0
