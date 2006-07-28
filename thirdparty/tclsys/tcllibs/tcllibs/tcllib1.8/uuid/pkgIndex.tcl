# pkgIndex.tcl - 
#
# uuid package index file
#
# $Id$

if {![package vsatisfies [package provide Tcl] 8.2]} {return}
package ifneeded uuid 1.0.1 [list source [file join $dir uuid.tcl]]
