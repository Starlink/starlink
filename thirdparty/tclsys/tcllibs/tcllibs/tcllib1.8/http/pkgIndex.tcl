# pkgIndex.tcl for the tcllib http module.
#
# $Id$

if {![package vsatisfies [package provide Tcl] 8.2]} {return}
package ifneeded autoproxy 1.2.1 [list source [file join $dir autoproxy.tcl]]
