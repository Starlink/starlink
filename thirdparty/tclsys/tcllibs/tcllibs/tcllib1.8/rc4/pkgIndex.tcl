# pkgIndex.tcl - 
#
# RC4 package index file
#
# This package has been tested with tcl 8.2.3 and above.
#
# $Id$

if {![package vsatisfies [package provide Tcl] 8.2]} {
    # PRAGMA: returnok
    return
}
package ifneeded rc4 1.0.1 [list source [file join $dir rc4.tcl]]
