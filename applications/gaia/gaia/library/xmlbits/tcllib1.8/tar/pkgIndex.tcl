if {![package vsatisfies [package provide Tcl] 8.2]} {return}
package ifneeded tar 0.1 [list source [file join $dir tar.tcl]]
