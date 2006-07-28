if {[package vsatisfies [package provide Tcl] 8.5]} {
    package ifneeded snit 2.0 \
        [list source [file join $dir snit2.tcl]]
}

if {[package vsatisfies [package provide Tcl] 8.4]} {
    package ifneeded snit 1.1 \
        [list source [file join $dir snit.tcl]]
} elseif {[package vsatisfies [package provide Tcl] 8.3]} {
    package ifneeded snit 1.1 \
        [list source [file join $dir snit83.tcl]]
}
