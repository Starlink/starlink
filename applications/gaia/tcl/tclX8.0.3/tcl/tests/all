# This file contains a top-level script to run all of the Extended Tcl
# tests.  Execute it by invoking "source all" when running tclTest
# in this directory.

global errorInfo

foreach i [concat [lsort [glob *.test]] [lsort [glob -nocomplain compat/*.test]]] {
    puts $i
    if [catch {source $i} msg] {
        puts [replicate "=" 75]
        puts "Uncaught error in $i: $msg"
        puts $errorInfo
        puts [replicate "=" 75]
    }
}

if !$tcl_interactive {
    exit
}


