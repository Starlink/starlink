set n 0
foreach testfile [lsort [glob t*.tcl]] {
    if {"$testfile" != "test.tcl"} {
	incr n
	set name [string range [file rootname $testfile] 1 end]
	pack \
	    [button .b$n \
		 -text $name \
		 -command "exec ../bin/tclutil_wish $testfile -geometry +300+300 &"] \
	    -side top -fill x
    }
}
