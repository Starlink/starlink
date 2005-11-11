set n 0
foreach testfile [glob t*.tcl] {
    if {"$testfile" != "test.tcl"} {
	incr n
	set name [string range [file rootname $testfile] 1 end]
	pack [button .b$n -text $name -command "exec ../bin/rtdimage_wish $testfile &"] \
	    -side top -fill x
    }
}
