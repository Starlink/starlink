source test.tcl

#option add *highlightThickness 0 

set w [ProgressBar .pb \
	   -text "Test label:" \
	   -from 0 -to 100 \
	   -value [set v 0] \
	  ]
pack $w -fill x -expand 1

proc foo {} {
    global w v
    incr v
    $w config -text "${v}% done" -value $v
    if {$v >= 100} {
	exit
    }
    after 100 foo
}

$w look_busy
after 3000 foo
