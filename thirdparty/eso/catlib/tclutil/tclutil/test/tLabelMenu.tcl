source test.tcl

set w [LabelMenu .lm \
	   -text "Test label:" \
	   -labelfont -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
	   -valuefont 8x13 \
	   -relief raised \
	   -borderwidth 3 \
	   -orient horizontal \
	  ]
pack $w -fill x -expand 1


proc foo {args} {
    global w
    puts "$args"
    puts "get = [$w get]"
}

$w clear
$w add -label "one" -command "foo one"
$w add -label "two" -command "foo two"
$w add -label "three" -command "foo three"

$w config -value two
