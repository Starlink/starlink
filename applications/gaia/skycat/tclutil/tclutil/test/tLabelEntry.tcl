source test.tcl

set myvar "lkjlkjljk"

set w [LabelEntry .le \
	   -text "Test label:" \
	   -value 42 \
	   -labelwidth 20 \
	   -valuefont 8x13 \
	   -relief sunken \
	   -borderwidth 3 \
	   -command puts \
	   -textvariable "myvar" \
	  ]
pack $w -fill x -expand 1

# note: there seems to be a bug in itcl2.0 that has to do with the
# -textvariable option...

#$w config -textvariable [$w cget -textvariable]
set myvar {set from variable}
puts "1. var = [$w configure -textvariable] ($myvar)"

proc foo {w} {
    global ::myvar
    set myvar {set from variable again}
    puts "2. var = [$w configure -textvariable] ($myvar)"
}

after 1000 "foo $w"
