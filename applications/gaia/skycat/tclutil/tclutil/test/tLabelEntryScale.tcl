source test.tcl

proc foo {arg} {puts "foo: $arg"}

set w [LabelEntryScale .les \
	   -text "Test label:" \
	   -from 10 \
	   -to 120 \
	   -value 25 \
	   -valuewidth 4 \
           -validate real \
	   -relief sunken \
	   -borderwidth 3 \
	   -command puts \
	   -entrycommand foo \
	   -validate real \
	   -orient horizontal \
	  ]
pack $w -fill x -expand 1

