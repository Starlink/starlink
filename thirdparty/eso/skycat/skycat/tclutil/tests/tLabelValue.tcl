source test.tcl

set w [LabelValue .le \
	   -text "Test label:" \
	   -value "test value" \
	  ]
pack $w -anchor w -fill x

