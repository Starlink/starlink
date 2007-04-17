source test.tcl

set w [LabelCheck .lc \
		  -text {Test Label:} \
		  -rows 3 \
		  -choice {one two three four five six seven eight nine ten} \
		  -value {three six nine} \
		  -command puts]
pack $w -fill x -expand 1

