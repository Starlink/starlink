#source test.tcl

set w [LabelMeter .lm \
	   -text "Test label:" \
	   -value 0.5 \
	   -labelwidth 20 \
	   -valuewidth 20]

pack $w -fill x -expand 1

