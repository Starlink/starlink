source test.tcl

set w [LabelChoice .lc \
	   -text "Test label:" \
	   -choice {one two three four five six seven eight nine ten eleven twelve {a b c} {d e f}} \
	   -value three \
	   -valuewidth 20 \
	   -labelfont -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
	   -valuefont 8x13 \
	   -relief groove \
	   -anchor w \
	   -borderwidth 3 \
	   -variable xxx \
	   -command puts \
	   -orient vertical \
	  ]
pack $w -fill both -expand 1

#after 2000 "$w clear; $w config -choice {{Take it} {Leave it}}"
#after 4000 "$w config -orient horizontal"
