source test.tcl

set w [Chooser .c \
	   -title "The Chooser" \
	   -dir . \
	   -suffix .tcl \
	   -files [glob ./*.tcl] \
	   -command puts]
pack $w -fill both -expand 1

