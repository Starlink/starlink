source test.tcl

itcl::class PwDemo {
    inherit util::TopLevelWidget
    
    # constructor: create a toplevel window for the demo

    constructor {args} {
	wm title $w_ "DoubleTableList /etc/passwd Demo"

	# create the DoubleTableList
	itk_component add dtlist {
	    DoubleTableList $w_.dtlist \
		-lefttitle "Keep" \
		-righttitle "Don't Keep" \
		-hscroll 1 \
		-updown 1 \
		-selectmode extended
	}
	pack $itk_component(dtlist) -fill both -expand 1

	# add a row of buttons at botton
	itk_component add btns {
	    ButtonFrame $w_.btns -ok_cmd exit
	}
	pack $itk_component(btns) -side bottom -fill x

	# note that the TableList widget sets gridded geometry on
	wm geometry $w_ 60x20

	eval itk_initialize $args
    }


    # -- public variables (also program options) -- 

    # if true, use ypcat, otherwise use /etc/passwd
    itk_option define -use_yp use_yp Use_yp 0 {
	if {$itk_option(-use_yp)} {
	    set fd [open "|ypcat passwd"]
	} else {
	    set fd [open /etc/passwd]
	}
	while {[gets $fd buf] != -1} {
	    lappend info_ [split $buf :]
	}	

	$itk_component(dtlist) config \
	    -headings $headings_ \
	    -leftinfo $info_ \
	    -rightinfo {}
    }

    
    # -- protected variables --
    
    # table headings
    protected variable headings_ {User Password UID GID Name Home Shell} 

    # data: list of table rows, where each row is a list strings
    protected variable info_ {}
}


# Start the demo:
# Note that "start" is a "member" proc in the TopLevelWidget class.
# It creates an instance of the above class and handles options and
# error checking.

util::TopLevelWidget::start PwDemo

