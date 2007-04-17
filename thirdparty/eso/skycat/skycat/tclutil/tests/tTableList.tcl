source test.tcl

itcl::class PwDemo {
    inherit util::TopLevelWidget
    
    # constructor: create a toplevel window for the demo

    constructor {args} {
	wm title $w_ "TableList /etc/passwd Demo"
	
	# add a menubar
	add_menubar
	set m [add_menubutton File]
	#$m add command -label "Sort..." -command [code $this sort]
	$m add command -label "Exit" -command [code $this quit]

	# create the TableList
	itk_component add tlist {
	    TableList $w_.tlist \
		-hscroll 1 \
		-menubar $itk_component(menubar) \
		-title "Contents of Passwd File" \
		-selectmode extended
	}
	pack $itk_component(tlist) -fill both -expand 1

	# add a row of buttons at botton
	itk_component add btns {
	    ButtonFrame $w_.btns -ok_cmd exit
	}
	pack $itk_component(btns) -side bottom -fill x

	# note that the TableList widget sets gridded geometry on
	wm geometry $w_ 60x20

	eval itk_initialize $args
    }

    
    # pop up a dialog to sort the list

    method sort {} {
	$itk_component(tlist) sort_dialog
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

	$itk_component(tlist) config \
	    -headings $headings_ \
	    -info $info_ 
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

