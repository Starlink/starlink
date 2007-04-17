# Test the DoubleList widget 
#


source test.tcl

itcl::class DLDemo {
    inherit util::TopLevelWidget
    
    # constructor: create a toplevel window for the demo

    constructor {args} {
	wm title $w_ "DoubleList Demo"

	# create the DoubleTableList
	itk_component add dlist {
	    DoubleList $w_.dtlist \
		-lefttitle "Keep" \
		-righttitle "Don't Keep" \
		-hscroll 1 \
		-updown 1 \
		-selectmode extended
	}
	pack $itk_component(dlist) -fill both -expand 1

	[$itk_component(dlist) component left] set_contents \
	    {one two three four five six}
	[$itk_component(dlist) component right] set_contents \
	    {seven eight nine ten}

	# add a row of buttons at botton
	itk_component add btns {
	    ButtonFrame $w_.btns -ok_cmd exit
	}
	pack $itk_component(btns) -side bottom -fill x

	eval itk_initialize $args
    }
}


# Start the demo:
# Note that "start" is a "member" proc in the TopLevelWidget class.
# It creates an instance of the above class and handles options and
# error checking.

util::TopLevelWidget::start DLDemo

