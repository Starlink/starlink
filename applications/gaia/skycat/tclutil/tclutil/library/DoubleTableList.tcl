# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id"
#
# DoubleTableList.tcl - Widget displaying two TableLists with arrows between them.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual DoubleTableList {
}

# A DoubleTableList is an itcl widget for displaying two TableLists
# with arrows between them for moving items back and forth.  You can
# access the left and right TableLists as itk components "left" and
# "right".

itcl::class util::DoubleTableList {
    inherit util::FrameWidget

    #  create a new object of this class

    constructor {args} {
	# left table component (TableList(n)).
	itk_component add left {
	    util::TableList $w_.left
	} {
	    rename -title -lefttitle leftTitle LeftTitle
	    rename -info -leftinfo leftInfo LeftInfo
	    keep -headings -hscroll -vscroll -exportselection -selectmode -headinglines
	}
	pack $itk_component(left) -side left -fill both -expand 1

	# arrows
	pack [frame $w_.arrows -bd 1m] \
		-side left -fill y -ipadx 0.5m

	pack [label $w_.arrows.fill -text " "] \
		-side top -fill x

	pack [button $w_.arrows.right \
		-bitmap right_arrow \
		-command [code $this move_right]] \
		-side top -fill y -expand 1

	pack [button $w_.arrows.left \
		-bitmap left_arrow \
		-command [code $this move_left]] \
		-side top -fill y -expand 1 -pady 1m

	# right table (see TableList(n))
	itk_component add right {
	    TableList $w_.right
	} {
	    rename -title -righttitle rightTitle RightTitle
	    rename -info -rightinfo rightInfo RightInfo
	    keep -headings -hscroll -vscroll -exportselection -selectmode -headinglines
	}
	pack  $itk_component(right) -side right -fill both -expand 1

	# disable table configure here
	bind $itk_component(left).headbox <1> { }
	bind $itk_component(right).headbox <1> { }

	eval itk_initialize $args
    }


    # move the selected elements from the left to the right TableList

    public method move_right {} {
	$itk_component(right) append_rows [$itk_component(left) remove_selected]
	eval $command
    }


    # move the selected elements from the right to the left TableList

    public method move_left {} {
	$itk_component(left) append_rows [$itk_component(right) remove_selected]
	eval $command
    }


    # move the selected elements from the right to the left TableList

    public method move_up {} {
	$itk_component(left) move_up
	eval $command
    }


    # move the selected elements from the right to the left TableList

    public method move_down {} {
	$itk_component(left) move_down
	eval $command
    }


    # make the TableLists empty

    public method clear {} {
	$itk_component(left) clear
	$itk_component(right) clear
	eval $command
    }


    # -- options --

    # command to evaluate when the list contents change
    public variable command {}


    # flag: if true, also display up and down buttons for moving
    # table items vertically
    itk_option define -updown upDown UpDown 0 {
	if {$itk_option(-updown)} {
	    pack [button $w_.arrows.up \
		      -bitmap up_arrow \
		      -command [code $this move_up]] \
		-side top -fill y -expand 1

	    pack [button $w_.arrows.down \
		-bitmap down_arrow \
		      -command [code $this move_down]] \
		-side top -fill y -expand 1
	} else {
	    catch {destroy $w_.arrows.up}
	    catch {destroy $w_.arrows.down}
	}
    }
}
