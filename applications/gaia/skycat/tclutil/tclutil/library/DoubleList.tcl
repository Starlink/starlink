# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: DoubleList.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $"
#
# DoubleList.tcl - Widget displaying two lists with arrows between them
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created


itk::usual DoubleList {
}


# A DoubleList is an itcl widget for displaying two lists with arrows between
# them for moving items back and forth.

itcl::class util::DoubleList {
    inherit util::FrameWidget


    #  create a new object of this class

    constructor {args} {
	# left list component (see ListboxWidget(n))
	itk_component add left {
	    util::ListboxWidget $w_.left
	} {
	    rename -title -lefttitle leftTitle LeftTitle
	    keep -hscroll -vscroll -exportselection -selectmode -width -height
	}
	pack $itk_component(left) -side left -fill both -expand 1

	# arrows
	pack [frame $w_.arrows -bd 1m] \
		-side left -fill y -ipadx 0.5m

	pack [label $w_.arrows.fill -text " " ] \
		-side top -fill x

	pack [button $w_.arrows.right \
		-bitmap right_arrow \
		-command [code $this move_right]] \
		-side top -fill y -expand 1

	pack [button $w_.arrows.left \
		-bitmap left_arrow \
		-command [code $this move_left]] \
		-side top -fill y -expand 1 -pady 1m

	# right list component (see ListboxWidget(n))
	itk_component add right {
	    util::ListboxWidget $w_.right
	} {
	    rename -title -righttitle rightTitle RightTitle
	    keep -hscroll -vscroll -exportselection -selectmode -width -height
	}
	pack $itk_component(right) -side right -fill both -expand 1

	eval itk_initialize $args
    }

    # move the selected elements from the left to the right list

    public method move_right {} {
	$itk_component(right) append_list [$itk_component(left) remove_selected]
    }


    # move the selected elements from the right to the left list

    public method move_left {} {
	$itk_component(left) append_list [$itk_component(right) remove_selected]
    }

    # move the selected elements from the right to the left list

    public method move_up {} {
	$itk_component(left) move_up
    }


    # move the selected elements from the right to the left list

    public method move_down {} {
	$itk_component(left) move_down
    }


    # make the lists empty

    public method clear {} {
	$itk_component(left) clear
	$itk_component(right) clear
    }


    # -- options --

    # flag: if true, also display up and down buttons for moving
    # list items vertically
    itk_option define -updown upDown UpDown 0 {
	if {$itk_option(-updown)} {
	    pack [button $w_.arrows.up \
		-bitmap up_arrow \
		-command [code $this move_up]] \
		-side top -fill y -expand 1

	    pack [button $w_.arrows.down \
		-bitmap down_arrow \
		-command [code $this move_down]] \
		-side top -fill y -expand 1 -pady 1m
	} else {
	    catch {destroy $w_.arrows.up}
	    catch {destroy $w_.arrows.down}
	}
    }
}

