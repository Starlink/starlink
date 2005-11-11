# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: ListboxWidget.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $"
#
# ListboxWidget.tcl - Widget for scrolled lists, based on the Tk listbox
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual ListboxWidget {}

# ListboxWidget is an itcl widget for scrolled lists, based on the Tk listbox.

itcl::class util::ListboxWidget {
    inherit util::FrameWidget

    #  create a new object of this class

    constructor {args} {
	pack [frame $w_.tf] -side top -fill x
	
	# optional title
	itk_component add title {
	    label $w_.tf.title
	} {
	    usual
	    rename -font -titlefont titleFont TitleFont
	}
	
	# Tk listbox
	itk_component add listbox {
	    listbox $w_.listbox -setgrid 1
	} {
	    usual
	    keep -font -relief -borderwidth -width -height -relief -exportselection \
		-selectmode
	}
	set listbox_ $itk_component(listbox)

	# optional scrollbars
	pack [frame $w_.vf] -side right -fill y
	# optional vertical scrollbar
	itk_component add vscroll {
	    scrollbar $w_.vf.vscroll \
		-relief sunken \
		-command [code $listbox_ yview]
	}

	pack [frame $w_.hf] -side bottom -fill x
	# optional horizontal scrollbar
	itk_component add hscroll {
	    scrollbar $w_.hf.hscroll \
		-relief sunken \
		-command [code $listbox_ xview] \
		-orient horizontal
	}

	pack $listbox_ \
	    -side top -fill both -expand 1

	bind $listbox_ <ButtonRelease-1> +[code $this edit_row]

	eval itk_initialize $args
    }


    # pop up a dialog window to edit the value in the selected row

    public method edit_row {} {
	if {! $itk_option(-editable)} {
	    return
	}
	set sel [lindex [get_selected] 0]
	
	if {"$sel" == ""} {
	    set buttons [list [list "Add new row" [code $this add_row]]]
	} else {
	    set buttons [list [list "Add new row" [code $this add_row]] \
			     [list "Delete this row" [code $this remove_row]]]
	}
	
	catch {destroy $w_.form}
	EntryForm $w_.form \
	    -title "Edit Value" \
	    -labels "Value" \
	    -values [list $sel] \
	    -command [code $this set_row $sel] \
	    -buttons $buttons
    }


    # replace the contents of the given row with the new row
    # Note: this assumes that no 2 rows are exactly alike.

    public method set_row {oldrow newrow} {
	set index [lsearch -exact [get_contents] $oldrow]
	if {$index < 0} {
	    append $newrow
	    return
	}
	set_contents [lreplace $info $index $index $newrow]
    }


    # scroll to the end of the table and display the last set of rows

    public method show_last_row {} {
	$listbox_ yview moveto 1
    }


    # set the contents of the listbox from the list

    public method set_contents {list} {
	$listbox_ delete 0 end
	foreach i $list {
	    $listbox_ insert end $i
	}
    }


    # return the contents of the listbox as a tcl list

    public method get_contents {} {
	set list {}
	set n [$listbox_ size]
	for {set i 0} {$i < $n} {incr i} {
	    lappend list [$listbox_ get $i]
	}
	return $list
    }


    # return a list of the selected items

    public method get_selected {} {
	set list {}
	foreach i [$listbox_ curselection] {
	    lappend list [$listbox_ get $i]
	}
	return $list
    }


    # remove the selected items from the listbox
    # and return them as a list

    public method remove_selected {} {
	set list [get_selected]
	set n 0
	foreach i [lsort -decreasing [$listbox_ curselection]] {
	    $listbox_ delete $i
	    incr n
	}
	if {$n} {
	    select_row $i
	}
	return $list
    }


    # append a line to the list

    public method append {line} {
	$listbox_ insert end $line
    }

    
    # append a list of items to the list

    public method append_list {list} {
	foreach i $list {
	    $listbox_ insert end $i
	}
    }

    
    # return the number of rows currently selected in the table

    public method num_selected {} {
	return [llength [$listbox_ curselection]]
    }

    
    # select the row with the given index. If clear is 1, clear
    # the selection first.

    public method select_row {n {clear 1}} {
	if {$clear} {
	    $listbox_ selection clear 0 end
	}
	$listbox_ selection set $n
 	$listbox_ see $n
    }


    # deselect the row with the given index

    public method deselect_row {n} {
	$listbox_ selection clear $n
    }


    # select a range of rows

    public method select_rows {from to} {
	$listbox_ selection clear 0 end
	$listbox_ selection set $from $to
 	$listbox_ see $from
    }


    # clear the selection

    public method clear_selection {} {
	$listbox_ selection clear 0 end
    }
    

    
    # move the selected row down 1 row

    public method move_down {} {
	set list [$listbox_ curselection]
	if {[lempty $list]} {
	    return
	}
	set rlist [lsort -decreasing $list]
	foreach i $rlist {
	    set s [$listbox_ get $i]
	    $listbox_ delete $i
	    $listbox_ insert [expr {$i+1}] $s
	}
	select_rows [expr {[lindex $list 0]+1}] \
	    [expr {[lindex $rlist 0]+1}]
    }

    
    # move the selected row up 1 row

    public method move_up {} {
	set list [$listbox_ curselection]
	if {[lempty $list]} {
	    return
	}
	foreach i $list {
	    set s [$listbox_ get $i]
	    $listbox_ delete $i
	    $listbox_ insert [expr {$i-1}] $s
	}
	select_rows [expr {[lindex $list 0]-1}] [expr {$i-1}]
    }


    # make the list empty

    public method clear {} {
	$listbox_ delete 0 end
    }


    # -- options --

    # title string for listbox
    itk_option define -title title Title {} {
	$itk_component(title) config -text $itk_option(-title)
	if {"$itk_option(-title)" == ""} {
	    pack forget $itk_component(title)
	} else {
	    pack $itk_component(title) -side top
	}
    }

    # flag: if true, list will have a vertical scrollbar
    itk_option define -vscroll vscroll Vscroll {1} {
	if {$itk_option(-vscroll)} {
	    pack $itk_component(vscroll) -side right -fill y
	    $listbox_ config -yscrollcommand [code $itk_component(vscroll) set]
	} else {
	    pack forget $itk_component(vscroll)
	    $listbox_ config -yscrollcommand {}
	}
    }

    # flag: if true, list will have a horizontal scrollbar
    itk_option define -hscroll hscroll Hscroll {0} {
	if {$itk_option(-hscroll)} {
	    pack $itk_component(hscroll) -side bottom -fill x
	    $listbox_ config -xscrollcommand [code $itk_component(hscroll) set]
	} else {
	    pack forget $itk_component(hscroll)
	    $listbox_ config -xscrollcommand {}
	}
    }

    # enable/disable editing of rows
    itk_option define -editable editable Editable 0 

    
    # -- protected members --

    # listbox widget
    protected variable listbox_
}


