# E.S.O. - VLT project/ESO Archive
# @(#) $Id: SearchConfig.tcl,v 1.1.1.1 2002/04/04 20:11:47 brighton Exp $
#
# SearchConfig.tcl - Widget for editing a list of search columns
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 22 Oct 97   created


itk::usual SearchConfig {}


# Class SearchConfig defines dialog window for editing the list of 
# search columns for the AstroCat catalog window. This allows you
# to specify a list of columns to search by, save the information
# in a local catalog config file and reconfigure the current windows
# to include entries for the search columns.

itcl::class cat::SearchConfig {
    inherit util::TopLevelWidget


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }

    
    # called after options have been evaluated

    protected method init {} {
	wm title $w_ "Search Columns for $catalog ($itk_option(-number))"
	wm iconname $w_ "Search Columns for $catalog"

	add_tables
	add_dialog_buttons
	fill_tables
	make_short_help
    }


    # add a table listing the search columns and entries to edit

    protected method add_tables {} {

	pack [set dtab \
		  [DoubleTableList $w_.dtab \
		       -selectmode extended \
		       -command [code $this update_selection] \
		       -updown 1]] \
	    -side top -padx 2m -pady 2m -fill both -expand 1

	set left_ [$dtab component left]
	set right_ [$dtab component right]
	bind [$left_ component listbox] <ButtonRelease-1> +[code $this select_row $left_]
	bind [$right_ component listbox] <ButtonRelease-1> +[code $this select_row $right_]

	$left_ config -headings {{Search Columns} MinLabel MaxLabel}
	$right_ config -headings {{Other Columns} MinLabel MaxLabel}

	# hide some columns
	$left_ set_option MinLabel Show 0
	$left_ set_option MaxLabel Show 0
	$right_ set_option MinLabel Show 0
	$right_ set_option MaxLabel Show 0

	$left_ config -width 12 -height 4
	$right_ config -width 12 -height 4
	
	# add entries for editing (left) table
	pack [set f [frame $w_.f -borderwidth 3 -relief groove]] \
	    -side top -padx 2m -pady 2m -ipadx 2m -ipady 2m -fill x

	pack \
	    [set col_ [LabelEntry $f.col \
			   -text "Column name:" \
			   -relief groove \
			   -disabledforeground black \
			   -state disabled \
			   -labelwidth $itk_option(-labelwidth) \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    [set min_ [LabelEntry $f.min \
			   -text "Label for min value:" \
			   -command [code $this set_row] \
			   -labelwidth $itk_option(-labelwidth) \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    [set max_ [LabelEntry $f.max \
			   -text "Label for max value:" \
			   -command [code $this set_row] \
			   -labelwidth $itk_option(-labelwidth) \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    -side top -fill x -expand 1
    }


    # fill the left and right tables with the column info

    protected method fill_tables {} {
	# get current search cols
	if {[catch {set search_cols [$astrocat searchcols]} msg]} {
	    error_dialog $msg
	    quit
	}
 	if {[llength $search_cols]} {
	    $left_ config -info $search_cols
	    $left_ select_row 0
	    select_row $left_
	}
	
	# set other available columns on right side
	foreach i $search_cols {
	    set a([lindex $i 0]) 1
	}
	set l {}
	foreach i $columns {
	    if {![info exists a($i)]} {
		lappend l [list $i "Min $i" "Max $i"]
	    }
	}
    	$right_ config -info $l
    }

    
    # Update the selected row with the typed in values

    protected method set_row {args} {
	set row [get_row]
	if {"$row" != "" && "$sel_" != ""} {
	    $left_ set_row $sel_ $row
	    set sel_ $row
	}
	incr save_needed_
    }

    
    # called when a table row (left or right) is selected
    
    protected method select_row {tab} {
	set sel_ [lindex [$tab get_selected] 0]
	lassign $sel_ col min max
	$col_ config -value $col
	$min_ config -value $min
	$max_ config -value $max
	if {"$tab" == "$left_"} {
	    $w_.update config -state normal
	} else {
	    $w_.update config -state disabled
	}
    }

    
    # update the selection in the listboxes after a change

    protected method update_selection {} {
	if {[llength [$left_ get_selected]]} {
	    select_row $left_
	} elseif {[llength [$right_ get_selected]]} {
	    select_row $right_
	} else {
	    $col_ config -value {}
	    $min_ config -value {}
	    $max_ config -value {}
	}
	incr save_needed_
    }


    # return a row based on the current entries or empty if not all of
    # the entries contain values

    protected method get_row {} {
	set row {}
	foreach w {col_ min_ max_} {
	    set val [[set $w] get]
	    if {"$val" == ""} {
		error_dialog "Please specify the column name and the min and max labels"
		return
	    }
	    lappend row $val
	}
	return $row
    }

    
    # apply the changes and close the window

    protected method apply {{ask 0}} {
	if {$save_needed_} {
	    if {[catch {$astrocat searchcols [join [$left_ get_contents] :]} msg]} {
		error_dialog $msg $w_
		return
	    }

	    if {!$ask || [confirm_dialog "Do you want to save your changes in the default catalog \
                                  config file (~/.skycat/skycat.cfg)?" $w_]} {
		cat::CatalogInfo::save {} $w_ $ask
		set save_needed_ 0
	    }
	    
	    if {"$command" != ""} {
		eval $command
	    }
	}
	
	quit
    }


    # add the dialog button frame

    protected method add_dialog_buttons {} {
	# dialog buttons
	pack [frame $w_.buttons -borderwidth 2 -relief raised] \
	    -side top -fill x
	pack \
	    [button $w_.ok \
		 -text "OK" \
		 -command [code $this apply]] \
	    [button $w_.cancel \
		 -text "Cancel" \
		 -command [code $this quit]] \
	    [button $w_.update \
		 -text "Update" \
		 -state disabled \
		 -command [code $this set_row]] \
	    -side left -expand 1 -padx 15m -pady 2m -in $w_.buttons
    }



    # add a short help window and set the help texts
    
    protected method make_short_help {} {
	TopLevelWidget::make_short_help

	add_short_help $left_ {List of search columns to display as entries in catalog window}
	add_short_help $right_ {List of other column names, select+click arrow buttons to move}
	add_short_help $col_ {Column name: name of selected search column (read-only)}
	add_short_help $min_ {Min value label: to be displayed on left side of catalog window}
	add_short_help $max_ {Max value label: to be displayed on right side of catalog window}
	add_short_help $w_.ok {OK: Apply changes and close window}
	add_short_help $w_.cancel {Cancel changes and close window}
	add_short_help $w_.update {Update list at top with current values}
    }


    # -- options --

    # command to eval when done
    public variable command {}

    # catalog name
    public variable catalog {none}

    # astrocat (C++ based) object
    public variable astrocat

    # list of column headings for the catalog
    public variable columns {}

    # font to use for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*

    # font to use for values
    itk_option define -valuefont valueFont ValueFont -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*

    # set the width for  displaying labels
    itk_option define -labelwidth labelWidth LabelWidth 18

    # set the anchor for labels
    itk_option define -anchor anchor Anchor e

    
    # -- protected vars --

    # left listbox widget
    protected variable left_
    
    # right listbox widget
    protected variable right_

    # Widget displaying column name: name of selected search column (read-only)
    protected variable col_

    # Widget displaying min value label.
    protected variable min_

    # Widget displaying max value label.
    protected variable max_

    # table row last selected
    protected variable sel_ {}

    # set to true if the catalog configuration has been edited and needs to be saved
    protected variable save_needed_ 0
}
