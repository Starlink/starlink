# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: TableListSort.tcl,v 1.1.1.1 2006/01/12 16:40:47 abrighto Exp $"
#
# TableListSort.tcl - itcl popup widget for sorting contents of a TableList
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created


itk::usual TableListSort {}


# TableListSort is an itcl popup widget for sorting contents of a TableList.

itcl::class util::TableListSort {
    inherit util::TopLevelWidget


    # constructor - create the windows and load config file

    constructor {args} {
	eval itk_initialize $args
    }
    
    
    # initialize components
    
    protected method init {} {
	if {"$itk_option(-table)" == ""} {
	    error "table not specified"
	}
	set table_ $itk_option(-table)
	set config_file_ [$table_ component config_file]
	create_main_window
	load
	wm title $w_ "Table Sort"
    }


    # do the main window layout

    protected method create_main_window {} {
	make_dlist_window
	make_options_window
	make_button_row
    }

    
    # create a window with 2 lists - one for the headings
    # and one for the sort keys.
    
    protected method make_dlist_window {} {
	# DoubleList(n) widget to display sort keys to use or not use.
	itk_component add dlist {
	    DoubleList $w_.dlist \
		-selectmode extended \
		-updown 1 \
		-lefttitle {Sort Keys} \
		-righttitle {Table Headings}
	} {
	}
	pack $itk_component(dlist) \
	    -side top -fill both -expand 1
    }


    # create a window for setting column options

    protected method make_options_window {} {
	pack [frame $w_.opts -bd 1m] \
		-side top -fill x

	pack [label $w_.opts.title -text "Options"] \
		-side top -fill x -expand 1 -ipady 1m

	pack [set f [frame $w_.opts.f -bd 3 -relief groove]] \
		-pady 1m -padx 1m -ipady 1m -ipadx 1m -fill x -expand 1

	pack [util::LabelChoice $w_.order \
		  -text {Order:} \
		  -choice {increasing decreasing} \
		  -value [$config_file_ cget -sort_order]] \
	    -side top -expand 1 -ipadx 1m -ipady 1m -in $f
    }


    # make the row of buttons at bottom

    protected method make_button_row {} {
	pack [util::ButtonFrame $w_.btns \
		  -relief raised \
		  -borderwidth 2 \
		  -ok_cmd [code $this apply] \
		  -cancel_cmd [code $this cancel]] \
	    -side bottom -fill x -expand 1 -ipady 1m
	$w_.btns append {Reset} [code $this reset]
    }


    # reset the sorting parameters to the default values

    public method reset {} {
	$itk_component(dlist) clear
	foreach i [$config_file_ cget -headings] {
	    [$itk_component(dlist) component right] append $i
	}
	$w_.order config -value increasing
    }


    # called for the cancel button

    public method cancel {} {
	quit
	$itk_component(dlist) clear
	load
    }


    # apply the changes (called for OK button)

    public method apply {} {

	# map headings to col index
	set i -1
	set headings [$config_file_ cget -headings]
	
	set sort_cols [[$itk_component(dlist) component left] get_contents]
	set sort_order [$w_.order get]
	
	# eval command with results
	if {"$command" != ""} {
	    set cmd $command
	    lappend cmd $sort_cols $sort_order
	    eval $cmd
	}
	
	quit
    }

    
    # default method called when sort options have been selected. May be overridden
    # with the -command option.
    # sort_cols is a list of column names to sort by
    # sort_order is one of "increasing", "decreasing"

    public method table_sort {sort_cols sort_order} {
	busy {
	    $table_ config -sort_cols $sort_cols
	    $table_ config -sort_order $sort_order
	    $table_ new_info
	}
    }


    # load the sort settings from the config file object

    public method load {} {
	set headings [$config_file_ cget -headings]
	set sort_by [$config_file_ cget -sort_by]
	foreach i $sort_by {
	    set key([lindex $headings $i]) 1
	}

	foreach i $headings {
	    if {[info exists key($i)]} {
		[$itk_component(dlist) component left] append $i
	    } else {
		[$itk_component(dlist) component right] append $i
	    }
	}
    }
   
    # -- public member variables --

    # TableList whose layout is being edited
    itk_option define -table table Table {}

    # command to eval when sort options have been selected.
    # called with 2 args: 1: list of sort cols, 2: sort order
    # (increasing or decreasing)
    public variable command {table_sort} {
	if {"$command" == ""} {
	    set command table_sort
	}
    }

    # -- protected members --
    
    # object managing config info
    protected variable config_file_

    # TableList widget
    protected variable table_
       

}

