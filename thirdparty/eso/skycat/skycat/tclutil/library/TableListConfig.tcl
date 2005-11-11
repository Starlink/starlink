# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: TableListConfig.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $"
#
# TableListConfig.tcl - Popup widget for editing table layout for TableList
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual TableListConfig {
    keep -activebackground -activerelief -background -borderwidth -cursor \
	 -elementborderwidth -foreground -highlightcolor -highlightthickness \
	 -jump -labelfont -selectbackground -selectborderwidth \
	 -selectforeground -textbackground -textfont -troughcolor 
}

# TableListConfig is an itcl popup widget for editing table layout
# for the TableList class.

itcl::class util::TableListConfig {
    inherit util::TopLevelWidget


    # constructor - create the windows and load config file

    constructor {args} {
	eval itk_initialize $args
    }
    
    
    # initialize layout 

    protected method init {} {
	if {"$table_" == ""} {
	    error "table not specified"
	}
	create_main_window
	load
	wm title $w_ "Select Table Columns"
    }


    protected method create_main_window {} {
	make_list_window
	make_button_row
    }

    
    # create a window with 2 scrolling lists and arrows
    # between then to move items back and forth.
    
    protected method make_list_window {} {
	# DoubleList widget for choosing columns to show or hide.
	itk_component add dlist {
	    DoubleList $w_.dlist \
		-lefttitle {Show} \
		-righttitle {Don't Show} \
		-updown 1 \
		-exportselection 0 \
		-selectmode extended
	} {
	    usual
	}
	pack $itk_component(dlist) \
	    -side top -fill both -expand 1

	set rightbox_ [$itk_component(dlist) component right]
	set leftbox_ [$itk_component(dlist) component left]

	# enable setting options when the left list is active
	bind [$leftbox_ component listbox] <ButtonRelease-1> "+[code $this left_selected]"
	bind [$rightbox_ component listbox] <ButtonRelease-1> "+[code $this right_selected]"
    }


    # called when an item in the left listbox is selected

    protected method left_selected {} {
	$rightbox_ clear_selection
    }


    # called when an item in the right listbox is selected

    protected method right_selected {} {
	$leftbox_ clear_selection
    }


    # deselect all items in the lists and disable the entries and options

    public method deselect_all {} {
	$leftbox_ clear_selection
	$rightbox_ clear_selection
    }

    
    # make the row of buttons at bottom

    protected method make_button_row {} {
	# Row of buttons (ButtonFrame(n))
	itk_component add btns {
	    util::ButtonFrame $w_.btns \
		-relief raised \
		-borderwidth 2 \
		-ok_cmd [code $this ok] \
		-cancel_cmd [code $this cancel]
	}
	$w_.btns append {Reset} [code $this reset]
	pack $itk_component(btns) \
		-side top -fill x -ipady 1m
    }


    # reset to default configuration

    public method reset {} {
	deselect_all
	$config_file_ set_defaults 1
	$itk_component(dlist) clear
	load
    }


    # called for the cancel button - reset to original values and quit

    public method cancel {} {
	deselect_all
	quit
	$itk_component(dlist) clear
	load
    }

    
    # accept the current information

    public method ok {args} {
	# get/set the list of headings to show/hide
	set list {}
	foreach name [$leftbox_ get_contents] {
	    set options_($name,Show)  1
	    lappend list $name
	}
	foreach name [$rightbox_ get_contents] {
	    set options_($name,Show) 0
	    lappend list $name
	}
	busy {
	    $config_file_ set_options options_
	    $config_file_ config -order $list
	    $table_ update_sort_info
	    $table_ new_info
	}

	# we're done, make window go away now
	deselect_all
	if {"$command" != ""} {
	    set cmd $command
	    lappend cmd [$leftbox_ get_contents]
	    eval $cmd
	}
	quit
    }

    # The information was read by $config_file_ already, so we
    # just display it here.

    public method load {} {
	$config_file_ get_options options_
	$leftbox_ clear
	$rightbox_ clear
	foreach name [$config_file_ get_order] {
	    if {$options_($name,Show)} {
		$leftbox_ append $name
	    } else {
		$rightbox_ append $name
	    }
	}
    }

    
    # -- itk_option define -member member member variables --

    # TableList whose layout is being edited
    itk_option define -table table Table {} {
	set table_ $itk_option(-table)
	set config_file_ [$table_ component config_file]
    }

    # command to eval when done
    public variable command {}

    # -- protected variable members --

    # array(name,option) of option values, loaded from config_file
    protected variable options_

    # left and right listboxes
    protected variable leftbox_
    protected variable rightbox_

    # TableList whose layout is being edited
    protected variable table_ {}

    # table config class
    protected variable config_file_
}

