# This is a simple program to demonstrate the TableList itcl widget 
# by displaying generated rows and columns of data
#
# Usage: mywish -f tablelist ?-numrows number? ?-numcols number?
#    (where mywish has the Itcl, BLT and TclX extensions)
#
# (The numrows and numcols options determine how man rows and columns 
# are inserted in the table)

source test.tcl

itcl::class TableListDemo {
    inherit util::TopLevelWidget
    
    
    # constructor: create a toplevel window for the demo

    constructor {args} {
	wm title $w_ "TableList Demo"

	# add a menubar
	add_menubar
	set m [add_menubutton File]
	$m add command -label "Exit" -command [code $this quit]

	# create the TableList widget
	itk_component add tlist {
	    set table_ \
		[TableList $w_.tlist \
		     -menubar $itk_component(menubar) \
		     -use_regexp 1 \
		     -height 15 \
		     -headinglines 2 \
		     -selectmode extended]
	}
	pack $table_ -fill both -expand 1

	# add a row of buttons at botton
	itk_component add btns {
	    ButtonFrame $w_.b -ok_cmd exit
	}
	pack $itk_component(btns) -side bottom -fill x

	eval itk_initialize $args

	# generate some data and display it
	gen_data
	$table_ config \
	    -title "Demo" \
	    -headings $headings_ \
	    -info $info_

	bind [$table_ component listbox] <1> "[bind Listbox <1>]; [code $this select_item]"
	bind [$table_ component listbox] <Double-Button-1> [code $this show_selected_item]

	# add a test frame
	add_test_frame

    }


    # generate the table data

    method gen_data {} {
	puts "generating data to display in table ($numrows_ rows x $numcols_ columns)..."
	set headings_ {}
	set sizes_ {}
	set info_ {}
	for {set i 0} {$i < $numcols_} {incr i} {
	    lappend headings_ "Column No-$i"
	    lappend sizes_ 10
	}
	for {set i 0} {$i < $numrows_} {incr i} {
	    set col {}
	    for {set j 0} {$j < $numcols_} {incr j} {
		lappend col Item-$i,$j
	    }
	    lappend info_ $col
	}
	puts "done."
    }

    
    # add a frame with some widgets for testing the tablelist
    
    method add_test_frame {} {
	pack [frame $w_.test -bd 2 -relief raised] \
	    -side top -fill x -expand 1

	pack [label $w_.test.title \
		  -text "Test TableList Features"] \
	    -side top
		  
	pack [frame $w_.test.top -bd 3] \
	    -side top -fill x

	pack [frame $w_.test.top.left -bd 2 -relief groove] \
	    -side left -fill both -expand 1 -padx 1m -pady 1m

	pack [frame $w_.test.top.right -bd 2 -relief groove] \
	    -side left -fill both -expand 1 -padx 1m -pady 1m

	pack [LabelChoice $w_.test.top.left.sline \
		  -text {Show ----- Line:} \
		  -anchor e \
		  -labelwidth 17 \
		  -choice {Yes No} \
		  -value No \
		  -command [code $this show_dashed_line -]] \
	    -side top -anchor w
	
	pack [LabelChoice $w_.test.top.left.dline \
		  -text {Show ===== Line:} \
		  -anchor e \
		  -labelwidth 17 \
		  -choice {Yes No} \
		  -value No \
		  -command [code $this show_dashed_line =]] \
	    -side top -anchor w

	pack [LabelChoice $w_.test.top.left.pos \
		  -text {Show This Line:} \
		  -anchor e \
		  -labelwidth 17 \
		  -choice {First Middle Last} \
		  -command [code $this show_this_line]] \
	    -side top -anchor w

	pack [LabelChoice $w_.test.top.left.sel \
		  -text {Select This Line:} \
		  -anchor e \
		  -labelwidth 17 \
		  -choice {First Middle Last} \
		  -value {} \
		  -command [code $this select_this_line]] \
	    -side top -anchor w


	pack [LabelCheck $w_.test.top.right.head \
		  -text {Show Only:} \
		  -rows 4 \
		  -choice $headings_ \
		  -value $headings_ \
		  -command [code $this show_only]] \
	    -side top
    }


    # called for double-click on a table row
    
    method show_selected_item {} {
	set sel [lindex [$table_ get_selected] 0]

	# if a dashed line was selected, ignore it
	if {[string length $sel] <= 1} {
	    $table_ clear_selection
	    return
	}
	info_dialog "Selected:\n\n$sel"
	
	# test the set_row method by changing the contents of this row
	regsub -all Item $sel Selected newrow
	$table_ set_row $sel $newrow
    }


    # called when a row in the table is selected
    
    method select_item {} {
	set sel [$table_ get_selected]

	# if a dashed line was selected, ignore it
	if {[string length $sel] == 1} {
	    $table_ clear_selection
	    return
	}
    }


    # insert or remove a dashed line. Dashed lines are inserted as a 
    # single "_", "-" or "=" in the table info list.
    # Args: dash is either "=" or "-", bool is Yes or No

    method show_dashed_line {dash bool} {
	set info_ [lremove_item $info_ $dash]
	if {"$bool" == "Yes"} {
	    if {"$dash" == "="} {
		set i 10
	    } else {
		set i 5
	    }
	    set info_ [linsert $info_ $i $dash]
	}
	busy {
	    $table_ config -info $info_
	}
	set numrows_ [llength $info_]
    }

    
    # make sure the given line is visible
    # which is one of (First, Middle, Last)

    method show_this_line {which} {
	switch $which {
	    First {
		$table_ yview moveto 0
	    }
	    Middle {
		$table_ yview moveto 0.5
	    }
	    Last {
		$table_ yview moveto 1
	    }
	}
    }


    # select the given line is visible
    # which is one of (First, Middle, Last)

    method select_this_line {which} {
	show_this_line $which
	switch $which {
	    First {
		$table_ select_row 0
	    }
	    Middle {
		$table_ select_row [expr $numrows_/2]
	    }
	    Last {
		$table_ select_row [expr $numrows_-1]
	    }
	}
    }


    # display only the columns selected
    
    method show_only {cols} {
	$table_ set_options $headings_ Show 0
	$table_ set_options $cols Show 1
	busy {
	    $table_ new_info
	}
    }


    # -- public variables (also program options) -- 

    # number of rows to generate
    itk_option define -numrows numrows Numrows {40} {
	set numrows_ $itk_option(-numrows)
    }

    # number of columns to generate
    itk_option define -numcols numcols Numcols {10} {
	set numcols_ $itk_option(-numcols)
    }

    
    # -- protected variable variables --
    
    # table wisget
    protected variable table_ {} 

    # table headings
    protected variable headings_ {} 

    # data: list of table rows, where each row is a list strings
    protected variable info_ {}

    # list of column sizes. This would be calculated if we didn't know it,
    # but it is faster if you know the size ahead of time
    protected variable sizes_ {}

    # number or rows/columns
    protected variable numrows_
    protected variable numcols_
}


# Start the demo:
# Note that "start" is a "member" proc in the TopLevelWidget class.
# It creates an instance of the above class and handles options and
# error checking.

util::TopLevelWidget::start TableListDemo
