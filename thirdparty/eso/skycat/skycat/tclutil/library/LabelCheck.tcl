# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelCheck.tcl,v 1.1.1.1 2006/01/12 16:40:38 abrighto Exp $"
#
# LabelCheck.tcl - Itcl megawidget for choosing options.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created


itk::usual LabelCheck {}

# LabelCheck is an itcl megawidget for choosing options using a 
# label and radiobuttons (uses blt_table to arrange the buttons in 
# rows and columns).

itcl::class util::LabelCheck {
    inherit util::LabelWidget

    constructor {args} {
	set variable_ $w_.choice
	
	# Tk frame containing checkbuttons.
	itk_component add table {
	    frame $w_.table
	} {
	    keep -background -relief -borderwidth
	}

	blt::table $itk_component(table)
 	eval itk_initialize $args
    }

    
    # clear the choice display
    
    public method clear {} {
	if {[info exists but_]} {
	    foreach i [array names but_] {
		destroy $but_($i)
	    }
	    unset but_
	}
    }


    # layout the choices according to the options

    protected method do_layout {} {
	global ::$variable_
	clear
	set rows $itk_option(-rows)
	set cols $itk_option(-cols)
	set choice $itk_option(-choice)
	set n [llength $choice]
	set row 0
	set col 0
	set i 0

	# determine how many rows and columns
	if {$rows != 0} {
	    set cols [expr {$n/$rows+1}]
	} elseif {$cols != 0} {
	    set rows [expr {$n/$cols+1}]
	} elseif {"$itk_option(-orient)" == "horizontal"} {
	    set rows 1
	    set cols $n
	} else {
	    set rows $n
	    set cols 1
	}
	
	for {set row 0} {$row < $rows} {incr row} {
	    for {set col 0} {$col < $cols} {incr col} {
		set name [lindex $choice $i]
		set ${variable_}($name) 0
		# One Tk component is created for each item in the list 
		# given by the -choice argument.
		itk_component add $name {
		    set but_($name) \
			[checkbutton $itk_component(table).choice$i \
			     -text $name \
			     -variable ${variable_}($name) \
			     -command [code $this command_proc_]]
		} {
		    keep -state -anchor -background -foreground
		    rename -font -valuefont valueFont Font
		    rename -width -valuewidth valueWidth Width
		}
		blt::table $itk_component(table) $but_($name) $row,$col -fill x
		if {[incr i] == $n} {
		    break
		}
	    }
	    if {$i == $n} {
		break
	    }
	}
    }


    # return the current value of this option
    
    public method get {} {
	global ::$variable_
	set value {}
	foreach i $itk_option(-choice) {
	    if {[set ${variable_}($i)]} {
		lappend value $i
	    }
	}
	return $value
    }


    # configure the individual buttons by name

    public method itemconfig {name args} {
	eval "$but_($name) config $args"
    }

    
    # call the command with the selected item
    
    private method command_proc_ {} {
	set cmd $itk_option(-command)
 	if {"$cmd" != ""} {
	    lappend cmd [$this get]
	}
	eval $cmd
    }

    
    # -- options --

    # name of global variable to set (defaults to $w_.choice)
    itk_option define -variable variable Variable {} {
	if {"$itk_option(-variable)" != ""} {
	    set variable_ $itk_option(-variable)
	    global ::$variable_
	    if {[info exists but_]} {
		foreach i [array names but_] {
		    $but_($i) config -variable ${variable_}($i)
		}
	    }
	}
    }

    # list of choices to display as ON, all others are then OFF
    itk_option define -value value Value {} {
	global ::$variable_
	if {[info exists $variable_]} {
	    foreach i [array names $variable_] {
		set ${variable_}($i) 0
	    }
	}
	foreach i $itk_option(-value) {
	    set ${variable_}($i) 1
	}
    }

    # list of choices
    itk_option define -choice choice Choice {} {
	do_layout
    }

    # command to run when value is changed (new value is appended)
    itk_option define -command command Command {}
    
    # widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
	pack $itk_component(table) \
	    -side $side_ -padx 1m -pady 1m -fill both -expand 1
    }
    
    # max number of rows to display (0 for unlimited) 
    # note: specify either -rows OR -cols, but not both
    itk_option define -rows rows Rows {0} 

    # max number of columns to display (0 for unlimited)
    # note: specify either -rows OR -cols, but not both
    itk_option define -cols cols Cols {0} 

   
    # -- protected members --
    
    # array(name) of button widgets
    protected variable but_

    # name of text variable array for tracing
    protected variable variable_
}

