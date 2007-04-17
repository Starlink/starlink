# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelMenu.tcl,v 1.1.1.1 2006/01/12 16:40:54 abrighto Exp $"
#
# LabelMenu.tcl - Itcl widget for displaying a label and a menubutton 
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual LabelMenu {}

# This widget displays a label and a menubutton with a selector and a
# menu of radiobuttons. This can be used for choosing items from a list
# and displaying the current choice. The widget supports adding of items
# to the radiobutton menu and keeps track of which items are selected.

itcl::class util::LabelMenu {
    inherit util::LabelWidget

    #  create a LabelMenu
    constructor {args} {
	set menu $w_.mb.m
	# Menubutton containing the menu.
	itk_component add mb {
	    menubutton $w_.mb -menu $menu
	} {
	    keep -indicatoron -relief -borderwidth -state
	    rename -width -valuewidth valueWidth Width
	    rename -font -valuefont valueFont Font
	    rename -anchor -valueanchor valueAnchor ValueAnchor
	    ignore -disabledforeground
	}

	# Component for the menu.
	itk_component add menu {
	    menu $menu
	} {
	    ignore -disabledforeground
	}

	set default_bg_ [$itk_component(mb) cget -background]

	# trace this radiobutton var to change menubutton label
	global ::$w_.var
	set $w_.var ""
	trace variable $w_.var w [code $this update_menubutton]

	eval itk_initialize $args
    }

    
    # destructor

    destructor {
	global ::$w_.var
	catch {unset $w_.var}
    }

    
    # remove all of the items in the menu

    public method clear {} {
	$itk_component(menu) delete 0 end
    }


    # return the current value

    public method get {} {
	global ::$w_.var
	set v [set $w_.var]
	foreach i [array names values_] {
	    if {"$values_($i)" == "$v"} {
		return $i
	    }
	}
    }

    
    # add a separator item to the menu
    
    public method add_separator {} {
	$itk_component(menu) add separator
    }


    # add an item to the menu.
    # The args may be the options: 
    #     -label <label for menuitem and menubutton when chosen>
    #     -bitmap <bitmap for menuitem and menubutton when chosen>
    #     -command <cmd to execute when item is selected>
    #     -background <color of menu item and button when chosen>
    #    -font <font of menu item and button when chosen>

    public method add {args} {
	global ::$w_.var
	
	# value of radiobutton variable is the command to configure the menubutton
	set value "$itk_component(mb) config"

	# command to add menu item
	set cmd "$itk_component(menu) add radiobutton"

	set n [llength $args]
	for {set i 0} {$i < $n} {incr i 2} {
	    lassign [lrange $args $i end] opt arg
	    switch -exact [string range $opt 1 end] {
		label {
		    lappend value -text $arg
		    lappend cmd -label $arg
		    set id $arg
		}
		bitmap {
		    lappend value -bitmap $arg
		    lappend cmd -bitmap $arg
		    set id $arg
		}
		command {
		    lappend cmd -command $arg
		}
		bg -
		background {
		    if {"$arg" == ""} {
			set mb_color $default_bg_
		    } else {
			set mb_color $arg
		    }
		    lappend value -background $mb_color -activebackground $mb_color
		    lappend cmd -background $arg -activebackground $arg
		}
		font {
		    lappend value -font $arg
		    lappend cmd -font $arg
		}
		value {
		    set id $arg
		}
		default {
		    lappend cmd $opt $arg
		}
	    }
	}
	
	lappend cmd -variable $w_.var -value $value 
	eval $cmd
	
	# save label or bitmap name for later reference
	set values_($id) $value

	# set default value to first item
	if {"[set $w_.var]" == ""} {
	    set $w_.var $value
	}
    }

    
    # return true if the LabelMenu contains the given item
    # (specified by the label or bitmap name)

    public method contains {name} {
	return [info exists values_($name)]
    }

    
    # update the label on the menubutton

    public method update_menubutton {args} {
	global ::$w_.var
	if {"[set $w_.var]" != ""} {
	    eval [set $w_.var]
	}
    }

    
    # called when "$variable_" has changed, via trace, to update menu
    
    protected method variable_changed {args} {
	global $variable_
	config -value [set $variable_]
    }


    # options


    # set the selected value (referenced by label or bitmap)
    # note: this.var is the trace variable and values_ holds the
    # valid values.
    itk_option define -value value Value {} {
	global ::$w_.var
	catch {set $w_.var $values_($itk_option(-value))}
    }

    # global variable linked to menu
    itk_option define -variable variable Variable {} {
	set variable_ $itk_option(-variable)
	global $variable_
	trace variable $variable_ w [code $this variable_changed]
    }

    # widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
	pack $itk_component(mb) \
	    -side $side_ -fill x -expand 1 -padx 1m -ipadx 1m

    }


    # -- protected variables --

    # default background color
    protected variable default_bg_

    # array(label or bitmap name) of values for selecting a radiobutton
    protected variable values_

    # trace variable name
    protected variable variable_
}


