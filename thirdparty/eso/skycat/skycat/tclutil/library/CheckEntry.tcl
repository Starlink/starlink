# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: CheckEntry.tcl,v 1.8 1998/10/28 17:46:36 abrighto Exp $"
#
# CheckEntry.tcl - Itcl widget combining a checkbutton and an entry
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual CheckEntry {}

# This class defines an Itk widget combining a checkbutton and an entry.

itcl::class util::CheckEntry {
    inherit util::FrameWidget


    #  constructor: create new CheckEntry

    constructor {args} {
	set variable_ $w_.var
	global ::$variable_

	# Tk checkbutton widget
	itk_component add check {
	    checkbutton $w_.check -variable $variable_ 
	} {
	    keep -text
	}
	pack $itk_component(check) \
	    -side left -padx 1m -pady 2m -ipadx 1m

	# Tk entry widget
	itk_component add entry {
	    entry $w_.entry -relief sunken
	}
	pack $itk_component(entry) \
	    -side left -expand 1 -fill x -padx 2m -pady 2m -ipadx 2m -ipady 1m

	bind $itk_component(entry) <1> "[bind Entry <1>]; $itk_component(check) select"

	eval itk_initialize $args
    }


    #  Get the value in the entry

    public method get {} {
	global ::$variable_
	if {[set $variable_]} {
	    return [$itk_component(entry) get]
	}
	return {}
    }
   

    #  called for Return in the entry

    protected method _command_proc {} {
	if {"$itk_option(-command)" != ""} {
	    set a $itk_option(-command)
	    lappend a [$this get]
	    eval $a
	}
    }
   

    # -- options --


    # set the value in the entry
    itk_option define -value value Value {} {
	global ::$variable_
	if {"$itk_option(-value)" == ""} {
	    set $variable_ 0
	} else {
	    if {[winfo exists $itk_component(entry)]} {
		set $variable_ 1
		$itk_component(entry) delete 0 end
		$itk_component(entry) insert 0 [cget -value]
		$itk_component(entry) icursor end
		$itk_component(entry) xview moveto 1
	    }
	}
    }

    
    # the command for <Return> in the entry
    itk_option define -command command Command {} {
	bind $itk_component(entry) <Return> [code $this _command_proc]
    }

    # optionally specify the trace variable to use
    itk_option define -variable variable Variable {} {
	if {"$itk_option(-variable)" != ""} {
	    $itk_component(check) configure -variable [set variable_ $itk_option(-variable)]
	}
    }

    
    # -- protected variables --
    
    # trace var for checkbutton
    protected variable variable_
}

