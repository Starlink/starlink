#+
#
#  Name:
#    StarLabelWidgetChild
#
#  Type of module:
#    [incr Tcl] class
#
#  Purpose:
#    A very simple descendent of the LabelWidget class, which adds a
#    `childsite' method.  This allows us to add a LabelWidget which
#    does not use any of the standard descendants, perhaps because it
#    has several entries within it.
#
#    It also optionally supplies a checkbox.
#
#  Invocations:
#
#    gaia::StarLabelWidgetChild object_name [configuration options]
#
#  Configuration options:
#
#    -enablebutton
#        If true, then a checkbox is provided in the widget's
#        interior.
#    -initialstate
#        The initial state for any provided checkbox.
#    -state
#        Similarly to other Widgets, the -state option allows the
#        widget to be in either the `normal' or the `disabled' state.
#    -disabledforeground
#        The colour of the foreground when the widget is disabled.
#
#  Methods:
#
#    childsite (string)
#        Return the name of the frame widget that acts as a container
#        of the component widgets (ie, the value of the protected
#        itk_interior variable).
#
#    is_enabled (boolean)
#        Returns true if the checkbox is ticked.
#
#    reset
#        Restore to initial state.
#
#  Inheritance:
#    util::FrameWidget
#
#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#    NG: Norman Gray (Starlink, Glasgow)
#
#  History:
#    24-Nov-1999 (NG)
#      Original version
#
#  RCS id:
#    $Id$
#-

itk::usual StarLabelWidgetChild {}

itcl::class gaia::StarLabelWidgetChild {
    inherit util::FrameWidget

    constructor {args} {

	# Make the internal labels start with underscores, so that
	# they don't clash with any labels which the user puts in the
	# interior, and have the label and button, but _not_ the kid,
	# end with underscores, so that they're classed as `internal'
	# as far as setstate_ is concerned.
	itk_component add label {
	    label $itk_interior._label_ -anchor w
	} {
	    keep -text -background -foreground
	    rename -width -labelwidth labelWidth LabelWidth
	    rename -font -labelfont labelFont LabelFont
	}
	pack $itk_component(label) -side left -ipadx 1m -ipady 1m

	eval itk_initialize $args

	set is_enabled_ $initialstate
	set initial_button_state_ $is_enabled_

	if {$enablebutton} {
	    itk_component add intbutton {
		checkbutton $itk_interior._button_ \
			-variable [scope is_enabled_] \
			-command [code $this setstate_ 1 ]
	    }
	    pack $itk_component(intbutton) -side left -ipadx 1m -ipady 1m
	}

	if {$childtype != {}} {
	    itk_component add kid {
		eval $childtype $itk_interior._kid $childopts
	    }
	    pack $itk_component(kid) -side left -ipadx 1m -ipady 1m
	}
    }

    # --- Public methods

    # Return childsite, so the caller can place further widgets there.
    public method childsite {}
    # Is the checkbutton enabled?
    public method is_enabled {}
    # Reset to initial state.
    public method reset {}

    # --- Private methods

    # Set the state of all the child widgets.
    #
    # If $st is 0, then disable everything.
    # If $st is 1, then enable the internal widgets, and
    #   enable/disable the non-internal widgets depending on the value
    #   of is_enabled_.
    private method setstate_ {st} {
	foreach kid [winfo children $itk_interior] {
	    if {[regexp {_$} $kid]} {
		# internal
		set enable_it_ $st
	    } else {
		set enable_it_ [expr $st && $is_enabled_]
	    }
	    #puts "st=$st, kid=$kid ==> enable_it_=$enable_it_"
	    if {[catch {$kid configure -state}]} {
		# no -state option
		if {![catch {$kid configure -foreground}]} {
		    # ...but there is a -foreground option
		    if {$enable_it_} {
			$kid configure -foreground $itk_option(-foreground)
		    } else {
			$kid configure \
				-foreground $itk_option(-disabledforeground)
		    }
		}
	    } else {
		$kid configure \
			-state [expr {$enable_it_ ? "normal" : "disabled"}]
	    }
	}
    }

    # --- Public variables and options

    public variable enablebutton 0
    public variable initialstate 1
    public variable childtype ""
    public variable childopts ""

    # Define -state option, can be `normal' or `disabled'.
    # Also, set the states of all children of itk_interior which have
    # a -state option
    itk_option define -state state State normal {
	setstate_ [expr {$itk_option(-state) == "normal"}]
	#if {[info exists itk_component(kid)]} {
	#    $itk_component(kid) configure -state $itk_option(-state)
	#}
	#if {$itk_option(-state) == "normal"} {
	#    $itk_component(label) configure \
	#	    -foreground $itk_option(-foreground)
	#    if {[info exists itk_component(intbutton)]} {
	#	$itk_component(intbutton) configure -state normal
	#    }
	#} else {
	#    $itk_component(label) configure \
	#	    -foreground $itk_option(-disabledforeground)
	#    if {[info exists itk_component(intbutton)]} {
	#	$itk_component(intbutton) configure -state disabled
	#    }
	#}
    }

    # Disabled foreground colour
    itk_option define -disabledforeground disabledForeground DisabledForeground {grey}

    # --- Private variables
    private variable is_enabled_ 0

    # saved initial button state
    private variable initial_button_state_ {}
}

body gaia::StarLabelWidgetChild::childsite {} {
    return $itk_interior
}

body gaia::StarLabelWidgetChild::is_enabled {} {
    return $is_enabled_
}

body gaia::StarLabelWidgetChild::reset {} {
    setstate_ $initial_button_state_
    if {$childtype != {}} {
	eval $itk_component(kid) configure $childopts
    }
}
