#+
#
#  Name:
#    GaiaEspSelectList
#
#  Type of module:
#    [incr Tcl] class
#
#  Purpose:
#    Provides a top-level widget for managaging a list of selections
#    on a canvas.
#
#  Description:
#    Manages a list of GaiaEspSelectObject objects, allowing the user
#    to select sources.
#
#    The module is based on code from Peter Draper's GaiaArd and
#    GaiaPhotom* widgets.
#
#  Invocation:
#    GaiaEspSelectList object_name [configuration options]
#
#    This creates an instance of a GaiaEspSelectObject object. The return is
#    the name of the object.
#
#        object_name configure -configuration_options value
#
#    Applies any of the configuration options (after the instance has
#    been created).
#
#        object_name method arguments
#
#    Performs the given method on this widget.
#
#  Inheritance:
#    None
#
#  Copyright:
#    Copyright 2000, Central Laboratory of the Research Councils
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
#
#  Author:
#    NG: Norman Gray (Starlink, Glasgow)
#    {enter_new_authors_here}
#
#  History:
#    01-NOV-1999 (NG):
#      Original version -- simplified from GaiaArd.tcl
#    {enter_further_changes_here}
#
#  RCS Id:
#    $Id$
#-

itk::usual GaiaEspSelectList {}

itcl::class gaia::GaiaEspSelectList {

    # --- Inheritances
    # None

    # --- Constructor
    constructor {args} {
	# Evaluate any options
	eval configure $args

	default_config
    }

    # --- Destructor
    destructor {
	# Nothing
    }

    # --- Public methods and variables

    # Canvas, canvasdraw and rtdimage variables
    public variable canvas {} {}
    public variable canvasdraw {} {}
    public variable rtdimage {} {}

    # Short help window.
    public variable short_help {}

    # Drawing mode for shapes on canvas
    public variable sourceshape {} {}

    # Maximum number of objects to be drawn
    public variable maxobjects {} {}

    # Colour of selection shapes
    public variable selection_colour white {
	config_all selection_colour $selection_colour
    }

    # Create an editor panel, which manipulates the list of objects.
    public method edit_sourcelist {parent}

    # Reset.  Discard the list of sources
    public method reset {}

    # Set and return the list of sources.
    #
    # set_sourcelist is primarily provided so that the source list
    # editor can manipulate the source list in this, its parent.
    public method set_sourcelist {new_list}

    # get_sourcelist: return a list of the names of the objects
    # created on the canvas.
    public method get_sourcelist {}

    # default_config: (re-)initialise.
    public method default_config {}

    # Set a configuration option for all objects.
    public method config_all {item value}

    # --- Protected variables
    # None

    # --- Private variables

    # The list of selected sources, maintained simply by
    # set_sourcelist, which is typically called by
    # GaiaEspSelectListEditor.
    private variable object_list_ {}

    private variable editor_ {}
}

body gaia::GaiaEspSelectList::edit_sourcelist {parent} {
    if {$editor_ == {}} {
	set editor_ [gaia::GaiaEspSelectListEditor \
		$parent.#auto $this $object_list_ \
		-canvas $canvas \
		-canvasdraw $canvasdraw \
		-rtdimage $rtdimage \
		-drawmode $sourceshape \
		-maxobjects $maxobjects \
                -short_help $short_help ]
    }
    return $editor_
}

body gaia::GaiaEspSelectList::reset {} {
    if {$editor_ != {}} {
	$editor_ reset
    }
    set object_list_ {}
}

body gaia::GaiaEspSelectList::set_sourcelist {l} {
    set object_list_ $l
}

body gaia::GaiaEspSelectList::get_sourcelist {} {
    return $object_list_
}

body gaia::GaiaEspSelectList::default_config {} {
    set sourceshape circle
    set maxobjects 0
}

# Set a configuration option for all objects.
body gaia::GaiaEspSelectList::config_all {item value} {
    foreach o $object_list_ {
	$o configure -$item $value
    }
}
