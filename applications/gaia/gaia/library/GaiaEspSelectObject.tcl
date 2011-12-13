#+
#  Name:
#    GaiaEspSelectObject
#
#  Type of Module:
#    [incr Tcl] class
#
#  Purpose:
#    Encapsulates a selected object on a canvas.
#
#  Description:
#    The ESP widget needs to have the user select sources and matching
#    radii.  Encapsulate a StarCanvasDraw widget.
#
#    This module is based on, and simplified from, Peter Draper's
#    GaiaArd and GaiaPhotom* widgets.
#
#  Invocation:
#    GaiaEspSelectObject object_name [configuration options]
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
#  Notes:
#
#    There are methods in here to select and update both squares and
#    sectors.  These were required for an early version of this
#    toolkit, which implemented several of the ESP tools, but they're
#    not used at present (in the version of the toolbox which only
#    provides the ELLPRO/FOU interface).
#
#  History:
#    01-NOV-1999 (NG):
#      Original version -- simplified from GaiaArd.tcl
#    {enter_further_changes_here}
#
#  RCS Id:
#    $Id$
#-

itk::usual GaiaEspSelectObject {}

itcl::class gaia::GaiaEspSelectObject {

    #  Inheritances:
    #  (None)

    #  Constructor
    constructor {args} {

	# Evaluate any options
	eval configure $args
    }

    #  Destructor
    destructor {
	# Clear the canvas.  Better would be to clear only those objects
	# which this class has created, but that'll have to wait until I
	# sort out how to manage those internally.  See GaiaPhotomObject
	$canvasdraw clear
    }

    # Select a source
    public method select_source {callback}

    # Return the information about this object
    public method coords {} {
	if {$issector_} {
	    return [list $posX_ $posY_ $sr_ $spa_ $soa_]
	} else {
	    return [list $posX_ $posY_ $pos3_]
	}
    }
    public method canvas_id {} {
	return $canvas_id_
    }

    private method created_circle_ {id args}
    private method update_circle_  {id mode}
    private method created_square_ {id args}
    private method update_square_  {id mode}
    private method created_sector_ {id args}
    private method update_sector_  {id mode}

    # do we need an analogue of GaiaPhotomObject's sync method?

    # --- Configuration options (public variables)

    # Name of canvas, canvasdraw and rtdimage objects
    public variable canvas {} {}
    public variable canvasdraw {} {}
    public variable rtdimage {} {}

    # Shape to draw on the canvas
    public variable drawmode {} {}

    # Code which is executed with the canvasid as argument,
    # when an object is updated
    public variable update_callback {} {}

    itk_option define -selection_colour selection_colour Selection_colour {white} {
	$canvasdraw configure -outlinecolour $itk_option(-selection_colour)
    }


    # --- Protected variables (available to inheritors)

    #  None

    # --- Private variables (instance only)

    # Canvas id
    private variable canvas_id_ {}

    # Callback proceduce after creating source circle
    private variable creation_callback_

    # Position of object, and a third value.
    # In the case of the circle, the third value is the radius; in the
    # case of the square, it's the length of the side.
    # All of these are in image units, not canvas.
    private variable posX_ {}
    private variable posY_ {}
    private variable pos3_ {}

    # Position of origin of sector, position angle, radius, opening angle.
    # Image units.
    private variable sr_  {}
    private variable spa_ {}
    private variable soa_ {}
    private variable issector_ 0

}

body gaia::GaiaEspSelectObject::select_source  {callback} {
    set creation_callback_ $callback
    $canvasdraw \
	    set_drawing_mode $drawmode [code $this created_${drawmode}_]
}

body gaia::GaiaEspSelectObject::created_circle_ {id args} {
    set canvas_id_ $id
    update_circle_ $id create

    # Update display and deselect object (immediate resize isn't
    # desirable).
    update idletasks
    $canvasdraw deselect_object $canvas_id_
    $canvas addtag esp_sel withtag $canvas_id_

    if { $creation_callback_ != {} } {
	eval $creation_callback_
	set creation_callback_ {}
    }
}

body gaia::GaiaEspSelectObject::update_circle_ {id mode} {
    if {$mode != "delete"} {
	lassign [$canvas coords $id] canvas_posX canvas_posY
	set canvas_posR [$canvas itemcget $id -semimajor]

	$rtdimage convert coords \
		$canvas_posX $canvas_posY canvas \
		posX_ posY_ image
	$rtdimage convert dist $canvas_posR 0 canvas dist1 dist2 image
	set pos3_ [expr $dist1+$dist2]

	if {$mode == "create"} {
	    $canvasdraw add_notify_cmd $id [code $this update_circle_ $id] 0
	}
    }
    if {$update_callback != {}} {
	eval $update_callback $id
    }
}

# created_square_ is essentially identical to created_circle_
body gaia::GaiaEspSelectObject::created_square_ {id args} {
    set canvas_id_ $id
    update_square_ $id create

    # Update display and deselect object (immediate resize isn't
    # desirable).
    update idletasks
    $canvasdraw deselect_object $canvas_id_

    if { $creation_callback_ != {} } {
	eval $creation_callback_
	set creation_callback_ {}
    }
}

body gaia::GaiaEspSelectObject::update_square_ {id mode} {
    if {$mode != "delete"} {
	lassign [$canvas coords $id] x0 y0 x1 y1

	set centx [expr ($x0+$x1)/2]
	set centy [expr ($y0+$y1)/2]
	set widx [expr $x1-$x0]
	set widy [expr $y1-$y0]
	set wid [expr {$widx > $widy ? $widx : $widy}]
	set hwid [expr $wid/2]

	$rtdimage convert coords $centx $centy canvas posX_ posY_ image
	$rtdimage convert dist $wid 0 canvas dist1 dist2 image
	set pos3_ [expr $dist1+$dist2]

	$canvas coords $id [expr $centx-$hwid] [expr $centy-$hwid] \
		[expr $centx+$hwid] [expr $centy+$hwid]

	if {$mode == "create"} {
	    $canvasdraw add_notify_cmd $id [code $this update_square_ $id] 0
	}
    }
    if {$update_callback != {}} {
	eval $update_callback $id
    }
}

# created_sector_ is essentially identical to created_circle_
body gaia::GaiaEspSelectObject::created_sector_ {id args} {
    set canvas_id_ $id
    update_sector_ $id create
    set issector_ 1

    # Update display and deselect object (immediate resize isn't
    # desirable).
    update idletasks
    $canvasdraw deselect_object $canvas_id_

    if { $creation_callback_ != {} } {
	eval $creation_callback_
	set creation_callback_ {}
    }
}

body gaia::GaiaEspSelectObject::update_sector_ {id mode} {
    if {$mode != "delete"} {
	set sclist [$canvasdraw sector_coords $id]
	if {$sclist == {}} {
	    #puts "whoops... no update_sector_"
	    return {}
	} else {
	    lassign $sclist x y rad spa_ soa_
	}

	$rtdimage convert coords $x $y canvas posX_ posY_ image
	$rtdimage convert dist $rad 0 canvas dist1 dist2 image
	set sr_ [expr $dist1+$dist2]
    }
    if {$mode == "create"} {
	$canvasdraw add_notify_cmd $id [code $this update_sector_ $id] 0
    }
    if {$update_callback != {}} {
	eval $update_callback $id
    }
}


