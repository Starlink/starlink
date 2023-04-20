#+
#
#  Name:
#    GaiaEspSelectListEditor
#
#  Type of module:
#    [incr Tcl] class
#
#  Purpose:
#    Provides a top-level widget for managaging a list of selections
#    on a canvas.
#
#  Description:
#    Edits the list of GaiaEspSelectObject objects in its parent
#    class, allowing the user to select sources.
#
#    The module is based on code from Peter Draper's GaiaArd and
#    GaiaPhotom* widgets.
#
#  Invocation:
#    GaiaEspSelectListEditor object_name [configuration options]
#
#    This creates an instance of a GaiaEspSelectObjectEditor
#    object. The return is the name of the object.
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
#    This widget inherits itk::Widget and gaia::GaiaEspSelectList.
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

itk::usual GaiaEspSelectListEditor {}

itcl::class gaia::GaiaEspSelectListEditor {

    # --- Inheritances
    inherit itk::Widget 
    #gaia::GaiaEspSelectList

    # --- Constructor
    constructor {parent_select_list initial_object_list args} {
	itk::Widget::constructor
    } {
	set parent $parent_select_list
	set object_list_ $initial_object_list

	eval itk_initialize $args

	itk_component add objlist {
	    util::TableList $itk_interior.objlist \
		    -title "Selected objects" \
		    -headings {X Y Size canvasid} \
		    -sizes {10 10 10 10}
	}
	$itk_component(objlist) set_option canvasid Show 0
	$itk_component(objlist) set_option X Precision 1
	$itk_component(objlist) set_option Y Precision 1
	$itk_component(objlist) set_option Radius Precision 1
	pack $itk_component(objlist) -fill both -expand 1
	# Now clear the table, even though there's nothing in it.
	# This repaints the table, so that the canvasid column will be
	# properly hidden as required.
	$itk_component(objlist) clear

	# Initialise the table with the current list of sources, if any
	if { $object_list_ != {} } {
	    remake_table_
	}

	itk_component add AddSource {
	    button $itk_interior.addsource \
		    -text "Add object" \
		    -command [code $this add_source]
	}
	$itk_component(AddSource) configure -highlightbackground black
        if { $short_help != {} } {
            $short_help add_short_help $itk_component(AddSource) \
               {Add an object to the list}
        }
	itk_component add DeleteSource {
	    button $itk_interior.deletesource \
		    -text "Delete object" \
		    -command [code $this delete_source]
	}
        if { $short_help != {} } {
           $short_help add_short_help $itk_component(DeleteSource) \
              {Remove selected objects from the list}
        }
        pack $itk_component(AddSource) \
            $itk_component(DeleteSource) \
            -side left -fill x -expand 1 -padx 2 -pady 2
    }

    # --- Destructor
    destructor {
	# Nothing
    }

    # --- Public methods and variables

    # Methods to add a source to the list, delete one,
    # and close the editor window
    public method add_source {}
    public method delete_source {}
    public method close_window {}
    public method reopen_window {}

    # Reset the editor, and close the window
    public method reset {}

    # Canvas, canvasdraw and rtdimage variables
    public variable canvas {} {}
    public variable canvasdraw {} {}
    public variable rtdimage {} {}

    # Short help window.
    public variable short_help {}

    # Shape to draw on the canvas
    public variable drawmode {} {}

    # Maximum number of objects to be drawn
    public variable maxobjects {} {}

    #itk_option define -canvasdraw canvasdraw CanvasDraw {} {}
    #itk_option define -canvas canvas Canvas {} {}

    # --- Private methods and variables

    # Finish adding a source.  This is a callback.
    private method add_source_finish_ {}
    # Update the entry for a source.  This is a callback registered with
    # the GaiaEspSelectObject object
    private method update_source_ {id}
    # Remake the table from scratch
    private method remake_table_ {}

    # Parent GaiaEspSelectList, to which we report changes in the source list
    private variable parent {}

    # This class's copy of the object list
    private variable object_list_ {}
}


body gaia::GaiaEspSelectListEditor::add_source {} {
    $itk_component(AddSource) configure -state disabled -relief sunken

    # Create a new GaiaEspSelectObject object.
    set newobj [gaia::GaiaEspSelectObject #auto \
	    -canvas $canvas \
	    -canvasdraw $canvasdraw \
	    -rtdimage $rtdimage \
	    -drawmode $drawmode]

    # Append the new object to the object_list_.  Do so with the object's
    # namespace prepended, so that we can examine it outside this
    # current object.
    lappend object_list_ [namespace current]::$newobj

    $newobj configure -update_callback [code $this update_source_]
    $newobj select_source [code $this add_source_finish_]
}

body gaia::GaiaEspSelectListEditor::add_source_finish_ {} {
    $itk_component(AddSource) configure -state normal -relief raised
    set lastobj [lindex $object_list_ [expr [llength $object_list_] - 1]]

    # synchronise the parent's object list
    $parent set_sourcelist $object_list_

    # ...and remake the source table
    remake_table_

    # Disable the add-source button if the maximum number of selections
    # has been reached.
    if {$maxobjects > 0 && [llength $object_list_] >= $maxobjects} {
	$itk_component(AddSource) configure -state disabled
    }
}

# Update the table contents, after GaiaEspSelectObject::update_circle_
# has changed the drawing on the canvas.  Don't be too clever here --
# clear the table and start again.
body gaia::GaiaEspSelectListEditor::update_source_ {id} {
    remake_table_
}

# Maintain the list of elements
# in the same order as the objects in object_list_
body gaia::GaiaEspSelectListEditor::remake_table_ {} {
    set newcontents {}
    foreach s $object_list_ {
	set c [$s coords]
	if {[llength $c] > 3} {
	    set c [lrange $c 0 2]
	}
	lappend newcontents [concat $c [$s canvas_id]]
    }
    $itk_component(objlist) clear
    $itk_component(objlist) append_rows $newcontents
    $itk_component(objlist) new_info
}

# Delete a source from the canvas, the table, and the object_list_
# The elements in the table are in the same order as the elements in
# object_list_ (maintained so by update_source_).
#
# Could also delete object when they are selected on the canvas, using
# [$canvasdraw selected_items], but this seems less good, since
# they're less obviously selected, and so there's a greater danger of
# them being deleted accidentally.
body gaia::GaiaEspSelectListEditor::delete_source {} {
    set dlist [$itk_component(objlist) get_selected_with_rownum]
    if {[llength $dlist] == 0} {
	error_dialog "Select a source in the source list first"
    } else {
	# go through the list is descending order, so we remove items from
	# object_list_ from the end
	set sdlist [lsort -decreasing $dlist]
	foreach row $sdlist {
	    set rownum [lindex $row 0]
	    set rowval [lindex $row 1]
	    # delete from object_list_.  The object _should_ be in the
	    # correct place in the object_list_, but put in an extra
	    # test just in case something's been messed up.
	    # Everything should come out in the wash with
	    # remake_table_ anyway.
	    if {[llength $object_list_] < $rownum} {
		set object_list_ [lreplace $object_list_ $rownum $rownum]
	    }
	    # Delete from canvas last.  This update to the canvas calls
	    # update_source_, which in turn calls remake_table_.
	    $canvasdraw delete_object [lindex $rowval 3]
	}
	# Enable the add-source button if the object_list_ length is now
	# below the maximum number.
	if {$maxobjects > 0 && [llength $object_list_] < $maxobjects} {
	    $itk_component(AddSource) configure -state normal
	}

	# Now synchronise the parent class's source list
	$parent set_sourcelist $object_list_
    }
}

body gaia::GaiaEspSelectListEditor::reset {} {
    # delete all of the sources, and close the window
    foreach o $object_list_ {
	delete object $o
	#$o configure -update_callback {}
	#$canvasdraw delete_object $o
    }
    set object_list_ {}

    # clear the table by remaking it with the newly empty objectlist
    remake_table_
}
