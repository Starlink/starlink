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
#    This widget inherits TopLevelWidget.
#
#  Copyright:
#    Copyright 1999, Central Laboratory of the Research Councils
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
    inherit util::TopLevelWidget gaia::GaiaEspSelectList

    # --- Constructor
    constructor {parent_select_list initial_object_list args} {
	util::TopLevelWidget::constructor
    } {
	set parent $parent_select_list
	set object_list_ $initial_object_list

	eval itk_initialize $args

	# Set the top-level window title
	wm title $w_ "GAIA: ESP source selections ($itk_option(-number))"

	itk_component add objlist {
	    TableList $w_.objlist \
		    -title "Selected objects" \
		    -headings {X Y Size canvasid} \
		    -sizes {10 10 10 10}
	}
	$itk_component(objlist) set_option canvasid Show 0
	$itk_component(objlist) set_option X Precision 1
	$itk_component(objlist) set_option Y Precision 1
	$itk_component(objlist) set_option Radius Precision 1
	pack $itk_component(objlist) -fill both -expand 1

	# Initialise the table with the current list of sources, if any
	if { $object_list_ != {} } {
	    remake_table_
	}

	itk_component add AddSource {
	    button $w_.addsource \
		    -text "Add object" \
		    -command [code $this add_source]
	}
	$itk_component(AddSource) configure -highlightbackground black
	add_short_help $itk_component(AddSource) \
		{Press and then drag out source limit on image}
	pack $itk_component(AddSource) \
		-side left -expand true -padx 2 -pady 2

	itk_component add DeleteSource {
	    button $w_.deletesource \
		    -text "Delete object" \
		    -command [code $this delete_source]
	}
	add_short_help $itk_component(DeleteSource) \
		{Delete selected source from the list}
	pack $itk_component(DeleteSource) \
		-side right -expand true -padx 2 -pady 2

	itk_component add Close {
	    button $w_.close \
		    -text "Close" \
		    -command [code $this close_window]
	}
	add_short_help $itk_component(Close) \
		{Close window}
	pack $itk_component(Close) \
		-side right -expand true -padx 2 -pady 2
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

    # Canvas, canvasdraw and rtdimage variables
    public variable canvas {} {}
    public variable canvasdraw {} {}
    public variable rtdimage {} {}

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
    #puts "add_source_finish: [$lastobj coords]"

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
    #puts "remake: new contents: $newcontents"
    $itk_component(objlist) clear
    $itk_component(objlist) append_rows $newcontents
    $itk_component(objlist) new_info
}

# Delete a source from the canvas, the table, and the object_list_
# The elements in the table are in the same order as the elements in 
# object_list_ (maintained so by update_source_)
body gaia::GaiaEspSelectListEditor::delete_source {} {
    set dlist [$itk_component(objlist) get_selected_with_rownum]
    if {[llength $dlist] == 0} {
	error_dialog "Select a source first"
    } else {
	# go through the list is descending order, so we remove items from
	# object_list_ from the end
	set sdlist [lsort -decreasing $dlist]
	#puts "delete-list: $dlist"
	foreach row $sdlist {
	    set rownum [lindex $row 0]
	    set rowval [lindex $row 1]
	    #puts "Deleting $row"
	    # delete from object_list_
	    set object_list_ [lreplace $object_list_ $rownum $rownum]
	    # Delete from canvas last.  This update to the canvas calls
	    # update_source, which in turn calls remake_table_.
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

body gaia::GaiaEspSelectListEditor::close_window {} {
    delete object $this
}
    
