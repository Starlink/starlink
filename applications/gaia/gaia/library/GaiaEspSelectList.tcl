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

itk::usual GaiaEspSelectList {}

itcl::class gaia::GaiaEspSelectList {

    # --- Inheritances
    # None

    # --- Constructor
    constructor {args} {
	# Evaluate any options
	eval configure $args
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

    # Drawing mode for shapes on canvas
    public variable sourceshape {} {}

    # Maximum number of objects to be drawn
    public variable maxobjects {} {}

    # Create an editor panel
    public method edit_sourcelist {}

    # Set and return the list of sources
    public method set_sourcelist {new_list}
    public method get_sourcelist {}
    public method default_config {}

    # --- Protected variables
    # None

    # --- Private variables

    # The list of selected sources, edited by GaiaEspSelectListEditor,
    # and set by calls to set_sourcelist
    private variable object_list_ {}
}

body gaia::GaiaEspSelectList::edit_sourcelist {} {
    gaia::GaiaEspSelectListEditor .#auto $this $object_list_ \
	    -canvas $canvas \
	    -canvasdraw $canvasdraw \
	    -rtdimage $rtdimage \
	    -drawmode $sourceshape \
	    -maxobjects $maxobjects
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
