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
#  Invocations:
#
#    gaia::StarLabelWidgetChild object_name [configuration options]
#
#  Configuration options:
#
#    No additional options
#
#  Inheritance:
#    LabelWidget
#
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
    inherit util::LabelWidget

    constructor {args} {
	uplevel $itk_component(label) config -justify left
	eval itk_initialize $args
    }
    destructor {
    }

    public method childsite {} {
	return $itk_interior
    }
}
