   itcl_class Ccd_labent {
#+
#  Name:
#     Ccd_labent

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines widget consisting of an label and an entry widget.

#  Description:
#     This routine defines a widget class which consist of a label
#     widget and an entry widget. A labelled entry widget.

#  Invocations:
#
#        Ccd_labent window 
#
#     Returns a command "window" which is available in the global scope.
#     This may be optionally followed by any of the configuration
#     options.
#
#        window configure -configuration_options value
#
#     Applies any of the configuration options (after the widget
#     instance has been created).
#
#        window method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#
#        -text  "string"
#
#     Set the text string of the label widget. Defaults to "label".
#
#        -placelabel "left|right|top|bottom"
#
#     The place that the label is put with respect to the entry widget.
#     Must be one of left, right, top or bottom.
#
#        -textvariable global_variable_name
#
#     Specifies a global variable to contain the contents of the entry
#     widget. This is updated at all times to contain the contents of
#     the entry widget (and vice-versa).
#
#        -width [width]
#
#     Sets the width of the entry widget (usually in characters). If the
#     value for width is not supplied then the current width is
#     returned.
#
#        -state (normal|disabled)
#
#     Sets the state of the entry widget. Disabled means that text
#     cannot be entered.

#  Methods:
#     constructor
#        Creates the widget command.
#
#        This method is invoked automatically when the class is asked to
#	 create an object. It is also invoked by classes which inherit
#	 from this class (this happens after the constructor method of
#	 the inheriting class unless the constructor of this class is
#	 invoked explicitly). 
#     destructor
#        Deletes the widget command via the "delete" method.
#     configure
#        Activates the configuration options. 
#     get
#        Returns the contents of the entry widget.
#     clear args
#        Clears the contents of the entry widget. The args are those
#	 which the entry widget "delete" command expects. I.e. a first
#	 and (optional) last index.
#     insert args
#        Inserts text into the entry widget. The args are those used by
#	 the entry widget. I.e. an index and the text string.
#     sethelp document label.
#        Sets the context sensitive help information for all the 
#        components of the meta-widget that exist. $document should be
#        the name of a HTML document and $label the label (HTML
#        anchor) within the document that identifies the part to be
#        displayed. 

#  Inheritance:
#     This widget inherits methods and configuration options from the
#     following superclasses.
#
#        Ccd_base
#
#     These should be consulted for the methods and options which they
#     supply.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     22-MAR-1994 (PDRAPER):
#     	 Original version.
#     22-MAR-1995 (PDRAPER):
#        Added sethelp method.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     30-JUN-1995 (PDRAPER):
#        Added -state configuration option.
#     {enter_changes_here}

#-

#  Inheritance:
      inherit Ccd_base
#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construct the widget from the basic components.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { config } {

#  Now add the label and entry widgets
         label $oldthis.label
	 entry $oldthis.entry

#  Check options database for values to override widget defaults. Look for more
#  specific option of having a class specified, if this fails try for less 
#  specific class.
         set opt [ _getoption "placelabel Placelabel PlaceLabel"]
         if { $opt != {} } { set placelabel $opt }

#  Set default configurations.
         configure -placelabel $placelabel
         configure -text $text
         configure -textvariable $textvariable
         configure -state $state

#  Define sub-component widgets for configuration via the wconfig and bind.
         set widgetnames($oldthis:label) $oldthis.label
         set widgetnames($oldthis:entry) $oldthis.entry
         set widgetfocus($oldthis:entry) $oldthis.entry
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Insert text into the entry widget.
      method insert { args } {
         eval $oldthis.entry insert $args
      }

#  Delete text in entry widget.
      method clear { args } {
         eval $oldthis.entry delete $args
      }

#  Return the text in the entry widget.
      method get {} {
         return [$oldthis.entry get]
      }

#  Method for assigning context help.
      method sethelp {docname label} {
	 Ccd_base::sethelp $oldthis $docname $label
	 Ccd_base::sethelp $oldthis.entry $docname $label
         Ccd_base::sethelp $oldthis.label $docname $label
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      public text "label" {
         if $exists {
            $oldthis.label configure -text $text
	 }
      }

#  Configure label and entry relative positions.
      public placelabel left {
         if $exists {
            if { [ regexp (left|right|top|bottom) $placelabel] } {

#  Unpack the widgets in preparation for re-packing.
               pack forget $oldthis.label $oldthis.entry

#  Do the actions necessary to pack the label and entry.
               pack $oldthis.label -side $placelabel -fill x
               pack $oldthis.entry -side $placelabel -expand true -fill both

#  Not a recognised option.
            } else {
               error "Unknown placement for label \"$placelabel\" should be\
one of \"left\", \"right\", \"top\" or \"bottom\""
            }
         }
      }

#  Define a textvariable to hold the contents of the entry widget.
      public textvariable {} {
         if $exists {
            if { $textvariable != {} } {
               $oldthis.entry configure -textvariable $textvariable
            } else {
               $oldthis.entry configure -textvariable {}
            }
         }
      }

#  Width of the entry widget. Returns current width if no value.
      public width {} {
         if $exists {
            if { $width != {} } { 
               $oldthis.entry configure -width $width 
            } else {
               set realwidth [ $oldthis.entry configure -width ]
               return [ lindex $realwidth 4 ]
	    }
	 }
      }

#  Set state of entry widget.
      public state normal {
         if $exists  { 
            $oldthis.entry configure -state $state
         }
      }
  
#  End of class definition.
   }
# $Id$
