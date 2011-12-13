#+
#  Name:
#     Ccd::labent

#  Purpose:
#     Defines widget consisting of an label and an entry widget.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     This routine defines a widget class which consist of a label
#     widget and an entry widget. A labelled entry widget.

#  Configuration Options:
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

#  Inheritance:
#     This widget inherits methods and configuration options from the
#     following superclasses.
#
#        Ccd::base
#
#     These should be consulted for the methods and options which they
#     supply.

#  Invocations:
#        Ccd::labent window
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

#  Methods:
#     constructor
#        Creates the widget command.
#
#        This method is invoked automatically when the class is asked to
#        create an object. It is also invoked by classes which inherit
#        from this class (this happens after the constructor method of
#        the inheriting class unless the constructor of this class is
#        invoked explicitly).
#     destructor
#        Deletes the widget command via the "delete" method.
#     configure
#        Activates the configuration options.
#     get
#        Returns the contents of the entry widget.
#     clear args
#        Clears the contents of the entry widget. The args are those
#        which the entry widget "delete" command expects. I.e. a first
#        and (optional) last index.
#     insert args
#        Inserts text into the entry widget. The args are those used by
#        the entry widget. I.e. an index and the text string.
#     sethelp document label.
#        Sets the context sensitive help information for all the
#        components of the meta-widget that exist. $document should be
#        the name of a HTML document and $label the label (HTML
#        anchor) within the document that identifies the part to be
#        displayed.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2000 Central Laboratory of the Research
#     Councils. Copyright (C) 2006 Particle Physics & Astronomy
#     Research Council. All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     22-MAR-1994 (PDRAPER):
#        Original version.
#     22-MAR-1995 (PDRAPER):
#        Added sethelp method.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     30-JUN-1995 (PDRAPER):
#        Added -state configuration option.
#     12-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     27-JAN-2006 (PDRAPER):
#        Updated for itcl::class syntax.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   itcl::class Ccd::labent {

#  Inheritance:
      inherit Ccd::base
#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construct the widget from the basic components.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { args } {

#  Now add the label and entry widgets
         CCDTkWidget Labelwidget labelwidget label $oldthis.label
	 CCDTkWidget Entrywidget entrywidget entry $oldthis.entry

#  Check options database for values to override widget defaults. Look for more
#  specific option of having a class specified, if this fails try for less
#  specific class.
         set opt [ _getoption "placelabel Placelabel PlaceLabel"]
         if { $opt != {} } { set placelabel $opt }

#  Set default configurations.
         eval configure $args
         configure -placelabel $placelabel
         configure -text $text
         configure -textvariable $textvariable
         configure -state $state

#  Define sub-component widgets for configuration via the wconfig and bind.
         set widgetnames($Oldthis:label) $Labelwidget
         set widgetnames($Oldthis:entry) $Entrywidget
         set widgetfocus($Oldthis:entry) $Entrywidget
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Insert text into the entry widget.
      method insert { args } {
         eval $Entrywidget insert $args
      }

#  Delete text in entry widget.
      method clear { args } {
         eval $Entrywidget delete $args
      }

#  Return the text in the entry widget.
      method get {} {
         return [$Entrywidget get]
      }

#  Method for assigning context help.
      method sethelp {docname label} {
	 Ccd::base::sethelp $Oldthis $docname $label
	 Ccd::base::sethelp $Entrywidget $docname $label
         Ccd::base::sethelp $Labelwidget $docname $label
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      public variable text "label" {
         if { $exists } {
            $Labelwidget configure -text $text
	 }
      }

#  Configure label and entry relative positions.
      public variable placelabel left {
         if { $exists } {
            if { [ regexp (left|right|top|bottom) $placelabel] } {

#  Unpack the widgets in preparation for re-packing.
               pack forget $labelwidget $entrywidget

#  Do the actions necessary to pack the label and entry.
               pack $labelwidget -side $placelabel -fill x
               pack $entrywidget -side $placelabel -expand true -fill both

#  Not a recognised option.
            } else {
               error "Unknown placement for label \"$placelabel\" should be\
one of \"left\", \"right\", \"top\" or \"bottom\""
            }
         }
      }

#  Define a textvariable to hold the contents of the entry widget.
      public variable textvariable {} {
         if { $exists } {
            if { $textvariable != {} } {
               $Entrywidget configure -textvariable $textvariable
            } else {
               $Entrywidget configure -textvariable {}
            }
         }
      }

#  Width of the entry widget. Returns current width if no value.
      public variable width {} {
         if { $exists } {
            if { $width != {} } {
               $Entrywidget configure -width $width
            } else {
               set realwidth [ $Entrywidget configure -width ]
               return [ lindex $realwidth 4 ]
	    }
	 }
      }

#  Set state of entry widget.
      public variable state normal {
         if { $exists } {
            $Entrywidget configure -state $state
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      protected variable Labelwidget
      protected variable labelwidget ""
      protected variable Entrywidget
      protected variable entrywidget ""

#  End of class definition.
   }
# $Id$
