#+
#  Name:
#     Ccd::checkarray

#  Purpose:
#     Declares a class for creating an array of checkbuttons with a
#     label.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     This class creates an instance of a checkarray. The checkarray
#     contains checkbuttons which each have a different text label (via
#     which they are referred) and which have a single descriptive label
#     associated with them.

#  Configuration Options:
#        -stack horizontal|vertical
#
#     The order in which the checkbuttons are stacked. "vertical" is the
#     default.
#
#        -label "string"
#
#     Specifies the label to apply to the checkarray as a whole. If
#     stacking is vertical then this appears to the left of the array.
#     If stacking is horizontal then it appears above the array.

#  Inheritance:
#     This class inherits Ccd::base and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Invocations:
#        Ccd::checkarray window [-option value]...
#
#     This command create an instance of a checkarray and returns a
#     command "window" for manipulating it via the methods and
#     configuration options described below. Configuration options may
#     be appended to the command.
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
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#        creates the "class " widget with a default configuration,
#        except when overridden by command line options.
#     destructor
#        Destroys the "class" instance, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#        is given then the current value of any known option is returned
#        in a form similar (but not identical to) the Tk widget command.
#     invoke name
#        Invokes the named checkbutton. The name is that used when creating
#        it.
#     _repack
#        Internal method for re-packing the checkbuttons.
#     addcommand name command
#        Adds a command to the named checkbutton.
#     addvariable name variable args
#        Adds a variable to the checkbutton. This is set to the value 0
#        or 1 if no on and off values are given, The on and off values
#        are the optional args argument.
#     addbutton name args
#        Creates a checkbutton and packs it into the array.  "args" is
#        optional and may be any of the arguments as passed to the
#        "checkbutton" command (i.e. -variable variable -onvalue
#        onvalue -offvalue offvalue etc.).
#     state name button_state
#        Set the state of the named button. "button_state" should be one
#        of normal, active or disabled.
#     toggle name
#        Toggles the selection state of the named button.
#     select name
#        Selects the named button.
#     deselect name
#        Deselects the named button.
#     sethelp name document label.
#        Sets the context sensitive help information for the button
#        $name. $document should be the name of a HTML document and
#        $label the label (HTML anchor) within the document that
#        identifies the part to be displayed. If $name is "all" then
#        the document is associated with all sub-components of the
#        widget at that time.

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
#     23-APR-1994 (PDRAPER):
#        Original version.
#     9-MAY-1994 (PDRAPER):
#        Added state and toggle methods.
#     24-MAY-1994 (PDRAPER):
#        Added select and de-select methods.
#     23-MAR-1995 (PDRAPER):
#        Added sethelp method.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     12-MAY-2000 (MBT):
#        Upgraded to Tcl8.
#     27-JAN-2006 (PDRAPER):
#        Updated to itcl::class syntax.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   itcl::class Ccd::checkarray {

#  Inheritances:
      inherit Ccd::base

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the class and configures it with
#  the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { args } {

#  Create a frame widget. This must have the same name as the class
#  command.
#  Set default configurations.
         eval configure $args
         configure -label             $label
         configure -stack             $stack
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Add a checkbutton to the array. Buttons are named using a incremented
#  integer (since we can not have uppercase in window names we cannot
#  just use the text label) and the actual names (with which other
#  methods are directed) are the indices of an array which points to the
#  integer.
      method addbutton { name args } {
         set newwidth [ string length $name ]
         incr newwidth +6
         if { $newwidth > $buttonwidth } {
	    set buttonwidth $newwidth
            set resize 1
         }
         incr nbutton
         eval CCDTkWidget Button button \
            checkbutton $oldthis.button$nbutton \
                  -text "$name" \
	          -width $buttonwidth \
	          -anchor w \
                  $args
         set Buttons($name) $Button
         lappend Buttonlist $Button

#  And repack buttons.
         _repack

#  Define sub-component widgets for configuration via the wconfig
#  method.
         set widgetnames($Oldthis:$name) $Button
         set widgetfocus($Oldthis:$name) $Button
      }

#  Method to add a command to a button.
      method addcommand { name command } {
         if { [ info exists Buttons($name) ] } {

#  Add the command.
            $Buttons($name) configure -command "$command"
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  Private method for repacking the array.
      method _repack {} {
         if { $stack == "horizontal" } {
            set side "left"
         } else {
            set side "top"
         }
         if { $nbutton > 0 } {
            foreach Button $Buttonlist {

#  See if buttons need resizing before packing
               if { $resize } { $Button configure -width $buttonwidth }
               set button [CCDPathOf $Button]
               pack forget $button
               pack $button -side $side -anchor w
	    }
            set resize 0
         }
      }

#  Method for invoking named button.
      method invoke name {
         if { [ info exists Buttons($name) ] } {

#  Invoke the button.
            $Button invoke
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  Method to add a variables with optional on and off values to a button.
      method addvariable { name variable args } {
         if { [ info exists Buttons($name) ] } {

#  See if on and off values have been given.
            set on [lindex $args 0]
	    set off [lindex $args 1]
	    if { $on != {} && $off != {} } {

#  Add the variable.
               $Buttons($name) configure \
                  -variable "$variable" -onvalue "$on" -offvalue "$off"
            } else {

#  Variable has existing on and off values (0 and 1).
               $Buttons($name) configure -variable "$variable"
            }
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  Set the state of a named button.
      method state { name state } {
         if { [ info exists Buttons($name) ] } {

#  Set the button state.
               $Buttons($name) configure -state $state
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  Toggle the selection state of a named button.
      method toggle { name } {
         if { [ info exists Buttons($name) ] } {

#  Toggle the button selection state.
               $Buttons($name) toggle
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  Select a named button.
      method select { name } {
         if { [ info exists Buttons($name) ] } {
               $Buttons($name) select
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  De-select a named button.
      method deselect { name } {
         if { [ info exists Buttons($name) ] } {
               $Buttons($name) deselect
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  Method for assigning context help to a button.
      method sethelp {name docname label} {
         if { $name == "all" } {

#  Request to bind all elements to this help.
	    if { $nbutton > 0 } {
               foreach oneof [ array names Buttons ] {
		  Ccd::base::sethelp \
		     $Buttons($oneof) $docname $label
	       }
	    }
	    Ccd::base::sethelp $Oldthis $docname $label
            if { [ winfo exists $labelwidget ] } {
	       Ccd::base::sethelp $Labelwidget $docname $label
            }
	 } else {
	    if { [ info exists Buttons($name) ] } {
	       Ccd::base::sethelp $Buttons($name) $docname $label
	    }
	 }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Order for packing buttons. Can horizontal or vertical.
      public variable stack vertical {
         if { $exists } {
            _repack
         }
      }

#  Add a label to the checkarray.
      public variable label {} {
         if { $exists } {
            if { $label != {} } {
               if { $havelabel } {
                  $labelwidget configure -text "$label"
               } else {
                  CCDTkWidget Labelwidget labelwidget \
                     label $oldthis.label -text "$label"
                  if { $stack == "vertical" } {
                     pack $labelwidget -side left -anchor w
                  } else {
                     pack $labelwidget -side top -anchor w
                  }
                  set havelabel 1
               }
               _repack
            } else {
               if { [ winfo exists $labelwidget ] } {
                  destroy $labelwidget
               }
            }
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Number of buttons in the menubar and their names.
      protected variable nbutton 0
      protected variable Buttons
      protected variable Buttonlist

#  Widget for label.
      protected variable Labelwidget
      protected variable labelwidget ""
      protected variable havelabel 0

#  The widths of the buttons. The actual width is never less than this
#  and all buttons are the same width.
      protected variable buttonwidth 12
      protected variable resize 0

#  End of class definition.
   }
# $Id$
