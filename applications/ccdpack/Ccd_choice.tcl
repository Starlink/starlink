#+
#  Name:
#     Ccd::choice

#  Purpose:
#     Declares a class for creating a choice bar.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     This class creates an instance of a choice bar. The choice bar
#     contains option buttons for performing one-off commands. The
#     standard choice bar contains the options "OK" and "Cancel".
#
#     The bar may have a label describing its purpose. This is placed
#     above buttons that are stacked horizontally and to the left of
#     buttons stacked vertically.

#  Configuration Options:
#        -standard boolean
#
#     This options causes the choice bar to have the standard
#     configuration of an "OK" and "Cancel" button. Standard true (1)
#     is the default. If this behaviour is to be overridden then the
#     "-standard 0" option should be used when the choice bar instance
#     is created.
#
#        -stack horizontal|vertical
#
#     The order in which the buttons are stacked. "horizontal" is the
#     default.
#
#        -label description
#
#     A label that describes the purpose of the choice bar. This goes
#     above the buttons if the stacking form is horizontal and to the
#     left if vertical. If null {} then no label is used. The default is
#     to have no label.
#
#        -width value
#
#     Width of the label. This is used to control vertical stacks that
#     require alignment.
#
#        -buttonwidth value
#
#     Minimum width of all the buttons, this may increase to
#     accomodate more text and all buttons will adopt the new size.

#  Inheritance:
#     This class inherits Ccd::base and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Invocations:
#        Ccd::choice window [-option value]...
#
#     This command creates an instance of a choice bar and returns a
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
#        Invokes the named button. The name is that used when creating
#        it (OK, Cancel etc.).
#     _repack
#        Internal method for re-packing the buttons in the choice bar.
#     addcommand name command
#        Adds a command to the named button.
#     addbutton name args
#        Creates a button and packs it into the choice bar. "args" is
#        optional and should be the button command if used.
#     focus name
#        Sets the focus to the named button.
#     sethelp name document label.
#        Sets the context sensitive help information for the button
#        $name. $document should be the name of a HTML document and
#        $label the label (HTML anchor) within the document that
#        identifies the part to be displayed. If $name is "all" then
#        the document is associated with all sub-components of the
#        widget at that time.
#     state name (normal|disabled|active)
#        Sets the state of the button $name. If $name is "all" then all
#        buttons are set to this state.

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
#     28-MAR-1994 (PDRAPER):
#        Original version.
#     22-MAR-1995 (PDRAPER):
#        Added sethelp method.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     22-MAY-1995 (PDRAPER):
#        Added configuration option for a label.
#     23-MAY-1995 (PDRAPER):
#        Added state method.
#     24-MAY-1995 (PDRAPER):
#        Added buttonwidth configuration option.
#     7-NOV-1995 (PDRAPER):
#        Added focus method.
#     12-MAY-2000 (MBT):
#        Upgraded to Tcl8.
#     27-JAN-2006 (PDRAPER):
#        Updated to itcl::class syntax.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   itcl::class Ccd::choice {

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
         configure -standard          $standard
         configure -label             $label
         configure -maxwidth          $maxwidth
         configure -width             $width
         configure -buttonwidth       $buttonwidth
         configure -stack             $stack
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add a button to menubar. Buttons are named using a incremented
#  integer (since we can not have uppercase in window names we cannot
#  just use the text label) and the actual names (with which other
#  methods are directed) are the indices of an array which points to the
#  integer.
      method addbutton { name args } {
         set newwidth [ string length $name ]
         if { $newwidth > $buttonwidth } {
            if { $newwidth <= $maxwidth || $maxwidth == 0 } {
               set buttonwidth $newwidth
               set resize 1
            }
         }
         incr nbutton
         if { $args == {} } {
            CCDTkWidget Button button \
               button $oldthis.button$nbutton \
                      -text "$name" -width $buttonwidth
         } else {
            CCDTkWidget Button button \
               button $oldthis.button$nbutton \
                      -text "$name" -width $buttonwidth \
                      -command [join $args]
         }
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
            error "No choice button of name \"$name\""
         }
      }

#  Private method for repacking the choice bar.
      method _repack {} {
         if { $stack == "horizontal" } {
            set side "left"
         } else {
            set side "top"
         }

#  First unpack buttons to make sure label is in the correct place.
         if { $nbutton > 0 } {
            foreach Button $Buttonlist {
               pack forget [CCDPathOf $Button]
            }
         }

#  Pack label first.
         if { $havelabel } {
            if { $side == "top" } {
               pack $labelwidget -side left -expand true -fill x
            } else {
               pack $labelwidget -side top -expand true -fill x
            }
         }
         if { $nbutton > 0 } {
            foreach Button $Buttonlist {

#  See if buttons need resizing before packing
               if { $resize } {
                  $Button configure -width $buttonwidth
               }
               pack [CCDPathOf $Button] -side $side -expand true
            }
            set resize 0
         }
      }

#  Method for setting focus to a button.
      method focus name {
         if { [info exists Buttons($name)] } {
            ::focus [CCDPathOf $Buttons($name)]
         } else {
            error "No choice button of name \"$name\""
         }
      }

#  Method for invoking named button.
      method invoke name {
         if { [ info exists Buttons($name) ] } {

#  Invoke the button.
            $Buttons($name) invoke
         } else {
            error "No choice button of name \"$name\""
         }
      }

#  Method for assigning context help to a button.
      method sethelp {name docname label} {
         if { $name == "all" } {

#  Request to bind all elements to this help.
            if { $nbutton > 0 } {
               foreach oneof [ array names Buttons ] {
                  Ccd::base::sethelp $Buttons($oneof) $docname $label
               }
            }
            Ccd::base::sethelp $Oldthis $docname $label
         } else {
            if { [ info exists Buttons($name) ] } {
               Ccd::base::sethelp $Buttons($name) $docname $label
            }
         }
      }

#  Set state of button method.
      method state {name status} {
         if { $name == "all" } {
            if { $nbutton > 0 } {
               foreach oneof [ array names Buttons ] {
                  $Buttons($oneof) configure -state $status
               }
            }
         } else {
            if { [ info exists Buttons($name) ] } {
               $Buttons($name) configure -state $status
            }
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add the standard buttons to choice bar.
      public variable standard 1 {
         if { $exists } {
            if { $standard && ! $havestandard } {
               addbutton OK
               addbutton Cancel
               set havestandard 1
            }
         }
      }

#  Order for packing buttons. Can horizontal or vertical.
      public variable stack horizontal {
         if { $exists } {
            _repack
         }
      }

#  Optional label.
      public variable label "" {
         if { $exists } {
            if { $label != "" } {
               if { $havelabel } {
                  $Labelwidget configure -text "$label"
               } else {
                  CCDTkWidget Labelwidget labelwidget \
                     label $oldthis.label -text "$label" \
                            -justify center -anchor center
               }
               set havelabel 1
               _repack
            } else {
               if { $havelabel } {
                  destroy $labelwidget
                  _repack
               }
               set havelabel 0
            }
         }
      }

#  Width of label.
      public variable width 0 {
         if { $exists } {
            if { $label != "" } {
               $Labelwidget configure -width $width
            }
         }
      }

#  Width of all buttons.
      public variable buttonwidth 7 {
         if { $buttonwidth > $maxwidth && $maxwidth != 0 } {
            set buttonwidth $maxwidth
         }
         set resize 1
      }

#  Maximum width of buttons
      public variable maxwidth 0 {}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Number of buttons in the menubar and their names.
      protected variable nbutton 0
      protected variable Buttons
      protected variable Buttonlist

#  Notice if buttons need resizing.
      protected variable resize 0

#  Presence of label.
      protected variable havelabel 0

#  Presence of standard buttons.
      protected variable havestandard 0

#  Name of label widget.
      protected variable Labelwidget
      protected variable labelwidget ""

#  End of class defintion.
   }
# $Id$
