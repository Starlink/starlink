#+
#  Name:
#     Ccd::reveal

#  Purpose:
#     Provides a class for hiding and revealing many sub-windows within
#     one window.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     This class provides a method for controlling the display of one
#     of a set of related windows in a larger window. A row of radio
#     buttons are programmed to reveal a named window (and all of its
#     packed children), in a space above or below the button bar. This
#     is for use when many displays of information are required, but
#     window size considerations make it possible to view only one at
#     a time.
#
#
#
#           |   |View 1|  |View 2|  |View 3|  |View 4|   |
#
#
#
#
#
#           |                  V I E W  1                |
#
#
#
#
#
#     So pressing button "View 2" would unpack the "VIEW 1" window and
#     pack into its place "VIEW 2" etc.
#
#     Note that unless you provide a -in option then the windows
#     revealed should be created after invoking this class and need to
#     be descended from the instance created (so that packing
#     works). No information about the actual contents of the windows
#     is contained here, so for a tidy exit they should be destroyed
#     before this.

#  Configuration Options:
#        -placebar (top|bottom|left|right)
#
#     This option configures where to place the choice (button) bar
#     component, either above, below, to the left or to the right of
#     the part to contain the windows that are revealed.
#
#        -stack (horizontal|vertical|array)
#
#     Controls how the array of buttons are displayed (See Ccd::radioarray).
#
#         -columns columns
#
#     Number of columns used when -stack is set to array.
#
#         -label "string"
#
#     String for labelling the radiobuttons (See Ccd::radioarray).
#
#          -in window
#
#     Name of a window to pack the revealed window into. This must
#     exist and be a parent or a descendent of the parent of the
#     windows to be controlled. If not supplied this defaults to the
#     name of this instance, so the windows to be controlled should be
#     children of this. Reconfiguring in will apply to all windows.

#  Inheritance:
#     This class inherits "Ccd::base" and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Invocations:
#        Ccd::reveal window [-option value]...
#
#     This command creates an instance of a "Ccd::reveal" and returns a
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
#         creates the "Ccd::reveal" widget with a default configuration,
#         except when overridden by command line options.
#     destructor
#        Destroys the "Ccd::reveal" instance, invoked by the "delete"
#        method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#         is given then the current value of any known option is returned
#         in a form similar (but not identical to) the Tk widget command.
#     addbutton text [window]
#        This method adds a button to the choice bar and enters the
#        text as its label. The window argument is optional and should
#        be the name of a window to reveal in response to a press of
#        this button.
#     addwindow text window
#        Adds/replaces a window associated with the button whose label
#        is "text".
#     resettext name text
#        Set the text label of a button to a new value.
#     seewindow window
#        Reveals the named window (must be registered with a button).

#  Copyright:
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
#     15-JUN-1995 (PDRAPER):
#        Original version.
#     15-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     27-JAN-2006 (PDRAPER):
#        Updated for itcl::class syntax.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   itcl::class Ccd::reveal {

#  Inheritances:
      inherit Ccd::base

#.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the class and configures it with
#  the default and command-line options.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { args } {

#  Create a base frame widget. This must have the same name as the class
#  command.
#  Create the radioarray bar for the containing the buttons.
         CCDCcdWidget Bar bar Ccd::radioarray $oldthis.bar

#  Set default configurations.
         eval configure $args
         configure -placebar          $placebar
         configure -columns           $columns
         configure -stack             $stack
         configure -label             $label

#  Define sub-component widgets for configuration via the wconfig
#  method.
         set widgetnames($Oldthis:bar) $Bar
      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add a button to the choice bar.
      method addbutton { text args } {
         if { $args == "" } {
            $Bar addbutton $text [list $text]
         } else {
            $Bar addbutton $text [list $text] "$Oldthis seewindow $args"
            set lastwin $args
         }
      }

#  Change/add window to be revealed.
      method addwindow { text Window } {
         $Bar addcommand $text "$Oldthis seewindow $Window"
      }

#  Change the revealed window (use the last window if none given).
      method seewindow { Window } {
         set window [CCDPathOf $Window]
         if { $Window == "" } {
            if { $Lastwin != "" } {
               set Window $Lastwin
            }
         }
         if { $Packed != $Window } {
            if { $Packed != "" } {
               set packed [CCDPathOf $Packed]
               pack forget $packed
            }
            set inwin [CCDPathOf $in]
            switch -exact $placebar {
               top {
                  pack $window -in $inwin -side bottom -fill both -expand true
                  pack $bar -side top -fill x
               }
               bottom {
                  pack $window -in $inwin -side top -fill both -expand true
                  pack $bar -side bottom -fill x
               }
               left {
                  pack $window -in $inwin -side right -fill both -expand true
                  pack $bar -side left -fill y
               }
               right {
                  pack $window -in $inwin -side left -fill both -expand true
                  pack $bar -side right -fill y
               }
            }
            set Packed $Window
         }
      }

#  Return the name of the currently select window.
      method current {} {
         if { $Packed != "" } {
            return $Packed
         } else {
            return ""
         }
      }

#  Invoke the button associated with a window.
      method invoke name {
         if { $name != "" } {
            $Bar invoke $name
         }
      }

#  Set a button text string to a new value.
      method resettext { name text } {
         $Bar resettext $name $text
      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Where to place the array of radio buttons.
      public variable placebar {top} {
         if { $exists } {
            if { $Packed != "" } {
               set Window $Packed
               set Packed ""
               seewindow $Window
            } else {
               switch -regexp $placebar {
                  (top|bottom) {
                     pack $bar -side $placebar -fill x
                  }
                  (left|right) {
                     pack $bar -side $placebar -fill y
                  }
               }
            }
         }
      }

#  Label for the radiobuttons.
      public variable label {} {
         if { $exists } {
            $Bar configure -label $label
         }
      }

#  Stack method for the radiobuttons.
      public variable stack {array} {
         if { $exists } {
            $Bar configure -stack $stack
         }
      }

#  Number of columns used when stacking in an array.
      public variable columns 5 {
         if { $exists } {
            $Bar configure -columns $columns
         }
      }

#  Window to parent the windows that are controlled.
      public variable in {} {
         if { $in == "" } {
            set in $Oldthis
         }
      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Names of widgets.
      protected variable Bar
      protected variable bar ""

#  Name of currently packed window.
      protected variable Packed ""

#  Name of last window
      protected variable Lastwin ""

#  End of class defintion.
   }

# $Id$
