#+
#  Name:
#     Ccd::option

#  Purpose:
#     Defines the class of option labelled entry widgets.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     This class creates a labelled entry widget with an additional
#     menubutton. A menu of possible options can be created which will
#     be available using a menubutton (which shows a distinctive
#     bitmap). The chosen option is then entered into the entry
#     widget. In this way specific string choices for the entry widget
#     may be provided. Options can be either a symbolic name and
#     associated value or just a value shown for both.

#  Configuration Options:
#        -placelabel.
#
#     Shouldn't be used, always on the left.
#
#        -contrain boolean
#
#     Whether to make the value shown in the entry field always one
#     of the options. This is achieved by binding <1> to show the
#     popup in the entry (so it is impossible to type into this
#     field).

#  Inheritance:
#     This class inherits Ccd::labent and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Invocations:
#        Ccd::option window [-option value]...
#
#     This command create an instance of an option entry widget and
#     returns a command "window" for manipulating it via the methods
#     and configuration options described below. Configuration options
#     may be appended to the command.
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
#     destructor
#        Destroys the "class" instance, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#         is given then the current value of any known option is returned
#         in a form similar (but not identical to) the Tk widget command.
#     addoption label [value]
#        Adds a label to the menu. This creates a menu item with
#         this label and inserts this value into the entry widget
#        if no value is given, otherwise value is used.
#     postmenu x y
#        Forces the posting of the menu at the given (absolute)
#         position.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 1997, 2000 Central Laboratory of the Research
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
#     29-MAR-1994 (PDRAPER):
#        Original version.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     19-MAY-1994 (PDRAPER):
#        Removed bitmap in menubutton and replaced with -indicatoron.
#        Fixed up problems with posting under 4 (no point in moving to
#        tk_optionMenu, this requires that all option values are known
#        when the menu is created). Removed unpost method as no longer
#        required. All now considerably simplified.
#     15-APR-1997 (PDRAPER):
#        Extended to use a label and value, rather than just a single
#        value for both fields. Removed popup behaviour when in entry
#        field. This behaviour should now be asked for explicitly
#        via the constrain option.
#     12-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     27-JAN-2006 (PDRAPER):
#        Updated for itcl::class syntax.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   itcl::class Ccd::option {


#  Inheritances:
      inherit Ccd::labent

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the class and configures it with
#  the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { args } {

#  Add a menubutton for displaying the options and bitmap.
         CCDTkWidget Mb mb menubutton $oldthis.mb \
                              -indicatoron 1 \
	                      -menu $oldthis.mb.m \
	                      -relief raised

#  And create the menu.
         CCDTkWidget Menu menu menu $mb.m -tearoff 0

#  Configuration only allows label on left.
         eval configure $args
         configure -placelabel $placelabel
         configure -constrain $constrain

#  Pack sub-widgets.
         pack $mb -side left

#  Define sub-component widgets for configuration via the basic
#  configuration methods.
         set widgetnames($Oldthis:menubutton) $Mb
         set widgetfocus($Oldthis:menubutton) $Mb
         set widgetnames($Oldthis:menu) $Menu
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add an option method.
      method addoption { label args } {
         switch [llength $args] {
            0 {
               $Menu add command \
                  -label $label \
                  -command "$Oldthis clear 0 end
                            $Oldthis insert 0 \"$label\"
                           "
            }
            1 {
               $Menu add command \
                  -label $label \
                  -command "$Oldthis clear 0 end
                            $Oldthis insert 0 \"$args\"
                           "
            }
            2 {
               $Menu add command \
                  -label $label \
                  -command "$Oldthis clear 0 end
                            $Oldthis insert 0 \"[lindex $args 0]\"
                            [lindex $args 1]
                           "
            }
         }
      }

#  Control menu posting method.
      method postmenu { x y } {
	 tk_popup $menu $x $y
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Set the placement of the label using the internal method of
#  Ccd::labent which controls this. Should always be on the left.
      public variable placelabel left {
         if { $exists } {
            set placelabel left
            configure -Ccd::labent::placelabel $placelabel
	 }
      }

#  Bind <1> in the entry field to select only from the menu of options.
#  Set the default binding for posting the menu. Note unbind B1-Leave
#  as autoscan is started and doesn't stop for some reason (probably
#  because the cancel scan binding is broken by binding <1> with break).
      public variable constrain 0 {
         if { $constrain } {
            bind entry <B1-Leave> {break}
            bind entry <1> "$this postmenu %X %Y;break"
         } else {
            bind entry <B1-Leave> {}
            bind entry <1> {}
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      protected variable Mb
      protected variable mb ""
      protected variable Menu
      protected variable menu ""

#  End of class defintion.
   }

# $Id$
