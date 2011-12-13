#+
#  Name:
#     Ccd::menubar

#  Purpose:
#     Defines the "class of menubar" used in CCDPACK.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     This class defines a menubar for the standard configuration
#     of "Options" and "Help" as well as allowing the addition of extra
#     menubuttons and commands for the menus. The menubar is registered
#     with Tk so that is can be invoked using keyboard accelerators and
#     the "Help" button is placed on the right.

#  Configuration Options:
#        -borderwidth width
#
#     The borderwidth of the underlying frame widget (defaults to 2).
#
#         -background colour
#
#     Sets the background colour of frame widget. Defaults to ""
#     which causes no change.
#
#        -relief  "raised|flat|groove|ridge|sunken"
#
#     Sets the relief of the border. Defaults to raised.
#
#        -standard boolean
#
#     Defines whether the menubar should be setup using the standard
#     configuration or not. This defaults to true (1) and needs to be
#     configured when the class instance (using "-standard 0") is
#     created to be overridden.

#  Inheritance:
#     This class inherits Ccd::base and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Invocations:
#        Ccd::menubar window [-option value]...
#
#     This command create an instance of a menubar and returns a
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
#     addbutton name underline
#        Adds a menubutton to the menubar. The button is labelled with
#        the string "name". The character indicated by the "underline"
#        value (see the menubutton description) is underlined for use in
#        keyboard acceleration.
#     addcheckbutton name label args
#        Adds a checkbutton to the menu associated with menubutton "name".
#        It is given the label "label". The extra arguments "args" are
#        used to configure the checkbutton as described in the menu
#        manpage.
#     addcommand name label command
#        Adds a command to the menu associated with menubutton "name".
#         The entry is given the label "label".
#     addseparator name
#        Appends a separator to the menu with menubutton "name".
#     _repack
#        Internal method for re-packing the menubuttons.
#     enable name index
#        Set the state of the command under button "name" with the given
#        index to enabled.
#     disable name index
#        Set the state of the command under button "name" with the given
#        index to disabled.
#     invoke name index
#        Invokes the command under button "name" with the given index.
#     sethelp name document label.
#        Sets the context sensitive help information for the menu
#        button $name. $document should be the name of a HTML document
#        and $label the xlabel (HTML anchor) within the document that
#        identifies the part to be displayed. If $name is "all" then
#        the document is associated with the whole of the menubar.

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
#     10-MAY-1994 (PDRAPER):
#        Added method for enabling, disabling a command.
#     14-MAR-1995 (PDRAPER):
#        Added addcheckbutton method.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     23-AUG-1995 (PDRAPER):
#        Added addseparator method.
#     12-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     27-JAN-2006 (PDRAPER):
#        Updated fot itcl::class syntax.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   itcl::class Ccd::menubar {

#  Inheritances:
      inherit Ccd::base

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the Ccd::menubar class and
#  configures it with the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { args } {

#  Create a frame so that real menu options are not confused by
#  tk_ procedures with those of this class instance.
         CCDTkWidget Menubar menubar frame $oldthis.menubar

#  Check options database for values to override widget defaults. Look for more
#  specific option of having a class specified, if this fails try for less
#  specific class.
         set opt [ _getoption "relief Relief"]
         if { $opt != {} } { set relief $opt }
         set opt [ _getoption "borderwidth Borderwidth BorderWidth"]
         if { $opt != {} } { set borderwidth $opt }
         set opt [ _getoption "background Background BackGround"]
         if { $opt != {} } { set background $opt }

#  And set default configurations.
         eval configure $args
         configure -relief          $relief
         configure -borderwidth     $borderwidth
         configure -background      $background
         configure -standard        $standard

#  Pack the menubar
         pack $menubar -fill x -expand true
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Add a menubutton to menubar. Buttons are named using a incremented
#  integer (since we can not have uppercase in window names we cannot
#  just use the text label) and the actual names (with which other
#  methods are directed) are the indices of an array which points to the
#  integer.
      method addbutton { name underline } {
         incr nbutton
         CCDTkWidget Button button \
            menubutton $menubar.button$nbutton \
               -menu $menubar.button$nbutton.m \
               -text "$name" \
               -underline $underline
         CCDTkWidget Buttonmenu buttonmenu menu $button.m
         set Buttons($name) $Button
         set Buttonmenus($name) $Buttonmenu
         lappend Buttonlist $Button

#  Catch the creation of Help buttons (want to pack these on right).
         if { $name == "Help" } {  set Helpbutton $Button  }
         _repack

#  Define sub-component widgets for configuration via the wconfig
#  method.
         set widgetnames($Oldthis:$name) $Button
         set widgetfocus($Oldthis:$name) $Button
      }

#  Method to add a checkbutton to a menu button.
      method addcheckbutton { name label args } {
         if { [ info exists Buttons($name) ] } {

#  Add the command.
            eval $Buttonmenus($name) add checkbutton -label {$label} $args
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }

#  Method to add a command to a menu button.
      method addcommand { name label command } {
         if { [ info exists Buttons($name) ] } {

#  Add the command.
            $Buttonmenus($name) add command \
               -label   "$label" \
               -command "$command"
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }


#  Method to add a separator to a menu.
      method addseparator { name } {
         if { [info exists Buttons($name)] } {

#  Add the separator
            $Buttonmenus($name) add separator
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }

#  Private method for repacking the menubar. Looks for "Help" buttons
#  and packs on right.
      method _repack {} {
         if { $nbutton > 0 } {
            foreach Button $Buttonlist {
               set button [CCDPathOf $Button]
               pack $button -side left
               lappend items $button
	    }

#  Deal with a help button
            if { [info exists Helpbutton] } {
               pack [CCDPathOf $Helpbutton] -side right
            }
         }
      }

#  Invoke a named entry in a menu.
      method invoke { name index } {
         if { [ info exists Buttons($name) ] } {

#  Invoke the menu item.
            $Buttonmenus($name) invoke $index
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }

#  Method to enable a command in a menu button.
      method enable { name index } {
         if { [ info exists Buttons($name) ] } {

#  Enable the menu item.
            $Buttonmenus($name) entryconfigure $index -state normal
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }

#  Method to disable a command in a menu button.
      method disable { name index } {
         if { [ info exists Buttons($name) ] } {

#  Disable the menu item.
            $Buttonmenus($name) entryconfigure $index -state disabled
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }

#  Method for assigning context help.
      method sethelp { name docname label} {
         if { "$name" == "all" } {
            Ccd::base::sethelp $Oldthis $docname $label
         } else {
            if { [ info exists Buttons($name) ] } {
               Ccd::base::sethelp $Buttons($name) $docname $label
            } else {
               error "No menubutton of name \"$name\""
            }
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  The menubar relief.
      public variable relief raised {
         if { [ winfo exists $menubar ] } {
            $Menubar configure -relief $relief
	 }
      }

#  Its borderwidth.
      public variable borderwidth 2 {
         if { [ winfo exists $menubar ] } {
            $Menubar configure -borderwidth $borderwidth
	 }
      }

#  Its background colour
      public variable background {} {
         if { [ winfo exists $menubar ] } {
            $Menubar configure -background $background
	 }
      }

#  Whether the standard menubar should be created.
      public variable standard 1 {
         if { ! [catch {winfo exists $menubar}] } {
            if { $standard } {
               addbutton File 0
               addbutton Options 0
               addbutton Help 0
            }
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Number of buttons in the menubar.
      protected variable nbutton 0
      protected variable Buttons
      protected variable Buttonmenus
      protected variable Buttonlist
      protected variable Helpbutton

#  Names of widgets.
      protected variable Menubar
      protected variable menubar ""

#  End of class defintion.
   }

# $Id$
