   itcl_class Ccd_menubar {

#+
#  Name:
#     Ccd_menubar

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines the "class of menubar" used in CCDPACK.

#  Description:
#     This class defines a menubar for the standard configuration
#     of "Options" and "Help" as well as allowing the addition of extra
#     menubuttons and commands for the menus. The menubar is registered
#     with Tk so that is can be invoked using keyboard accelerators and
#     the "Help" button is placed on the right.

#  Invocations:
#
#        Ccd_menubar window [-option value]...
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

#  Configuration options:
#
#        -borderwidth width
#
#     The borderwidth of the underlying frame widget (defaults to 2).
#
#	 -background colour
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

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#	 creates the "class " widget with a default configuration,
#	 except when overridden by command line options.
#     destructor
#        Destroys the "class" instance, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#	 is given then the current value of any known option is returned
#	 in a form similar (but not identical to) the Tk widget command.
#     addbutton name underline
#        Adds a menubutton to the menubar. The button is labelled with
#	 the string "name". The character indicated by the "underline"
#	 value (see the menubutton description) is underlined for use in
#	 keyboard acceleration.
#     addcheckbutton name label args
#        Adds a checkbutton to the menu associated with menubutton "name".
#        It is given the label "label". The extra arguments "args" are
#        used to configure the checkbutton as described in the menu
#        manpage.
#     addcommand name label command
#        Adds a command to the menu associated with menubutton "name".
#	 The entry is given the label "label".
#     addseparator name
#        Appends a separator to the menu with menubutton "name".
#     _repack
#        Internal method for re-packing the menubuttons.
#     enable name index
#        Set the state of the command under button "name" with the given
#	 index to enabled.
#     disable name index
#        Set the state of the command under button "name" with the given
#	 index to disabled.
#     invoke name index
#        Invokes the command under button "name" with the given index.
#     sethelp name document label.
#        Sets the context sensitive help information for the menu
#        button $name. $document should be the name of a HTML document
#        and $label the xlabel (HTML anchor) within the document that
#        identifies the part to be displayed. If $name is "all" then
#        the document is associated with the whole of the menubar.

#  Inheritance:
#     This class inherits Ccd_base and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     28-MAR-1994 (PDRAPER):
#     	 Original version.
#     10-MAY-1994 (PDRAPER):
#     	 Added method for enabling, disabling a command.
#     14-MAR-1995 (PDRAPER):
#        Added addcheckbutton method.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     23-AUG-1995 (PDRAPER):
#        Added addseparator method.
#     {enter_further_changes_here}

#-

#  Inheritances:
      inherit Ccd_base

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the Ccd_menubar class and
#  configures it with the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { config } {

#  Create a frame so that real menu options are not confused by
#  tk_ procedures with those of this class instance.
         frame $oldthis.menubar

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
         configure -relief          $relief
         configure -borderwidth     $borderwidth
         configure -background      $background
         configure -standard        $standard

#  Pack the menubar
         pack $oldthis.menubar -fill x -expand true
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration method to change public attributes.
#      method configure { config } { }

#  Add a menubutton to menubar. Buttons are named using a incremented
#  integer (since we can not have uppercase in window names we cannot
#  just use the text label) and the actual names (with which other
#  methods are directed) are the indices of an array which points to the
#  integer.
      method addbutton { name underline } {
         incr nbutton
         menubutton $oldthis.menubar.button$nbutton \
            -menu $oldthis.menubar.button$nbutton.m \
            -text "$name" \
            -underline $underline
         menu $oldthis.menubar.button$nbutton.m
         set buttonnames($name) $nbutton

#  Catch the creation of Help buttons (want to pack these on right).
         if { $name == "Help" } {  set helpbutton $nbutton  }
         _repack

#  Define sub-component widgets for configuration via the wconfig
#  method.
         set widgetnames($oldthis:$name) $oldthis.menubar.button$nbutton
         set widgetfocus($oldthis:$name) $oldthis.menubar.button$nbutton
      }

#  Method to add a checkbutton to a menu button.
      method addcheckbutton { name label args } {
         if { [ info exists buttonnames($name) ] } {

#  Add the command.
            set curbut button$buttonnames($name)
            eval $oldthis.menubar.$curbut.m add checkbutton \
	         -label {$label} $args
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }

#  Method to add a command to a menu button.
      method addcommand { name label command } {
         if { [ info exists buttonnames($name) ] } {

#  Add the command.
            set curbut button$buttonnames($name)
            $oldthis.menubar.$curbut.m add command \
               -label   "$label" \
               -command "$command"
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }


#  Method to add a separator to a menu.
      method addseparator { name } {
         if { [info exists buttonnames($name)] } {

#  Add the separator
            set curbut button$buttonnames($name)
            $oldthis.menubar.$curbut.m add separator
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }

#  Private method for repacking the menubar. Looks for "Help" buttons
#  and packs on right.
      method _repack {} {
         if { $nbutton > 0 } {
            for { set i 1 } { $i <= $nbutton } { incr i } {
               pack $oldthis.menubar.button$i -side left
               lappend items $oldthis.menubar.button$i
	    }

#  Deal with a help button
            if { $helpbutton != 0 } {
               pack $oldthis.menubar.button$helpbutton -side right
            }

#  Register menubar with Tk.
            eval tk_menuBar $oldthis.menubar $items
         }
      }

#  Invoke a named entry in a menu.
      method invoke { name index } {
         if { [ info exists buttonnames($name) ] } {

#  Invoke the menu item.
            $oldthis.menubar.button$buttonnames($name).m invoke $index
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }

#  Method to enable a command in a menu button.
      method enable { name index } {
         if { [ info exists buttonnames($name) ] } {

#  Enable the menu item.
            $oldthis.menubar.button$buttonnames($name).m entryconfigure $index -state normal
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }

#  Method to disable a command in a menu button.
      method disable { name index } {
         if { [ info exists buttonnames($name) ] } {

#  Disable the menu item.
            $oldthis.menubar.button$buttonnames($name).m entryconfigure $index -state disabled
	 } else {
            error "No menubutton of name \"$name\""
	 }
      }

#  Method for assigning context help.
      method sethelp { name docname label} {
         if { "$name" == "all" } {
            Ccd_base::sethelp $oldthis $docname $label
         } else {
            if { [ info exists buttonnames($name) ] } {
               Ccd_base::sethelp $oldthis.menubar.button$buttonnames($name) $docname $label
            } else {
               error "No menubutton of name \"$name\""
            }
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  The menubar relief.
      public relief raised {
         if { [ winfo exists $oldthis.menubar ] } {
            $oldthis.menubar configure -relief $relief
	 }
      }

#  Its borderwidth.
      public borderwidth 2 {
         if { [ winfo exists $oldthis.menubar ] } {
            $oldthis.menubar configure -borderwidth $borderwidth
	 }
      }

#  Its background colour
      public background {} {
         if { [ winfo exists $oldthis.menubar ] } {
            $oldthis.menubar configure -background $background
	 }
      }

#  Whether the standard menubar should be created.
      public standard 1 {
         if { ! [catch {winfo exists $oldthis.menubar}] } {
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
      protected nbutton 0
      protected buttonnames
      protected helpbutton 0

#  End of class defintion.
   }

# $Id$
