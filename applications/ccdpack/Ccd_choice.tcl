   itcl_class Ccd_choice {

#+
#  Name:
#     Ccd_choice

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Declares a class for creating a choice bar.

#  Description:
#     This class creates an instance of a choice bar. The choice bar
#     contains option buttons for performing one-off commands. The
#     standard choice bar contains the options "OK" and "Cancel".
#
#     The bar may have a label describing its purpose. This is placed
#     above buttons that are stacked horizontally and to the left of
#     buttons stacked vertically.

#  Invocations:
#
#        Ccd_choice window [-option value]...
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

#  Configuration options:
#
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


#  Inheritance:
#     This class inherits Ccd_base and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
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
#     {enter_changes_here}

#-

#  Inheritances:
      inherit Ccd_base

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the class and configures it with
#  the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { config } {

#  Create a frame widget. This must have the same name as the class
#  command.
         Ccd_base::constructor

#  Set default configurations.
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
            button $oldthis.button$nbutton \
               -text "$name" -width $buttonwidth
         } else {

#  Descend into "quoting hell".
            eval button \$oldthis.button$nbutton \
               -text \"$name\" -width $buttonwidth \
               -command "$args"
         }
         set buttonnames($name) $nbutton

#  And repack buttons.
         _repack

#  Define sub-component widgets for configuration via the wconfig
#  method.
         set widgetnames($oldthis:$name) $oldthis.button$nbutton
         set widgetfocus($oldthis:$name) $oldthis.button$nbutton
      }

#  Method to add a command to a button.
      method addcommand { name command } {
         if { [ info exists buttonnames($name) ] } {

#  Add the command.
            $oldthis.button$buttonnames($name) configure \
               -command "$command"
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
            for { set i 1 } { $i <= $nbutton } { incr i } {
               pack forget $oldthis.button$i
            }
         }

#  Pack label first.
         if { $havelabel } {
            if { $side == "top" } {
               pack $oldthis.label -side left -expand true -fill x
            } else {
               pack $oldthis.label -side top -expand true -fill x
            }
         }
         if { $nbutton > 0 } {
            for { set i 1 } { $i <= $nbutton } { incr i } {

#  See if buttons need resizing before packing
               if { $resize } {
                  $oldthis.button$i configure -width $buttonwidth
               }
               pack $oldthis.button$i -side $side -expand true
            }
            set resize 0
         }
      }

#  Method for setting focus to a button.
      method focus name {
         if { [info exists buttonnames($name)] } {
            ::focus $oldthis.button$buttonnames($name)
         } else {
            error "No choice button of name \"$name\""
         }
      }

#  Method for invoking named button.
      method invoke name {
         if { [ info exists buttonnames($name) ] } {

#  Invoke the button.
            $oldthis.button$buttonnames($name) invoke
         } else {
            error "No choice button of name \"$name\""
         }
      }

#  Method for assigning context help to a button.
      method sethelp {name docname label} {
         if { $name == "all" } {

#  Request to bind all elements to this help.
            if { $nbutton > 0 } {
               foreach oneof [ array names buttonnames ] {
                  Ccd_base::sethelp \
                     $oldthis.button$buttonnames($oneof) $docname $label
               }
            }
            Ccd_base::sethelp $oldthis $docname $label
         } else {
            if { [ info exists buttonnames($name) ] } {
               Ccd_base::sethelp \
                  $oldthis.button$buttonnames($name) $docname $label
            }
         }
      }

#  Set state of button method.
      method state {name status} {
         if { $name == "all" } {
            if { $nbutton > 0 } {
               foreach oneof [ array names buttonnames ] {
                  $oldthis.button$buttonnames($oneof) configure -state $status
               }
            }
         } else {
            if { [ info exists buttonnames($name) ] } {
               $oldthis.button$buttonnames($name) configure -state $status
            }
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add the standard buttons to choice bar.
      public standard 1 {
         if $exists {
            if { $standard } {
               addbutton OK
               addbutton Cancel
            }
         }
      }

#  Order for packing buttons. Can horizontal or vertical.
      public stack horizontal {
         if $exists {
            _repack
         }
      }

#  Optional label.
      public label "" {
         if $exists {
            if { $label != "" } {
               if { $havelabel } {
                  $oldthis.label configure -text "$label"
               } else {
                  label $oldthis.label -text "$label" \
                                    -justify center -anchor center
               }
               set havelabel 1
               _repack
            } else {
               if { $havelabel } {
                  destroy $oldthis.label
                  _repack
               }
               set havelabel 0
            }
         }
      }

#  Width of label.
      public width 0 {
         if $exists {
            if { $label != "" } {
               $oldthis.label configure -width $width
            }
         }
      }

#  Width of all buttons.
      public buttonwidth 7 {
         if { $buttonwidth > $maxwidth && $maxwidth != 0 } {
            set buttonwidth $maxwidth
         }
         set resize 1
      }

#  Maximum width of buttons
      public maxwidth 0 {}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Number of buttons in the menubar and their names.
      protected nbutton 0
      protected buttonnames

#   Notice if buttons need resizing.
      protected resize 0

#  Presence of label.
      protected havelabel 0

#  End of class defintion.
   }
# $Id$
