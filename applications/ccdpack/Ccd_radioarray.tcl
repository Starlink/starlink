   itcl_class Ccd_radioarray {

#+
#  Name:
#     Ccd_radioarray

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Declares a class for creating an array of radiobuttons with a
#     label.

#  Description:
#     This class creates an instance of a radioarray. The radioarray
#     contains radiobuttons which each have a different label (via
#     which they are referred) and which have a single descriptive label
#     associated with them. The standard radioarray contains the
#     options "true" and "false".

#  Invocations:
#
#        Ccd_radioarray window [-option value]...
#
#     This command create an instance of a radioarray and returns a
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
#     This options causes the radioarray to have the standard
#     configuration of "true" and "false". Standard false (0)
#     is the default.
#
#        -stack horizontal|vertical
#
#     The order in which the radiobuttons are stacked. "vertical" is the
#     default.
#
#        -label "string"
#
#     Specifies the label to apply to the radioarray as a whole. This
#     always appears on the left side.
#
#        -variable global_variable_name
#
#     The name of a global variable whose value will be set to the
#     "value" of the currently selected radiobutton. All the
#     radiobuttons will have this set as their variable. The value
#     associated with a particular radiobutton is set when using the
#     addbutton method. The standard configuration uses the values "1"
#     and "0".
#
#        -minwidth width
#
#     Defines the minimum width that buttons may have. This should be
#     set before any buttons are created, after which the default will
#     not be less than 12.
#
#        -maxwidth width
#
#     Defines the maximum width that buttons can be. This allows
#     control in the cases when the implied button width (ie. the
#     number of characters in the label) will be much greater than
#     required (if labels are spread over more than one line). The
#     default for this value is 0 which implies no maximum width.
#
#        -adampars boolean
#
#     Defines whether the standard boolean flags are 0 and 1 or
#     TRUE and FALSE as required by ADAM parameter system. The default
#     is false. Only relevant if -standard is 1.

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
#        Invokes the named radiobutton. The name is that used when creating
#        it (true, false etc.).
#     _repack
#        Internal method for re-packing the radiobuttons.
#     addcommand name command
#        Adds a command to the named radiobutton.
#     addbutton name value args
#        Creates a radiobutton and packs it into the array. "value" is
#        the value to be associated with the button when active. "args"
#        is optional and should be the button command if used.
#     resettext name text
#        Reconfigures the named button to show the given text. The
#        name used to refer to this button (in other methods) remains
#        unchanged (i.e. that used in the original addbutton).
#     state name button_state
#        Set the state of the named button. "button_state" should be one
#        of normal active or disabled. The name may be "all" in which
#        case all the buttons are state changed.
#     toggle name
#        Toggles the selection state of the named button. The name may
#        be "all" in which case all buttons are toggled.
#     sethelp document label.
#        Sets the context sensitive help information for all the
#        components of the meta-widget that exist. $document should be
#        the name of a HTML document and $label the label (HTML
#        anchor) within the document that identifies the part to be
#        displayed.

#  Inheritance:
#     This class inherits Ccd_base and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-APR-1994 (PDRAPER):
#        Original version.
#     20-MAY-1994 (PDRAPER):
#        Added state and toggle methods.
#     22-MAR-1995 (PDRAPER):
#        Added sethelp methods.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     29-AUG-1995 (PDRAPER):
#        Added resettext method.
#     {enter_further_changes_here}

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
         configure -minwidth          $minwidth
         configure -maxwidth          $maxwidth
         configure -label             $label
         configure -adampars          $adampars
         configure -standard          $standard
         configure -stack             $stack
         configure -variable          $variable
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add a radiobutton to the array. Buttons are named using a incremented
#  integer (since we can not have uppercase in window names we cannot
#  just use the text label) and the actual names (with which other
#  methods are directed) are the indices of an array which points to the
#  integer.
      method addbutton { name value args } {
         set newwidth [ string length $name ]
         incr newwidth +3
         if { $newwidth > $buttonwidth } {
            if { $newwidth <= $maxwidth || $maxwidth == 0 } {
               set buttonwidth $newwidth
               set resize 1
            }
         }
         incr nbutton
         if { $args == {} } {
            radiobutton $oldthis.button$nbutton \
               -text "$name" \
               -width $buttonwidth \
               -anchor w \
               -variable $variable \
               -value $value
         } else {

#  Descend into "quoting hell".
            eval radiobutton \$oldthis.button$nbutton \
               -text \"$name\" \
               -width $buttonwidth \
               -command "$args" \
               -anchor w \
               -variable $variable \
               -value $value
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
            error "No radiobutton of name \"$name\""
         }
      }

#  Method to change the label on a button. This only affects the appearance
#  the "name" of the button used internal and for access via methods, remains
#  the same.
      method resettext {name text} {
         if { [ info exists buttonnames($name) ] } {
            set newwidth [ string length $text ]
            if { $newwidth > $buttonwidth } {
               if { $newwidth <= $maxwidth || $maxwidth == 0 } {
                  set buttonwidth $newwidth
                  set resize 1
                  _repack
               }
            }
            $oldthis.button$buttonnames($name) configure -text "$text"
         }
      }

#  Private method for repacking the radioarray.
      method _repack {} {
         if { $stack == "array" } {

#  Buttons in a stack, $columns wide.
            if { $nbutton > 0 } {
               set incol 0
               for { set i 1 } { $i <= $nbutton } { incr i } {
                  if { $resize } {
                     $oldthis.button$i configure -width $buttonwidth
                  }
                  pack forget $oldthis.button$i
                  pack $oldthis.button$i -in $oldthis.frame$incol -anchor w -side top
                  raise $oldthis.button$i
                  incr incol
                  if { $incol == $columns } { set incol 0 }
               }
               for { set i 0 } { $i < $columns } { incr i } {
                  pack $oldthis.frame$i -side left -fill y
               }
            }
         } else {
            if { $stack == "horizontal" } {
               set side "left"
            } else {
               set side "top"
            }
            if { $nbutton > 0 } {
               for { set i 1 } { $i <= $nbutton } { incr i } {

#  See if buttons need resizing before packing
                  if { $resize } { $oldthis.button$i configure -width $buttonwidth }
                  pack forget $oldthis.button$i
                  pack $oldthis.button$i -side $side -anchor w
               }
               set resize 0
            }
         }
      }

#  Method for invoking named button.
      method invoke name {
         if { [ info exists buttonnames($name) ] } {

#  Invoke the button.
            $oldthis.button$buttonnames($name) invoke
         } else {
            error "No radiobutton of name \"$name\""
         }
      }

#  Set the state of a named button.
      method state { name state } {
         if { $name != "all" } {
            if { [ info exists buttonnames($name) ] } {

#  Set the button state.
               $oldthis.button$buttonnames($name) configure \
                  -state $state
            } else {
               error "No radiobutton of name \"$name\""
            }
         } else {
            foreach oneof [ array names buttonnames ] {

#  Set the button state.
               $oldthis.button$buttonnames($oneof) configure -state $state
            }
         }
      }

#  Toggle the selection state of a named button.
      method toggle { name } {
         if { $name != "all" } {
            if { [ info exists buttonnames($name) ] } {

#  Toggle the button selection state.
               $oldthis.button$buttonnames($name) toggle
            } else {
               error "No radiobutton of name \"$name\""
            }
         } else {
            foreach oneof [ array names buttonnames ] {

#  Toggle the button selection state.
               $oldthis.button$buttonnames($oneof) toggle
            }
         }
      }

#  Method for assigning context help.
      method sethelp {docname label} {

#  Request to bind all elements to help.
         if { $nbutton > 0 } {
            foreach oneof [ array names buttonnames ] {
               Ccd_base::sethelp \
                  $oldthis.button$buttonnames($oneof) $docname $label
            }
         }
         Ccd_base::sethelp $oldthis $docname $label
         if { [winfo exists $oldthis.label] } {
            Ccd_base::sethelp $oldthis.label $docname $label
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add the standard buttons to choice bar.
      public standard 0 {
         if $exists {
            if { $standard } {
               if $adampars { 
                  addbutton true TRUE
                  addbutton false FALSE
               } else { 
                  addbutton true 1
                  addbutton false 0
               }
            }
         }
      }

#  Order for packing buttons. Can horizontal or vertical.
      public stack vertical {
         if $exists {
            if { $stack == "array" } {
               configure -columns $columns
            } else {
               _repack
            }
         }
      }

#  Number of columns to use if stack is array.
      public columns 5 {
         if $exists {
            if { $availcols > $columns } {
               for { set i $availcols } { $i > $columns } { incr i -1 } {
                  destroy $oldthis.frame$i
               }
            } elseif { $availcols < $columns } {
               for { set i $availcols } { $i < $columns } { incr i } {
                  frame $oldthis.frame$i
               }
            }
            set availcols $columns
            _repack
         }
      }

#  Add a label to the radioarray.
      public label {} {
         if $exists  {
            if { $label != {} } {
               label $oldthis.label -text $label
	       pack $oldthis.label -side left -anchor w
               _repack
            } else {
               if { [ winfo exists $oldthis.label ] } {
                  destroy $oldthis.label
               }
            }
         }
      }

#  Set the global variable associated with all buttons.
      public variable { $oldthis } {
         if $exists {
            if { $nbutton > 0 } {
               for { set i 1 } { $i <= $nbutton } { incr i } {
                  $oldthis.button$i configure -variable $variable
               }
            }
         }
      }

#  Control the minimum width of buttons.
      public minwidth 12 {
         set buttonwidth $minwidth
      }

#  Control the maximum width of buttons
      public maxwidth 0 {}

#  Are we dealing with booleans that need ADAM parameter awareness?
      public adampars 0 {}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Number of buttons in the menubar and their names.
      protected nbutton 0
      protected buttonnames

#  The widths of the buttons. The actual width is never less than this
#  and all buttons are the same width.
      protected buttonwidth
      protected resize 0

#  Number of columns in use.
      protected availcols 0

#  End of class defintion.
   }
# $Id$
