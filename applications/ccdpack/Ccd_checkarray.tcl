   itcl_class Ccd_checkarray {

#+
#  Name:
#     Ccd_checkarray

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Declares a class for creating an array of checkbuttons with a
#     label.

#  Description:
#     This class creates an instance of a checkarray. The checkarray
#     contains checkbuttons which each have a different text label (via
#     which they are referred) and which have a single descriptive label
#     associated with them.

#  Invocations:
#
#        Ccd_checkarray window [-option value]...
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

#  Configuration options:
#
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
#     invoke name
#        Invokes the named checkbutton. The name is that used when creating
#	 it.
#     _repack
#        Internal method for re-packing the checkbuttons.
#     addcommand name command
#        Adds a command to the named checkbutton.
#     addvariable name variable args
#        Adds a variable to the checkbutton. This is set to the value 0
#	 or 1 if no on and off values are given, The on and off values
#	 are the optional args argument.
#     addbutton name args
#	 Creates a checkbutton and packs it into the array.  "args" is
#	 optional and may be any of the arguments as passed to the
#	 "checkbutton" command (i.e. -variable variable -onvalue
#	 onvalue -offvalue offvalue etc.).
#     state name button_state
#        Set the state of the named button. "button_state" should be one
#	 of normal, active or disabled.
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

#  Inheritance:
#     This class inherits Ccd_base and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     23-APR-1994 (PDRAPER):
#     	 Original version.
#     9-MAY-1994 (PDRAPER):
#     	 Added state and toggle methods.
#     24-MAY-1994 (PDRAPER):
#        Added select and de-select methods.
#     23-MAR-1995 (PDRAPER):
#        Added sethelp method.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
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
         if { $args == {} } {
            checkbutton $oldthis.button$nbutton \
               -text "$name" \
	       -width $buttonwidth \
	       -anchor w \
         } else {

#  Descend into "quoting hell".
            eval checkbutton \$oldthis.button$nbutton \
               -text \"$name\" \
	       -width $buttonwidth \
	       -anchor w \
               "$args"
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
            for { set i 1 } { $i <= $nbutton } { incr i } {

#  See if buttons need resizing before packing
               if { $resize } { $oldthis.button$i configure -width $buttonwidth }
               pack forget $oldthis.button$i
               pack $oldthis.button$i -side $side -anchor w
	    }
            set resize 0
         }
      }

#  Method for invoking named button.
      method invoke name {
         if { [ info exists buttonnames($name) ] } {

#  Invoke the button.
            $oldthis.button$buttonnames($name) invoke
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  Method to add a variables with optional on and off values to a button.
      method addvariable { name variable args } {
         if { [ info exists buttonnames($name) ] } {

#  See if on and off values have been given.
            set on [lindex $args 0]
	    set off [lindex $args 1]
	    if { $on != {} && $off != {} } {

#  Add the variable.
               $oldthis.button$buttonnames($name) configure \
                  -variable "$variable" -onvalue "$on" -offvalue "$off"
            } else {

#  Variable has existing on and off values (0 and 1).
               $oldthis.button$buttonnames($name) configure \
                  -variable "$variable"
            }
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  Set the state of a named button.
      method state { name state } {
         if { [ info exists buttonnames($name) ] } {

#  Set the button state.
               $oldthis.button$buttonnames($name) configure \
                  -state $state
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  Toggle the selection state of a named button.
      method toggle { name } {
         if { [ info exists buttonnames($name) ] } {

#  Toggle the button selection state.
               $oldthis.button$buttonnames($name) toggle
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  Select a named button.
      method select { name } {
         if { [ info exists buttonnames($name) ] } {
               $oldthis.button$buttonnames($name) select
	 } else {
            error "No checkbutton of name \"$name\""
	 }
      }

#  De-select a named button.
      method deselect { name } {
         if { [ info exists buttonnames($name) ] } {
               $oldthis.button$buttonnames($name) deselect
	 } else {
            error "No checkbutton of name \"$name\""
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
            if { [winfo exists $oldthis.label] } {
	       Ccd_base::sethelp $oldthis.label $docname $label
            }
	 } else {
	    if { [ info exists buttonnames($name) ] } {
	       Ccd_base::sethelp \
		  $oldthis.button$buttonnames($name) $docname $label
	    }
	 }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Order for packing buttons. Can horizontal or vertical.
      public stack vertical {
         if $exists {
            _repack
         }
      }

#  Add a label to the checkarray.
      public label {} {
         if $exists {
            if { $label != {} } {
               label $oldthis.label -text $label
               if { $stack == "vertical" } {
                  pack $oldthis.label -side left -anchor w
               } else {
                  pack $oldthis.label -side top -anchor w
	       }
               _repack
            } else {
               if { [ winfo exists $oldthis.label ] } {
                  destroy $oldthis.label
               }
            }
         }
      }

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
      protected buttonwidth 12
      protected resize 0

#  End of class defintion.
   }
# $Id$
