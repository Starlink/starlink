   itcl_class Ccd_option {

#+
#  Name:
#     Ccd_option

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines the class of option labelled entry widgets.

#  Description:
#     This class creates a labelled entry widget with an additional
#     menubutton. A menu of possible options can be created which will
#     be available using a menubutton (which shows a distinctive
#     bitmap). The chosen option is then entered into the entry
#     widget. In this way specific string choices for the entry widget
#     may be provided. Options can be either a symbolic name and
#     associated value or just a value shown for both.

#  Invocations:
#
#        Ccd_option window [-option value]...
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

#  Configuration options:
#
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

#  Methods:
#     destructor
#        Destroys the "class" instance, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#	 is given then the current value of any known option is returned
#	 in a form similar (but not identical to) the Tk widget command.
#     addoption label [value]
#        Adds a label to the menu. This creates a menu item with
#	 this label and inserts this value into the entry widget
#        if no value is given, otherwise value is used.
#     postmenu x y
#        Forces the posting of the menu at the given (absolute)
#	 position.

#  Inheritance:
#     This class inherits Ccd_labent and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     29-MAR-1994 (PDRAPER):
#     	 Original version.
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
#     {enter_changes_here}

#-

#  Inheritances:
      inherit Ccd_labent

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the class and configures it with
#  the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { config } {

#  Add a menubutton for displaying the options and bitmap.
         menubutton $oldthis.mb \
            -indicatoron 1 \
	    -menu $oldthis.mb.m \
	    -relief raised

#  And create the menu.
         menu $oldthis.mb.m -tearoff 0

#  Configuration only allows label on left.
         configure -placelabel $placelabel
         configure -constrain $constrain

#  Pack sub-widgets.
         pack $oldthis.mb -side left

#  Define sub-component widgets for configuration via the basic
#  configuration methods.
         set widgetnamess($oldthis:menubutton) $oldthis.mb
         set widgetfocus($oldthis:menubutton) $oldthis.mb
         set widgetnames($oldthis:menu) $oldthis.mb.m
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add an option method.
      method addoption { label args } {
         switch [llength $args] {
            0 {
               $oldthis.mb.m add command \
                  -label $label \
                  -command "$oldthis clear 0 end
                            $oldthis insert 0 \"$label\"
                           "
            }
            1 {
               $oldthis.mb.m add command \
                  -label $label \
                  -command "$oldthis clear 0 end
                            $oldthis insert 0 \"$args\"
                           "
            }
            2 {
               $oldthis.mb.m add command \
                  -label $label \
                  -command "$oldthis clear 0 end
                            $oldthis insert 0 \"[lindex $args 0]\"
                            [lindex $args 1]
                           "
            }
         }
      }

#  Control menu posting method.
      method postmenu { x y } {
	 tk_popup $oldthis.mb.m $x $y
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Set the placement of the label using the internal method of
#  Ccd_labent which controls this. Should always be on the left.
      public placelabel left {
         if $exists {
            set placelabel left
            configure -Ccd_labent::placelabel $placelabel
	 }
      }

#  Bind <1> in the entry field to select only from the menu of options.
#  Set the default binding for posting the menu. Note unbind B1-Leave
#  as autoscan is started and doesn't stop for some reason (probably
#  because the cancel scan binding is broken by binding <1> with break).
      public constrain 0 {
         if { $constrain } {
            bind entry <B1-Leave> {break}
            bind entry <1> "$this postmenu %X %Y;break"
         } else {
            bind entry <B1-Leave> {}
            bind entry <1> {}
         }
      }

#  End of class defintion.
   }

# $Id$
