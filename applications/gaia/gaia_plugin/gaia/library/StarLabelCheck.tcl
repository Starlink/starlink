#+
#  Name:
#     StarLabelCheck

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of labelled check button.

#  Description:
#     This file describes a class of mega-widget that constrains a
#     label and a checkbutton to work together and provides methods
#     and configuration options for controlling the behaviour.

#  Invocations:
#
#        StarLabelCheck object_name [configuration options]
#
#     This creates an instance of a StarLabelCheck object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#
#     The -command -variable -onvalue -offvalue and -state options
#     are as defined for a checkbutton.

#  Methods:
#
#        invoke
# 
#     Invokes the button as if pressed.
#
#        toggle
#
#     Toggles the selection state of the button.
#
#        select
#
#     Selects the button and sets the associated variable to 
#     the on value.
#
#        deselect
#
#     Deselects the button and sets the associated variable to 
#     the off value.
#
#        get
#    
#     Returns the current value.

#  Inheritance:
#     LabelWidget, see this for other possible methods and
#     configuration options.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     26-MAR-1996 (PDRAPER):
#        Original version.
#     5-JUL-1996 (PDRAPER):
#        Converted to itk.
#     {enter_further_changes_here}

#-

#.

itk::usual StarLabelCheck {}

class gaia::StarLabelCheck {

   #  Inheritances:
   #  -------------
   inherit LabelWidget

   #  Constructor:
   #  ------------
   constructor {args} {
      itk_component add button {
         checkbutton $w_.button -variable StarLabelCheck($this)
      } {
         keep -command -variable -onvalue -offvalue -state
      }
      pack $itk_component(label) -side left -ipadx 1m 
      pack $itk_component(button) -side left -padx 1m -ipadx 1m 

      #  Now handle unprocessed configurations.
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Invoke checkbutton.
   method invoke {} {
      $itk_component(button) invoke
   }

   #  Toggle the selection state of the button.
   method toggle {} {
      $itk_component(button) toggle
   }

   #  Select the button.
   method select {} {
      $itk_component(button) select
   }

   #  Deselect the button.
   method deselect {} {
      $itk_component(button) deselect
   }

   #  Get the current value.
   method get {} {
      global $itk_option(-variable)
      return $itk_option(-variable)
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Each instance has it's own variable defined as an element of
   #  this array.
   common variable StarLabelCheck


#  End of class definition.
}

