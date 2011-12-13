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

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     26-MAR-1996 (PWD):
#        Original version.
#     5-JUL-1996 (PWD):
#        Converted to itk.
#     {enter_further_changes_here}

#-

#.

itk::usual StarLabelCheck {}

itcl::class gaia::StarLabelCheck {

   #  Inheritances:
   #  -------------
   inherit util::LabelWidget

   #  Constructor:
   #  ------------
   constructor {args} {
      itk_component add button {
         checkbutton $w_.button -variable StarLabelCheck($this)
      } {
         keep -command -variable -onvalue -offvalue -state -relief
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

