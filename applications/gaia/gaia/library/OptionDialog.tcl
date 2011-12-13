#+
#  Name:
#     OptionDialog

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Extend DialogWidget to add additional option switch.

#  Description:
#     This class extends DialogWidget to add a checkbutton
#     below the main choice buttons. This checkbutton is expected to
#     add functionality that say allows the user to request that this
#     sort of information is not shown again. Control of the
#     checkbutton state is provided using a command (given by the
#     user) that should be executed when the checkbutton state is
#     toggled. The initial state and text of the checkbutton are given
#     by the option -option_state and -option_text. The command is
#     -option_cmd.

#  Invocations:
#
#        OptionDialog object_name [configuration options]
#
#     This creates an instance of a OptionDialog object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:
#     See itk_option definitions below

#  Methods:
#     See method declarations below.

#  Inheritance:
#     DialogWidget

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
#     23-MAY-2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual OptionDialog {}

itcl::class gaia::OptionDialog {

   #  Inheritances:
   #  -------------
   inherit util::DialogWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Add the checkbutton.
      itk_component add check {
         checkbutton $w_.check \
            -text "$itk_option(-option_text)" \
            -variable [scope itk_option(-option_state)] \
            -onvalue 1 \
            -offvalue 0 \
            -command [code $this do_option_cmd_]
      }
      set itk_option(-option_state) $itk_option(-option_state)
      pack $itk_component(check) -side bottom -anchor w \
         -ipadx 1m -ipady 1m
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Execute the option_cmd option if defined.
   protected method do_option_cmd_ {} {
      if { $itk_option(-option_cmd) != {} } {
         eval $itk_option(-option_cmd) $itk_option(-option_state)
      }
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  Initial state of checkbutton (0 or 1).
   itk_option define -option_state option_state Option_state 0

   #  Text describing the checkbutton function.
   itk_option define -option_text option_text Option_text {}

   #  Command to execute when the checkbutton state is changed.
   itk_option define -option_cmd option_cmd Option_cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
