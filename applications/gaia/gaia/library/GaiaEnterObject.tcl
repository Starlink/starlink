#+
#  Name:
#     GaiaEnterObject

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Extends the enter object class to pick out X and Y fields, rather
#     than RA and DEC.

#  Invocations:
#
#        GaiaEnterObject object_name [configuration options]
#
#     This creates an instance of a GaiaEnterObject object. The return is
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
#     None at this level.

#  Methods:
#
#        picked_object list
#
#     Method invoked when object is picked. This sets the X and Y
#     entry fields.

#  Inheritance:
#     EnterObject

#  Copyright:
#     Copyright (C) 1997-2005 Central Laboratory of the Research Councils.
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
#     19-DEC-1997 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaEnterObject {

   #  Inheritances:
   #  -------------

   inherit cat::EnterObject

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Override the picked_object method to pick out the X and Y fields
   #  instead of the RA and DEC ones.
   public method picked_object {list} {
      lassign $list x y ra dec equinox fwhmX fwhmY symetry object background
      set_entry x $x
      set_entry y $y
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
