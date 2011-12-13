#+
#  Name:
#     Gaia3dArdColumnPrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a column ARD prism (an axis aligned plane).

#  Description:
#     Class that extends Gaia3dVtkColumnPrism to support ARD column shapes.

#  Copyright:
#     Copyright (C) 2007 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     07-DEC-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkArdColumnPrism {

   #  Inheritances:
   #  -------------
   inherit gaia3d::Gaia3dVtkColumnPrism

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set any configuration variables.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods and procedures:
   #  -----------------------

   #  Get an ARD description for this shape.
   public method get_desc {} {
      return "COLUMN($coord)"
   }

   #  Set the properties of this object from an ARD description.
   public method set_from_desc {desc} {
      configure -coord [gaia3d::Gaia3dArdUtils::get_ard_args $desc]
   }

   #  Gaia3dArdPrismProxy support
   #  ===========================

   #  See if an ARD description presents a column.
   public proc matches {desc} {
      return [string match -nocase "col*" $desc]
   }

   #  Given an ARD description of a column create an instance of this class.
   #  Make sure this passes the matches check first.
   public proc instance {desc} {
      set coord [gaia3d::Gaia3dArdUtils::get_ard_args $desc]
      return [uplevel \#0 gaia3d::Gaia3dVtkArdColumnPrism \#auto \
                 -coord $coord]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
