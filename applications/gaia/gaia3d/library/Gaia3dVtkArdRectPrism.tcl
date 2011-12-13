#+
#  Name:
#     Gaia3dArdRectPrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a rectangular ARD prism.

#  Description:
#     Class that extends Gaia3dVtkRectPrism to support rectangular shapes.
#     A rectangle is axis aligned.

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

itcl::class ::gaia3d::Gaia3dVtkArdRectPrism {

   #  Inheritances:
   #  -------------
   inherit gaia3d::Gaia3dVtkRectPrism

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
      return "RECT($x0,$y0,$x1,$y1)"
   }

   #  Set the properties of this object from an ARD description.
   public method set_from_desc {desc} {
      lassign [gaia3d::Gaia3dArdUtils::get_ard_args $desc] x0 y0 x1 y1
      configure -x0 $x0 -x1 $x1 -y0 $y0 -y1 $y1
   }

   #  Gaia3dArdPrismProxy support
   #  ===========================

   #  See if an ARD description presents a polygon.
   public proc matches {desc} {
      return [string match -nocase "rec*" $desc]
   }

   #  Given an ARD description of a rectangle create an instance of this class.
   #  Make sure this passes the matches check first.
   public proc instance {desc} {
      lassign [gaia3d::Gaia3dArdUtils::get_ard_args $desc] x0 y0 x1 y1
      return [uplevel \#0 gaia3d::Gaia3dVtkArdRectPrism \#auto \
                 -x0 $x0 -x1 $x1 -y0 $y0 -y1 $y1]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
