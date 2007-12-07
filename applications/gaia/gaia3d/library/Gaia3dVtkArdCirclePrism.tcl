#+
#  Name:
#     Gaia3dArdCirclePrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a circular ARD prism.

#  Description:
#     Class that extends Gaia3dVtkArdPrism to support circular shapes.

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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     07-DEC-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkArdCirclePrism {

   #  Inheritances:
   #  -------------
   inherit gaia3d::Gaia3dVtkArdPrism

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

   #  Create the polygon for the circle locus.
   protected method create_polygon_ {} {

      $points_ Reset
      $cells_ Reset
      $cells_ InsertNextCell $segments

      set step [expr $2pi_/($segments-1)]
      for {set i 0} {$i < $segments} {incr i} {
         set x [expr $xcentre + $radius*cos($step*$i)]
         set y [expr $ycentre + $radius*sin($step*$i)]
         if { $axis == 1 } {
            $points_ InsertPoint $i 0.0 $x $y
         } elseif { $axis == 2 } {
            $points_ InsertPoint $i $x 0.0 $y
         } else {
            $points_ InsertPoint $i $x $y 0.0
         }
         $cells_ InsertCellPoint $i
      }
      $polydata_ SetPoints $points_
      $polydata_ SetPolys $cells_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  X position.
   public variable xcentre 0

   #  Y position.
   public variable ycentre 0

   #  Radius.
   public variable radius 1.0

   #  Number of segments used for the locus.
   public variable segments 30

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
