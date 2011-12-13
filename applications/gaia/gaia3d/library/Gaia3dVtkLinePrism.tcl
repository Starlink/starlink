#+
#  Name:
#     Gaia3dLinePrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a line prism (a plane).

#  Description:
#     Class that extends Gaia3dVtkPrism to support line shapes.

#  Copyright:
#     Copyright (C) 2009 Science and Technology Facilities Council
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
#     28-APR-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkLinePrism {

   #  Inheritances:
   #  -------------
   inherit gaia3d::Gaia3dVtkPrism

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

   #  Create the polygon for the line locus. Should extrude into a plane.
   #  Note -1 correction to VTK grid coords.
   protected method create_polygon_ {} {

      $points_ Reset
      $cells_ Reset
      $cells_ InsertNextCell 2

      set x0v [expr $x0-1]
      set x1v [expr $x1-1]
      set y0v [expr $y0-1]
      set y1v [expr $y1-1]
      set z0v [expr $zlow-1]

      if { $axis == 1 } {
         $points_ InsertPoint 0 $z0v $x0v $y0v
         $points_ InsertPoint 1 $z0v $x1v $y1v
      } elseif { $axis == 2 } {
         $points_ InsertPoint 0 $x0v $z0v $y0v
         $points_ InsertPoint 1 $x1v $z0v $y1v
      } else {
         $points_ InsertPoint 0 $x0v $y0v $z0v
         $points_ InsertPoint 1 $x1v $y1v $z0v
      }
      $cells_ InsertCellPoint 0
      $cells_ InsertCellPoint 1

      $polydata_ SetPoints $points_
      $polydata_ SetPolys $cells_
   }

   #  Apply a shift to the ends.
   protected method apply_shift_ {sx sy} {
      configure -x0 [expr $x0+$sx] -x1 [expr $x1+$sx] \
                -y0 [expr $y0+$sy] -y1 [expr $y1+$sy]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  X coordinate of first end point.
   public variable x0 0.0

   #  Y coordinate of first end point.
   public variable y0 0.0

   #  X coordinate of second end point.
   public variable x1 1.0

   #  Y coordinate of second end point.
   public variable y1 1.0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
