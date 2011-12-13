#+
#  Name:
#     Gaia3dRowPrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a row prism (an axis aligned plane).

#  Description:
#     Class that extends Gaia3dVtkPrism to support row shapes.

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

itcl::class ::gaia3d::Gaia3dVtkRowPrism {

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

   #  Create the polygon for the row locus. Should extrude into an axis
   #  aligned plane. Rows are along the second non-axis dimension, so the
   #  coordinate is an Y value. Note -1 correction to VTK grid coords.
   protected method create_polygon_ {} {

      $points_ Reset
      $cells_ Reset
      $cells_ InsertNextCell 2

      lassign [get_dimensions_] xdim ydim zdim
      set row [expr $coord-1]
      set z [expr $zlow-1]

      if { $axis == 1 } {
         $points_ InsertPoint 0 $z 0     $row
         $points_ InsertPoint 1 $z $ydim $row
      } elseif { $axis == 2 } {
         $points_ InsertPoint 0 0     $z $row
         $points_ InsertPoint 1 $xdim $z $row
      } else {
         $points_ InsertPoint 0 0     $row $z
         $points_ InsertPoint 1 $xdim $row $z
      }
      $cells_ InsertCellPoint 0
      $cells_ InsertCellPoint 1

      $polydata_ SetPoints $points_
      $polydata_ SetPolys $cells_
   }

   #  Apply a shift to the row position.
   protected method apply_shift_ {sx sy} {
      configure -coord [expr $coord+$sy]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Coordinate of the row.
   public variable coord 0.0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
