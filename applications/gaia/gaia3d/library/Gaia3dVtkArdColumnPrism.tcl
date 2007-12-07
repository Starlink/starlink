#+
#  Name:
#     Gaia3dArdColumnPrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a column ARD prism (an axis aligned plane).

#  Description:
#     Class that extends Gaia3dVtkArdPrism to support column shapes.

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

itcl::class ::gaia3d::Gaia3dVtkArdColumnPrism {

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

   #  Create the polygon for the column locus. Should extrude into an axis
   #  aligned plane. Columns are along the first non-axis dimension, so the 
   #  coordinate is an X value.
   protected method create_polygon_ {} {

      $points_ Reset
      $cells_ Reset
      $cells_ InsertNextCell 2
      
      lassign [get_dimensions_] xdim ydim zdim

      if { $axis == 1 } {
         $points_ InsertPoint 0 0.0 $coord 0
         $points_ InsertPoint 1 0.0 $coord $zdim
      } elseif { $axis == 2 } {
         $points_ InsertPoint 0 $coord 0.0 0
         $points_ InsertPoint 1 $coord 0.0 $zdim
      } else {
         $points_ InsertPoint 0 $coord 0 0.0
         $points_ InsertPoint 1 $coord $ydim 0.0
      }
      $cells_ InsertCellPoint 0
      $cells_ InsertCellPoint 1

      $polydata_ SetPoints $points_
      $polydata_ SetPolys $cells_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Coordinate of the column.
   public variable coord 0.0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
