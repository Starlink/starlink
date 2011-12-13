#+
#  Name:
#     Gaia3dPolygonPrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a polygon prism.

#  Description:
#     Class that extends Gaia3dVtkPrism to support polygon shapes.

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

itcl::class ::gaia3d::Gaia3dVtkPolygonPrism {

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

   #  Create the polygon locus. Note -1 correction to VTK grid coords.
   protected method create_polygon_ {} {

      $points_ Reset
      $cells_ Reset

      set npoints [expr [llength $coords]/2]
      $cells_ InsertNextCell $npoints

      set z [expr $zlow-1]

      #  Separate loops for speed.
      set i 0
      if { $axis == 1 } {
         foreach {x y} $coords {
            $points_ InsertPoint $i $z [expr $x-1] [expr $y-1]
            $cells_ InsertCellPoint $i
            incr i
         }
      } elseif { $axis == 2 } {
         foreach {x y} $coords {
            $points_ InsertPoint $i [expr $x-1] $z [expr $y-1]
            $cells_ InsertCellPoint $i
            incr i
         }
      } else {
         foreach {x y} $coords {
            $points_ InsertPoint $i [expr $x-1] [expr $y-1] $z
            $cells_ InsertCellPoint $i
            incr i
         }
      }
      $polydata_ SetPoints $points_
      $polydata_ SetPolys $cells_
   }

   #  Apply a shift to the polygon.
   protected method apply_shift_ {sx sy} {
      set newcoords {}
      foreach {x y} $coords {
         lappend newcoords [expr $x+$sx] [expr $y+$sy]
      }
      configure -coords $newcoords
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  X, Y coordinates in pairs.
   public variable coords {0.0 0.0 1.0 1.0}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
