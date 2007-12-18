#+
#  Name:
#     Gaia3dArdPolygonPrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a polygon ARD prism.

#  Description:
#     Class that extends Gaia3dVtkArdPrism to support polygon shapes.

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

itcl::class ::gaia3d::Gaia3dVtkArdPolygonPrism {

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

   #  Create the polygon locus. Note -1 correction to VTK grid coords.
   protected method create_polygon_ {} {

      $points_ Reset
      $cells_ Reset

      set npoints [expr [llength $coords]/2]
      $cells_ InsertNextCell $npoints

      #  Separate loops for speed.
      set i 0
      if { $axis == 1 } {
         foreach {x y} $coords {
            $points_ InsertPoint $i 0.0 [expr $x-1] [expr $y-1]
            $cells_ InsertCellPoint $i
            incr i
         }
      } elseif { $axis == 2 } {
         foreach {x y} $coords {
            $points_ InsertPoint $i [expr $x-1] 0.0 [expr $y-1]
            $cells_ InsertCellPoint $i
            incr i
         }
      } else {
         foreach {x y} $coords {
            $points_ InsertPoint $i [expr $x-1] [expr $y-1] 0.0
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

   #  Get an ARD description for this shape.
   public method get_desc {} {
      return "POLYGON([join $coords ,])"
   }

   #  Set the properties of this object from an ARD description.
   public method set_from_desc {desc} {
      configure -coords "[get_ard_args $desc]"
   }

   #  See if an ARD description presents a polygon.
   public proc matches {desc} {
      return [string match -nocase {*pol*} $desc]
   }

   #  Given an ARD description of a polygon create an instance of this class.
   #  Make sure this passes the matches check first.
   public proc instance {desc} {
      set coords [get_ard_args $desc]
      return [uplevel \#0 gaia3d::Gaia3dVtkArdPolygonPrism \#auto \
                 -coords \"$coords\"]
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
