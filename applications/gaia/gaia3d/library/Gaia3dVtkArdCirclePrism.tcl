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

   #  Create the polygon for the circle locus. Note -1 correction to VTK grid
   #  coordinates 
   protected method create_polygon_ {} {

      #  Clear any existing positions.
      $points_ Reset
      $cells_ Reset
      $cells_ InsertNextCell $segments

      set xc [expr $xcentre-1]
      set yc [expr $ycentre-1]

      set step [expr $2pi_/($segments-1)]

      #  Separate loops per axis for speed.
      if { $axis == 1 } {
         for {set i 0} {$i < $segments} {incr i} {
            set x [expr $xc + $radius*cos($step*$i)]
            set y [expr $yc + $radius*sin($step*$i)]
            $points_ InsertPoint $i 0.0 $x $y
            $cells_ InsertCellPoint $i
         }
      } elseif { $axis == 2 } {
         for {set i 0} {$i < $segments} {incr i} {
            set x [expr $xc + $radius*cos($step*$i)]
            set y [expr $yc + $radius*sin($step*$i)]
            $points_ InsertPoint $i $x 0.0 $y
            $cells_ InsertCellPoint $i
         }
      } else {
         for {set i 0} {$i < $segments} {incr i} {
            set x [expr $xc + $radius*cos($step*$i)]
            set y [expr $yc + $radius*sin($step*$i)]
            $points_ InsertPoint $i $x $y 0.0
            $cells_ InsertCellPoint $i
         }
      }

      $polydata_ SetPoints $points_
      $polydata_ SetPolys $cells_
   }

   #  Apply a shift to the centre.
   protected method apply_shift_ {sx sy} {
      configure -xcentre [expr $xcentre+$sx] -ycentre [expr $ycentre+$sy]
   }

   #  Get an ARD description for this shape.
   public method get_desc {} {
      return "CIRCLE($xcentre,$ycentre,$radius)"
   }

   #  Set the properties of this object from an ARD description.
   public method set_from_desc {desc} {
      lassign [get_ard_args $desc] xcentre ycentre radius
      configure -xcentre $xcentre -ycentre $ycentre -radius $radius
   }

   #  See if an ARD description presents a circle.
   public proc matches {desc} {
      return [string match -nocase "cir*" $desc]
   }

   #  Given an ARD description of a circle create an instance of this class.
   #  Make sure this passes the matches check first.
   public proc instance {desc} {
      lassign [get_ard_args $desc] xcentre ycentre radius
      return [uplevel \#0 gaia3d::Gaia3dVtkArdCirclePrism \#auto \
                 -xcentre $xcentre -ycentre $ycentre -radius $radius]
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
