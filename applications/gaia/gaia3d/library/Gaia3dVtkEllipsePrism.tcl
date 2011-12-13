#+
#  Name:
#     Gaia3dVtkEllipsePrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate an elliptical prism.

#  Description:
#     Class that extends Gaia3dVtkPrism to support elliptical shapes.

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

itcl::class ::gaia3d::Gaia3dVtkEllipsePrism {

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

   #  Create the polygon for the elliptical locus. Note -1 correction of
   #  centre to VTK grid coords.
   protected method create_polygon_ {} {

      $points_ Reset
      $cells_ Reset
      $cells_ InsertNextCell $segments
      set step [expr $2pi_/($segments-1)]

      set cospa [expr cos($angle*$d2r_)]
      set sinpa [expr sin($angle*$d2r_)]
      set minorcospa [expr $semiminor*$cospa]
      set majorcospa [expr $semimajor*$cospa]
      set minorsinpa [expr $semiminor*$sinpa]
      set majorsinpa [expr $semimajor*$sinpa]

      set xc [expr $xcentre-1]
      set yc [expr $ycentre-1]
      set zc [expr $zlow-1]

      #  Separate loops by axis for speed.
      if { $axis == 1 } {
         for {set i 0} {$i < $segments} {incr i} {
            set costheta [expr cos($step*$i)]
            set sintheta [expr sin($step*$i)]
            set x [expr $xc + $majorcospa*$costheta - $minorsinpa*$sintheta]
            set y [expr $yc + $majorsinpa*$costheta + $minorcospa*$sintheta]
            $points_ InsertPoint $i $zc $x $y
            $cells_ InsertCellPoint $i
         }
      } elseif { $axis == 2 } {
         for {set i 0} {$i < $segments} {incr i} {
            set costheta [expr cos($step*$i)]
            set sintheta [expr sin($step*$i)]
            set x [expr $xc + $majorcospa*$costheta - $minorsinpa*$sintheta]
            set y [expr $yc + $majorsinpa*$costheta + $minorcospa*$sintheta]
            $points_ InsertPoint $i $x $zc $y
            $cells_ InsertCellPoint $i
         }
      } else {
         for {set i 0} {$i < $segments} {incr i} {
            set costheta [expr cos($step*$i)]
            set sintheta [expr sin($step*$i)]
            set x [expr $xc + $majorcospa*$costheta - $minorsinpa*$sintheta]
            set y [expr $yc + $majorsinpa*$costheta + $minorcospa*$sintheta]
            $points_ InsertPoint $i $x $y $zc
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

   #  Configuration options: (public variables)
   #  ----------------------

   #  X position.
   public variable xcentre 0

   #  Y position.
   public variable ycentre 0

   #  Semimajor axis.
   public variable semimajor 1.0

   #  Semiminor axis.
   public variable semiminor 1.0

   #  Position angle.
   public variable angle 0.0

   #  Number of segments used for the locus.
   public variable segments 30

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
