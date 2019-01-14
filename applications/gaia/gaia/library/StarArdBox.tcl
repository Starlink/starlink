#+
#  Name:
#     StarArdBox

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of object for controlling an ARD box drawn
#     using a StarCanvasDraw object.

#  Description:
#     This class provides the basic functionality for controlling an
#     ARD box region. It provides the basic draw facilities and
#     returns an ARD description of the region.

#  Invocations:
#
#        StarArdBox object_name [configuration options]
#
#     This creates an instance of a StarArdBox object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:
#     See public variable defintions.

#  Methods:
#     See method definitions.

#  Inheritance:
#     This class inherits StarArdPrim.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     8-MAY-1996 (PWD):
#        Original version.
#     6-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     11-JAN-2019 (PWD):
#        Added getcoords and getregion.
     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarArdBox {

   #  Inheritances:
   #  -------------

   inherit gaia::StarArdPrim

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set the type of canvas object.
      configure -mode rectangle
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Get the coordinates of the canvas object.
   method getcoords {{do_update 1}} {

      #  Make sure that the coords are up to date, if allowed.
      if { $do_update} { update $canvas_id_ resize }

      lassign $coords x0 y0 x1 y1
      lassign [image_coord $x0 $y0] x0 y0
      lassign [image_coord $x1 $y1] x1 y1
      set xside [expr $x1-$x0]
      set yside [expr $y1-$y0]
      set xcen [expr $x0+($xside/2.0)]
      set ycen [expr $y0+($yside/2.0)]
      set xside [expr abs($xside)]
      set yside [expr abs($yside)]
      return [list $xcen $ycen $xside $yside]
   }

   #  Return the ARD description of the object.
   method getard {{do_update 1}} {
      lassign [get_coords_ $do_update] xcen ycen xside yside
      return "BOX($xcen,$ycen,$xside,$yside)"
   }

   #  Return an "AST" region description of the object.
   method getregion {{do_update 1}} {
      lassign [get_coords $do_update] xo yo xside yside
      lassign [grid_coord $xo $yo] xo yo
      set xcorn [expr $xo + $xside]
      set ycorn [expr $yo + $yside]
      return "box $xo $yxo $xcorn $ycorn"
   }

   #  Set the properties of the object to those of an ARD description
   #  of an object of this type.
   method setard {desc} {
      if {$desc != "" } {
         set failed 1
         if { [check_description box $desc] } {
            if { [llength $qualifiers_] == 4 } {
               lassign $qualifiers_ xcen ycen xside yside
               set xside [expr $xside/2.0]
               set yside [expr $yside/2.0]
               set x0 [expr $xcen-$xside]
               set y0 [expr $ycen-$yside]
               set x1 [expr $xcen+$xside]
               set y1 [expr $ycen+$yside]
               lassign [canvas_coord $x0 $y0] x0 y0
               lassign [canvas_coord $x1 $y1] x1 y1
               configure -coords "$x0 $y0 $x1 $y1"
               set failed 0
            }
         }
         if { $failed } {
            error "Failed to interpret \"$desc\" as an ARD box"
         }
      }
   }

   #  Create a new box using an ARD description.
   method createard {desc {cmd {}}} {
      setard "$desc"
      create_no_resize $cmd $coords
   }

   #  Procedures: (access common values)
   #  -----------

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
