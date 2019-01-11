#+
#  Name:
#     StarArdRow

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of object for controlling an ARD row drawn
#     using a StarCanvasDraw object.

#  Description:
#     This class provides the basic functionality for controlling an
#     ARD row region. It provides the basic draw facilities and
#     returns an ARD description of the region.


#  Invocations:
#
#        StarArdRow object_name [configuration options]
#
#     This creates an instance of a StarArdRow object. The return is
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
#     See public variable defintions below.

#  Methods:
#     See method declarations.

#  Inheritance:
#     This class inherits StarArdPrim.

#  Copyright:
#     Copyright (C) 1997 Central Laboratory of the Research Councils
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
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarArdRow {

   #  Inheritances:
   #  -------------

   inherit gaia::StarArdPrim

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set the type of canvas object.
      configure -mode row
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Return the coordinates of the canvas object.
   method getcoords {{do_update 1}} {

      #  Make sure that the coords are up to date, if allowed.
      if { $do_update} { update $canvas_id_ resize }
      lassign $coords x0 y0 x1 y1
      lassign [image_coord $x0 $y0] x0 y0

      # Rows should really be specified in pixel indices.
      set y [expr round($y0+0.5)]
      return $y
   }

   #  Return the ARD description of the object.
   method getard {{do_update 1}} {
      set y [getcoords $do_update]
      return "ROW($y)"
   }

   #  Return an "AST" region description of the object. This is invalid.
   method getregion {{do_update 1}} {
      return {}
   }

   #  Set the properties of the object to those of an ARD description
   #  of an object of this type.
   method setard {desc} {
      if {$desc != "" } {
         set failed 1
         if { [check_description row $desc] } {
            if { [llength $qualifiers_] == 1 } {

            #  Pixel indices to coordinates before transformation.
               set y [expr $qualifiers_-0.5]
               lassign [canvas_coord 1 $y] xs ys
               configure -coords "0 $ys [$rtdimage dispwidth] $ys"
               set failed 0
            }
         }
         if { $failed } {
            error "Failed to interpret \"$desc\" as an ARD row"
         }
      }
   }

   #  Create a new row using an ARD description.
   method createard {desc {cmd ""}} {
      setard "$desc"
      create_no_resize $cmd $coords
   }

   #  Create a top-level window that describes the current object and
   #  allows its values to be changed.
   method show_properties {{name ""}} {
      if { $name == {} } {
         set name ".row$canvas_id_"
      }
      if { ! [create_properties_window $name] } {

         #  Now add the buttons for the description.
         set Y_ [LabelEntry $Frame_.y \
                    -text {Row:} \
                    -command [code $this configure -y]]
         pack $Y_ -side top -fill x
      }

      #  Update the information to be current.
      update_properties
   }

   #  Update all properties.
   method update_properties {} {

      #  Make sure record of canvas item values is up todate.
      update $canvas_id_ resize
      lassign $coords x0 y0 x1 y1
      lassign [image_coord $x0 $y0] x y
      set y [expr round($y+0.5)]

      #  Update the properties box if it exists.
      if { [winfo exists $Properties_] } {
         $Y_ configure -value $y
      }
   }

   #  Procedures: (access common values)
   #  -----------

   #  Position of the row in pixel coordinates.
   public variable y {1} {
      if { [winfo exists $Y_] } {
         $Y_ configure -value $y
         lassign [canvas_coord 1 $y] xs ys
         set coords "0 $ys [$rtdimage dispwidth] $ys"
         redraw
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Row position entry widget.
   protected variable Y_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

