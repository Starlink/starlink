#+
#  Name:
#     StarArdLine

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of object for controlling an ARD line drawn
#     using a StarCanvasDraw object.

#  Description:
#     This class provides the basic functionality for controlling an
#     ARD line region. It provides the basic draw facilities and
#     returns an ARD description of the region.


#  Invocations:
#
#        StarArdLine object_name [configuration options]
#
#     This creates an instance of a StarArdLine object. The return is
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
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarArdLine {

   #  Inheritances:
   #  -------------

   inherit gaia::StarArdPrim

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set the type of canvas object.
      configure -mode line
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
      lassign [image_coord $x0 $y0] xlower ylower
      lassign [image_coord $x1 $y1] xupper yupper
      set endpoints "$xlower $ylower $xupper $yupper"
      return [list $xlower $ylower $xupper $yupper]
   }

   #  Return the ARD description of the object.
   method getard {{do_update 1}} {
      lassign [getcoords $do_update] xlower ylower xupper yupper
      return "LINE($xlower,$ylower,$xupper,$yupper)"
   }

   #  Return an "AST" region description of the object.
   method getregion {{do_update 1}} {
      lassign [getcoords $do_update] xlower ylower xupper yupper
      lassign [grid_coord $xlower $ylower] xlower ylower
      lassign [grid_coord $xupper $yupper] xupper yupper

      set dx [expr ($xupper-$xlower)]
      set dy [expr ($yupper-$ylower)]

      set result "polygon 5 "
      if {$dx > $dy} {
         append result "$xlower [expr $ylower+0.5] "
         append result "$xlower [expr $ylower-0.5] "
         append result "$xupper [expr $yupper-0.5] "
         append result "$xupper [expr $yupper+0.5] "
         append result "$xlower [expr $ylower+0.5]"
      } else {
         append result "[expr $xlower+0.5] $ylower "
         append result "[expr $xlower-0.5] $ylower "
         append result "[expr $xupper-0.5] $yupper "
         append result "[expr $xupper+0.5] $yupper "
         append result "[expr $xlower+0.5] $ylower"
      }
      puts "$result"
      return $result
   }

   #  Set the properties of the object to those of an ARD description
   #  of an object of this type.
   method setard {desc} {
      if {$desc != "" } {
         set failed 1
         if { [check_description line $desc] } {
            if { [llength $qualifiers_] == 4 } {
               lassign $qualifiers_ xlower ylower xupper yupper
               set endpoints "$xlower $ylower $xupper $yupper"
               lassign [canvas_coord $xlower $ylower] x0 y0
               lassign [canvas_coord $xupper $yupper] x1 y1
               configure -coords "$x0 $y0 $x1 $y1"
               set failed 0
            }
         }
         if { $failed } {
            error "Failed to interpret \"$desc\" as an ARD line"
         }
      }
   }

   #  Create a new line using an ARD description.
   method createard {desc {cmd {}}} {
      setard "$desc"
      create_no_resize $cmd $coords
   }

   #  Set new endpoints to appropriate variables in correct coordinate
   #  systems.
   method process_endpoints_ {imagecoords} {
      set endpoints $imagecoords
      lassign $endpoints x0 y0 x1 y1
      lassign [canvas_coord $x0 $y0] x0 y0
      lassign [canvas_coord $x1 $y1] x1 y1
      set coords "$x0 $y0 $x1 $y1"
   }

   #  Process coordinates and assign the result to the public
   #  variables, endpoints and coords in the correct system.
   method process_coords_ {canvcoords} {
      set coords $canvcoords
      lassign $coords x0 y0 x1 y1
      lassign [image_coord $x0 $y0] x0 y0
      lassign [image_coord $x1 $y1] x1 y1
      set endpoints "$x0 $y0 $x1 $y1"
   }

   #  Create a top-level window that describes the current object and
   #  allows its values to be changed.
   method show_properties {{name ""}} {
      if { $name == {} } {
         set name ".poly$canvas_id_"
      }
      if { ! [create_properties_window $name] } {

         #  Now add the entry  for the description.
         set Endpoints_ [ETable $Frame_.table -columns 2 -rows 2 \
                            -action [code $this update_from_table] \
                         -scrollbarplaces {none none} ]
         $Endpoints_ setlabel 0 X
         $Endpoints_ setlabel 1 Y
         pack $Endpoints_ -side top -fill both -expand true
      }

      #  Update the information to be current.
      update_properties
   }

   #  Update all properties.
   method update_properties {} {

      #  Make sure record of canvas item values is up todate.
      update $canvas_id_ resize
      process_coords_ $coords

      #  Update the properties box if it exists.
      if { [winfo exists $Endpoints_] } {
         lassign $endpoints x0 y0 x1 y1
         $Endpoints_ insert 0 $x0 $y0
         $Endpoints_ insert 1 $x1 $y1
      }
   }

   #  Update coords from the table.
   method update_from_table {} {
      if { [winfo exists $Endpoints_] } {
         process_endpoints_  "[$Endpoints_ get 0] [$Endpoints_ get 1]"
         redraw
      }
   }

   #  Procedures: (access common values)
   #  -----------
   #  Configuration options: (public variables)
   #  ----------------------


   #  Configuration options: (public variables)
   #  ----------------------

   #  Coordinates of endpoints in image coordinates.
   public variable endpoints {} {
      if { $endpoints != {} } {
         process_endpoints_ $endpoints
         redraw
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Widget for displaying/changing endpoints.
   protected variable Endpoints_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
