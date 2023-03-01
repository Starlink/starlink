#+
#  Name:
#     StarArdRect

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of object for controlling an ARD rectangle drawn
#     using a StarCanvasDraw object.

#  Description:
#     This class provides the basic functionality for controlling an
#     ARD rectangle region. It provides the basic draw facilities and
#     returns an ARD description of the region.

#  Invocations:
#
#        StarArdRect object_name [configuration options]
#
#     This creates an instance of a StarArdRect object. The return is
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
#
#        corners
#
#     List of the coordinates of the box corners.

#  Methods:
#
#        getard {update 1}
#
#     Returns the ARD description of this object. The current
#     information is updated first if update is set 1, otherwise
#     the existing values are returned.
#
#        setard desc
#
#     Sets the configuration of an object using a ARD description
#     of it.
#
#        createard desc {cmd ""}
#
#     Creates a new object from an ARD description. See the
#     create_no_resize method of the base classes to understand the
#     function of cmd.
#
#        show_properties {name ""}
#
#     Creates a window for viewing and editing the properties of an
#     object. This is usually invoked by double clicking on the object.
#
#        update_properties
#
#     Modifies the properties of the object to mirror the
#     current configuration options.
#
#        update_from_table
#
#     Modifies the current configuration to reflect the values entered
#     into the show_properties window by the user.
#
#     Private methods:
#        process_corners_ imagecoords
#        process_coords_ canvcoords

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

itcl::class gaia::StarArdRect {

   #  Inheritances:
   #  -------------

   inherit gaia::StarArdPrim

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set the type of canvas object.
      eval configure $args
      configure -mode rectangle
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Get the coordinates of the canvas object.
   public method getcoords {{do_update 1}} {

      #  Make sure that the coords are up to date, if allowed.
      if { $do_update} { update $canvas_id_ resize }
      lassign $coords x0 y0 x1 y1
      lassign [image_coord $x0 $y0] xlower ylower
      lassign [image_coord $x1 $y1] xupper yupper
      set corners "$xlower $ylower $xupper $yupper"
      return [list $xlower $ylower $xupper $yupper]
   }

   #  Return the ARD description of the object.
   public method getard {{do_update 1}} {
      lassign [getcoords] xlower ylower xupper yupper
      return "RECT($xlower,$ylower,$xupper,$yupper)"
   }

   #  Return an "AST" region description of the object.
   method getregion {{do_update 1}} {
      lassign [getcoords $do_update] xlower ylower xupper yupper
      lassign [grid_coord $xlower $ylower] xlower ylower
      lassign [grid_coord $xupper $yupper] xupper yupper
      return "box $xlower $ylower $xupper $yupper"
   }

   #  Set the properties of the object to those of an ARD description
   #  of an object of this type.
   public method setard {desc} {
      if {$desc != "" } {
         set failed 1
         if { [check_description rectangle $desc] } {
            if { [llength $qualifiers_] == 4 } {
               lassign $qualifiers_ xlower ylower xupper yupper
               set corners "$xlower $ylower $xupper $yupper"
               lassign [canvas_coord $xlower $ylower] x0 y0
               lassign [canvas_coord $xupper $yupper] x1 y1
               configure -coords "$x0 $y0 $x1 $y1"
               set failed 0
            }
         }
         if { $failed } {
            error "Failed to interpret \"$desc\" as an ARD rectangle"
         }
      }
   }

   #  Create a new rectangle using an ARD description.
   public method createard {desc {cmd ""}} {
      setard "$desc"
      create_no_resize $cmd $coords
   }

   #  Set new corners to appropriate variables in correct coordinate
   #  systems.
   private method process_corners_ {imagecoords} {
      set corners $imagecoords
      lassign $corners x0 y0 x1 y1
      lassign [canvas_coord $x0 $y0] x0 y0
      lassign [canvas_coord $x1 $y1] x1 y1
      set coords "$x0 $y0 $x1 $y1"
   }

   #  Process coordinates and assign the result to the public
   #  variables, corners and coords in the correct system.
   private method process_coords_ {canvcoords} {
      set coords $canvcoords
      lassign $coords x0 y0 x1 y1
      lassign [image_coord $x0 $y0] x0 y0
      lassign [image_coord $x1 $y1] x1 y1
      set corners "$x0 $y0 $x1 $y1"
   }

   #  Create a top-level window that describes the current object and
   #  allows its values to be changed.
   public method show_properties {{name ""}} {
      if { $name == {} } {
         set name ".poly$canvas_id_"
      }
      if { ! [create_properties_window $name] } {

         #  Now add the entry  for the description.
         set Corners_ [gaia::ETable $Frame_.table -columns 2 -rows 2 \
                          -scrollbarplaces {none none} \
                          -action [code $this update_from_table]]
         $Corners_ setlabel 0 X
         $Corners_ setlabel 1 Y
         pack $Corners_ -side top -fill both -expand true
      }

      #  Update the information to be current.
      update_properties
   }

   #  Update all properties.
   public method update_properties {} {

      #  Make sure record of canvas item values is up todate.
      update $canvas_id_ resize
      process_coords_ $coords

      #  Update the properties box if it exists.
      if { [winfo exists $Corners_] } {
         lassign $corners x0 y0 x1 y1
         $Corners_ insert 0 $x0 $y0
         $Corners_ insert 1 $x1 $y1
      }
   }

   #  Update coords from the table.
   public method update_from_table {} {
      if { [winfo exists $Corners_] } {
         process_corners_  "[$Corners_ get 0] [$Corners_ get 1]"
         redraw
      }
   }

   #  Procedures: (access common values)
   #  -----------

   #  Configuration options: (public variables)
   #  ----------------------

   #  Coordinates of corners in image coordinates.
   public variable corners {} {
      if { $corners != {} } {
         process_corners_ $corners
         redraw
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Widget for displaying/changing corners.
   protected variable Corners_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
