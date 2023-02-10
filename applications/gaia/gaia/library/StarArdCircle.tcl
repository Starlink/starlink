#+
#  Name:
#     StarArdCircle

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of object for controlling an ARD circle drawn
#     using a StarCanvasDraw object.

#  Description:
#     This class provides the basic functionality for controlling an
#     ARD circle region. It provides the basic draw facilities and
#     returns an ARD description of the region.

#  Invocations:
#
#        StarArdCircle object_name [configuration options]
#
#     This creates an instance of a StarArdCircle object. The return is
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
#     5-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     11-JAN-2019 (PWD):
#        Added getcoords and getregion.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarArdCircle {

   #  Inheritances:
   #  -------------
   inherit gaia::StarArdPrim

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Process args.
      eval configure $args

      #  Set the type of canvas object.
      configure -mode circle
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
      if { $do_update} {update $canvas_id_ resize}
      lassign $coords x y
      lassign [image_coord $x $y] x y
      set rad [$canvas itemcget $canvas_id_ -semimajor]
      set rad [image_dist $rad]
      return [list $x $y $rad]
   }

   #  Return the ARD description of the object.
   method getard {{do_update 1}} {
      lassign [getcoords $do_update] x y rad
      return "$unary_operator CIRCLE($x,$y,$rad)"
   }

   #  Return an "AST" region description of the object.
   method getregion {{do_update 1}} {
      lassign [getcoords $do_update] x y rad
      lassign [grid_coord $x $y] x y
      return "circle $x $y $rad"
   }

   #  Set the properties of the object to those of an ARD description
   #  of an object of this type.
   method setard {desc} {
      if {$desc != "" } {
         set failed 1
         if { [check_description $mode $desc] } {
            if { [llength $qualifiers_] == 3 } {
               lassign $qualifiers_ x y rad
               lassign [canvas_coord $x $y] x y
               set rad [canvas_dist $rad]
               configure -coords "$x $y"
               configure -rad $rad
               set failed 0
               set_radius_
            }
         }
         if { $failed } {
            error "Failed to interpret \"$desc\" as an ARD circle"
         }
      }
   }

   #  Create a new circle using an ARD description.
   method createard {desc {cmd {}}} {
      setard "$desc"
      create_no_resize [code $this set_radius_ $cmd] $coords
   }

   #  Set the radius after a static creation command and evaluate the
   #  creation command.
   method set_radius_ {args} {
      $canvas itemconfigure $canvas_id_ \
         -semimajor $rad -semiminor $rad -angle 0
      if { [lindex $args 0] != {} } {
         eval $args
      }
   }

   #  Create a top-level window that describes the current object and
   #  allows its values to be changed.
   method show_properties {{name ""}} {
      if { $name == {} } {
         set name ".circle$canvas_id_"
      }
      if { ! [create_properties_window $name] } {

         #  Now add the buttons for the description.
         set Xcentre_ [util::LabelEntry $Frame_.xcentre \
                          -text {X centre:} \
                          -labelwidth $labelwidth_ \
                          -command [code $this configure -x]]
         set Ycentre_ [util::LabelEntry $Frame_.ycentre \
                          -text {Y centre:} \
                          -labelwidth $labelwidth_ \
                          -command [code $this configure -y]]
         set Radius_ [util::LabelEntry $Frame_.radius \
                         -text {Radius:}\
                         -labelwidth $labelwidth_ \
                         -command [code $this configure -rad]]
         pack $Xcentre_ $Ycentre_ $Radius_ -side top -fill x
      }

      #  Update the information to be current.
      update_properties
   }

   #  Update all properties.
   method update_properties {} {

      #  Make sure record of canvas item values is up todate.
      update $canvas_id_ resize
      lassign $coords x y
      lassign [image_coord $x $y] x y
      set rad [$canvas itemcget $canvas_id_ -semimajor]
      set rad [image_dist $rad]

      #  Update the properties box if it exists.
      if { [winfo exists $Properties_] } {
         $Xcentre_ configure -value $x
         $Ycentre_ configure -value $y
         $Radius_ configure -value $rad
      }
   }

   #  Procedures: (access common values)
   #  -----------

   #  Configuration options: (public variables)
   #  ----------------------

   #  X position in image coordinates.
   public variable x {0} {
      if { [winfo exists $Xcentre_] } {
         $Xcentre_ configure -value $x
         lassign [canvas_coord $x $y] xc yc
         set coords "$xc $y"
         redraw
      }
   }

   #  Y position in image coordinates.
   public variable y {0} {
      if { [winfo exists $Ycentre_] } {
         $Ycentre_ configure -value $y
         lassign [canvas_coord $x $y] xc yc
         set coords "$x $yc"
         redraw
      }
   }

   #  Radius in image pixels.
   public variable rad {5} {
      if { [winfo exists $Radius_] } {
         $Radius_ configure -value $rad
         set crad [canvas_dist $rad]
         $canvas itemconfigure $canvas_id_ \
            -semimajor $crad -semiminor $crad -angle 0
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Names of various widgets.
   protected variable Xcentre_ {}
   protected variable Ycentre_ {}
   protected variable Radius_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
