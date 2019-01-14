#+
#  Name:
#     StarArdEllipse

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of object for controlling an ARD ellipse drawn
#     using a StarCanvasDraw object.

#  Description:
#     This class provides the basic functionality for controlling an
#     ARD rectangle region. It provides the basic draw facilities and
#     returns an ARD description of the region.

#  Invocations:
#
#        StarArdEllipse object_name [configuration options]
#
#     This creates an instance of a StarArdEllipse object. The return is
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

itcl::class gaia::StarArdEllipse {

   #  Inheritances:
   #  -------------

   inherit gaia::StarArdPrim

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set the type of canvas object.
      configure -mode ellipse
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
      lassign $coords xs ys
      set maj [$canvas itemcget $canvas_id_ -semimajor]
      set min [$canvas itemcget $canvas_id_ -semiminor]
      set ang [$canvas itemcget $canvas_id_ -angle]
      lassign [image_coord $xs $ys] x y
      set semimajor [image_dist $maj]
      set semiminor [image_dist $min]
      set angle [image_angle $ang]
      return [list $x $y $semimajor $semiminor $angle]
   }

   #  Return the ARD description of the object.
   method getard {{do_update 1}} {
      lassign [getcoords $do_update] x y semimajor semiminor angle
      return "ELLIPSE($x,$y,$semimajor,$semiminor,$angle)"
   }

   #  Return an "AST" region description of the object.
   method getregion {{do_update 1}} {
      lassign [getcoords $do_update] x y semimajor semiminor angle
      lassign [grid_coord $x $y] x y
      set angle [expr (90.0-$angle) * acos(-1)/180.0]
      return "ellipse $x $y $semimajor $semiminor $angle"
   }

   #  Set the properties of the object to those of an ARD description
   #  of an object of this type.
   method setard {desc} {
      if {$desc != "" } {
         set failed 1
         if { [check_description ellipse $desc] } {
            if { [llength $qualifiers_] == 5 } {
               lassign $qualifiers_ x y semimajor semiminor angle
               lassign [canvas_coord $x $y] xs ys
               set smaj [canvas_dist $semimajor]
               set smin [canvas_dist $semiminor]
               set sang [canvas_angle $angle $x $y]
               configure -coords "$xs $ys"
               configure -semimajor $smaj
               configure -semiminor $smin
               configure -angle $sang
               set failed 0
               set_size_
            }
         }
         if { $failed } {
            error "Failed to interpret \"$desc\" as an ARD ellipse"
         }
      }
   }

   #  Create a new ellipse using an ARD description.
   method createard {desc {cmd {}}} {
      setard "$desc"
      create_no_resize [code $this set_size_ $cmd] $coords
   }

   #  Set the size after a static creation command and evaluate the
   #  creation command.
   method set_size_ {args} {
      $canvas itemconfigure $canvas_id_ \
         -semimajor $semimajor -semiminor $semiminor -angle $angle
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
         set Xcentre_ [LabelEntry $Frame_.xcentre \
                          -text {X centre:} \
                          -labelwidth $labelwidth_ \
                          -command [code $this configure -x]]
         set Ycentre_ [LabelEntry $Frame_.ycentre \
                          -text {Y centre:} \
                          -labelwidth $labelwidth_ \
                          -command [code $this configure -y]]
         set Major_ [LabelEntry $Frame_.major \
                        -text {Semimajor axis:}\
                        -labelwidth $labelwidth_ \
                        -command [code $this configure -semimajor]]
         set Minor_ [LabelEntry $Frame_.minor \
                        -text {Semiminor axis:}\
                        -labelwidth $labelwidth_ \
                        -command [code $this configure -semiminor]]
         set Angle_ [LabelEntry $Frame_.angle \
                        -text {Position angle:}\
                        -labelwidth $labelwidth_ \
                        -command [code $this configure -angle]]
         pack $Xcentre_ $Ycentre_ $Major_ $Minor_ $Angle_ -side top -fill x
      }

      #  Update the information to be current.
      update_properties
   }

   #  Update all properties.
   method update_properties {} {

      #  Make sure record of canvas item values is up todate.
      update $canvas_id_ resize
      lassign $coords xs ys
      set maj [$canvas itemcget $canvas_id_ -semimajor]
      set min [$canvas itemcget $canvas_id_ -semiminor]
      set ang [$canvas itemcget $canvas_id_ -angle]
      lassign [image_coord $xs $ys] x y
      set semimajor [image_dist $maj]
      set semiminor [image_dist $min]
      set angle [image_angle $ang]

      #  Update the properties box if it exists.
      if { [winfo exists $Properties_] } {
         $Xcentre_ configure -value $x
         $Ycentre_ configure -value $y
         $Major_ configure -value $semimajor
         $Minor_ configure -value $semiminor
         $Angle_ configure -value $angle
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

   #  Semi-major axis in image pixels.
   public variable semimajor {10} {
      if { [winfo exists $Major_] } {
         $Major_ configure -value $semimajor
         set maj [canvas_dist $semimajor]
         $canvas itemconfigure $canvas_id_ -semimajor $maj
      }
   }

   #  Semi-minor axis in image pixels.
   public variable semiminor {5} {
      if { [winfo exists $Minor_] } {
         $Minor_ configure -value $semiminor
         set min [canvas_dist $semiminor]
         $canvas itemconfigure $canvas_id_ -semiminor $min
      }
   }

   #  Position angle.
   public variable angle {0} {
      if { [winfo exists $Angle_] } {
         $Angle_ configure -value $angle
         set ang [canvas_angle $angle $x $y]
         $canvas itemconfigure $canvas_id_ -angle $ang
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Properties widgets.
   protected variable Xcentre_ {}
   protected variable Ycentre_ {}
   protected variable Major_ {}
   protected variable Minor_ {}
   protected variable Angle_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
