#+
#  Name:
#     StarArdAnnRect

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of ARD Rect with an annulus of the same shape.

#  Description:
#     This class creates an ARD canvas Rect item and draws an
#     annulus at the given scale factor about it. The annulus is
#     updated with the ARD Rect.

#  Invocations:
#
#        StarArdAnnRect object_name [configuration options]
#
#     This creates an instance of a StarArdAnnRect object. The return is
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
#        -scale factor
#
#     The factor by which the annulus is larger than the ordinary ARD
#     Rect.

#  Configuration options:
#     See public variable defintions.

#  Methods:
#     See method definitions.

#  Inheritance:
#     This object inherits StarArdRect.

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
#     21-JUN-1996 (PWD):
#        Original version.
#     20-AUG-1996 (PWD):
#        Converted itcl2.0.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarArdAnnRect {

   #  Inheritances:
   #  -------------

   inherit gaia::StarArdRect

   #  Constructor:
   #  ------------
   constructor {args} {
      set notify_created_cmd_ [code $this create_first_annulus_]
      set notify_update_cmd_  [code $this redraw_annulus_]
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $annulus_id_ != {} } {
         $canvas delete $annulus_id_
      }
   }

   #  Methods:
   #  --------

   #  Create the annulus from an interactive call.
   private method create_first_annulus_ {args} {
      set created_ 1
      set notify_created_cmd_ {}
      create_annulus_
   }

   #  Create the annulus.
   private method create_annulus_ {args} {
      if { $show_annulus } {
         set annulus_id_ [eval $canvas create rectangle $coords \
                             -outline $deselected_colour]
         $canvas addtag $annulus_tag withtag $annulus_id_
         redraw_annulus_
      }
   }

   #  Redraw the annulus. Note we determine the centre of the object
   #  and then scale the whole object.
   private method redraw_annulus_ {} {
      if { $annulus_id_ != {} } {
         lassign $coords x1 y1 x2 y2
         set xcen [expr ($x2+$x1)/2.0]
         set ycen [expr ($y2+$y1)/2.0]
         $canvas coords $annulus_id_ $x1 $y1 $x2 $y2
         $canvas scale $annulus_id_ $xcen $ycen $scale $scale
      }
   }

   #  Return ARD description of annulus. Uses the getard method with
   #  falsified coords.
   public method getann {} {
      if { $annulus_id_ != {} } {
         set oldcoords $coords
         set coords [$canvas coords $annulus_id_]
         set ard [getard 0]
         set coords $oldcoords
         return $ard
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The annulus scaling factor.
   public variable scale {1.5} {
      if { $annulus_id_ != {} } {
         redraw_annulus_
      }
   }

   #  Whether to show annulus or not.
   public variable show_annulus {1} {
      if { $show_annulus } {
         if { $annulus_id_ == {} && $created_ } {
            create_annulus_
            set notify_update_cmd_  [code $this redraw_annulus_]
         }
      } else {
         if { $annulus_id_ != {} } {
            $canvas delete $annulus_id_
            set annulus_id_ {}
            set notify_update_cmd_  {}
         }
      }
   }

   #  Canvas tag to add this object to.
   public variable annulus_tag {ArdRect} {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Canvas id of annulus.
   protected variable annulus_id_ {}

   #  Whether annulus object creation has been invoked interactively
   #  or not. In which case show_annulus will change the state.
   protected variable created_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
