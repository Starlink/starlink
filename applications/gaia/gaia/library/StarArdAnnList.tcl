#+
#  Name:
#     StarArdAnnList

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of StarArdList that is specialised to control
#     ARD regions that may have an annulus.

#  Description:
#     This class creates objects that control the drawing and display
#     of a list of ARD objects. The objects may have annuli which are
#     associated with them.

#  Invocations:
#
#        StarArdAnnList object_name [configuration options]
#
#     This creates an instance of a StarArdAnnList object. The return is
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
#     This object inherits StarArdList.

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
#     24-JUN-1996 (PWD):
#        Original version.
#     16-AUG-1996 (PWD):
#        Converted to itcl2.0.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarArdAnnList {

   #  Inheritances:
   #  -------------

   inherit gaia::StarArdList

   #  Constructor:
   #  ------------
   constructor {args} {
      regsub {\-scale[\ ]+[^\ ]+} "$args" {} safeargs
      regsub {\-show_annuli[\ ]+[^\ ]+} "$args" {} safeargs
      eval gaia::StarArdList::constructor -routine_prefix StarArdAnn $safeargs
   } {
      #  Initialise the tag for annular objects.
      set tag_annuli_ "${this}ann_tag"
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Remove all ARD objects.
      for { set i 1 } { $i <= $highest_index_ } { incr i } {
         if { [info exists local_objects_($i)] } {
            delete object $local_objects_($i)
            set objects_($i) {}
            set local_objects_($i) {}
         }
      }
   }

   #  Methods:
   #  --------

   #  Provide create_region that also sets the configurations required
   #  at this level.
   public method create_region {type {resize 1} {desc ""}} {
      global ::tcl_version
      if { [string first $type $known_types_] != -1 } {
         set selected_ [incr highest_index_]
         set local_objects_($selected_) \
            [gaia::$routine_prefix$type \#auto \
                -selected_colour $selected_colour \
                -deselected_colour $deselected_colour \
                -notify_delete_cmd [code $this deleted_object_ $selected_] \
                -tag $tag_ \
                -annulus_tag $tag_annuli_ \
                -show_annulus $show_annuli \
                -continuous_updates $continuous_updates \
                -canvas $canvas \
                -rtdimage $rtdimage \
                -canvasdraw $canvasdraw \
                -scale $scale]

         #  Make sure base classes can use these objects in their
         #  namespaces.
         set objects_($selected_) [code $local_objects_($selected_)]

         #  Create with or without resizing as appropriate.
         if { $resize } {
            $local_objects_($selected_) create_and_resize \
               [code $this created_object $selected_]
         } else {
            $local_objects_($selected_) createard "$desc"
         }
      } else {
         error "Unknown ARD region type \"$type\""
      }
   }

   #  Remove deleted objects from the lists.
   method deleted_object_ {id} {
      catch {unset objects_($id)}
      catch {unset local_objects_($id)}
   }

   #  Write the current annuli descriptions to a stream.
   public method save_annuli_description {ios} {
      set ok 0
      for {set i 0} {$i <= $highest_index_} {incr i} {
         if { [info exists local_objects_($i)] } {
            puts $ios [$local_objects_($i) getann]
            set ok 1
         }
      }
      return $ok
   }

   #  Return a bounding box for all the annuli of the items in the
   #  list. Note all ARD annuli are created as part of the named tag
   #  so can use the tag plus canvas widget to just get a bbox for
   #  them.
   public method bbox_annuli {} {
      lassign [$canvas bbox $tag_annuli_] x1 y1 x2 y2
      lassign [image_coord $x1 $y1] x1 y1
      lassign [image_coord $x2 $y2] x2 y2
      return "$x1 $y1 $x2 $y2"
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Display or hide the object annuli as required.
   public variable show_annuli {1} {
      if { $show_annuli } {
         for {set i 0} {$i <= $highest_index_} {incr i} {
            if { [info exists local_objects_($i)] } {
               $local_objects_($i) configure -show_annulus 1
            }
         }
      } else {
         for {set i 0} {$i <= $highest_index_} {incr i} {
            if { [info exists local_objects_($i)] } {
               $local_objects_($i) configure -show_annulus 0
            }
         }
      }
   }

   #  The scale factor for annuli.
   public variable scale {1.5} {
      if { $show_annuli } {
         for {set i 0} {$i <= $highest_index_} {incr i} {
            if { [info exists local_objects_($i)] } {
               $local_objects_($i) configure -scale $scale
            }
         }
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Tag for annulus objects.
   protected variable tag_annuli_ {}

   #  Name of ARD objects at this scope (objects_ is for base
   #  classes).
   protected variable local_objects_


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
