#+
#  Name:
#     StarArdList

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Controls a list of ARD objects.

#  Description:
#     This class controls a list of ARD objects displayed on a canvas.
#     It controls the creation of many objects from an ARD description
#     stored in a file (the format at this time must be simple and
#     just consist of REGION(parameter) statements). It also writes a
#     description of the current ARD regions to disk file.

#  Invocations:
#
#        StarArdList object_name [configuration options]
#
#     This creates an instance of a StarArdList object. The return is
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
#        -canvasdraw StarCanvasDraw_object
#
#     Name of the StarCanvasDraw object that is to be used to create
#     and control the graphical representations of the ARD regions.
#
#        -canvas canvas
#
#     Name of the canvas widget associated with the CanvasDraw object.
#
#        -rtdimage rtdimage
#
#     Name of the image widget displayed in the canvas (not the item
#     number the image identifier).
#
#        -notify_created_cmd command
#
#     Command to execute when an ARD region is created.

#  Methods:
#
#        createard description
#
#     Command to create an ARD region from a textual description. This
#     should be a complete description including the keyword,
#     qualifiers and possible operators as described in the ARD
#     documentation. Returns 0 if the description cannot be interpreted.
#
#        save_description ios
#
#     Writes an ARD description of the current list of objects to the
#     given input/output stream.
#
#        save_selected_description ios
#
#     Writes an ARD description of the currently selected objects to the
#     given input/output stream.
#
#        create_region type {resize 1} {desc ""}
#
#     Creates a region of the given type (Circle Box etc.) and either
#     allows it to be resized interactively (resize=1), or creates it
#     from an ARD description.
#
#        created_object index id
#
#     Private callback for object creation.
#
#        known_types newtypes
#
#     Set or query the ARD types known to the list.
#
#        deleted_object_ id
#
#     Private callback use when an object is interactively deleted.
#
#        bbox
#
#     Returns an approximate bounding box for the ARD regions in the list.
#
#        image_coord x y
#
#     Converts a canvas coordinate into an image coordinate.

#  Inheritance:
#     This class inherits no other classes.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     15-MAY-1996 (PDRAPER):
#        Original version.
#     5-JUL-1996 (PDRAPER):
#        Converted to itcl2.0.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarArdList {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      eval configure $args

      #  Initialise the tag for ARD objects.
      set tag_ "${this}tag"
   }

   #  Destructor:
   #  -----------
   destructor  {
      #  Remove all ARD objects.
      for { set i 1 } { $i <= $highest_index_ } { incr i } {
         if { [info exists objects_($i)] && $objects_($i) != {} } {
            delete object $objects_($i)
         }
      }
   }

   #  Methods:
   #  --------

   #  Create an ARD region from its description.
   method createard {desc} {
      if { $desc != {} } {
         switch -glob $desc {
            *ROT* {
               set mode RotBox
            }
            *BOX* {
               set mode Box
            }
            *CIR* {
               set mode Circle
            }
            *COL* {
               set mode Column
            }
            *ELL* {
               set mode Ellipse
            }
            *LIN* {
               set mode Line
            }
            *PIX* {
               set mode Pixel
            }
            *POL* {
               set mode Poly
            }
            *REC* {
               set mode Rect
            }
            *ROW* {
               set mode Row
            }
            default {

               #  No match to any known shape.
               return 0
            }
         }
         create_region $mode 0 "$desc"
      }
      return 1
   }


   #  Write the current description to a stream.
   method save_description {ios} {
      set ok 0
      for {set i 1} {$i <= $highest_index_} {incr i} {
         if { [info exists objects_($i)] } {
            puts $ios [$objects_($i) getard]
            set ok 1
         }
      }
      return $ok
   }

   #  Write the description of the currently selected regions to a
   #  file.
   method save_selected_description {ios} {
      set ok 0

      #  Get a list of the currently selected objects.
      for {set i 1} {$i <= $highest_index_} {incr i} {
         if { [info exists objects_($i)] } {
            if { [$objects_($i) is_selected] } { 
               puts $ios [$objects_($i) getard]
               set ok 1
            }
         }
      }
      return $ok
   }

   #  Create an ARD region of the given type.
   method create_region {type {resize 1} {desc ""}} {
      if { [string first $type $known_types_] != -1 } {
         set selected_ [incr highest_index_]
         set objects_($selected_) \
            [$routine_prefix$type \#auto \
                -selected_colour $selected_colour \
                -deselected_colour $deselected_colour \
                -notify_delete_cmd [code $this deleted_object_ $selected_] \
                -tag $tag_ \
                -continuous_updates $continuous_updates \
                -canvas $canvas \
                -rtdimage $rtdimage \
                -canvasdraw $canvasdraw ]
         if { $resize } { 
            $objects_($selected_) create_and_resize \
               [code $this created_object $selected_]
         } else { 
            $objects_($selected_) createard "$desc"
         }
      } else {
         error "Unknown ARD region type \"$type\""
      }
   }

   #  Method to deal with object creation confirmation.
   method created_object {index id} {
       if { $notify_created_cmd != {} } { 
	   eval $notify_created_cmd $index $id
       }
   }

   #  Set and/or return a string containing all the known region types.
   method known_types {newtypes} {
      if {$newtypes == {} } {
         return $known_types_
      } else {
         set known_types_ $newtypes
         return $known_types_
      }
   }

   #  Deal with object deletion from lists (note this is call back
   #  only version, from the object itself, it doesn't delete the object).
   method deleted_object_ {id} {
      catch {unset objects_($id)}
   }

   #  Return a bounding box for all the items in the list. Note all
   #  ARD objects are created as part of the named tag so can use the
   #  tag plus canvas widget to just get a bbox for them.
   method bbox {} {
      lassign [$canvas bbox $tag_] x1 y1 x2 y2
      lassign [image_coord $x1 $y1] x1 y1
      lassign [image_coord $x2 $y2] x2 y2
      return "$x1 $y1 $x2 $y2"
   }

   #  Convert from canvas coordinates to image coordinates (note
   #  no origin corrections are applied).
   method image_coord { x y } {
      if { $rtdimage != {} } {
         $rtdimage convert coords $x $y canvas x y image
         set x [expr $x-0.5]
         set y [expr $y-0.5]
      }
      return "$x $y"
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Prefix of name of ARD routines to call. This is the first part
   #  of the name and excludes the type (i.e. if the routine is
   #  StarArdCircle then this value is "StarArd", the "Circle" part if
   #  generated by the type of the various methods.
   public variable routine_prefix StarArd {}

   #  Name of StarCanvasDraw object that controls the overlay graphics
   #  of the objects.
   public variable canvasdraw {} {}

   #  Name of canvas.
   public variable canvas {} {}

   #  Name of GaiaImage type object for converting aperture sizes
   #  into displayed canvas sizes.
   public variable rtdimage {} {}

   #  Command to execute when a new object is created.
   public variable notify_created_cmd {} {}

   #  Colours of regions when selected/deselected.
   public variable selected_colour white {}
   public variable deselected_colour green {}

   #  Control whether objects return a stream of updates or just one
   #  when completed change.
   public variable continuous_updates {1} {
      if { $continuous_updates } {
         for { set i 1 } { $i <= $highest_index_ } { incr i } {
            if { [info exists objects_($i)] } {
               $objects_($i) configure -continuous_updates 1
            }
         }
      } else {
         for { set i 1 } { $i <= $highest_index_ } { incr i } {
            if { [info exists objects_($i)] } {
               $objects_($i) configure -continuous_updates 0
            }
         }
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Array of the names of the ARD regions created. These are indexed
   #  by an integer.
   protected variable objects_

   #  Index of the current object.
   protected variable selected_ {}

   #  Highest index of the objects that have been created.
   protected variable highest_index_ 0

   #  The known region types.
   protected variable known_types_ \
      "Ellipse Circle Rect Line Poly RotBox Column Row Pixel"

   #  Tag for all canvas items created by this list.
   protected variable tag_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}


