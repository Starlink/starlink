#+
#  Name:
#     Gaia3dArdPrismProxy

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Proxy interface for a collection of a line and related ARD prism objects.

#  Description:
#     This class manages the life-cycle of a collection of objects that
#     can be renderered into a scene. The objects represent a prism shaped
#     region bounded within a cube, this can be a simple line or more
#     complex shapes based on ARD descriptions. Only one object of the
#     collection is active at any time.

#  Copyright:
#     Copyright (C) 2007 Science and Technology Facilities Council
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
#     07-AUG-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dArdPrismProxy {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set any configuration variables.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      remove_from_window
      foreach region [array names collection_] {
         ::delete object $collection_($region)
      }
   }

   #  Methods and procedures:
   #  -----------------------

   #  Add current region to the render window.
   public method add_to_window {} {
      set add_to_window_ 1
      if { $current_ != {} } {
         $collection_($current_) add_to_window
      }
   }

   #  Remove from the render window.
   public method remove_from_window {} {
      set add_to_window_ 0
      if { $current_ != {} } {
         $collection_($current_) remove_from_window
      }
   }

   #  Make visible.
   public method set_visible {} {
      set visible_ 1
      if { $current_ != {} } {
         $collection_($current_) set_visible
      }
   }

   #  Make invisible.
   public method set_invisible {} {
      set visible_ 0
      if { $current_ != {} } {
         $collection_($current_) set_invisible
      }
   }

   #  Accept a region description. Look for an existing object that supports
   #  this type and apply the description, or create new supporting object.
   public method set_description {type desc} {
      set oldtype $current_
      if { $type == "p" } {
         #  Simple line with x,y position.
         if { ! [info exists collection_($type)] } {
            set collection_($type) [gaia3d::Gaia3dVtkLine \#auto]
         }
         eval $collection_($type) set_axis_position $desc

      } else {
         #  Full ARD description.
         set type [gaia3d::Gaia3dArdUtils::get_ard_region $desc]
         if { ! [info exists collection_($type)] } {
            set collection_($type) [instance $desc]
         } else {
            $collection_($type) set_from_desc $desc
         }
      }

      #  Set the current object. If we've seen the add_to_window and
      #  set visible commands, then enable this immediately. Make a shape that
      #  is already visible invisible, if a shape change has occurred.
      set current_ $type
      apply_configuration_

      if { $type != $oldtype } {
         if { $add_to_window_ } {
            add_to_window
         }
         if { $visible_ } {
            if { $oldtype != {} } {
               $collection_($oldtype) set_invisible
            }
            set_visible
         }
      }
   }

   #  Return a description and the current type (p for point extraction).
   public method get_description {} {
      if { $current_ == "p" } {
         return [list "p" [$collection_($current_) get_axis_position]]
      }
      return [list $current_ [$collection_($current_) get_desc]]
   }

   #  Set the position of the shape. If a point the ix,iy,iz values are used
   #  if a general shape then the delta shifts will be applied.
   public method set_position {ix iy iz dx dy dz} {
      if { $current_ != {} } {
         if { $current_ == "p" } {
            $collection_($current_) set_position $ix $iy $iz
         } else {
            $collection_($current_) shift_position $dx $dy $dz
         }
      }
   }

   #  Make the extent along the axis fit the data cube limits.
   public method fit_to_data {} {
      if { $current_ != {} } {
         $collection_($current_) fit_to_data
      }
   }

   #  Get the prism type, will be "p" for a point.
   public method get_type {} {
      return $current_
   }

   #  Apply the current configuration to the currently active object.
   protected method apply_configuration_ {} {
      if { $current_ != {} } {
         $collection_($current_) configure \
            -dataset $dataset \
            -renwindow $renwindow \
            -align_to_axis $align_to_axis \
            -axis $axis \
            -colour $colour
         fit_to_data
      }
   }


   #  =================================
   #  Procedures for creating instances
   #  =================================

   #  Factory method to create an instance for a given ARD description.
   #  The result is a subclass of this one.
   public proc instance {desc} {
      foreach shape [array names subclasses_] {
         if { [eval gaia3d::$subclasses_($shape)::matches "\$desc"] } {
            return [eval gaia3d::$subclasses_($shape)::instance "\$desc"]
         }
      }
      error "Failed to parse \"$desc\" into an ARD region"
   }

   #  Return the ARD class for a shape. If not known returns {}.
   public proc get_ard_class {shape} {
      if { [info exists subclasses_($shape)] } {
         return $subclasses_($shape)
      }

      #  Check case before giving up.
      set shape [string tolower $shape]
      if { [info exists subclasses_($shape)] } {
         return $subclasses_($shape)
      }
      return {}
   }

   #  Configuration options: (public variables)
   #  ----------------------
   #  Note all these should be supported by the proxied objects.

   #  The render window (a Gaia3dVtkWindow instance).
   public variable renwindow {} {
      apply_configuration_
   }

   #  The current vtkImageData instance. Make sure extent information is
   #  available.
   public variable dataset {} {
      if { $dataset != {} } {
         $dataset Update
         apply_configuration_
         fit_to_data
      }
   }

   #  Whether to align to an axis.
   public variable align_to_axis 0 {
      apply_configuration_
   }

   #  The axis to align to.
   public variable axis 3 {
      apply_configuration_
   }

   #  The colour.
   public variable colour {#0ff} {
      apply_configuration_
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  The proxied objects, indexed by a region description.
   protected variable collection_

   #  The current object.
   protected variable current_ {}

   #  If proxied object should be added to the render window.
   protected variable add_to_window_ 0

   #  If proxied object should be made visible.
   protected variable visible_ 0


   #  Common variables: (shared by all instances)
   #  -----------------

   #  Array of the known sub-classes. Used to create instances from the
   #  instance factory method and to map shapes to classes.
   common subclasses_
   array set subclasses_ {
      circle  Gaia3dVtkArdCirclePrism
      column  Gaia3dVtkArdColumnPrism
      ellipse Gaia3dVtkArdEllipsePrism
      line    Gaia3dVtkArdLinePrism
      polygon Gaia3dVtkArdPolygonPrism
      rotbox  Gaia3dVtkArdRotboxPrism
      rect    Gaia3dVtkArdRectPrism
      row     Gaia3dVtkArdRowPrism
   }

#  End of class definition.
}
