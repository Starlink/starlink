#+
#  Name:
#     GaiaPhotomList

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class to manage lists of photometry objects.

#  Description:
#     This class manages a list of GaiaPhotomObjects, providing the
#     ability to read, write and display a list and create or delete
#     objects in the list. The list can be displayed in a scrolled
#     listbox and bindings may be associated with actions in the list.
#
#     The objects (and more importantly their properties) can be
#     displayed graphically in a GaiaImage widget. A state is
#     associated with each object -- selected, displayed or new -- and
#     the object appears graphically in the correct form. Objects that
#     are displayed are drawn in a different colour to those that are
#     selected.
#
#     The types of photometry objects are split into three different
#     forms (which have slightly different lists of properties). These
#     are the standard aperture, optimal and psf types. Which of these is
#     controlled by this list is decided by the configuration options
#     "phottype" and "psf". These options should be set during
#     initialisation.

#  Invocations:
#
#        GaiaPhotomList object_name [configuration options]
#
#     This creates an instance of a GaiaPhotomList object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#
#        -modified boolean
#
#     Set true when any of the apertures are modified. This should
#     not normally be set from outside this class.
#
#        -allow_resize boolean {1}
#
#     Whether apertures can be interactively resized or not during
#     creation.
#
#         -angle angle {0.0}
#
#     Default position angle of any elliptical apertures.
#
#         -annulus boolean {1}
#
#     Whether sky regions are defined by annuli or not.
#
#         -canvas name_of_canvas {}
#
#     The name of the canvas widget used to display the apertures.
#
#         -canvasdraw name_of_canvasdraw {}
#
#     The name of the StarCanvasDraw widget used to control the
#     apertures.
#
#        -details name_of_details {}
#
#     The name of the widget (GaiaPhotomDetails) used to display the
#     selected apertures details.
#
#        -eccentricity float {0.0}
#
#     The default eccentricity of elliptical apertures.
#
#        -innerscale float {1.5}
#
#     The default scale factor of the inner locus of sky region, if
#     defined using an annulus
#
#        -linewidth integer {1}
#
#     Linewidth used to draw apertures and annuli.
#
#        -modified boolean
#
#     Set true when any of the apertures are modified. This should
#     not normally be set from outside this class.
#
#        -notify_created_cmd command {}
#
#     Command to execute when an aperture is created (callback).
#
#        -outerscale float {2.0}
#
#     The default scale factor of the outer locus of sky region, if
#     defined using an annulus
#
#        -positions (annulus|regions) {annulus}
#
#     How the sky region are determined. Either from an annulus or
#     from a set of apertures.
#
#        -rtdimage {}
#
#     Name of the rtdimage displayed on the canvas.
#
#        -scrollbox name_of_listbox {.gaiaphotomlist}
#
#     Name of a listbox or Scrollbox used to display the values
#     associated with the current apertures.
#
#        -semimajor float {5}
#
#     The default semi-major axis of elliptical and diameter of
#     circular apertures.
#
#        -shape (circle|ellipse) {circle}
#
#     The default shape of apertures when created.
#
#        -show_list boolean {0}
#
#     Whether to display the list of all measurements in the scrollbox
#     or not.
#
#        -usemags boolean {1}
#
#     Whether calculations are in magnitudes or not.
#
#        -phottype (aperture|optimal) {aperture}
#
#     The type of photometry objects being managed.
#
#        -psf (1|0) {0}
#
#     If phottype is optimal then this decides if the list of objects
#     are PSF reference ones, or not.
#
#        -notify_changed_cmd
#
#     Command to execute when the current object properties are
#     changed. Use this to track coupled changes into other lists.

#  Methods:
#     public:
#        add update args
#           Add an object to the list. If update is true and the object
#           exists then its current values are changed, otherwise a
#           new object is created with the given values. $args should
#           be a list of the following values.
#
#              index (other arguments)
#
#        config_selected item value
#           Configure all the selected objects with the given item
#           value configuration.
#        config_all item value
#           Configure all the objects with the given item
#           value configuration.
#        copy
#           Create a new object that is a copy of the currently
#           selected one (the last if many are selected).
#        create_object
#           Create an aperture on the canvas (user provides position
#           using mouse).
#        create_sky_region
#           Create an aperture as a sky region
#        new_object index
#           Create a new object with the current configuration and
#           store its details using index.
#        read_file filename update
#           Read the details of a list of objects from the given
#           file. If update is true then existing objects with the
#           same index values are modified.
#        write_file filename {all 1}
#           Write a photom file (extended form). If all is true (default)
#           then all objects are written, otherwise just the currently
#           selected objects or last selected object is used.
#        append_file comment filename
#           Append a list of the current objects to a file (which may or may
#           not already exist). The comment string, if not {}, is written
#           before the record (so is useful for recording filenames etc.).
#
#     private:
#        add_scrollbox_bindings_
#           Add bindings to elements in listbox so that an object can
#           be selected by double clicking on its entry.
#        changed_ index
#           Process notification that object has changed.
#        created_object_ index id
#           Deal with notification that an aperture has been created.
#        deleted_ index
#           Deal with notification that an object should be deleted.
#        select_scrollbox_line_ y
#           Select a line in a the scrollbox by y position and make
#           the object at that line the current one.
#        update_details_
#           Deal with notication that an objects details should be
#           redisplayed.
#        update_scrollbox_
#           Deal with notication that the object details displayed in
#           the scrollbox should be redisplayed.

#  Inheritance:
#     This widget inherits no other classes.

#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
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
#     12-MAR-1996 (PWD):
#        Original version.
#     8-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     29-MAY-1999 (PWD):
#        Added changes to support optimal photometry.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaPhotomList {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor {

      #  Remove all photometry objects.
      for { set i $lowest_index_ } { $i <= $highest_index_ } { incr i } {
         if { [info exists objects_($i)] } {
            delete object $objects_($i)
         }
      }
   }

   #  Methods:
   #  --------

   #  Create a new GaiaPhotomObject with all the current
   #  configurations and an arbitrary name.
   public method new_object {index} {
      set object [gaia::GaiaPhotomObject \#auto \
                     -index $index \
                     -major $semimajor \
                     -eccen $eccentricity \
                     -innerscale $innerscale \
                     -outerscale $outerscale \
                     -positions $positions \
                     -angle $angle \
                     -shape $shape \
                     -linewidth $linewidth \
                     -canvasdraw $canvasdraw \
                     -rtdimage $rtdimage \
                     -canvas $canvas \
                     -fwhm1 $fwhm1 \
                     -fwhm2 $fwhm2 \
                     -seeing $seeing \
                     -phottype $phottype \
                     -psf $psf \
                     -selected_colour $selected_colour \
                     -deselected_colour $deselected_colour \
                     -selected_sky_colour $selected_sky_colour \
                     -deselected_sky_colour $deselected_sky_colour]
      set modified 1
      return $object
   }

   #  Copy the currently "selected" object.
   public method copy {} {
      if {[info exists objects_($selected_)] } {

         #  Get current state of object.
         set state [$objects_($selected_) getvalues all]

         #  Get an index for the new object.
         incr highest_index_
         set selected_ $highest_index_

         #  Set the values to those of the original, except we use the
         #  new index. Need to re-parse to extract the sky regions.
         foreach line [split $state "\n"] {
            switch -glob $line {
               \#SKY* {
                  set rest [lassign $line comment index]
                  eval $objects_($selected_) setsky SKY $rest
               }
               \#ANN* {
                  set rest [lassign $line comment index]
                  eval $objects_($selected_) setsky ANN $rest
               }
               default {
                  set rest [lassign $line index]
                  eval add 0 $selected_ $rest
               }
            }
         }
      }

      #  Finally update the scrollbox and the selected object details.
      update_scrollbox_
      update_details_
   }

   #  Create an object on the canvas. If PSF then index can only be 0.
   public method create_object {} {
      if { $phottype != "aperture" && $psf } {
         if { [info exists objects_(0)] } {
            delete object $objects_(0)
         }
         set selected_ 0
      } else {
         set selected_ [incr highest_index_]
      }
      set objects_($selected_) [new_object $selected_]

      #  Two basic methods of operation are allowed -- fully
      #  interactive resize and placement, or just placement (size of
      #  aperture is fixed).
      if { $allow_resize } {
         if { $shape == "circle" } {
            $objects_($selected_) create_and_resize_circle \
               [code $this created_object_ $selected_]
         } else {
            $objects_($selected_) create_and_resize_ellipse \
               [code $this created_object_ $selected_]
         }
      } else {
         if { $shape == "circle" } {
            $objects_($selected_) create_no_resize_circle \
               [code $this created_object_ $selected_] $semimajor
         } else {
            $objects_($selected_) create_no_resize_ellipse \
               [code $this created_object_ $selected_] $semimajor $eccentricity $angle
         }
      }
   }

   #  Interactive creation finished.
   private method created_object_ {index id} {
      set objects_ids_($id) $index

      #  Set the notify command for when this object is changed.
      $objects_($index) configure \
         -notify_change_cmd [code $this changed_ $index]

      #  Set the notify command for when this object is deleted.
      $objects_($index) configure \
         -notify_delete_cmd [code $this deleted_ $index]

      #  Notify user of this class that a new object has been
      #  created.
      if { $notify_created_cmd != {} } {
         eval $notify_created_cmd
      }

      #  And simulate a notification of an initial change.
      if { $allow_resize } {
         changed_ $index
      }
   }

   #  Create a new sky region and associate with the current object.
   public method create_sky_region {} {
      if { $selected_ != {} } {
         if { $allow_resize } {
            if { $shape == "circle" } {
               $objects_($selected_) create_and_resize_sky_circle
            } else {
               $objects_($selected_) create_and_resize_sky_ellipse
            }
         } else {
            if { $shape == "circle" } {
               $objects_($selected_) create_no_resize_sky_circle
            } else {
               $objects_($selected_) create_no_resize_sky_ellipse
            }
         }
      }
   }

   #  Add an object to the list. If update is true and the object
   #  exists then its current values are changed, otherwise a new object
   #  is created with the given values.
   public method add {update args} {
      if { [llength $args] > 0 } {

         #  Extract the index from the list and keep the rest for later.
         set trail [lassign $args index]

         #  Set default configuration to reflect the given
         #  values. Also add defaults for unused parameters.
         set selected_ $index
         if { [llength $args] != 1 } {
            if { $phottype == "aperture" } {
               lassign $args selected_ xpos ypos mag magerr sky \
                  signal code semimajor eccentricity angle positions shape
            } else {
               if { $psf } {
                  lassign $args selected_ xpos ypos fwhm1 fwhm2 angle \
                     code semimajor seeing positions
                  set selected_ 0
                  set mag 0.0
                  set magerr 0.0
                  set sky 0.0
                  set signal 0.0
               } else {
                  lassign $args selected_ xpos ypos mag magerr sky \
                     signal code positions
               }
            }
         }

         #  Create a new object if it doesn't exist already, otherwise
         #  just update the existing object.
         if { ! [info exists objects_($index)] } {
            set objects_($selected_) [new_object $selected_]
            $objects_($selected_) draw_object
            $objects_($selected_) setallvalues \
               $selected_ $xpos $ypos $mag $magerr $sky $signal $code \
               $semimajor $eccentricity $angle $positions $shape \
               $fwhm1 $fwhm2 $seeing
            if { $selected_ > $highest_index_ } {
               set highest_index_ $selected_
            }
            set id [$objects_($selected_) canvas_id]
            set objects_ids_($id) $selected_

            #  Notify command when this object is changed.
            $objects_($selected_) configure -notify_change_cmd \
               [code $this changed_ $selected_]

            #  Notify command when this object is deleted.
            $objects_($selected_) configure -notify_delete_cmd \
               [code $this deleted_ $selected_]
            set modified 1

         } elseif { $update } {

            #  Object exists and update is allowed so modify the
            #  current values.
            if { [llength $args] != 1 } {
               $objects_($selected_) setallvalues \
                  $selected_ $xpos $ypos $mag $magerr $sky $signal $code \
                  $semimajor $eccentricity $angle $positions $shape \
                  $fwhm1 $fwhm2 $seeing
            }
            if { $index > $highest_index_ } {
               set highest_index_ $index
            }
            set id [$objects_($selected_) canvas_id]
            set objects_ids_($id) $selected_
         }
      }
   }

   #  Read a photom file. Note that update should be set if any
   #  objects that already exist are to be changed. A possible problem
   #  with this method is that aperture with sky regions will have new
   #  sky regions created when being updated (rather than having the
   #  existing ones superceded), so it is not possible to sensibly update
   #  this aperture type.
   public method read_file {filename update} {
      if { [file readable $filename] } {
         set fid [open $filename r]
         if { $phottype == "aperture" } {
            read_apfile_ $fid $filename $update
         } else {
            if { $psf } {
               read_psffile_ $fid $filename $update
            } else {
               read_optfile_ $fid $filename $update
            }
         }
         close $fid

         #  Finally update the scrollbox and the selected object details.
         update_scrollbox_
         update_details_

         #  Objects have changed, so update modified flag.
         set modified 1
      } else {
         error "Cannot read file: $filename."
      }
   }

   #  Read back an aperture file.
   protected method read_apfile_ {fid filename update} {
      set old_selected $selected_
      set ok 1

      #  Loop over non-blank lines. If line starts with # it is
      #  either a comment or a sky region spec. Sky region specs
      #  start with '#SKY' or '#ANN'.
      while { $ok  } {
         set llen [gets $fid line]
         if { $llen > 0 } {
            switch -glob $line {
               \#SKY* {
                  if { ! $update } {
                     set rest [lassign $line comment index]
                     eval $objects_($index) setsky SKY $rest
                  }
               }
               \#ANN* {
                  set rest [lassign $line comment index]
                  eval $objects_($index) setsky ANN $rest
               }
               \#* { ;#  Do nothing for comments
               }
               default {
                  eval add $update $line
               }
            }
         } elseif { $llen < 0 } {
            set ok 0
         }
      }

      if { $old_selected != {} } {
         set selected_ $old_selected
      }
   }

   #  Read an optimal photometry file. This is a special case as the
   #  reference PSF star, with index 0, is ignored.
   protected method read_optfile_ {fid filename update} {
      set ok 1
      set old_selected $selected_

      #  Loop over non-blank lines. If line starts with # it is
      #  either a comment or a sky region spec. Sky region specs
      #  start with '#SKY' or '#ANN'.
      while { $ok  } {
         set llen [gets $fid line]
         if { $llen > 0 } {
            switch -glob $line {
               \#SKY* {
                  if { ! $update } {
                     set rest [lassign $line comment index]
                     if { $index != 0 } {
                        eval $objects_($index) setsky SKY $rest
                     }
                  }
               }
               \#ANN* {
                  if { $update } {
                     set rest [lassign $line comment index]
                     if { $index != 0 } {
                        eval $objects_($index) setsky ANN $rest
                     }
                  }
               }
               \#* { ;#  Do nothing for comments
                  }
               default {
                  lassign $line index
                  if { $index != 0} {
                     eval add $update $line
                  }
               }
            }
         } elseif { $llen < 0 } {
            set ok 0
         }
      }

      if { $old_selected != {} } {
         set selected_ $old_selected
      }
   }

   #  Read a PSF object entry. This is the one with index 0.
   protected method read_psffile_ {fid filename update} {
      set ok 1
      set old_selected $selected_

      #  Loop over non-blank lines. If line starts with # it is
      #  either a comment or a sky region spec. Sky region specs
      #  start with '#SKY' or '#ANN'. Note in this case index is 0.
      while { $ok  } {
         set llen [gets $fid line]
         if { $llen > 0 } {

            switch -glob $line {
               \#SKY* {
                  if { ! $update } {
                     set rest [lassign $line comment index]
                     if { $index == 0 } {
                        eval $objects_($index) setsky SKY $rest
                     }
                  }
               }
               \#ANN* {
                  if { $update } {
                     set rest [lassign $line comment index]
                     if { $index == 0 } {
                        eval $objects_($index) setsky ANN $rest
                     }
                  }
               }
               \#* { ;#  Do nothing for comments
                  }
               default {
                  lassign $line index
                  if { $index == 0} {
                     eval add $update $line
                  }
               }
            }
         } elseif { $llen < 0 } {
            set ok 0
         }
      }

      if { $old_selected != {} } {
         set selected_ $old_selected
      }
   }

   #  Write a photom file (extended form). If all is true (default)
   #  then all objects are written, otherwise just the currently selected
   #  objects are used, or the $selected_ object.
   public method write_file {filename {all 1} } {
      set ok 0
      set fid [open $filename w]
      if { $all } {
         for { set i $lowest_index_ } { $i <= $highest_index_ } { incr i } {
            if { [info exists objects_($i)] } {
               puts $fid "[$objects_($i) getvalues all]"
               set ok 1
            }
         }
      } else {
         set ids [$canvasdraw selected_items]
         if { $ids != {} } {
            foreach id "$ids" {
               if { [info exists objects_ids_($id)] } {
                  puts $fid "[$objects_($objects_ids_($id)) getvalues all]"
                  set ok 1
               }
            }
         } else {
            puts $fid "[$objects_($selected_) getvalues all]"
            set ok 1
         }
      }
      close $fid
      set modified 0
      return $ok
   }

   #  Append the current objects to a file. Add a comment, if not empty.
   public method append_file {comment filename} {
      set ok 0
      set fid [open $filename a+]
      if { $comment != {} } {
         puts $fid "# $comment"
      }
      for { set i $lowest_index_ } { $i <= $highest_index_ } { incr i } {
         if { [info exists objects_($i)] } {
            puts $fid "[$objects_($i) getvalues all]"
            set ok 1
         }
      }
      close $fid
      return $ok
   }

   #  Read a positions list and create phometry objects with default
   #  settings at those positions. A position list is a simple file
   #  of "x y", "id x y" or "id ra dec x y" values. Only supported for
   #  aperture photometry.
   public method read_positions_file {filename update} {
      if { $phottype != "aperture" } {
         error "Cannot read a positions file for non-aperture photometry"
      }
      if { [file readable $filename] } {
         set fid [open $filename r]
         read_ap_positions_file_ $fid $filename $update
         close $fid

         #  Finally update the scrollbox and the selected object details.
         update_scrollbox_
         update_details_

         #  Objects have changed, so update modified flag.
         set modified 1
      } else {
         error "Cannot read file: $filename."
      }
   }


   #  Read a positions file and create photometry objects with default
   #  settings for all unknown configuration data.
   protected method read_ap_positions_file_ {fid filename update} {
      set old_selected $selected_
      set ok 1

      # XXX get these from the caller.
      set object_defaults_ "0.0 0.0 0.0 0.0 OK 9.0 0.0 0.0 annulus circle"
      set sky_defaults_ "1.5 2.5"


      #  Loop over non-blank lines. If line starts with # it is a comment.
      #  Other lines should be "id x y"
      set index 0
      while { $ok  } {
         set llen [gets $fid line]
         if { $llen > 0 && [string index $line 0] != "\#" } {
            set nwords [llength $line]
            if { $nwords == 2 } {

               #  Simple x y, fake an index.
               incr index
               eval add $update "$index $line" $object_defaults_
               #  Sky region.
               eval $objects_($index) setsky ANN $sky_defaults_
            } elseif { $nwords == 3 } {

               #  Simple id x y
               eval add $update $line $object_defaults_
               #  Sky region.
               lassign $line index
               eval $objects_($index) setsky ANN $sky_defaults_
            } elseif { $nwords == 5 } {

               #  GAIA positions file. Should support those.
               lappend {id ra dec x y}
               eval add $update "$id $x $y" $object_defaults_
               lassign $line index
               eval $objects_($index) setsky ANN $sky_defaults_
            } else {

               error "Unknown contents: $line"
            }
         } elseif { $llen < 0 } {

            #  End of file.
            set ok 0
         }
      }

      if { $old_selected != {} } {
         set selected_ $old_selected
      }
   }


   #  Set a configuration option for the currently selected
   #  objects. If in "psf" mode then only one object exists.
   public method config_selected {item value} {
      if { $phottype != "aperture" && $psf } {
         if { [info exists objects_(0)] } {
            $objects_(0) sync
            $objects_(0) configure -$item $value
         }
         return
      }
      if { ! $coupled } {
         set ids [$canvasdraw selected_items]
         if { $ids != {} } {
            foreach id "$ids" {
               if { [info exists objects_ids_($id)] } {
                  if { [info exists objects_($objects_ids_($id))] } {
                     $objects_($objects_ids_($id)) sync
                     $objects_($objects_ids_($id)) configure -$item $value
                  }
               }
            }
         }
      } else {
         #  Using coupled scheme, or all values the same.
         config_all $item $value
      }
   }

   #  Set a configuration option for all objects. Note we make sure
   #  that the object is as drawn (sync), before applying the update.
   public method config_all {item value} {
      for {set i $lowest_index_} {$i <= $highest_index_} {incr i} {
         if { [info exists objects_($i)] } {
            $objects_($i) sync
            $objects_($i) configure -$item $value
         }
      }
   }

   #  Deal with notification that an object has been deleted.
   private method deleted_ {index} {
      unset objects_($index)
      set selected_ [lindex [array names objects] 0]
      update_details_
      set modified 1
   }

   #  Update the list of objects if allowed.
   private method update_scrollbox_ {} {
      if { [winfo exists $scrollbox] } {
         $scrollbox clear all
         $scrollbox insert 0 [gaia::GaiaPhotomObject::header short \
                                 $phottype $psf $usemags]
         for { set i $lowest_index_ } { $i <= $highest_index_ } { incr i } {
            if { [info exists objects_($i)] } {
               $scrollbox insert end [$objects_($i) getvalues short]
            }
         }
         raise [winfo toplevel $scrollbox]
      }
   }
   #  Update the displayed details of current object.
   private method update_details_ {} {
      global ::tcl_version
      if { [winfo exists $details] && $canvasdraw != {} } {
         if { $selected_ != {} && [info exists objects_($selected_)] } {
            set id [$objects_($selected_) canvas_id]
            if { "$id" != "" } {
               $details update_display [code $objects_($selected_)]
            }
         } else {
            # Reset details.
            $details reset_display
         }
      }
   }

   #  Add bindings to allow some control of the objects from the
   #  scrollbox. Double clicking a line selects that object.
   private method add_scrollbox_bindings_ {} {
      set listbox [$scrollbox listname]
      bind $listbox <Double-1> [code $this select_scrollbox_line_ %y]
   }

   #  Select a line in a the scrollbox by y position and make the
   #  object at that line the current one.
   private method select_scrollbox_line_ {y} {
      set listbox [$scrollbox listname]
      set line [$listbox nearest $y]
      if { $line != {} } {
         set contents [$scrollbox get $line]
         set index [lindex $line 0]
         if { [info exists objects_($index)] } {
            $objects_($selected_) deselected
            $objects_($index) selected
            changed_ $index
         }
      }
   }

   #  Deal with notification that an object has been changed
   #  interactively.
   private method changed_ {index} {
      set selected_ $index

      #  These values now become the default (for creation of
      #  new objects without resize). We also need to reset all objects
      #  they are coupled to the same properties.
      if { [info exists objects_($index)] } {
         if { $phottype == "aperture" } {
            lassign [$objects_($index) object_details] \
               index x y mag magerr sky signal code semimajor \
               eccentricity angle positions innerscale outerscale
	    if { $coupled } {
	       for {set i $lowest_index_} {$i <= $highest_index_} {incr i} {
		  if { [info exists objects_($i)] && $i != $index} {
                     $objects_($i) sync
		     $objects_($i) configure \
			-major $semimajor \
			-eccen $eccentricity \
			-angle $angle \
			-innerscale $innerscale \
			-outerscale $outerscale
		  }
	       }
	    }
         } else {
            if { $psf } {
               lassign [$objects_($index) object_details] \
                  index x y fwhm1 fwhm2 angle code semimajor seeing \
                  positions innerscale outerscale
	       if { $coupled } {
		  for {set i $lowest_index_} {$i <= $highest_index_} {incr i} {
		     if { [info exists objects_($i)] && $i != $index} {
			$objects_($i) sync
			$objects_($i) configure \
			   -fwhm1 $fwhm1 \
			   -fwhm2 $fwhm2 \
			   -angle $angle \
			   -major $semimajor \
			   -seeing $seeing \
			   -innerscale $innerscale \
			   -outerscale $outerscale
		     }
		  }
	       }
               set selected_ 0
            } else {
               lassign [$objects_($index) object_details] \
                  index x y mag magerr sky signal code \
                  positions innerscale outerscale semimajor
	       if { $coupled } {
		  for {set i $lowest_index_} {$i <= $highest_index_} {incr i} {
		     if { [info exists objects_($i)] && $i != $index} {
			$objects_($i) sync
			$objects_($i) configure \
			   -major $semimajor \
			   -innerscale $innerscale \
			   -outerscale $outerscale
		     }
		  }
	       }
            }
         }
      }

      #  Update displays of values.
      update_details_
      update_scrollbox_

      #  Send the changed notification, if needed.
      if { $notify_changed_cmd != {} } {
         eval $notify_changed_cmd
      }
      set modified 1
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Type of photometry objects we're dealing with.
   public variable phottype aperture {
      if { $phottype != "aperture" && $psf } {
         set lowest_index_ 0
      } else {
         set lowest_index_ 1
      }
   }
   public variable psf 0 {
      if { $phottype != "aperture" && $psf } {
         set lowest_index_ 0
      } else {
         set lowest_index_ 1
      }
   }

   #  Whether any objects are new or modified since a file was last
   #  written.
   public variable modified 0 {}

   #  Name of StarCanvasDraw object that controls the overlay graphics
   #  of the objects.
   public variable canvasdraw {} {}

   #  Name of canvas.
   public variable canvas {} {}

   #  Name of GaiaImage type object for converting aperture sizes
   #  into displayed canvas sizes.
   public variable rtdimage {} {}

   #  Show a list of objects in a listbox.
   public variable show_list 0 {
      if { $show_list } {
         if { ![winfo exists $scrollbox] } {
            gaia::Scrollbox $scrollbox
            pack $scrollbox -fill both -expand true

            #  Add bindings to change the currently selected object to
            #  the one in the list when double clicked.
            add_scrollbox_bindings_
         }
         update_scrollbox_
      } else {
         if { [winfo exists $scrollbox] } {
            pack forget $scrollbox
            delete object $scrollbox
         }
      }
   }

   #  Parent window for Scrollbox (if used).
   public variable scrollbox {.gaiaphotomlist} {}

   #  Display current object details in GaiaPhotomDetails object if required.
   public variable details {} {
      if { [winfo exists $details] } {
         set show_details_ 1
         update_details_
      } else {
         set show_details 0
      }
   }

   #  Shape of apertures.
   public variable shape circle {
      config_selected shape $shape
   }

   #  Default semimajor axis (radius) of apertures. Send notify
   #  command to track changes. Should also do this for other options,
   #  but don't as not used yet.
   public variable semimajor 5.0 {
      config_selected major $semimajor
      if { $notify_changed_cmd != {} } {
         eval $notify_changed_cmd
      }
   }

   #  Default inner scale of apertures.
   public variable innerscale 1.5 {
      config_selected innerscale $innerscale
   }

   #  Default outer scale of apertures.
   public variable outerscale 2.0 {
      config_selected outerscale $outerscale
   }

   #  Default boolean for annular or region sky methods.
   public variable annulus 1 {
      if { $annulus } {
         configure -positions annulus
      } else {
         configure -positions regions
      }
   }
   public variable positions annulus {
      config_selected positions $positions
   }

   #  Default line width of graphical objects.
   public variable linewidth 1 {
      config_selected linewidth $linewidth
   }

   #  Default eccentricity of apertures.
   public variable eccentricity 0.0 {
      config_selected eccen $eccentricity
   }

   #  Default position angle of apertures.
   public variable angle 0.0 {
      config_selected angle $angle
   }

   #  Default seeing.
   public variable seeing 2.0 {
      config_all seeing $seeing
   }

   #  Default fwhm1 and fwhm2.
   public variable fwhm1 0.0 {
      config_all fwhm1 $fwhm1
   }
   public variable fwhm2 0.0 {
      config_all fwhm2 $fwhm2
   }

   #  Whether objects may be created using resizing.
   public variable allow_resize {1} {}

   #  Whether aperture/objects properties are coupled.
   public variable coupled {0} {}

   #  Command to execute when a new object is created.
   public variable notify_created_cmd {} {}

   #  Command to execute when an object semimajor axis is modified.
   public variable notify_changed_cmd {} {}

   #  Whether calculations are in magnitudes or counts.
   public variable usemags 1 {}

   #  Control of the various colours (effects all apertures).
   public variable selected_colour white {
      config_all selected_colour $selected_colour
   }
   public variable deselected_colour green {
      config_all deselected_colour $deselected_colour
   }
   public variable selected_sky_colour yellow {
      config_all selected_sky_colour $selected_sky_colour
   }
   public variable deselected_sky_colour blue {
      config_all deselected_sky_colour $deselected_sky_colour
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Lowest index of objects, set to 0 for PSF, otherwise 1.
   protected variable lowest_index_ 1

   #  Highest index of objects. Used to limit for loops searching for
   #  existing objects (objects need to be sorted by index no.).
   protected variable highest_index_ 0

   #  Currently selected object.
   protected variable selected_ {}

   #  Array of object names (index by index number).
   protected variable objects_

   #  Array of canvas ids for the objects (index by id, value is object index).
   protected variable objects_ids_

   #  State of details tracking.
   protected variable show_details_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
