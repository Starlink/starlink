#+
#  Name:
#     GaiaPhotomObject

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class for dealing with photometry object details.

#  Description:
#     A photom object is a container for all the information about a
#     photometry aperture or object. This includes whether it has been
#     measured, if so what the measurements are, the positions of the
#     sky regions used etc.
#
#     If performing aperture photometry then methods for reading and
#     writing standard PHOTOM files as well as the extended format that
#     contains complete information (as output by the AUTOPHOTOM
#     program) are provided.
#
#     If working in optimal photometry mode then two record types are
#     allowed. These are controlled by a public variable -- psf -- and
#     suitable guestimation of the record contents are made. Note
#     -psf is only used if -phottype is set to optimal.

#  Invocations:
#
#        GaiaPhotomObject object_name [configuration options]
#
#     This creates an instance of a StarPhotom object. The return is
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
#     See public variable declarations.

#  Methods:
#     See desciptions with method statements.

#  Inheritance:
#     This class inherits no other classes.

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
#     9-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     29-APR-1998 (PWD):
#        Reworked <up>, <down>, <left> and <right> bindings to
#        preserve original bindings and remove when deselected.
#     10-DEC-1998 (PWD):
#        Changed to refresh detached sky apertures (these could be
#        incorrect if the zoom is changed).
#     27-MAY-1999 (PWD):
#        Added changes to allow optimal photometry objects (normal and
#        psf reference).
#     17-MAY-2000 (PWD):
#        Fixed problem with saving sky circles/ellipses. These could
#        occasionally reconfigure the main aperture using image,
#        rather than canvas coordinates (dependended on major axis
#        being smaller than minor).
#     {enter_further_changes_here}

#-
#.

itcl::class gaia::GaiaPhotomObject {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      #  Increment the number of photom objects.
      incr number_of_objects

      #  New object so no graphical presence yet.
      set state_ new

      # No annuli yet.
      set annulus_(1) {}
      set annulus_(2) {}

      #  Initialise configuration options.
      if { $args != {} } {
         eval configure $args
      }

      #  Strings to convert screen coordinates to canvas coordinates.
      set canvasX_ "\[$canvas canvasx %x\]"
      set canvasY_ "\[$canvas canvasy %y\]"
   }

   #  Destructor:
   #  -----------
   destructor {
      incr number_of_objects -1

      #  Delete the graphic item.
      if { $canvas_id_ != {} } {
         $canvasdraw remove_notify_cmd $canvas_id_
         $canvasdraw delete_object $canvas_id_

         #  Remove the associated sky regions.
         set canvas_id_ {}
         if { $positions == "annulus" } {
            delete_annular_regions
         } else {
            delete_sky_regions
         }
      }
      if { $notify_delete_cmd != {} } {
         eval $notify_delete_cmd
      }

      #  Undo any grip bindings.
      catch {
         $canvas bind grip <ButtonRelease-1> {}
      }

      #  Remove bindings to shift objects by increments & arrow keys.
      if { $old_left_ != {} } {
         bind $canvas <Left>  $old_left_
         bind $canvas <Right> $old_right_
         bind $canvas <Up>    $old_up_
         bind $canvas <Down>  $old_down_
      }
   }

   #  Methods:
   #  --------

   #  Set all the measurements of an object. We have three different
   #  cases, aperture, optimal and psf. Aperture details can be given
   #  in various ways. If 10 or 11 values are given then this is an
   #  old-style PHOTOM measurement. Note sky values are set separately
   #  by the setsky method.
   public method setvalues {args} {
      switch -exact $phottype {
         aperture {
            set argc [llength $args]
            if { $argc == 10 } {
               lassign $args \
                  index xpos ypos mag magerr sky signal major eccen angle
            } elseif { $argc == 11 } {
               lassign $args \
                  index xpos ypos mag magerr sky signal code major eccen angle
            } else {
               lassign $args \
                  index xpos ypos mag magerr sky signal code major eccen \
                  angle positions shape
            }
         }
         optimal {
            if { $psf } {
               lassign $args \
                  index xpos ypos fwhm1 fwhm2 angle code major seeing positions
               set index 0
            } else {
               lassign $args \
                  index xpos ypos mag magerr sky signal code positions
            }
         }
      }

      #  Pretend this object has been measured (for now).
      set measured_ 1

      #  Convert image positions into canvas positions for X and Y.
      lassign [canvas_coord $xpos $ypos] xpos ypos

      #  Convert major axis into a canvas scale.
      set major [canvas_dist $major]

      #  Modify minor and eccen if shape is circular, otherwise set
      #  minor axis to correct value.
      if { $shape == "circle" } {
         set minor $major
         set eccen 0.0
      } else {
         set minor [expr $major*sqrt(1.0-$eccen*$eccen)]
      }

      #  Convert the angle into a canvas one.
      set angle [canvas_angle $angle]

      #  Now redraw the object with these parameters.
      redraw_object
   }

   #  Set all values in one go after object is created (more efficient
   #  than repeated configures). The values set are:
   #
   #     index xpos ypos mag magerr sky signal code major eccen angle
   #     positions shape fwhm1 fwhm2 seeing
   #
   #  All fields must be supplied, just set unnecesary ones to {}.
   public method setallvalues {args} {
      lassign $args \
         index xpos ypos mag magerr sky signal code major eccen \
         angle positions shape fwhm1 fwhm2 seeing

      #  Pretend this object has been measured (for now).
      set measured_ 1

      #  Convert image positions into canvas positions for X and Y.
      lassign [canvas_coord $xpos $ypos] xpos ypos

      #  Convert major axis into a canvas scale.
      set major [canvas_dist $major]

      #  Modify minor and eccen if shape is circular, otherwise set
      #  minor axis to correct value.
      if { $shape == "circle" } {
         set minor $major
         set eccen 0.0
      } else {
         set minor [expr $major*sqrt(1.0-$eccen*$eccen)]
      }

      #  Convert the angle into a canvas one.
      set angle [canvas_angle $angle]

      #  Now redraw the object with these parameters.
      redraw_object
   }

   #  Set the sky properties of the object. There are two forms that
   #  this takes, either an annular region or many independent regions
   #  (which are set one after the other). The format of the list of
   #  arguments to this method dicates which form we're dealing
   #  with. If the type arguments is "ANN" then the next two elements
   #  indicate the inner and outer scales. If type is "SKY" then the
   #  following elements indicate the x & y position followed by the
   #  shape, semimajor axis and eccentricity and the position angle of
   #  the ellipse (see getvalues). Note this should only be used to
   #  create sky regions, not to update them, sky regions in general
   #  cannot be updated non-interactively.
   public method setsky {type args} {
      if { $type == "SKY" } {
         incr sky_regions
         lassign $args \
            x y sky_details_($sky_regions,shape) \
            maj ecc sky_details_($sky_regions,angle)

         #  Convert image positions into canvas positions for X and Y.
         lassign [canvas_coord $x $y] x y
         set sky_details_($sky_regions,x) $x
         set sky_details_($sky_regions,y) $y

         #  Convert angle into a canvas angle.
         set sky_details_($sky_regions,angle) \
            [canvas_angle $sky_details_($sky_regions,angle)]

         #  Convert major axis into a canvas scale.
         set sky_details_($sky_regions,major) [canvas_dist $maj]

         set sky_details_($sky_regions,minor) \
            [expr $sky_details_($sky_regions,major)*sqrt(1.0-$ecc*$ecc)]

         set interactive_ 0
         if { $shape == "circle" } {
            create_sky_circle $x $y
         } else {
            create_sky_ellipse $x $y
         }
         set newpositions regions
      } else {
         set positions annulus
         lassign $args innerscale outerscale
         set newpositions annulus
      }
      configure -positions $newpositions
   }

   #  Set details of a sky region that will be used when creating
   #  without allowing resizing.
   public method setskydefaults {major ecc angle shape} {
       set sky_defaults_(angle) [canvas_angle $angle]
       set sky_defaults_(major) $major
       set sky_defaults_(minor) [expr $major*sqrt(1.0-$ecc*$ecc)]
       set sky_defaults_(shape) $shape
   }

   #  Synchronise the configuration options to reflect the drawn
   #  objects. Calling this method is necessary to follow any changes
   #  in the image zoom, which is never seen.
   public method sync {} {
      update_eccen
      if { $shape == "circle" } {
         update_circle $canvas_id_ resize
      } else {
         update_ellipse $canvas_id_ resize
      }
   }

   #  Return the values of all options. If mode is "all" then for
   #  apertures the extended form is used, otherwise only basic PHOTOM
   #  values are returned. If object isn't measured then this will
   #  return the defaults.
   public method getvalues {{mode all}} {

      #  Make sure values are up to date as canvas zoom events are
      #  never received.
      sync
      if { $phottype == "aperture" } {
         return [getapvals_ $mode]
      } else {
         if { $psf } {
            return [getpsfvals_ $mode]
         } else {
            return [getoptvals_ $mode]
         }
      }
   }

   #  Return a list of the current aperture values. If mode is "all"
   #  then the full set is returned.
   protected method getapvals_ {mode} {
      lassign [image_coord $xpos $ypos] x y
      set maj [image_dist $major]
      set ang [image_angle $angle]
      set description \
         "[format {%10d %10f %10f %12g %12g %12g %16g %10s %10f %10f %10f} \
             $index $x $y $mag $magerr $sky $signal $code $maj \
             $eccen $ang]"
      if { $mode == "all" } {
         append description \
            "[format {%10s %10s} $positions $shape]"
         append description "\n[list_sky_]"
      }
      return "$description"
   }

   #  Return a list of current psf object values. Only index 0 is allowed.
   protected method getpsfvals_ {mode} {
      lassign [image_coord $xpos $ypos] x y
      set maj [image_dist $major]
      set ang [image_angle $angle]
      configure -index 0
      set description \
         "[format {%10d %10f %10f %10f %10f %10f %10s %10f %10f %10s} \
             $index $x $y $fwhm1 $fwhm2 $ang $code $maj $seeing $positions]"
      if { $mode == "all" } {
         append description "\n[list_sky_]"
      }
      return $description
   }

   #  Return a list of optimal photometry values.
   protected method getoptvals_ {mode} {
      lassign [image_coord $xpos $ypos] x y
      set description \
         "[format {%10d %10f %10f %12g %12g %12g %12g %10s %10s} \
             $index $x $y $mag $magerr $sky $signal $code $positions]"
      if { $mode == "all" } {
         append description "\n[list_sky_]"
      }
      return $description
   }

   #  Return a list of sky aperture properties.
   protected method list_sky_ {} {
      if { $positions == "annulus" } {
         set description "\#ANN    $index   $innerscale $outerscale"
      } else {
         #  Loop over all sky apertures. Note we update the values
         #  and correct for major/minor axes swapping
         set description "\#"
         for { set i 1 } { $i <= $sky_regions } { incr i } {
            if { $sky_details_($i,shape) == "circle" } {
               update_sky_circle $sky_details_($i,id) resize
            } else {
               update_sky_ellipse $sky_details_($i,id) resize
            }
            if { [info exists sky_details_($i,x)] } {
               lassign [image_coord \
                           $sky_details_($i,x) $sky_details_($i,y)] x y
               set maj $sky_details_($i,major)
               set min $sky_details_($i,minor)
               set ang [image_angle $sky_details_($i,angle)]
               if { $maj < $min } {
                  set temp $min
                  set min $maj
                  set maj $temp
                  set ang [expr fmod($ang+90.0,180.0)]
                  $canvas itemconfigure $sky_details_($i,id) \
                     -semimajor $maj -semiminor $min -angle $ang
               }
               set maj [image_dist $maj]
               set min [image_dist $min]
               set ecc [expr sqrt(1.0-(($min*$min)/($maj*$maj)))]
               append description \
                  "\n\#SKY$i [format "%10d %10f %10f %10s %10f %10f %10f" \
                                  $index $x $y $sky_details_($i,shape) \
                                  $maj $ecc $ang]"
            }
         }
      }
      return "$description"
   }

   #  Method to return values specific to the aperture (quick form of
   #  getapvals_ and friends).
   public method object_details {} {

      #  Make sure values are up to date as canvas zoom events are
      #  never received.
      sync
      if { $phottype == "aperture" } {
         return [getapdets_]
      } else {
         if { $psf } {
            return [getpsfdets_]
         } else {
            return [getoptdets_]
         }
      }
   }

   #  Return aperture details.
   protected method getapdets_ {} {
      lassign [image_coord $xpos $ypos] x y
      set maj [image_dist $major]
      set ang [image_angle $angle]
      update_eccen
      return "$index $x $y $mag $magerr $sky $signal $code $maj \
              $eccen $ang $positions $innerscale $outerscale"
   }

   #  Return optimal details. Note included extra major so that
   #  interactive resizing can be tracked.
   protected method getoptdets_ {} {
      lassign [image_coord $xpos $ypos] x y
      set maj [image_dist $major]
      return "$index $x $y $mag $magerr $sky $signal $code \
              $positions $innerscale $outerscale $maj"
   }

   #  Return PSF details.
   protected method getpsfdets_ {} {
      lassign [image_coord $xpos $ypos] x y
      set maj [image_dist $major]
      set ang [image_angle $angle]
      configure -index 0
      return "$index $x $y $fwhm1 $fwhm2 $angle $code $maj $seeing \
              $positions $innerscale $outerscale"
   }

   #  Status of object.
   public method status {} {
      if { $measured_ } {
         return {measured}
      } else {
         return {not_measured}
      }
   }

   #  Test if given index matches object index.
   public method testindex {match} {
      if { $match == $index } {
         return 1
      } else {
         return 0
      }
   }

   #  Distance between a test point and the object position.
   public method separation {testx testy} {
      set dx [expr $xpos-$testx]
      set dy [expr $ypos-$testy]
      set dist [expr sqrt($dx*$dx+$dy*$dy)]
      return [image_dist $dist]
   }

   #  Method to draw the object on the canvas, if not already drawn.
   public method draw_object {} {
      if { $state_ == "new" } {
         if { "$shape" == "circle" } {
            create_circle
         } else {
            create_ellipse
         }
         set state_ drawn
      }
   }

   #----------------------
   #  Circular apertures
   #----------------------

   #  Create an interactively resizable circle.
   public method create_and_resize_circle {cmd} {
      if { "$canvasdraw" != {} } {
         set create_cmd_ $cmd
         $canvasdraw set_drawing_mode circle [code $this created_circle]
      }
   }

   #  Create a circular aperture without resizing.
   public method create_no_resize_circle {cmd tmajor} {
      if { "$canvasdraw" != {} } {
         set create_cmd_ $cmd
         set default_binding_ [bind $canvas <1>]
         set default_cursor_ [$canvas cget -cursor]
         $canvas configure -cursor circle
         configure -major $tmajor
         configure -minor $tmajor
         configure -eccen 0.0

         #  Get the position of the circle and create it.
         bind $canvas <1> [code eval $this placed_circle_ $canvasX_ $canvasY_]
      }
   }

   #  Do the actual creation of a circular aperture at the given position.
   protected method placed_circle_ {x y} {

      #  Reset the bindings and cursor.
      bind $canvas <1> [code $default_binding_]
      $canvas configure -cursor $default_cursor_
      set xpos $x
      set ypos $y
      create_circle
   }

   #  Draw a circular aperture with the current values.
   public method create_circle {} {
      if { "$canvasdraw" != {} } {

         #  Need to store and later restore current configure as this is
         #  overriden by update called during these commands.
         set x $xpos; set y $ypos
         set ma $major; set mi $minor; set a $angle

         $canvasdraw set_drawing_mode circle [code $this created_circle]
         $canvasdraw create_object $xpos $ypos
         $canvasdraw create_done $xpos $ypos

         set xpos $x; set ypos $y
         set major $ma; set minor $mi; set angle $a

         $canvas coords $canvas_id_ $xpos $ypos
         $canvas itemconfigure $canvas_id_ \
            -semimajor $major -semiminor $minor -angle $angle
         update_circle $canvas_id_ move

         #  Update display and deselect object (immediate resize isn't
         #  desirable).
         update idletasks
         $canvasdraw deselect_object $canvas_id_
      }
   }

   #  Finally created the new aperture, record details and add call
   #  backs for interactive updates etc. Execute the requested
   #  creation command so that the user knows that the aperture is
   #  created.
   public method created_circle {id args} {
      set state_ drawn
      set canvas_id_ $id
      update_circle $id create
      add_bindings_ $id
      notify_change
      if { $create_cmd_ != {} } {
         eval $create_cmd_ $id
         set create_cmd_ {}
      }
   }

   #  Update apertures on creation, movement, resize or delete.
   public method update_circle { id mode } {
      switch -exact $mode {
         create {
            lassign [$canvas coords $id] xpos ypos
            set major [$canvas itemcget $id -semimajor]
            set minor [$canvas itemcget $id -semiminor]
            set angle [$canvas itemcget $id -angle]
            if { $positions == "annulus" } {
               set annulus_(1) [$canvas create rtd_ellipse \
                                   $xpos $ypos \
                                   -semimajor [expr $major*$innerscale] \
                                   -semiminor [expr $major*$innerscale] \
                                   -angle $angle \
                                   -width $linewidth \
                                   -outline $selected_sky_colour]
               set annulus_(2) [$canvas create rtd_ellipse \
                                   $xpos $ypos \
                                   -semimajor [expr $major*$outerscale] \
                                   -semiminor [expr $major*$outerscale] \
                                   -angle $angle \
                                   -width $linewidth \
                                   -outline $selected_sky_colour]
            }

            #  Now receive movement and resize updates.
            $canvasdraw add_notify_cmd $id [code $this update_circle $id] 1
         }
         move -
         resize {
            lassign [$canvas coords $id] xpos ypos
            set major [$canvas itemcget $id -semimajor]
            set minor [$canvas itemcget $id -semiminor]
            set angle [$canvas itemcget $id -angle]
            if { $positions == "annulus" } {
               $canvas coords $annulus_(1) $xpos $ypos
               $canvas itemconfigure $annulus_(1) \
                  -semimajor [expr $major*$innerscale] \
                  -semiminor [expr $major*$innerscale] -angle $angle
               $canvas coords $annulus_(2) $xpos $ypos
               $canvas itemconfigure $annulus_(2) \
                  -semimajor [expr $major*$outerscale] \
                  -semiminor [expr $major*$outerscale] -angle $angle
            }
         }
         delete {
            delete object $this
         }
      }
   }

   #------------------------
   #  Circular sky apertures
   #------------------------

   #  Create an interactively resizable circular sky region.
   public method create_and_resize_sky_circle {} {
      if { "$canvasdraw" != {} } {
         $canvasdraw set_drawing_mode circle [code $this created_sky_circle]
      }
   }

   #  Create a circular sky region without resizing.
   public method create_no_resize_sky_circle {} {
       if { "$canvasdraw" != {} } {
	   set default_binding_ [bind $canvas <1>]
	   set default_cursor_ [$canvas cget -cursor]
	   $canvas configure -cursor circle

	   #  Get the position of the circle and create it.
	   set interactive_ 0
	   bind $canvas <1> [code eval $this placed_sky_circle_ $canvasX_ $canvasY_]
       }
   }

   #  Respond to request to create a circular aperture at the given position.
   protected method placed_sky_circle_ {x y} {

       #  Reset the bindings and cursor.
       bind $canvas <1> [code $default_binding_]
       $canvas configure -cursor $default_cursor_
       incr sky_regions

       #  Set the apertiure detail defaults.
       set sky_details_($sky_regions,x) $x
       set sky_details_($sky_regions,y) $y
       if { [info exists sky_defaults_] } {
	   set sky_details_($sky_regions,major) \
		   [canvas_dist $sky_defaults_(major)]
	   set sky_details_($sky_regions,minor) \
		   [canvas_dist $sky_defaults_(minor)]
	   set sky_details_($sky_regions,angle) $sky_defaults_(angle)
	   set sky_details_($sky_regions,shape) $sky_defaults_(shape)
       } else {
	   set sky_details_($sky_regions,major) $major
	   set sky_details_($sky_regions,minor) $minor
	   set sky_details_($sky_regions,angle) $angle
	   set sky_details_($sky_regions,angle) $shape
       }
       create_sky_circle $x $y
   }

   #  Draw a circular sky region at the given position.
   public method create_sky_circle {x y} {
      if { "$canvasdraw" != {} } {
         $canvasdraw set_drawing_mode circle [code $this created_sky_circle]
         $canvasdraw create_object $x $y
         $canvasdraw create_done $x $y
      }
   }

   #  Finally created the new aperture, record details (if not already
   #  done, otherwise set them) and add call backs for interactive
   #  updates etc.
   public method created_sky_circle {id args} {
       if { $interactive_ } {
	   incr sky_regions
	   lassign [$canvas coords $id] x y
	   set maj [$canvas itemcget $id -semimajor]
	   set min [$canvas itemcget $id -semiminor]
	   set ang [$canvas itemcget $id -angle]
	   set sky_details_($sky_regions,x) $x
	   set sky_details_($sky_regions,y) $y
	   set sky_details_($sky_regions,shape) circle
	   set sky_details_($sky_regions,major) $maj
	   set sky_details_($sky_regions,minor) $min
	   set sky_details_($sky_regions,angle) $ang
       } else {
	   #  Non-interactive create, assume sky_details are set.
	   $canvas coords $id \
		   $sky_details_($sky_regions,x) \
		   $sky_details_($sky_regions,y)
	   $canvas itemconfigure $id \
		   -semimajor $sky_details_($sky_regions,major) \
		   -semiminor $sky_details_($sky_regions,minor) \
		   -angle $sky_details_($sky_regions,angle)
	   set interactive_ 1
       }
       set sky_details_($id,index) $sky_regions
       set sky_details_($sky_regions,id) $id
       $canvasdraw add_notify_cmd $id [code $this update_sky_circle $id] 0
       add_bindings_ $id

       #  Inform sub-classes that a new aperture has been created.
       if { $create_cmd_ != {} } {
	   eval $create_cmd_ $id
	   set create_cmd_ {}
       }
   }

   #  Update sky aperture on movement, resize or delete.
   public method update_sky_circle { id mode } {
      set sky_id $sky_details_($id,index)
      switch -exact $mode {
         move -
         resize {
            lassign [$canvas coords $id] x y
            set maj [$canvas itemcget $id -semimajor]
            set min [$canvas itemcget $id -semiminor]
            set ang [$canvas itemcget $id -angle]
            set sky_details_($sky_id,x) $x
            set sky_details_($sky_id,y) $y
            set sky_details_($sky_id,major) $maj
            set sky_details_($sky_id,minor) $min
            set sky_details_($sky_id,angle) $ang
         }
         delete {
            delete_sky_regions
         }
      }
   }

   #----------------------
   #  Elliptical apertures
   #----------------------

   #  Create an elliptical aperture. Args are an set of values for
   #  the centre of the ellipse.
   public method create_ellipse {} {
      if { "$canvasdraw" != {} } {

         #  Need to store and later restore current configure as this is
         #  overriden by update called during these commands.
         set x $xpos; set y $ypos
         set ma $major; set mi $minor; set a $angle

         $canvasdraw set_drawing_mode ellipse [code $this created_ellipse]
         $canvasdraw create_object $xpos $ypos
         $canvasdraw create_done $xpos $ypos

         set xpos $x; set ypos $y
         set major $ma; set minor $mi; set angle $a

         $canvas coords $canvas_id_ $xpos $ypos
         $canvas itemconfigure $canvas_id_ \
            -semimajor $major -semiminor $minor -angle $angle
         update_ellipse $canvas_id_ move

         #  Update display and deselect object (immediate resize isn't
         #  desirable).
         update idletasks
         $canvasdraw deselect_object $canvas_id_
      }
   }

   #  Create an elliptical aperture without resizing.
   public method create_no_resize_ellipse {cmd tmajor teccen tangle} {
      if { "$canvasdraw" != {} } {
         set create_cmd_ $cmd
         set default_binding_ [bind $canvas <1>]
         set default_cursor_ [$canvas cget -cursor]
         $canvas configure -cursor circle
         configure -major $tmajor
         configure -eccen $teccen
         configure -angle $tangle

         #  Get the position of the ellipse and create it.
         bind $canvas <1> [code eval $this placed_ellipse_ $canvasX_ $canvasY_]
      }
   }

   #  Do the actual creation of an ellptical aperture at the given position.
   protected method placed_ellipse_ {x y} {

      #  Reset the bindings and cursor.
      bind $canvas <1> [code $default_binding_]
      $canvas configure -cursor $default_cursor_
      set xpos $x
      set ypos $y
      create_ellipse
   }

   #  Create an interactive elliptical aperture.
   public method create_and_resize_ellipse {cmd} {
      if { "$canvasdraw" != {} } {
         set create_cmd_ $cmd
         $canvasdraw set_drawing_mode ellipse [code $this created_ellipse]
      }
   }

   #  Finally created new elliptical aperture.
   public method created_ellipse { id args } {
      set state_ drawn
      set canvas_id_ $id
      update_ellipse $id create
      add_bindings_ $id
      notify_change
      if { $create_cmd_ != {} } {
         eval $create_cmd_ $id
         set create_cmd_ {}
      }
   }

   #  Update elliptical object.
   public method update_ellipse { id mode } {
      switch -exact $mode {
         create {
            lassign [$canvas coords $id] xpos ypos
            set major [$canvas itemcget $id -semimajor]
            set minor [$canvas itemcget $id -semiminor]
            set angle [$canvas itemcget $id -angle]
            if { $positions == "annulus" } {
               set annulus_(1) [$canvas create rtd_ellipse \
                                   $xpos $ypos \
                                   -semimajor [expr $major*$innerscale] \
                                   -semiminor [expr $minor*$innerscale] \
                                   -angle $angle \
                                   -width $linewidth \
                                   -outline $selected_sky_colour]
               set annulus_(2) [$canvas create rtd_ellipse \
                                   $xpos $ypos \
                                   -semimajor [expr $major*$outerscale] \
                                   -semiminor [expr $minor*$outerscale] \
                                   -angle $angle \
                                   -width $linewidth \
                                   -outline $selected_sky_colour]
            }

            #  Now receive movement and resize updates.
            $canvasdraw add_notify_cmd $id [code $this update_ellipse $id] 1
         }
         move -
         resize {
            lassign [$canvas coords $id] xpos ypos
            set major [$canvas itemcget $id -semimajor]
            set minor [$canvas itemcget $id -semiminor]
            set angle [$canvas itemcget $id -angle]
            if { $positions == "annulus" } {
               $canvas coords $annulus_(1) $xpos $ypos
               $canvas itemconfigure $annulus_(1) \
                  -semimajor [expr $major*$innerscale] \
                  -semiminor [expr $minor*$innerscale] -angle $angle
               $canvas coords $annulus_(2) $xpos $ypos
               $canvas itemconfigure $annulus_(2) \
                  -semimajor [expr $major*$outerscale] \
                  -semiminor [expr $minor*$outerscale] -angle $angle
            }
         }
         delete {
            delete object $this
         }
      }
   }

   #--------------------------
   #  Elliptical sky apertures
   #--------------------------

   #  Create an interactively resizable elliptical sky region.
   public method create_and_resize_sky_ellipse {} {
      if { "$canvasdraw" != {} } {
         $canvasdraw set_drawing_mode ellipse [code $this created_sky_ellipse]
      }
   }

   #  Create an elliptical sky region without resizing.
   public method create_no_resize_sky_ellipse {} {
      if { "$canvasdraw" != {} } {
         set default_binding_ [bind $canvas <1>]
         set default_cursor_ [$canvas cget -cursor]
         $canvas configure -cursor circle

         #  Get the position of the ellipse and create it.
         bind $canvas <1> [code eval $this placed_sky_ellipse_ $canvasX_ $canvasY_]
      }
   }

   #  Respond to request to create an elliptical aperture at the given
   #  position.
   protected method placed_sky_ellipse_ {x y} {

      #  Reset the bindings and cursor.
      bind $canvas <1> [code $default_binding_]
      $canvas configure -cursor $default_cursor_
      create_sky_ellipse $x $y
   }

   #  Draw an elliptical sky region at the given position.
   public method create_sky_ellipse {x y} {
      if { "$canvasdraw" != {} } {
         $canvasdraw set_drawing_mode ellipse [code $this created_sky_ellipse]
         $canvasdraw create_object $x $y
         $canvasdraw create_done $x $y
      }
   }

   #  Finally created the new aperture, record details and add call
   #  backs for interactive updates etc.
   public method created_sky_ellipse {id args} {
      if { $interactive_ } {
         incr sky_regions
         lassign [$canvas coords $id] x y
         set maj [$canvas itemcget $id -semimajor]
         set min [$canvas itemcget $id -semiminor]
         set ang [$canvas itemcget $id -angle]
         set sky_details_($sky_regions,x) $x
         set sky_details_($sky_regions,y) $y
         set sky_details_($sky_regions,shape) ellipse
         set sky_details_($sky_regions,major) $maj
         set sky_details_($sky_regions,minor) $min
         set sky_details_($sky_regions,angle) $ang
      } else {
	  #  Non-interactive create, assume sky_details are set .
	  $canvas coords $id \
		  $sky_details_($sky_regions,x) \
		  $sky_details_($sky_regions,y)
	  $canvas itemconfigure $id \
		  -semimajor $sky_details_($sky_regions,major) \
		  -semiminor $sky_details_($sky_regions,minor) \
		  -angle $sky_details_($sky_regions,angle)
      }
      set sky_details_($id,index) $sky_regions
      set sky_details_($sky_regions,id) $id
      $canvasdraw add_notify_cmd $id [code $this update_sky_ellipse $id] 0
      add_bindings_ $id

      #  Inform sub-classes that a new aperture has been created.
      if { $create_cmd_ != {} } {
         eval $create_cmd_ $id
         set create_cmd_ {}
      }
   }

   #  Update sky aperture on movement, resize or delete.
   public method update_sky_ellipse { id mode } {
      set sky_id $sky_details_($id,index)
      switch -exact $mode {
         move -
         resize {
            lassign [$canvas coords $id] x y
            set maj [$canvas itemcget $id -semimajor]
            set min [$canvas itemcget $id -semiminor]
            set ang [$canvas itemcget $id -angle]
            set sky_details_($sky_id,x) $x
            set sky_details_($sky_id,y) $y
            set sky_details_($sky_id,major) $maj
            set sky_details_($sky_id,minor) $min
            set sky_details_($sky_id,angle) $ang
         }
         delete {
            delete_sky_regions
         }
      }
   }

   #  Return the canvas id of the object.
   public method canvas_id {} {
      return $canvas_id_
   }

   #  Add the bindings to this object that are appropriate to the
   #  current settings. Also sets up the call backs for when object is
   #  selected or deselected by canvasdraw.
   protected method add_bindings_ {id} {
      $canvas bind $id <1> \
	 "+focus $canvas;\
          $canvas bind grip <ButtonRelease-1> \"[code $this notify_change]\""
      $canvas bind $id <ButtonRelease-1> "+[code $this notify_change]"
      $canvasdraw add_selected_notify $id "[code $this update_selection]"
   }

   #  Shift the current position.
   public method shift_x {amount} {

      #  Make sure values are up to date as canvas zoom events are
      #  never received.
      if { $shape == "circle" } {
         update_circle $canvas_id_ resize
      } else {
         update_ellipse $canvas_id_ resize
      }
      set xpos [expr $xpos+$amount]
      redraw_object
      notify_change
   }
   public method shift_y {amount} {

      #  Make sure values are up to date as canvas zoom events are
      #  never received.
      if { $shape == "circle" } {
         update_circle $canvas_id_ resize
      } else {
         update_ellipse $canvas_id_ resize
      }
      set ypos [expr $ypos+$amount]
      redraw_object
      notify_change
   }


   #  Notify that object may have changed.
   public method notify_change {} {
      if { $notify_change_cmd != {} } {
         eval $notify_change_cmd
      }
   }

   #  Object selection state is changed.
   public method update_selection {state} {
      if { $state == "selected" } {
         selected
      } else {
         deselected
      }
   }

   #  Object is selected so colour all associated elements.
   public method selected {} {
      $canvas itemconfigure $canvas_id_ -outline $selected_colour
      if { $positions == "annulus" } {
	 $canvas itemconfigure $annulus_(1) -outline $selected_sky_colour
	 $canvas itemconfigure $annulus_(2) -outline $selected_sky_colour
      } else {
	 if { $sky_regions > 0 } {
	    for { set i 1 } { $i <= $sky_regions } { incr i } {
	       if { [info exists sky_details_($i,id)] } {
		  $canvas itemconfigure $sky_details_($i,id) \
                     -outline $selected_sky_colour
               }
            }
         }
      }

      #  Bindings to shift objects by increments & arrow keys.
      set old_left_  [bind $canvas <Left>]
      set old_right_ [bind $canvas <Right>]
      set old_up_    [bind $canvas <Up>]
      set old_down_  [bind $canvas <Down>]
      bind $canvas <Left>  +[code $this shift_x -1]
      bind $canvas <Right> +[code $this shift_x +1]
      bind $canvas <Up>    +[code $this shift_y -1]
      bind $canvas <Down>  +[code $this shift_y +1]
   }

   #  Object is deselected so colour all associated elements.
   public method deselected {} {
      $canvas itemconfigure $canvas_id_ -outline $deselected_colour
      if { $positions == "annulus" } {
	 $canvas itemconfigure $annulus_(1) -outline $deselected_sky_colour
	 $canvas itemconfigure $annulus_(2) -outline $deselected_sky_colour
      } else {
	 if { $sky_regions > 0 } {
	    for { set i 1 } { $i <= $sky_regions } { incr i } {
	       if { [info exists sky_details_($i,id)] } {
		  $canvas itemconfigure $sky_details_($i,id) \
                     -outline $deselected_sky_colour
               }
            }
         }
      }

      #  Remove bindings to shift objects by increments & arrow keys.
      if { $old_left_ != {} } {
         bind $canvas <Left>  $old_left_
         bind $canvas <Right> $old_right_
         bind $canvas <Up>    $old_up_
         bind $canvas <Down>  $old_down_
      }
   }

   #  Redraw the canvas item using the new public configs.
   public method redraw_object {} {
      if { $canvasdraw != {} && $canvas != {}} {
         $canvas coords $canvas_id_ $xpos $ypos
         $canvas itemconfigure $canvas_id_ \
            -semimajor $major -semiminor $minor -angle $angle
         if { $shape == "circle" } {
            update_circle $canvas_id_ resize
            $canvasdraw end_resize_circle $canvas_id_
         } else {
            update_ellipse $canvas_id_ resize
            $canvasdraw end_resize_ellipse $canvas_id_
         }
      }
   }

   #  Delete annular regions.
   public method delete_annular_regions {} {
      if { $annulus_(1) != {} } {
         $canvas delete $annulus_(1)
         set annulus_(1) {}
      }
      if { $annulus_(2) != {} } {
         $canvas delete $annulus_(2)
         set annulus_(1) {}
      }
   }

   #  Delete sky regions.
   public method delete_sky_regions {} {
      if { $sky_regions > 0 } {
         for { set i 1 } { $i <= $sky_regions } { incr i } {
            if { [info exists sky_details_($i,x)] } {
               $canvasdraw remove_selected_notify $sky_details_($i,id)
               $canvasdraw remove_notify_cmd $sky_details_($i,id)
               $canvasdraw delete_object $sky_details_($i,id)
               unset sky_details_($sky_details_($i,id),index)
               unset sky_details_($i,x)
               unset sky_details_($i,y)
               unset sky_details_($i,shape)
               unset sky_details_($i,major)
               unset sky_details_($i,minor)
               unset sky_details_($i,angle)
               unset sky_details_($i,id)
            }
         }
         set sky_regions 0
      }
   }

   #  Update the eccentricity, major and minor axes.
   public method update_eccen {} {
      if { $major < $minor } {
         set temp $minor
         set minor $major
         set major $temp
         set angle [expr fmod($angle+90.0,180.0)]
         $canvas itemconfigure $canvas_id_ \
            -semimajor $major -semiminor $minor -angle $angle
      }
      set eccen [expr sqrt(1.0-($minor*$minor)/($major*$major))]
   }

   #  Convert from canvas coordinates to image coordinates.
   public method image_coord { x y } {
      if { $rtdimage != {} } {
         $rtdimage convert coords $x $y canvas x y image
	 $rtdimage origin xo yo
         set x [expr $x-1.5+$xo]
         set y [expr $y-1.5+$yo]
      }
      return "$x $y"
   }

   #  Convert an angle measured on the canvas (+X through +Y) into
   #  an angle in image coords (Y flipped). Normally this method is a
   #  null function as these translate correctly without interaction
   #  (PHOTOM angles are position angles), but some care needs to be
   #  taken if the image is rotated and/or flipped.
   public method image_angle { angle } {
      set rad [expr $angle*0.017453292519943295]
      set dx [expr sin($rad)+$xpos]
      set dy [expr cos($rad)+$ypos]
      $rtdimage convert coords $dx $dy canvas ndx ndy image
      $rtdimage convert coords $xpos $ypos canvas nxcen nycen image
      set ndx [expr $ndx-$nxcen]
      set ndy [expr $ndy-$nycen]
      set angle [expr fmod(atan2($ndy,$ndx)*57.295779513082323+90.0,180.0)]
      return $angle
   }

   #  Convert a canvas distance to an image distance (along X axis,
   #  which may be swapped on output).
   public method image_dist { dist } {
      if { $rtdimage != {} } {
         $rtdimage convert dist $dist 0 canvas dist1 dist2 image
         set dist [expr $dist1+$dist2]
      }
      return "$dist"
   }

   #  Convert from image coordinates to canvas coordinates.
   public method canvas_coord { x y } {
      if { $rtdimage != {} } {
	 $rtdimage origin xo yo
         set x [expr $x+1.5-$xo]
         set y [expr $y+1.5-$yo]
         $rtdimage convert coords $x $y image x y canvas
      }
      return "$x $y"
   }

   #  Convert an angle measured on the image (+X through +Y) into
   #  an angle in canvas coords (Y flipped).
   public method canvas_angle { angle } {
      set rad [expr $angle*0.017453292519943295]
      $rtdimage convert coords $xpos $ypos canvas nxcen nycen image
      set dx [expr sin($rad)+$nxcen]
      set dy [expr cos($rad)+$nycen]
      $rtdimage convert coords $dx $dy image ndx ndy canvas
      set ndx [expr $ndx-$xpos]
      set ndy [expr $ndy-$ypos]
      set angle [expr fmod(atan2($ndy,$ndx)*57.295779513082323+90.0,180.0)]
      return $angle
   }

   #  Convert an image distance to a canvas distance (along X axis
   #  which may be swapped on output).
   public method canvas_dist { dist } {
      if { $rtdimage != {} } {
         $rtdimage convert dist $dist 0 image dist1 dist2 canvas
         set dist [expr $dist1+$dist2]
      }
      return "$dist"
   }

   #  Procedures: (access common values)
   #  -----------

   #  Return total number of GaiaPhotomObjects that exist.
   proc total_objects {} {
      return $number_of_objects
   }

   #  Return a header caption for formatted output lists.
   proc header {mode {phottype aperture} {psf 0} {usemags 1}} {
      if { $phottype == "aperture" } {
         if { "$mode" == "all" } {
            if { $usemags } {
               return [format {%10s %10s %10s %12s %12s %12s %16s %10s \
                                  %10s %10s %10s %10s %10s} \
                          Index Xpos Ypos Mag Magerr Sky Signal Code \
                          Major Eccen Angle Positions Shape]
            } else {
               return [format {%10s %10s %10s %12s %12s %12s %16s %10s \
                                  %10s %10s %10s %10s %10s} \
                          Index Xpos Ypos Count Counterr Sky Signal Code \
                          Major Eccen Angle Positions Shape]
            }
         } else {
            if { $usemags } {
               return [format {%10s %10s %10s %12s %12s %12s %16s %10s \
                                  %10s %10s %10s} \
                          Index Xpos Ypos Mag Magerr Sky Signal Code \
                          Major Eccen Angle]
            } else {
               return [format {%10s %10s %10s %12s %12s %12s %16s %10s \
                                  %10s %10s %10s} \
                          Index Xpos Ypos Count Counterr Sky Signal Code \
                          Major Eccen Angle]
            }
         }
      } else {
         if { $psf } {
            return [format {%10s %10s %10s %10s %10s %10s %10s %10s %10s %10s} \
                       Index Xpos Ypos Fwhm1 Fwhm2 Rot Code Clip Seeing Positions]
         } else {
            if { $usemags } {
               return [format {%10s %10s %10s %12s %12s %12s %12s %10s %10s} \
                          Index Xpos Ypos Mag Magerr Sky Signal Code Positions]
            } else {
               return [format {%10s %10s %10s %12s %12s %12s %12s %10s %10s} \
                          Index Xpos Ypos Count Counterr Sky Signal Code Positions]
            }
         }
      }
   }

   #  Set the value of a common variable.
   proc set_common {variable value} {
      if {[info exists $variable]} {
         set $variable $value
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Type of photometry objects -- "aperture" or "optimal". Plus if
   #  optimal is it a psf object. Define these once when object is created.
   public variable phottype aperture {}
   public variable psf 0 {}

   #  Name of StarCanvasDraw object that controls the overlay graphics
   #  of the object.
   public variable canvasdraw {} {}

   #  Name of canvas.
   public variable canvas {} {}

   #  Name of GaiaImageCtrl type object for converting aperture size
   #  into displayed canvas size and coordinates to image.
   public variable rtdimage {} {}

   #  Object info.
   public variable index {0} {}
   public variable xpos {0} {
      lassign [canvas_coord $xpos $ypos] xpos ypos
      if { $state_ != "new" } {
         redraw_object
      }
   }
   public variable ypos {0} {
      lassign [canvas_coord $xpos $ypos] xpos ypos
      if { $state_ != "new" } {
         redraw_object
      }
   }
   public variable mag {0} {}
   public variable magerr {0} {}
   public variable sky {0} {}
   public variable signal {0} {}
   public variable code {?} {}
   public variable major {0} {
      set major [canvas_dist $major]
      if { $state_ != "new" } {
         set minor [expr $major*sqrt(1.0-$eccen*$eccen)]
         redraw_object
      }
   }
   public variable minor {0} {
      set minor [canvas_dist $minor]
      if { $state_ != "new" } {
         update_eccen
         redraw_object
      }
   }
   public variable eccen {0} {
      set minor [expr $major*sqrt(1.0-$eccen*$eccen)]
      if { $state_ != "new" } {
         redraw_object
      }
   }
   public variable angle {0} {
      if { $state_ != "new" } {
         redraw_object
      }
   }

   #  Define how the objects sky regions are determined. On swap
   #  then delete existing sky regions and redraw the annular parts as
   #  required.
   public variable positions {annulus} {
      if { $state_ != "new" } {
         if { $positions != $last_positions_ } {
            if { $last_positions_ == "annulus" } {
               delete_annular_regions
            } else {
               delete_sky_regions
            }
            if { $positions == "annulus" } {
               if { $shape == "circle" } {
                  update_circle $canvas_id_ create
               } else {
                  update_ellipse $canvas_id_ create
               }
            }
         }
        redraw_object
      }
      set last_positions_ $positions
   }
   public variable shape {circle} {
      if { $state_ != "new" } {
         # It's not possible to change the aperture shape after creation.
         set shape $first_shape_
         notify_change
      } else {
         set first_shape_ $shape
      }
      if { $shape == "circle" } {
         configure -angle 0.0
         configure -eccen 0.0
         configure -minor $major
      }
   }

   public variable innerscale {1.5} {
      if { $state_ != "new" } {
         redraw_object
      }
   }
   public variable outerscale {2.0} {
     if { $state_ != "new" } {
         redraw_object
      }
   }
   public variable linewidth {1} {}

   #  A PSF object defines these extra fields.
   public variable fwhm1 2 {}
   public variable fwhm2 2 {}
   public variable seeing 2 {}

   #  Command to execute if object is possibly updated.
   #  canvas.
   public variable notify_change_cmd {} {}

   #  Command to execute if object is deleted.
   public variable notify_delete_cmd {} {}

   #  Set colour of parts of object.
   public variable selected_colour white
   public variable deselected_colour green
   public variable selected_sky_colour yellow
   public variable deselected_sky_colour blue

   #  Protected variables: (available to instance)
   #  --------------------

   #  Object state, new until drawn.
   protected variable state_ new

   #  Status of object.
   protected variable measured_ 0

   #  Last value of the positions variable (default should be same).
   protected variable last_positions_ annulus

   #  Canvas id's of annular items.
   protected variable annulus_

   #  Id of object aperture.
   protected variable canvas_id_ {}

   #  Command to execute after interactive object is completed.
   protected variable create_cmd_ {}

   #  Binding of <1> on canvas before placing a new object.
   protected variable default_binding_ {}

   #  Canvas cursor used before placing a new object.
   protected variable default_cursor_ {}

   #  Strings used to convert screen coordinates (from bindings) to
   #  canvas coordinates
   protected variable canvasX_ {}
   protected variable canvasY_ {}

   #  Sky region details.
   protected variable sky_details_

   #  Defaults for sky regions created non-interactively
   protected variable sky_defaults_

   #  Shape of object on first config.
   protected variable first_shape_

   #  Whether creation of region is interactive.
   protected variable interactive_ 1

   #  Original bindings.
   protected variable old_left_ {}
   protected variable old_right_ {}
   protected variable old_up_ {}
   protected variable old_down_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Number of Photom objects created.
   common number_of_objects 0

   #  Private variables: only visible here.
   #  ==================
   private variable sky_regions {0}

#  End of class definition.
}
