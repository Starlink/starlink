#+
#  Name:
#     GaiaCubeChanmap

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Controls for the creation of a chanmap from a cube.

#  Description:
#     This class creates a panel of controls for creating a chanmap
#     image from a sequence of planes of a cube.

#  Invocations:
#
#        GaiaCubeChanmap object_name [configuration options]
#
#     This creates an instance of a GaiaCubeChanmap object. The return is
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
#     See itk_option definitions below.

#  Methods:
#     See individual method declarations below.

#  Inheritance:
#     gaia::GaiaCubeApps

#  Copyright:
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
#     31-MAY-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCubeChanmap {}

itcl::class gaia::GaiaCubeChanmap {

   #  Inheritances:
   #  -------------
   inherit gaia::GaiaCubeApps

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      eval gaia::GaiaCubeApps::constructor $args -ref_colour "magenta"
   } {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      close
   }

   #  Methods:
   #  --------

   #  Take any actions necessary when the cube window is closed.
   public method close {} {
      catch {
         $canvas_ delete "channel_map"
      }
   }

   protected method add_controls_ {} {

      #  Use default controls (range and combination method).
      gaia::GaiaCubeApps::add_controls_

      #  The number of channels to create.
      itk_component add nchan {
         util::LabelEntryScale $w_.nchan \
            -text "Number of channels:" \
            -value 4 \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from 2 \
            -to 100 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_nchan_]
      }
      pack $itk_component(nchan) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(nchan) \
         {Number of channels to appear in the channel map}

      #  The number of channels to create.
      itk_component add shape {
         util::LabelEntryScale $w_.shape \
            -text "X-axis channels:" \
            -value 2 \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from 1 \
            -to 20 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_shape_]
      }
      pack $itk_component(shape) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(shape) \
         {Number of channels along the X-axis of the output image}

      #  Value of the selected channel panel.
      itk_component add selected {
         util::LabelValue $w_.selected \
            -text "Selected coord:" \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth)
      }
      pack $itk_component(selected) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(selected) \
         {Coordinate of selected image panel, click to update}
   }

   #  Run the CHANMAP application.
   protected method run_main_app_ {ndfname axis lb ub} {

      #  There are some constraints on the estimator and the number of
      #  channels per sub-image. Test for that now (note chanmap creates
      #  an output image even when exiting in error for the estimator,
      #  so just letting it fail isn't an option).
      set lbp [expr min($itk_option(-lower_limit),$itk_option(-upper_limit))]
      set ubp [expr max($itk_option(-lower_limit),$itk_option(-upper_limit))]
      set ael [expr $ubp-$lbp+1]
      set pixchan [expr int($ael)/int($itk_option(-nchan))]
      if { $pixchan <= 3 } {
         if { $pixchan == 1 } {
            set types "Mean,Max,Min,Comax,Comin,Sum,Iwc,Integ"
         } else {
            set types "Mean,WMean,Max,Min,Comax,Comin,Absdev,Sum,Iwc,Integ"
         }
         if { [string first $combination_type_ $types] == -1 } {
            warning_dialog \
               "Too few channels for $combination_type_ estimator" $w_
            blt::busy release $w_
            return
         }
      }

      #  Start up the CHANMAP application, if not done.
      if { $maintask_ == {} } {
         global env
         set maintask_ [gaia::GaiaApp \#auto -application \
                            $env(KAPPA_DIR)/chanmap \
                            -notify [code $this app_completed_]]
      }

      #  Create a temporary file name.
      incr count_
      set tmpimage_ [gaia::GaiaTempName::make_name \
                        "GaiaTempChanmap" $count_ ".sdf"]
      $maintask_ runwiths "in=$ndfname out=$tmpimage_ axis=$axis \
                          low=\"$lb\" high=\"$ub\" \
                          estimator=$combination_type_ \
                          nchan=$itk_option(-nchan) \
                          shape=$itk_option(-shape) accept"

      #  Tell cube to use these limits for spectral extraction.
      $itk_option(-gaiacube) set_extraction_range \
         $itk_option(-lower_limit) $itk_option(-upper_limit)
   }

   #  Display the chanmap image.
   protected method app_do_present_ {} {
      set file {}
      if { ! [file readable $tmpimage_] } {
         if { ! [file readable ${tmpimage_}.sdf] } {
            blt::busy release $w_
            return
         }
         set file ${tmpimage_}.sdf
      } else {
         set file $tmpimage_
      }
      if { $file != {} } {

         #  If the coordinate system of doesn't match this (coordinate system
         #  != default), then change to this.
         lassign [$itk_option(-spec_coords) get_system] system units
         if { $system != "default" && $system != {} } {
            set_coordinate_system_ $file $system $units
         }

         $itk_option(-gaiacube) display $file 1

         #  Set bindings to report the spectral coordinate of the current pane,
         #  when clicked on.
         set cubespectrum [$itk_option(-gaiacube) component spectrum]
         $cubespectrum close
         add_bindings_
      }
   }

   #  Grab the clicks on the image so that we can update the spectral
   #  readout.
   protected method add_bindings_ {} {
      $canvas_ bind $rtdimage_ <1> [code $this display_coord_ %x %y]
   }

   #  Determine the spectral coordinate associated with a canvas position
   #  and update the readout. The target coordinate system should have an
   #  Ident of "ORIGSKY" and the coordinate we require is the third.
   protected method display_coord_ {cx cy} {

      #  Get the current base domain frame index.
      set current [$rtdimage_ astget current]

      set ccx [$canvas_ canvasx $cx]
      set ccy [$canvas_ canvasy $cy]
      $rtdimage_ convert coords $ccx $ccy canvas ix iy image

      #  Get the current "sky" coordinates.
      lassign [$rtdimage_ astpix2wcs $ix $iy] ra dec

      #  Visit all frames in the image frameset and check for the "ORIGSKY"
      #  Ident.
      set nframes [$rtdimage_ astget "nframe"]

      for { set i 1 } { $i <= $nframes } { incr i } {
         $rtdimage_ astset current $i
         set ident [$rtdimage_ astget "Ident"]
         if { $ident == "ORIGSKY" } {
            catch {
               set xylist [$rtdimage_ astpix2cur $ix $iy]
               $itk_component(selected) configure -value [lindex $xylist 2]
            }
            break
         }
      }
      if { [info exists ra] && [info exists dec] } {

         #  Draw markers on all maps, one for each ROI<n> domain we have.
         $canvas_ delete "channel_map"
         for { set i 1 } { $i <= $nframes } { incr i } {
            $rtdimage_ astset current $i
            set ident [$rtdimage_ astget "Ident"]
            if { [string match "ROI*" $ident] } {
               catch {
                  lassign [$rtdimage_ astcur2pix $ra $dec] ix iy
                  $rtdimage_ convert coords $ix $iy image cx cy canvas
                  $canvas_ create rtd_mark $cx $cy \
                     -type cross -outline blue -tag "channel_map"
               }
            }
         }
      }

      #  Restore default domain.
      $rtdimage_ astset current $current
   }

   #  Set a new nchan value.
   protected method set_nchan_ {value} {
      configure -nchan $value
   }

   #  Set a new shape value.
   protected method set_shape_ {value} {
      configure -shape $value
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The number of channels to create in the map.
   itk_option define -nchan nchan Nchan 4

   #  The number of channels to show along the X-axis.
   itk_option define -shape shape Shape 2

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
