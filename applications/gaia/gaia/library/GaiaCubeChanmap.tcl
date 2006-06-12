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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

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
      #  Nothing to do.
   }

   #  Methods:
   #  --------

   protected method add_controls_ {} {

      #  Use default controls (range and combination method).
      GaiaCubeApps::add_controls_

      #  The number of channels to create.
      itk_component add nchan {
         LabelEntryScale $w_.nchan \
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
      pack $itk_component(nchan) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(nchan) \
         {Number of channels to appear in the channel map}

      #  The number of channels to create.
      itk_component add shape {
         LabelEntryScale $w_.shape \
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
      pack $itk_component(shape) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(shape) \
         {Number of channels along the X-axis of the output image}
   }

   #  Run the CHANMAP application.
   protected method run_main_app_ {ndfname axis lb ub} {

      #  Start up the CHANMAP application, if not done.
      if { $maintask_ == {} } {
         global env
         set maintask_ [GaiaApp \#auto -application \
                            $env(KAPPA_DIR)/chanmap \
                            -notify [code $this app_completed_]]
      }

      #  Create a temporary file name.
      set tmpimage_ "GaiaTempChanmap${count_}"
      incr count_
      $maintask_ runwiths "in=$ndfname out=$tmpimage_ axis=$axis \
                          low=$lb high=$ub estimator=$combination_type_ \
                          nchan=$itk_option(-nchan) \
                          shape=$itk_option(-shape) accept"
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
         $itk_option(-gaiacube) display $file 1
      }
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
