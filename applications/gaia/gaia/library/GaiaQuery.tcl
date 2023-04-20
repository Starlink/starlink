#+
#  Name:
#     GaiaQuery

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for extending the abilities of SkyQuery

#  Description:
#     This class extends SkyQuery adding the facilities required for
#     GAIA. At present this is limited to adding a method for
#     setting the maximum number of objects, setting a sensible area
#     for pixel based coordinates and dealing with problems caused by
#     forking searches on local catalogues.

#  Invocations:
#
#        GaiaQuery object_name [configuration options]
#
#     This creates an instance of a GaiaQuery object. The
#     return is the name of the object.
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
#     See the "itk_option define" declarations below.

#  Methods:
#     See the method declarations below.

#  Inheritance:
#     skycat::SkyQuery

#  Copyright:
#     Copyright (C) 1998-2000 Central Laboratory of the Research Councils
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council
#     Copyright (C) 2009 Science and Technology Facilities Council
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
#     14-SEP-1998 (PWD):
#        Original version.
#     21-AUG-2000 (PWD):
#        Added origin corrections to image center.
#     21-MAR-2001 (PWD):
#        Changed set_maxobjs to return -1 when feature is not
#        available (no search facilities).
#     08-JAN-2001 (PWD):
#        Added set_default_values override so that we have some
#        control over the default number of objects that are searched
#        for. Increased from 1000 to 20000. Images are getting bigger
#        and deeper.
#     29-APR-2003 (PWD):
#        Increased precision of arc-minutes radius to 4 (from 2).
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaQuery {}

itcl::class gaia::GaiaQuery {
   inherit skycat::SkyQuery

   #  Constructor.
   constructor {args} {
      eval itk_initialize $args
   }

   #  Method to set the maximum number of objects allowed. Returns -1
   #  if we couldn't do request, because not relevant to interface.
   public method set_maxobjs {value} {
      if { [info exists maxnum_] && [winfo exists $maxnum_] } {
         $maxnum_ configure -value $value
         return 1
      }
      return -1
   }

   #  Override the set_from_image and get_image_center_radius method
   #  so we can change the ispix result to always include the whole
   #  image by default, rather than just a center part (i.e. return
   #  diagonal). Also need to add in NDF origins.
   public method set_from_image {} {
      set iswcs [{*}$astrocat iswcs]
      if {$iscat_} {
         if { $iswcs } {
            set_pos_radius [get_image_center_radius $iswcs]
         } else {
            lassign [{*}$astrocat origin] xo yo
            lassign [get_image_center_radius $iswcs] x y rad
            set_pos_radius [list [expr $x+$xo] [expr $y+$yo] $rad]
         }
      } else {
         if { $iswcs } {
            set_pos_width_height [get_image_center_width_height $iswcs]
         } else {
            lassign [{*}$astrocat origin] xo yo
            lassign [get_image_center_width_height $iswcs] x y w h
            set_pos_width_height [list [expr $x+$xo] [expr $y+$yo] $w $h]
         }
      }
   }
   public method get_image_center_radius {wcs_flag} {
      if {[$image_ isclear]} {
         return
      }
      if {$wcs_flag} {
         # using world coords
         set center [$image_ wcscenter]
         if {[llength $center] >= 2} {
            lassign $center ra dec equinox
            set radius [format "%.4f" [$image_ wcsradius]]
            if {$radius} {
               return [list $ra $dec $equinox $radius]
            }
         }
      } else {
         # using image coords
         set w [$image_ width]
         set h [$image_ height]
         set x [format "%.4f" [expr $w/2.]]
         set y [format "%.4f" [expr $h/2.]]
         set radius [format "%.4f" [expr sqrt($w*$w+$h*$h)/2.]]
         return [list $x $y $radius]
      }
   }

   #  Override the select image area method so that we can adjust for
   #  the NDF origin information is needed.
   public method select_area {} {
      set iswcs [{*}$astrocat iswcs]
      if {$iscat_} {
         if { $iswcs } {
            set_pos_radius [select_image_area $iswcs]
         } else {
            lassign [{*}$astrocat origin] xo yo
            lassign [select_image_area $iswcs] x y rad
            set_pos_radius [list [expr $x+$xo] [expr $y+$yo] $rad]
         }
      } else {
         if { $iswcs } {
            set_pos_width_height [select_image_area $iswcs]
         } else {
            lassign [{*}$astrocat origin] xo yo
            lassign [select_image_area $iswcs] x y w h
            set_pos_width_height [list [expr $x+$xo] [expr $y+$yo] $w $h]
         }
      }
   }

   #  Start the catalog search based on the current search options
   #  and display the results in the table. This is overridden from
   #  AstroQuery so that we can make sure that local catalogues are
   #  not processed using a forked process. This is necessary so that
   #  changes in the local catalogue internal state are seen (things
   #  like the modification dates and mapped contents).
   public method search {args} {
      if {$iscat_} {
         set cmd "$astrocat query"
      } else {
         set cmd "$astrocat getimage"
      }
      if {[{*}$astrocat iswcs] || [{*}$astrocat ispix]} {
         set equinox ""
         if {[{*}$astrocat iswcs]} {
            set name [$name_ get]
            set x [$ra_ get]
            set y [$dec_ get]
            set equinox [$equinox_ get]
         } elseif {[{*}$astrocat ispix]} {
            set name ""
            set x [$x_ get]
            set y [$y_ get]
         }

         if {$iscat_} {
            set rad1 [$rad1_ get]
            set rad2 [$rad2_ get]
         } else {
            set width [$width_ get]
            set height [$height_ get]
         }

         if {"$equinox" != ""} {
            lappend cmd "-equinox" $equinox
         }
         if {"$name" != ""} {
            lappend cmd "-nameserver" $namesvr "-name" $name
         } elseif {"$x" != "" && "$y" != ""} {
            lappend cmd "-pos" [list $x $y]
         } else {
            #warning_dialog "Please specify either an object name or a position in WCS" $w_
         }

         if {$iscat_} {
            if {"$rad1" != "" || "$rad2" != ""} {
               lappend cmd "-radius" "$rad1 $rad2"
            }
            set maxnum [$maxnum_ get]
            if {"$maxnum" != ""} {
               lappend cmd "-nrows" $maxnum
            }
            if {"[set sort_cols [{*}$astrocat sortcols]]" != ""} {
               lappend cmd "-sort" $sort_cols "-sortorder" [{*}$astrocat sortorder]
            }
         } else {
            if {"$width" != "" || "$height" != ""} {
               lappend cmd -width $width -height $height
            }
         }
      }

      # add optional search columns
      if {"$search_cols_" != ""} {
         set minvalues {}
         set maxvalues {}
         set search_cols {}
         foreach col $search_cols_ {
            set min [$min_values_($col) get]
            if {[catch {set max [$max_values_($col) get]}]} {
               # if only one value, compare for equality, otherwise range
               set max $min
            }
            if {"$min" == "" && "$max" != "" || "$max" == "" && "$min" != ""} {
               error_dialog "Please specify min and max values for $col"
               return
            }
            if {"$min" == ""} {
               continue
            }
            lappend search_cols $col
            lappend minvalues $min
            lappend maxvalues $max
         }
         if {[llength $search_cols]} {
            lappend cmd -searchcols $search_cols -minvalues $minvalues -maxvalues $maxvalues
         }
      }

      if {"$itk_option(-feedbackcommand)" != ""} {
         eval $itk_option(-feedbackcommand) on
      }

      #  Local catalogues must be processed at this level so that
      #  internal changes are retained.
      if {"[{*}$astrocat servtype]" == "local"} {
         $w_.batch fg_eval [code $this do_query $cmd]
      } else {
         $w_.batch bg_eval [code $this do_query $cmd]
      }
   }

   # Set the default values for the form entries.
   # Overridden to increase default number of objects from 1000 to
   # 20000.
   public method set_default_values {} {
      skycat::SkyQuery::set_default_values

      # Use the given default equinox.
      if { $default_equinox_ != "notset" } {
         if { [info exists equinox_] } {
            $equinox_ configure -value $default_equinox_
         }
      }
      set_maxobjs 20000

   }

   #  Set the default equinox. If local catalogue with coordinate
   #  system information then we do not want to assume J2000 or that
   #  of the image as the catalogue positions will be transformed
   #  as needed (in those cases the best default equinox is {}).
   public method set_equinox {equinox} {
      set default_equinox_ $equinox
      if { [info exists equinox_] } {
         $equinox_ configure -value $equinox
      }
   }

   protected variable default_equinox_ "notset"
}
