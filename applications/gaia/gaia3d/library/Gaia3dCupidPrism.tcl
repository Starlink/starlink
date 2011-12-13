#+
#  Name:
#     Gaia3dCupidPrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Interface for a catalogue of CUPID detections

#  Description:
#     This class manages the life-cycle of a collection of objects that
#     can be renderered into a scene. The objects represent prism shaped
#     regions bounded within a cube that are CUPID catalogue detections.

#  Copyright:
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     07-AUG-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dCupidPrism {

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
      foreach index [array names collection_] {
         ::delete object $collection_($index)
      }
   }

   #  Methods and procedures:
   #  -----------------------

   #  Add regions to the render window.
   public method add_to_window {} {
      set add_to_window_ 1
      foreach index [array names collection_] {
         $collection_($index) add_to_window
      }
   }

   #  Remove from the render window.
   public method remove_from_window {} {
      set add_to_window_ 0
      foreach index [array names collection_] {
         $collection_($index) remove_from_window
      }
   }

   #  Make visible.
   public method set_visible {} {
      set visible_ 1
      foreach index [array names collection_] {
         $collection_($index) set_visible
      }
   }

   #  Make invisible.
   public method set_invisible {} {
      set visible_ 0
      foreach index [array names collection_] {
         $collection_($index) set_invisible
      }
   }

   #  Accept an gaia::GaiaCupidImport instance that has CUPID catalogues
   #  opened. Parse and create a suitable list of objects for rendering.
   public method set_importer {importer} {
      if { $importer != {} } {
         delete_objects_
         create_objects_ $importer
         add_to_window
         set_visible
      }
   }

   #  Extrude the 2D shapes along the selected axis.
   public method fit_to_data {} {
      foreach index [array names collection_] {
         $collection_($index) fit_to_data
      }
   }

   #  Delete all the existing objects.
   protected method delete_objects_ {} {
      foreach index [array names collection_] {
         ::delete object $collection_($index)
         unset collection_($index)
      }
   }

   #  Create objects to display the CUPID detections.
   protected method create_objects_ {importer} {

      #  Counter for all objects created.
      set n 0

      #  Loop over the catalogues in hand.
      foreach catwin [$importer get 1] {

         #  Connect the catalogue coordinates to the grid coordinates.
         set tranwcs [get_kaplibs_wcs_ $catwin]

         #  Get a colour for this catalogue, same as the plot symbol.
         set colour [get_symbol_colour_ $catwin]

         #  Draw the sort of graphics expected. Simple prism rectangles
         #  or STC regions.
         if { [$importer using_stc $catwin] } {
            set n [create_stc_objects_ $n $catwin $tranwcs $colour]
         } else {
            set n [create_rect_objects_ $n $catwin $tranwcs $colour]
         }
      }
      if { $tranwcs != 0 } {
         gaiautils::astannul $tranwcs
      }
      apply_configuration_
      fit_to_data
   }

   #  Connect the catalogue coordinates to the grid coordinates of the
   #  dataset. So see if the catalogue has a WCS, as this is from
   #  CUPID it should. Returns a 3D WCS that transforms sky and spectral
   #  values (dimensions 1, 2 and 3).
   protected method get_kaplibs_wcs_ {catwin} {
      set comments [$catwin comments]
      set tranwcs 0
      if { $comments != {} } {
         set astref [gaia::GaiaSearch::get_kaplibs_frameset $comments]
         if { $astref != 0 } {

            #  Want transformation from current coordinates of the
            #  catalogue to base coordinates of the cube. So we connect
            #  them, going via SKY-DSBSPECTRUM or PIXEL coordinates.
            set wcs_current [gaiautils::astget $wcs "Current"]
            set wcs_base [gaiautils::astget $wcs "Base"]

            gaiautils::astset $wcs "Current=$wcs_base"
            set tranwcs [gaiautils::astconvert $wcs $astref \
                            "SKY-DSBSPECTRUM,PIXEL,"]

            gaiautils::astset $wcs "Current=$wcs_current"
            gaiautils::astset $wcs "Base=$wcs_base"
            gaiautils::astannul $astref
         }
      }
      return $tranwcs
   }

   #  Get the colour of the symbol used to draw markers in the main window.
   protected method get_symbol_colour_ {catwin} {
      set symbol [$catwin get_symbol]
      set symbol [lindex [lindex $symbol 0] 1]
      set colour [lindex $symbol 1]
      if { $colour == {} } {
         return black
      }
      return $colour
   }

   #  Create STC region based objects.
   protected method create_stc_objects_ {index catwin tranwcs colour} {

      #  Get the data from the catalogue.
      foreach line [get_content_ $catwin] {
         lassign $line \
            pident peak1 peak2 peak3 cen1 cen2 cen3 size1 size2 size3 \
            sum peak volume shape

         #  Shape is a 2D region that connects to the sky coordinates.
         #  Need to connect this to the cube and transform into pixels.
         set region [gaiautils::stcregion $shape]
         set type [gaiautils::regiontype $region]
         if { $type == "ellipse" } {

            #  Get the parameterisation of the ellipse.
            set pars [gaiautils::astregionpars $region]
            lassign $pars cen1 cen2 a b angle d11 d12 d21 d22

            #  Transform from catalogue coordinates to grid. Note WCS
            #  has 12 axes, but only the first three are used to connect
            #  to the cube. If shape is used that represents an STC region.
            if { $tranwcs != 0 } {

               #  Radians to degrees.
               set cen1 [expr $cen1*$r2d_]
               set cen2 [expr $cen2*$r2d_]

               set d11 [expr $d11*$r2d_]
               set d12 [expr $d12*$r2d_]
               set d21 [expr $d21*$r2d_]
               set d22 [expr $d22*$r2d_]

               #  Size3 from catalogue column are distances, so offset
               #  from centre to get positions to match end points of ellipse.
               set d13 [expr $cen3-$size3]
               set d23 [expr $cen3+$size3]

               #  Transform end points of various kinds.
               lassign [tran3d_ $tranwcs 0 $cen1 $cen2 $cen3] cen1 cen2 cen3
               lassign [tran3d_ $tranwcs 0 $d11 $d12 $d13] d11 d12 d13
               lassign [tran3d_ $tranwcs 0 $d21 $d22 $d23] d21 d22 d23

               #  Recover angle, semi-major & semi-minor axes from end points.
               set xdiff [expr $d11-$cen1];
               set ydiff [expr $d12-$cen2];
               set smaj [expr sqrt($xdiff*$xdiff + $ydiff*$ydiff)];
               set angle [expr atan2($ydiff,$xdiff)*$r2d_];

               set xdiff [expr $d21-$cen1];
               set ydiff [expr $d22-$cen2];
               set smin [expr sqrt($xdiff*$xdiff + $ydiff*$ydiff)];

               #  Prism extent.
               set size3 [expr abs(0.5*($d23-$d13))]
               set z0 [expr $cen3-$size3]
               set z1 [expr $cen3+$size3]

               #  and create.
               set collection_($index) [gaia3d::Gaia3dVtkEllipsePrism \#auto \
                                           -xcentre $cen1 -ycentre $cen2 \
                                           -semimajor $smaj \
                                           -semiminor $smin \
                                           -angle $angle \
                                           -zlow $z0 -zhigh $z1 \
                                           -colour $colour]
               incr index
            }

         } elseif { $type == "polygon" } {

            #  Get the coordinates of the polygon vertices.
            set coords [gaiautils::astregionpars $region]

            #  Transform from catalogue coordinates to grid. Note WCS
            #  has 12 axes, but only the first three are used to connect
            #  to the cube. If shape is used that represents an STC region.
            if { $tranwcs != 0 } {

               #  Transform coordinates to grid positions, also do
               #  radians to degrees (catalogue WCS is in degrees).
               set newcoords {}
               foreach {x y} $coords {
                  set x1 [expr $x*$r2d_]
                  set y1 [expr $y*$r2d_]
                  lassign [tran3d_ $tranwcs 0 $x1 $y1 $cen3] x y z
                  lappend newcoords $x $y
               }


               #  Size3 from catalogue column are distances, so offset
               #  from centre to get positions to match end points of prism.
               set d13 [expr $cen3-$size3]
               set d23 [expr $cen3+$size3]

               #  Transform those so get the prism extent.
               set x1 [lindex $coords 0]
               set y1 [lindex $coords 1]
               lassign [tran3d_ $tranwcs 0 $x1 $y1 $d13] d11 d12 d13
               lassign [tran3d_ $tranwcs 0 $x1 $y1 $d23] d21 d22 d23

               #  Prism extent.
               set size3 [expr abs(0.5*($d23-$d13))]

               #  Offset from new centre along 3.
               lassign [tran3d_ $tranwcs 0 $x1 $y1 $cen3] x y cen3
               set z0 [expr $cen3-$size3]
               set z1 [expr $cen3+$size3]

               #  and create.
               set collection_($index) [gaia3d::Gaia3dVtkPolygonPrism \#auto \
                                           -coords $newcoords \
                                           -zlow $z0 -zhigh $z1 \
                                           -colour $colour]
               incr index
            }
         }
         gaiautils::astannul $region
      }
      return $index
   }

   #  Create rectangular region based objects.
   protected method create_rect_objects_ {index catwin tranwcs colour} {

      #  Get the data from the catalogue.
      foreach line [get_content_ $catwin] {
         lassign $line pid peak1 peak2 peak3 cen1 cen2 cen3 size1 size2 size3

         #  Transform from catalogue coordinates to grid. Note WCS
         #  has 12 axes, but only the first three are used to connect
         #  to the cube. If shape is used that represents an STC region.
         if { $tranwcs != 0 } {

            #  Unformat the ones that might be in sexagesimal.
            #  Note values from table are already transformed into
            #  celestial positions in RA and Dec, so we use that WCS
            #  to unformat not tranwcs, which is in plain degrees.
            set cen1 [gaiautils::astunformat $wcs 1 $cen1]
            set cen2 [gaiautils::astunformat $wcs 2 $cen2]

            #  Radians to degrees, now same as catalogue WCS.
            set cen1 [expr $cen1*$r2d_]
            set cen2 [expr $cen2*$r2d_]

            #  Sizes from arcsec to degrees.
            set size1 [expr ($size1/3600.0)]
            set size2 [expr ($size2/3600.0)]

            #  Size[123] are distances, so offset from centre to get
            #  positions.
            set d11 [expr $cen1-$size1]
            set d12 [expr $cen2-$size2]
            set d13 [expr $cen3-$size3]

            set d21 [expr $cen1+$size1]
            set d22 [expr $cen2+$size2]
            set d23 [expr $cen3+$size3]

            #  Transform end positions in degrees to pixels.
            lassign [tran3d_ $tranwcs 0 $d11 $d12 $d13] d11 d12 d13
            lassign [tran3d_ $tranwcs 0 $d21 $d22 $d23] d21 d22 d23

            #  Recover sizes in pixels.
            set size1 [expr abs(0.5*($d21-$d11))]
            set size2 [expr abs(0.5*($d22-$d12))]
            set size3 [expr abs(0.5*($d23-$d13))]

            #  Transform centre from degrees to pixels.
            lassign [tran3d_ $tranwcs 0 $cen1 $cen2 $cen3] cen1 cen2 cen3
         }

         #  Derive coordinate of ranges along each axis.
         set x0 [expr $cen1-$size1]
         set x1 [expr $cen1+$size1]

         set y0 [expr $cen2-$size2]
         set y1 [expr $cen2+$size2]

         set z0 [expr $cen3-$size3]
         set z1 [expr $cen3+$size3]

         #  and create.
         set collection_($index) [gaia3d::Gaia3dVtkRectPrism \#auto \
                                     -x0 $x0 -y0 $y0 -x1 $x1 -y1 $y1 \
                                     -zlow $z0 -zhigh $z1 -colour $colour]
         incr index
      }
      return $index
   }

   #  Get the contents of a given catalogue window. If selected is enabled
   #  only the selected rows are returned, otherwise all rows are returned
   #  (that are shown in the table window, not the underlying catalogue).
   protected method get_content_ {catwin} {
      set results [$catwin get_table]
      if { $selected } {
         return [$results get_selected]
      }
      return [$results get_contents]
   }

   #  Transform a 3D position. Assumes a catalogue-based WCS with
   #  3 input and 12 output axes. On output only the first three values
   #  will be used.
   protected method tran3d_ {wcs forward x1 x2 x3} {
      if { $forward } {
         lassign [gaiautils::asttrann $wcs 1 "$x1 $x2 $x3"] c1 c2 c3
         return [list $c1 $c2 $c3]
      }
      return [gaiautils::asttrann $wcs 0 "$x1 $x2 $x3 4 5 6 7 8 9 10 11 12"]
   }

   #  Apply the current configuration to all objects.
   protected method apply_configuration_ {} {
      foreach index [array names collection_] {
         $collection_($index) configure \
            -renwindow $renwindow \
            -align_to_axis $align_to_axis \
            -axis $axis
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------
   #  Note all these should be supported by the proxied objects.

   #  The render window (a Gaia3dVtkWindow instance).
   public variable renwindow {} {
      apply_configuration_
   }

   #  The WCS of the actual datacube.
   public variable wcs {}

   #  Whether to just plot the selected objects.
   public variable selected 0

   #  Whether to align to an axis.
   public variable align_to_axis 0 {
      apply_configuration_
   }

   #  The axis to align to.
   public variable axis 3 {
      apply_configuration_
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  The objects, indexed by a number.
   protected variable collection_

   #  Colour of each object. These differ per-catalogue.
   protected variable colour_

   #  If objects should be added to the render window.
   protected variable add_to_window_ 0

   #  If objects should be made visible.
   protected variable visible_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Degrees to radians.
   common d2r_ 0.017453292

   #  Radians to degrees.
   common r2d_ 57.2957795

#  End of class definition.
}
