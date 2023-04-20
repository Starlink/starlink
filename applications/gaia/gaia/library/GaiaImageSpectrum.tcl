#+
#  Name:
#     GaiaImageSpectrum

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Extends the RtdImageSpectrum class to add the ability to
#     save the spectrum to a file.

#  Description:
#     This class extends RtdImageSpectrum so that the currently
#     selected line across the line can be extracted and stored
#     as an image. It does this by adding an extra button
#     to the lower toolbar which initiates a fileselection dialog.
#     It then uses the foreign image command writeslice to extract the
#     line of data.

#  Invocations:
#
#        GaiaImageSpectrum object_name [configuration options]
#
#     This creates an instance of a GaiaImageSpectrum object.
#     The return is the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Methods:
#
#        save_as
#
#     This method creates a fileselection window to get a file name.
#     It then extracts the current line into an image.
#
#        make_buttons
#
#     This method overrides RtdImageSpectrum to create the buttons
#     at the bottom of the display. It adds the "save as..." option.

#  Notes:
#     Originally this routine used the FIGARO application SLICE, but
#     has been changed to use an internal routine instead. This is
#     because SLICE cannot deal with the situation when axes are
#     monotonically decreasing (which is pretty fundamental
#     when dealing with celestial coordinate systems). SLICE also
#     always insists on using axes values, which hardly ever useful
#     for us.

#  Inheritance:
#     This class inherits the abilities of RtdImageSpectrum.

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  Copyright:
#     Copyright (C) 1997-1999 Central Laboratory of the Research Councils
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

#  History:
#     16-JUL-1997 (PWD):
#        Original version.
#     26-NOV-1997 (PWD):
#        Modified to use foreign command to write slice.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaImageSpectrum {}

itcl::class gaia::GaiaImageSpectrum {
   inherit rtd::RtdImageSpectrum

   #  Constructor: create a new instance of this class. Note we need
   #  to force the base constructor to be invoked first
   constructor {args} {
      eval rtd::RtdImageSpectrum::constructor $args
   } {
      eval itk_initialize $args

      #  Create image name control object.
      set namer_ [gaia::GaiaImageName \#auto]
   }

   #  Destructor - clean up when deleted.
   destructor {
      blt::vector destroy $vVector_ $iVector_ $xVector_ $yVector_

      #  Trap RtdImageSpectrum destructor as vector names do not seem
      #  available at that scope.
      catch {rtd::RtdImageSpectrum::destructor}

      if { $namer_ != {} } {
         catch {delete object $namer_}
      }
   }

   #  Make the graph subwindow. Override to get control of ranging and
   #  add a widget to display the data value (so we get X,Y and data value of
   #  position in the image).
   method make_graph {} {

      #  Add the graph
      itk_component add graph {
         blt::graph $w_.graph \
            -width 6i \
            -height 3i \
            -borderwidth 3 \
            -relief groove \
            -title "Pixel Values"
      } {}
      set graph_ $itk_component(graph)
      pack $itk_component(graph) -fill both -expand 1 -padx 1m -pady 1m

      add_short_help $itk_component(graph) \
         {Graph: plot image pixel values along line, {bitmap dragb1} = zoom, {bitmap b2} = restore}
      #  Set axes labels.
      $graph_ yaxis configure -title {}
      $graph_ xaxis configure -title {Distance along slice}

      #  blt2.4f vector names ust start with a letter, no dots...
      #  someone also changed the default symbol to circle. Why?
      regsub -all {\.} v$graph_.xVector _ xVector_
      regsub -all {\.} v$graph_.yVector _ yVector_
      regsub -all {\.} v$graph_.iVector _ iVector_
      regsub -all {\.} v$graph_.vVector _ vVector_

      $graph_ legend config -hide 1
      if { ! [info exists $xVector_] && ! [info exists $yVector_] &&
           ! [info exists $iVector_] && ! [info exists $vVector_] } {
         blt::vector create $vVector_ $iVector_
         blt::vector create $xVector_ $yVector_
      }
      set symbol {}
      $graph_ element create elem -xdata $iVector_ -ydata $vVector_ -symbol $symbol

      # plot the distribution of pixel values
      notify_cmd

      # add BLT features
      ::Blt_ZoomStack $graph_
      ::Blt_ActiveLegend $graph_
      ::Blt_Crosshairs $graph_
      ::Blt_ClosestPoint $graph_
      bind bltCrosshairs$this <Any-Motion> [code $this dispXY %x %y]
      blt::AddBindTag $graph_ bltCrosshairs$this


      # Tk frame for X,Y positions.
      itk_component add fpos {
         frame $w_.fpos
      }

      # Tk label for X position.
      itk_component add xpos {
         label $itk_component(fpos).xpos -width 15 -anchor w
      }

      # Tk label for Y position.
      itk_component add yval {
         label $itk_component(fpos).yval -width 15 -anchor w
      }

      # Tk frame for Value positions.
      itk_component add val {
         label $itk_component(fpos).val -width 15 -anchor w
      }
      pack $itk_component(xpos) $itk_component(yval) \
         $itk_component(val) -fill x -expand 1 -side left

      #  Add logscale option for Y axis.
      itk_component add logscale {
         checkbutton $itk_component(fpos).logscale -text "Log y axis" \
            -variable [scope logscale_] \
            -offvalue 0 \
            -onvalue 1 \
            -command [code $this set_logscale_]
      }
      pack $itk_component(logscale) -fill none -expand 1

      pack $itk_component(fpos) -fill x -expand 1
   }

   #  Create an NDF or text file with the slice contents.
   method save_as {} {

      #  Get the coordinates of the line endpoints.
      lassign [$canvas_ coords $itk_option(-line_id)] x0 y0 x1 y1

      #  Replot the distribution of pixel values to get the number
      #  of pixels we want.
      set numValues [$image_ slice $graph_ elem $x0 $y0 $x1 $y1 canvas \
                        $iVector_ $vVector_ \
                        $xVector_ $yVector_ ]

      #  Get the pixel indices of the end-points.
      $image_ convert coords $x0 $y0 canvas xs ys image
      $image_ convert coords $x1 $y1 canvas xe ye image

      #  Get the name of the current image.
      #set image [$image_ cget -file]
      set image [$image_ fullname]
      if { $image != "" } {
         $namer_ configure -imagename $image
         set image [$namer_ ndfname]

         #  Get a name for the slice.
         set slice [filename_dialog "." "*.sdf" $w_]
         if { $slice != "" } {
            $namer_ configure -imagename $slice
            set slice [$namer_ ndfname]

            #  And create it.
            busy {
               $image_ foreign writeslice \
                  "-file $slice -line $xs $ys $xe $ye -nelem $numValues -name $image"
            }
         }
      } else {
         error_dialog "No image is displayed"
      }
   }
   private method unlock_ {} {
      blt::busy release $w_
   }


   #  Make the button frame at the bottom of the window.
   method make_buttons {} {
      set b [frame $w_.buttons -borderwidth 2 -relief groove]
      pack $b -side bottom -fill x
      button $b.print -text "Print..." -command [code $this print]
      button $b.save -text "Save as..." -command [code $this save_as]
      button $b.close -text "Close" -command [code $this quit]
      pack $b.print $b.save $b.close -side left -fill x -expand 1 \
         -padx 2m -pady 2m
   }

   # This method is called whenever the spectrum line is moved, resized
   # or deleted or when the image changed and the graph should be updated.
   # It updates the graph to show the image values along the line.

   method notify_cmd {{op update}} {

      if {"$op" == "delete"} {
         destroy $w_
         return 0
      }
      lassign [$canvas_ coords $itk_option(-line_id)] x0 y0 x1 y1

      # plot the distribution of pixel values
      if {[catch {set numValues_ [$image_ slice $graph_ elem \
                                     $x0 $y0 $x1 $y1 canvas \
                                     $iVector_ $vVector_ \
                                     $xVector_ $yVector_ ]} msg ]} {
         error_dialog "$msg"
         return 0
      }
      $graph_ xaxis configure -max $numValues_

      return 0
   }

   #  Display x, y values at cursor position.
   method dispXY {x y} {

      #  Update crosshair position.
      $graph_ crosshairs configure -position @$x,$y

      #  Now update the readout.
      if { ! [$graph_ element closest $x $y "" -interpolate 1 -halo 10000] } {
         return
      }
      lassign [$graph_ invtransform $x $y] index value
      set index [expr int(round($index))]
      if {$index < 0 || $index >= $numValues_} {
         return
      }
      catch {
         set value [$vVector_ range $index $index]
         set x [$xVector_ range $index $index]
         set y [$yVector_ range $index $index]
         $itk_component(xpos) config -text "X: $x"
         $itk_component(yval) config -text "Y: $y"
         $itk_component(val) config -text "Value: $value"
      }
   }

   #  Set the logscale as requested.
   method set_logscale_ {} {
      $graph_ yaxis configure -logscale $logscale_
   }

   #  BLT vectors.
   protected variable vVector_ {}
   protected variable iVector_ {}

   #  Image name controller.
   protected variable namer_ {}

   #  Log scale status.
   protected variable logscale_ 0
}
