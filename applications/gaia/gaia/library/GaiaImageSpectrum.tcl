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
#     monotonically decreasing (which is pretty fundermental
#     when dealing with celestial coordinate systems). SLICE also
#     always insists on using axes values, which hardly ever useful
#     for us.

#  Inheritance:
#     This class inherits the abilities of RtdImageSpectrum.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  Copyright:
#     Copyright (C) 1997 Central Laboratory of the Research Councils

#  History:
#     16-JUL-1997 (PDRAPER):
#        Original version.
#     26-NOV-1997 (PDRAPER):
#        Modified to use foreign command to write slice.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaImageSpectrum {}

class gaia::GaiaImageSpectrum {
   inherit rtd::RtdImageSpectrum

   #  Constructor: create a new instance of this class. Note we need
   #  to force the base constructor to be invoked first
   constructor {args} { 
      eval RtdImageSpectrum::constructor $args
   } {
      eval itk_initialize $args
   }
 
   #  Destructor - clean up when deleted.
   destructor {
   }
   
   #  Make the graph subwindow. Override to get control of ranging.
   method make_graph {} {
      # Note: make the graph in the global namespace for now so that
      # the old blt utils (features.tcl) still work. Shouldn't be needed
      # with blt-2.0 or later...
      set cmd \
         [list blt::graph $w_.graph \
             -width 5i \
             -height 3i \
             -borderwidth 3 \
             -relief groove \
             -title "Pixel Values"]
      itk_component add graph {
         set graph_ [uplevel "#0" $cmd]
      } {
      }
      pack $itk_component(graph) \
         -fill both -expand 1 -padx 1m -pady 1m

      add_short_help $itk_component(graph) \
         {Graph: plot image pixel values along line, {bitmap dragb1} = zoom, {bitmap b2} = restore}

      $graph_ legend config -mapped 0
      $graph_ yaxis configure -title {}
      $graph_ xaxis configure -title {Distance along slice}
      
      global $graph_.iVector $graph_.vVector $graph_.xVector $graph_.yVector
      if { ![info exists $graph_.xVector] &&  
           ![info exists $graph_.yVector] &&
           ![info exists $graph_.iVector] &&
           ![info exists $graph_.vVector] 
        } {

         #  Execute this command at the global level. This is
         #  necessary to make sure that the vector command goes away
         #  with the variable.
         uplevel #0 "blt::vector $graph_.xVector $graph_.yVector \
                                 $graph_.iVector $graph_.vVector"
      }
      $graph_ element create elem -xdata $graph_.iVector -ydata $graph_.vVector

      # plot the distribution of pixel values
      notify_cmd

      # add BLT features
      Blt_ZoomStack $graph_
      Blt_ActiveLegend $graph_
      Blt_Crosshairs $graph_
      Blt_ClosestPoint $graph_
      bind bltCrosshairs <Any-Motion> [code $this dispXY %x %y]
      itk_component add fpos {
         frame $w_.fpos -relief flat
      }
      itk_component add xpos {
         label $itk_component(fpos).xpos -width 15 -anchor w
      }
      itk_component add ypos {
         label $itk_component(fpos).ypos -width 15 -anchor w
      }
      itk_component add val {
         label $itk_component(fpos).val -width 15 -anchor w
      }
      pack $itk_component(xpos) $itk_component(ypos) \
	      $itk_component(val) -fill x -expand 1 -side left
      pack $itk_component(fpos) -fill none -expand 0
   }
   
   #  Create an NDF or text file with the slice contents.
   method save_as {} {   

      #  Get the coordinates of the line endpoints.
      lassign [$canvas_ coords $itk_option(-line_id)] x0 y0 x1 y1

      #  Replot the distribution of pixel values to get the number 
      #  of pixels we want.
      set numValues [$image_ slice $graph_ elem $x0 $y0 $x1 $y1 canvas \
                        $graph_.iVector $graph_.vVector\
			$graph_.xVector $graph_.yVector]

      #  Get the pixel indices of the end-points.
      $image_ convert coords $x0 $y0 canvas xs ys image
      $image_ convert coords $x1 $y1 canvas xe ye image

      #  Get the name of the current image (.sdf is special and needs
      #  to be stripped).
      set image [$image_ cget -file]
      if { $image != "" } {
         lassign [fileName $image] image slice
         if { [file extension $image] == ".sdf" } {
            set image "[file rootname $image]${slice}"
         }
         
         #  Get a name for the slice.
         set slice [filename_dialog "." "*.sdf" $w_]
         if { [file extension $slice] == ".sdf" } {
            set slice [file rootname $slice]
         }

         #  And create it.
         busy {
            $image_ foreign writeslice \
               "-file $slice -line $xs $ys $xe $ye -nelem $numValues -name $image"
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
      pack $b.print $b.save $b.close -side left -expand 1 -padx 2m -pady 2m
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
      global $graph_.iVector $graph_.vVector $graph_.xVector $graph_.yVector
      if {[catch {set numValues_ [$image_ slice $graph_ elem \
	      $x0 $y0 $x1 $y1 canvas $graph_.iVector \
	      $graph_.vVector $graph_.xVector \
	      $graph_.yVector]}]} {
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
      global $graph_.vVector
      set ret 0
      if { ![$graph_ element closest $x $y "" -interpolate 1 -halo 10000]} {
         return
      }
      lassign [$graph_ invtransform $x $y] index value
      set index [expr int(round($index))]
      if {$index < 0 || $index >= $numValues_} {
         return
      }
      catch {
         set value [$graph_.vVector range $index $index]
         set x [$graph_.xVector range $index $index]
         set y [$graph_.yVector range $index $index]
         $itk_component(xpos) config -text "X: $x"
         $itk_component(ypos) config -text "Y: $y"
         $itk_component(val) config -text "Value: $value"
      }
   }

   #  Protected variables.
   protected variable figslice_ {}
   protected variable pixel2axis_ {}
   protected variable maxCoord_ 0
   protected variable minCoord_ 0

}
