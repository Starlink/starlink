#+
#  Name:
#     GaiaXYHistogram

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Display the histogram of data in a rectangular region of an image.

#  Description:
#     This class creates a toolbox that displays the histogram
#     of all the data in a rectangular region on the image. The region is
#     displayed as a  rectangle that can be dragged around the image The
#     histogram is updated when the rectangle is dragged around the
#     image. When creating an instance of this class you must supply a
#     CanvasDraw rectangle (option -rect_id).

#  Invocations:
#
#        GaiaXYHistogram object_name [configuration options]
#
#     This creates an instance of a GaiaXYHistogram object. The return is
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

#  Inheritance:
#     util::TopLevelWidget

#  Copyright:
#     Copyright (C) 2014 Science and Technology Facilities Council.
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
#     09-JAN-2014 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaXYHistogram {}

itcl::class gaia::GaiaXYHistogram {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      wm withdraw $w_

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: XY Histogram ($itk_option(-number))"

      #  Add the File menu
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0

      #  Add the option menu.
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button xyhistogramusage "On Window..."

      #  Add short help window.
      make_short_help

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}


      #  Save histogram to text file.
      $File add command -label {Save as...} \
         -command [code $this save_as_] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this save_as_]
      $short_help_win_ add_menu_short_help $File \
         {Save as...} {Save histogram to text file}

      #  Set the exit menu item.
      $File add command -label Exit \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add toggle for continuous updates.
      $Options add checkbutton -label {Continuous updates} \
         -variable [scope itk_option(-continuous_updates)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this add_notify_]
      $short_help_win_ add_menu_short_help $Options \
         {Continuous updates} \
         {Change histogram during rectangle motion}

      #  Add toggle for using data limits of main image.
      $Options add checkbutton -label {Use full data limits} \
         -variable [scope itk_option(-use_data_limits)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_use_data_limits_]
      $short_help_win_ add_menu_short_help $Options \
         {Use data limits} \
         {Use the data limits of the main image to clip histogram range}

      #  Display gaussian fit.
      $Options add checkbutton -label {Display gaussian fit} \
         -variable [scope itk_option(-show_gaussian_histogram)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_show_gaussian_histogram_]
      $short_help_win_ add_menu_short_help $Options \
         {Display gaussian fit} \
         {Display the gaussian git}

      #  Display the previous histogram.
      $Options add checkbutton -label {Display previous histogram} \
         -variable [scope itk_option(-show_last_histogram)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_show_last_histogram_]
      $short_help_win_ add_menu_short_help $Options \
         {Display previous histogram} \
         {Display the previous histogram, not much use for interactive work}

      #  Colour options.
      $Options add cascade -label {Histogram color} \
         -menu [menu $Options.histogram]
      foreach colour $simplecolours_ {
         $Options.histogram add radiobutton \
             -background $colour \
             -variable [scope itk_option(-histogram_colour)] \
             -value $colour \
             -label {    } \
             -command [code $this set_histogram_colour_ $colour]
      }
      add_menu_short_help $Options {Histogram color} \
         {Change the colour of the histogram}

      $Options add cascade -label {Gaussian histogram color} \
         -menu [menu $Options.gaussianhistogram]
      foreach colour $simplecolours_ {
         $Options.gaussianhistogram add radiobutton \
             -background $colour \
             -variable [scope itk_option(-gaussian_histogram_colour)] \
             -value $colour \
             -label {    } \
             -command [code $this set_gaussian_histogram_colour_ $colour]
      }
      add_menu_short_help $Options {Gaussian histogram color} \
         {Change the colour of the gaussian histogram}

      $Options add cascade -label {Previous histogram color} \
         -menu [menu $Options.lasthistogram]
      foreach colour $simplecolours_ {
         $Options.lasthistogram add radiobutton \
             -background $colour \
             -variable [scope itk_option(-last_histogram_colour)] \
             -value $colour \
             -label {    } \
             -command [code $this set_last_histogram_colour_ $colour]
      }
      add_menu_short_help $Options {Previous histogram color} \
         {Change the colour of the previous histogram}

      #  Line width options.
      $Options add cascade -label {Histogram width} \
         -menu [menu $Options.histogramwidth]
      foreach i {1 2 3 4} {
         $Options.histogramwidth add radiobutton \
            -value $i \
            -bitmap width$i \
            -variable [scope itk_option(-histogram_width)] \
            -command [code $this set_histogram_width_ $i]
      }
      add_menu_short_help $Options {Histogram width} \
         {Set width of lines used to draw histogram}

      $Options add cascade -label {Gaussian histogram width} \
         -menu [menu $Options.gaussianwidth]
      foreach i {1 2 3 4} {
         $Options.gaussianwidth add radiobutton \
            -value $i \
            -bitmap width$i \
            -variable [scope itk_option(-gaussian_histogram_width)] \
            -command [code $this set_gaussian_histogram_width_ $i]
      }
      add_menu_short_help $Options {Gaussian histogram width} \
         {Set width of lines used to draw gaussian histogram fit}

      $Options add cascade -label {Previous histogram width} \
         -menu [menu $Options.lastwidth]
      foreach i {1 2 3 4} {
         $Options.lastwidth add radiobutton \
            -value $i \
            -bitmap width$i \
            -variable [scope itk_option(-last_histogram_width)] \
            -command [code $this set_last_histogram_width_ $i]
      }
      add_menu_short_help $Options {Previous histogram width} \
         {Set width of lines used to draw previous histograms}

      #  Set the initial corner coordinates of the rectangle.
      set_image_bounds_

      #  Create the BLT graph to display the histogram.
      make_graph_

      #  Add the control panel and buttons.
      make_buttons_

      #  Update the interface.
      notify_cmd
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Remove BLT vectors.
      catch {
         blt::vector destroy $xVector_ $iVector_
      }
      catch {
         blt::vector destroy $last_xVector_ $last_xiVector_
      }
      catch {
         blt::vector destroy $gxVector_ $giVector_
      }

      #  Remove rectangle.
      catch {
         $itk_option(-canvasdraw) remove_notify_cmd $itk_option(-rect_id)
         $itk_option(-canvasdraw) delete_object $itk_option(-rect_id)
      }
   }

   #  Methods:
   #  --------

   #  Close the window. Always destroy so that the rectangle will be redrawn
   #  on the next open.
   public method close {} {
      destroy $w_
   }

   #  Create a clone of this window.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Create the BLT graph and vectors.
   protected method make_graph_ {} {

      set lwidth 14
      set vwidth 14

      #  Create the X graph and add it to the upper pane.
      itk_component add xgraph {
         blt::graph $w_.xgraph \
            -width 400 \
            -height 225 \
            -borderwidth 3 \
            -relief groove \
            -title "Histogram"
      } {}
      set xgraph_ $itk_component(xgraph)
      pack $itk_component(xgraph) -fill both -expand 1 -padx 1m -pady 1m
      add_short_help $itk_component(xgraph) \
         {Graph: histogram, {bitmap dragb1} = zoom, {bitmap b2} = restore}

      #  Binning factor.
      itk_component add factor {
         util::LabelEntryScale $w_.factor \
            -text "Binning factor:" \
            -labelwidth $lwidth \
            -from 0.0 \
            -to 0.1 \
            -increment 0.001 \
            -resolution 0.001 \
            -show_arrows 1 \
            -anchor e \
            -delay 250 \
            -command [code $this set_binning_factor_]
      }
      pack $itk_component(factor) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(factor) \
         {Binning factor: fraction of counts required in modal bin}

      #  Best delayed for some reason.
      $itk_component(factor) configure -value $factor_

      #  Readouts for this graph.
      #  Table for current values.
      itk_component add tableframe {
         frame $w_.tableframe \
            -relief flat -borderwidth 1
      }
      pack $itk_component(tableframe) -fill x -expand 1

      #  Smaller labels in main area.
      set lwidth 9
      set vwidth 16

      #  Readout coordinate.
      itk_component add uppercoord {
         util::LabelValue $itk_component(tableframe).coord \
            -text "Intensity:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief groove \
            -anchor e
      }

      #  Readout value from current graph.
      itk_component add uppervalue {
         util::LabelValue $itk_component(tableframe).value \
            -text "Count:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief groove \
            -anchor e
      }
      blt::blttable $itk_component(tableframe) $itk_component(uppercoord) \
            0,0 -fill both
      blt::blttable $itk_component(tableframe) $itk_component(uppervalue) \
            0,1 -fill both

      #  Display derived results of various kinds.
      itk_component add intactual {
         util::LabelValue $itk_component(tableframe).intactual \
            -text "Peak:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -anchor e
      }
      itk_component add countactual {
         util::LabelValue $itk_component(tableframe).countactual \
            -text ":" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -anchor e
      }

      itk_component add intfit {
         util::LabelValue $itk_component(tableframe).intfit \
            -text "Parabolic fit:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -anchor e
      }
      itk_component add countfit {
         util::LabelValue $itk_component(tableframe).countfit \
            -text ":" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -anchor e
      }
      itk_component add fwhmfit {
         util::LabelValue $itk_component(tableframe).fwhmfit \
            -text "FWHM:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -anchor e
      }

      itk_component add intgauss {
         util::LabelValue $itk_component(tableframe).intgauss \
            -text "Gaussian fit:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -anchor e
      }
      itk_component add intgausserr {
         util::LabelValue $itk_component(tableframe).intgausserr \
            -text "std:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -anchor e
      }
      itk_component add sigmagauss {
         util::LabelValue $itk_component(tableframe).sigmagauss \
            -text "Sigma:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -anchor e
      }
      itk_component add sigmagausserr {
         util::LabelValue $itk_component(tableframe).sigmagausserr \
            -text ":" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -anchor e
      }


      blt::blttable $itk_component(tableframe) $itk_component(intactual) \
            1,0 -fill both
      blt::blttable $itk_component(tableframe) $itk_component(countactual) \
            1,1 -fill both

      blt::blttable $itk_component(tableframe) $itk_component(intfit) \
            2,0 -fill both
      blt::blttable $itk_component(tableframe) $itk_component(countfit) \
            2,1 -fill both
      blt::blttable $itk_component(tableframe) $itk_component(fwhmfit) \
            2,2 -fill both

      blt::blttable $itk_component(tableframe) $itk_component(intgauss) \
            3,0 -fill both
      blt::blttable $itk_component(tableframe) $itk_component(intgausserr) \
            4,0 -fill both
      blt::blttable $itk_component(tableframe) $itk_component(sigmagauss) \
            3,1 -fill both
      blt::blttable $itk_component(tableframe) $itk_component(sigmagausserr) \
            4,1 -fill both

      #  Set axes labels.
      $xgraph_ xaxis configure -title {Intensity}
      $xgraph_ yaxis configure -title {Count}

      #  Create vectors that contain coordinates.
      $xgraph_ legend config -hide 1
      if { ! [info exists $xVector_] && ! [info exists $iVector_] } {

         set xVector_ [blt::vector create \#auto]
         set iVector_ [blt::vector create \#auto]

         #  Copies for displaying last extractions as well.
         set last_xVector_ [blt::vector create \#auto]
         set last_iVector_ [blt::vector create \#auto]

         #  Gaussian realisation.
         set gxVector_ [blt::vector create \#auto]
         set giVector_ [blt::vector create \#auto]
      }

      set symbol {}

      #  Last vectors for comparison. Only enabled when requested.
      $xgraph_ element create last_elem \
         -xdata $last_xVector_ -ydata $last_iVector_ -symbol $symbol \
         -color $itk_option(-last_histogram_colour) \
         -smooth step

      #  Gaussian fit. Only enabled when requested.
      $xgraph_ element create gaussian_elem \
         -xdata $gxVector_ -ydata $giVector_ -symbol $symbol \
         -color $itk_option(-gaussian_histogram_colour)

      #  Main graph element.
      $xgraph_ element create elem \
         -xdata $xVector_ -ydata $iVector_ -symbol $symbol \
         -color $itk_option(-histogram_colour)

      #  Do the initial plot.
      add_notify_

      #  Add BLT features.
      ::Blt_ZoomStack $xgraph_
      ::Blt_ActiveLegend $xgraph_
      ::Blt_Crosshairs $xgraph_
      ::Blt_ClosestPoint $xgraph_
      bind bltCrosshairs$this <Any-Motion> [code $this dispXY_ %W %x %y]
      blt::AddBindTag $xgraph_ bltCrosshairs$this

   }

   #  Set/reset the notification call back on the rectangle.
   protected method add_notify_ {} {
      $itk_option(-canvasdraw) remove_notify_cmd $itk_option(-rect_id)
      $itk_option(-canvasdraw) add_notify_cmd $itk_option(-rect_id) \
         [code $this notify_cmd] $itk_option(-continuous_updates)
   }

   #  Deal with notification that rectangle has changed position. If
   #  the operation is "delete" (i.e. the rectangle has been removed)
   #  then the whole toolbox is deleted.
   public method notify_cmd {{op update}} {

      if { "$op" == "delete" } {
         configure -rect_id {}
         destroy $w_
         return 0
      }

      #  Previous vectors become last ones.
      if { $itk_option(-show_last_histogram) } {
         $xVector_ dup $last_xVector_
         $iVector_ dup $last_iVector_
      }

      set rowcut {}
      #  Set bounds of rectangle in image coordinates.
      set_image_bounds_

      #  Get the histogram.
      set results [$itk_option(-rtdimage) xyhistogram $xgraph_ elem \
                      $x0_ $y0_ $x1_ $y1_ image $itk_option(-use_data_limits) \
                      $factor_ $xVector_ $iVector_ $gxVector_ $giVector_]


      #  Kill gaussian if not wanted (XXX don't generate).
      if { ! $itk_option(-show_gaussian_histogram) } {
         $gxVector_ length 0
         $giVector_ length 0
      }

      $itk_component(intactual) config -value [lindex $results 1]
      $itk_component(countactual) config -value [lindex $results 2]
      $itk_component(intfit) config -value [lindex $results 3]
      $itk_component(countfit) config -value [lindex $results 4]
      $itk_component(fwhmfit) config -value [lindex $results 5]

      $itk_component(intgauss) config -value [lindex $results 6]
      $itk_component(intgausserr) config -value [lindex $results 7]
      $itk_component(sigmagauss) config -value [lindex $results 8]
      $itk_component(sigmagausserr) config -value [lindex $results 9]

      return 0
   }

   #  Set the image bounds to those of the current rectangle.
   protected method set_image_bounds_ {} {
      lassign [$itk_option(-canvas) coords $itk_option(-rect_id)] cx0_ cy0_ cx1_ cy1_
      $itk_option(-rtdimage) convert coords $cx0_ $cy0_ canvas x0_ y1_ image
      $itk_option(-rtdimage) convert coords $cx1_ $cy1_ canvas x1_ y0_ image

      #  Nearest image pixels.
      set x0_ [expr round($x0_)]
      set y0_ [expr round($y0_)]
      set x1_ [expr round($x1_)]
      set y1_ [expr round($y1_)]
   }

   #  Set the canvas bounds to those of the current image region.
   protected method set_canvas_bounds_ {} {
      $itk_option(-canvasdraw) deselect_objects
      $itk_option(-rtdimage) convert coords $x0_ $y0_ image cx0_ cy1_ canvas
      $itk_option(-rtdimage) convert coords $x1_ $y1_ image cx1_ cy0_ canvas
      $itk_option(-canvas) coords $itk_option(-rect_id) $cx0_ $cy0_ $cx1_ $cy1_
   }

   #  Display the X and Y position.
   protected method dispXY_ {w x y} {

      #  Update crosshair position.
      $w crosshairs configure -position @$x,$y

      #  Find the closest position and hence the current data value.
      #  If off the graph then do nothing.
      if { ![$w element closest $x $y result -interpolate yes -halo 10i -along x]} {
         return
      }
      $itk_component(uppercoord) config -value "$result(x)"
      $itk_component(uppervalue) config -value "$result(y)"
   }

   #  Add buttons to close window and make a hardcopy.
   protected method make_buttons_ {} {

      itk_component add bframe {
         frame $w_.buttons -borderwidth 4 -relief flat
      }
      itk_component add rframe {
         frame $w_.range -borderwidth 4 -relief flat
      }
      itk_component add lframe {
         frame $w_.logfile -borderwidth 4 -relief flat
      }

      #  Display coordinates of the region.
      set lwidth 5
      set vwidth 6
      itk_component add xmin {
         util::LabelValue $itk_component(rframe).xmin \
            -text "X min:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief flat \
            -anchor e \
            -value $x0_ \
            -textvariable [scope x0_]
      }
      itk_component add xmax {
         util::LabelValue $itk_component(rframe).xmax \
            -text "X max:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief flat \
            -anchor e \
            -value $x1_ \
            -textvariable [scope x1_]
      }
      itk_component add ymin {
         util::LabelValue $itk_component(rframe).ymin \
            -text "Y min:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief flat \
            -anchor e \
            -value $y0_ \
            -textvariable [scope y0_]
      }
      itk_component add ymax {
         util::LabelValue $itk_component(rframe).ymax \
            -text "Y max:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief flat \
            -anchor e \
            -value $y1_ \
            -textvariable [scope y1_]
      }

      #  Action buttons.
      itk_component add print {
         button $itk_component(bframe).print -text "Print..." \
            -command [code $this print]
      }
      add_short_help $itk_component(print) \
         {Print histogram to a printer or disk file}

      itk_component add refresh {
         button $itk_component(bframe).refresh -text "Refresh" \
            -command [code $this notify_cmd]
      }
      add_short_help $itk_component(refresh) \
         {Refresh histogram using current limits etc.}

      itk_component add close {
         button $itk_component(bframe).close -text "Close" \
            -command [code $this close]
      }
      add_short_help $itk_component(close) \
         {Close window}

      blt::blttable $itk_component(rframe) $itk_component(xmin) 0,0 -fill both
      blt::blttable $itk_component(rframe) $itk_component(xmax) 0,1 -fill both
      blt::blttable $itk_component(rframe) $itk_component(ymin) 0,2 -fill both
      blt::blttable $itk_component(rframe) $itk_component(ymax) 0,3 -fill both

      pack $itk_component(print) $itk_component(refresh) $itk_component(close) \
         -side left -expand 1 -padx 1m -pady 1m

      pack $itk_component(rframe) -side top -fill x
      pack $itk_component(bframe) -side bottom -fill x
   }

   #  Make a postscript copy of the histogram. XXX use simpler print option.
   public method print {} {
      utilReUseWidget gaia::MultiGraphPrint $w_.print -graphs $xgraph_
   }

   #  Restore the graphics rectangle.
   public method restore {} {

      #  If rect_id is still drawn, just need to update.
      if { $itk_option(-rect_id) != {} } {
         if { [$itk_option(-canvas) gettags $itk_option(-rect_id)] != {} } {
            notify_cmd
            return
         }
      }

      #  Else re-create the rectangle.
      $itk_option(-canvasdraw) set_drawing_mode rectangle [code $this restored_]
      $itk_option(-canvasdraw) create_object $cx0_ $cy0_
      $itk_option(-canvasdraw) create_done $cx0_ $cy0_
   }

   #  Restore of graphics object completed. Finish up by setting to the
   #  correct size and adding bindings.
   protected method restored_ {id args} {
      $itk_option(-canvasdraw) set_drawing_mode anyselect

      #  Assume the image has changed, so restore from image coordinates.
      $itk_option(-rtdimage) convert coords $x0_ $y0_ image cx0_ cy0_ canvas
      $itk_option(-rtdimage) convert coords $x1_ $y1_ image cx1_ cy1_ canvas

      $itk_option(-canvas) coords $id $cx0_ $cy0_ $cx1_ $cy1_
      configure -rect_id $id
      add_notify_
      notify_cmd
   }

   #  Toggle if the data limits are used
   protected method toggle_use_data_limits_ {} {
      notify_cmd
   }

   #  Toggle display of the gaussian histogram.
   protected method toggle_show_gaussian_histogram_ {} {
      if { ! $itk_option(-show_gaussian_histogram) } {
         $gxVector_ length 0
         $giVector_ length 0
      }
      notify_cmd
   }

   #  Toggle display of the last histogram.
   protected method toggle_show_last_histogram_ {} {
      if { ! $itk_option(-show_last_histogram) } {
         $last_xVector_ length 0
         $last_iVector_ length 0
      }
      notify_cmd
   }

   #  Set the colour of the histogram.
   protected method set_histogram_colour_ {colour} {
      configure -histogram_colour $colour
      $xgraph_ element configure elem -color $colour
   }

   #  Set the line width of the histogram.
   protected method set_histogram_width_ {width} {
      configure -histogram_width $width
      $xgraph_ element configure elem -linewidth $width
   }

   #  Set the colour of the gaussian histogram.
   protected method set_gaussian_histogram_colour_ {colour} {
      configure -gaussian_histogram_colour $colour
      $xgraph_ element configure gaussian_elem -color $colour
   }

   #  Set the line width of the gaussian histogram.
   protected method set_gaussian_histogram_width_ {width} {
      configure -gaussian_histogram_width $width
      $xgraph_ element configure gaussian_elem -linewidth $width
   }

   #  Set the colour of the last histogram.
   protected method set_last_histogram_colour_ {colour} {
      configure -last_histogram_colour $colour
      $xgraph_ element configure last_elem -color $colour
   }

   #  Set the line width of the last histogram.
   protected method set_last_histogram_width_ {width} {
      configure -last_histogram_width $width
      $xgraph_ element configure last_elem -linewidth $width
   }

   #  Set the colour of the graph peak lines.
   protected method set_peak_colour_ {colour} {
      configure -peak_colour $colour
      if { $xgraph_max_line_ != {} } {
         $xgraph_ marker configure $xgraph_max_line_ -outline $colour
      }
   }

   #  Set the line width of the graph peak lines.
   protected method set_peak_width_ {width} {
      configure -peak_width $width
      if { $xgraph_max_line_ != {} } {
         $xgraph_ marker configure $xgraph_max_line_ -linewidth $width
      }
   }

   #  Save histogram as a text file.
   protected method save_as_ {} {
      set w [FileSelect .\#auto -title "Save histogram to text file"]
      if { [$w activate] } {
         if { [catch {write_to_file_ [$w get]} msg] } {
            error_dialog "Failed to write histogram: $msg"
         }
      }
      destroy $w
   }

   #  Write histogram to a given text file. Use TOPCAT friendly format.
   public method write_to_file_ { filename } {
      if { $filename != {} } {

         #  Open the output file.
         set fid [::open $filename w]

         #  Write the header section.
         puts $fid "# Intensity  Count"

         #  Now write the values.
         set vl [$xVector_ length]
         for { set i 0 } {$i < $vl} {incr i} {
            puts $fid "[$xVector_ index $i] [$iVector_ index $i]"
         }

         #  Finally close the file.
         ::close $fid
      }
   }

   #  Set the binning factor.
   protected method set_binning_factor_ {value} {
      set factor_ $value
      notify_cmd
   }


   #  Configuration options: (public variables)
   #  ----------------------
   #  Name of canvas.
   itk_option define -canvas canvas Canvas {} {}

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Name of CanvasDraw widget.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  Canvas identifier of rectangle.
   itk_option define -rect_id rect_id Rect_Id {}

   #  Whether changes in position of rectangle are continuous. False as
   #  significant computation can be done.
   itk_option define -continuous_updates continuous_updates \
      Continuous_updates 0

   #  Whether to show the peak lines.
   itk_option define -show_peak_lines show_peak_lines Show_Peak_Lines 0

   #  Fonts used
   itk_option define -labelfont labelFont LabelFont TkDefaultFont
   itk_option define -valuefont valueFont ValueFont TkDefaultFont

   #  Colour for the histogram.
   itk_option define -histogram_colour histogram_colour Histogram_Colour blue

   #  Line width for the histograms.
   itk_option define -histogram_width histogram_width Histogram_Width 1

   #  Colour for graph peak lines.
   itk_option define -peak_colour peak_colour Peak_Colour black

   #  Width for graph peak lines.
   itk_option define -peak_width peak_width Peak_Width 1

   #  Colour for image peak lines.
   itk_option define -image_peak_colour image_peak_colour \
      Image_Peak_Colour blue

   #  Width for image peak lines.
   itk_option define -image_peak_width image_peak_width \
      Image_Peak_Width 1

   #  Colour for the gaussian histogram.
   itk_option define -gaussian_histogram_colour gaussian_histogram_colour \
      Gaussian_Histogram_Colour green

   #  Line width for the gaussian histogram.
   itk_option define -gaussian_histogram_width gaussian_histogram_width \
      Gaussian_Histogram_Width 1

   #  Whether to display the gaussian histogram fit for reference.
   itk_option define -show_gaussian_histogram show_gaussian_histogram \
      Show_Gaussian_Histogram 1

   #  Colour for the previous histograms.
   itk_option define -last_histogram_colour last_histogram_colour \
      Last_Histogram_Colour red

   #  Line width for the previous histograms.
   itk_option define -last_histogram_width last_histogram_width \
      Last_Histogram_Width 1

   #  Whether to display the previous histograms for reference.
   itk_option define -show_last_histograms show_last_histograms \
      Show_Last_Histograms 0

   #  Whether to use data limits for histogram.
   itk_option define -use_data_limits use_data_limits \
      Use_Data_Limits 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  X and Y blt graphs.
   protected variable xgraph_ {}

   #  X histogram BLT vectors.
   protected variable xVector_ {}
   protected variable iVector_ {}

   protected variable gxVector_ {}
   protected variable giVector_ {}

   protected variable last_xVector_ {}
   protected variable last_iVector_ {}

   #  Number of bins.
   protected variable numXValues_ 0

   #  Image coordinates of rectangle.
   protected variable x0_ 0
   protected variable x1_ 0
   protected variable y0_ 0
   protected variable y1_ 0

   #  Canvas coordinates of rectangle.
   protected variable cx0_ 0
   protected variable cx1_ 0
   protected variable cy0_ 0
   protected variable cy1_ 0

   #  Binning factor.
   protected variable factor_ 0.001

   #  Possible colours.
   protected variable simplecolours_ {
      "white" "black" "red" "green" "blue" "#0ff" "#f0f"
      "#ff0" "#f80" "#8f0" "#0f8" "#08f" "#80f"
      "#f08" "#512751275127" "#a8b4a8b4a8b4"
   }

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
