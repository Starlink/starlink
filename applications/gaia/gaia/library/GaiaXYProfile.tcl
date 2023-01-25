#+
#  Name:
#     GaiaXYProfile

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Display mean profile of data in the X and Y directions of a
#     rectangular region of an image.

#  Description:
#     This class creates a toolbox that displays the average
#     profiles of all the data in the X and Y directions of a
#     rectangular region on the image. The region is displayed as a
#     rectangle that can be dragged around the image The profiles are
#     updated when the rectangle is dragged around the image. When
#     creating an instance of this class you must supply a CanvasDraw
#     rectangle (option -rect_id).

#  Invocations:
#
#        GaiaXYProfile object_name [configuration options]
#
#     This creates an instance of a GaiaXYProfile object. The return is
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
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2008 Science and Technology Facilities Council.
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
#     10-JUL-2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaXYProfile {}

itcl::class gaia::GaiaXYProfile {

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
      wm title $w_ "GAIA: X-Y mean profiles ($itk_option(-number))"

      #  Add the File menu
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0

      #  Add the option menu.
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button xyprofileusage "On Window..."

      #  Add short help window.
      make_short_help

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

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
         {Change profiles during rectangle motion}

      #  Display peak lines.
      $Options add checkbutton -label {Display peak lines} \
         -variable [scope itk_option(-show_peak_lines)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_peak_lines_]
      $short_help_win_ add_menu_short_help $Options \
         {Display peak lines} \
         {Display the position of the peak values on main image}

      #  Display the previous profiles.
      $Options add checkbutton -label {Display previous profile} \
         -variable [scope itk_option(-show_last_profiles)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_show_last_profiles_]
      $short_help_win_ add_menu_short_help $Options \
         {Display previous profile} \
         {Display the previous profiles, not much use for interactive work}

      #  Colour options.
      $Options add cascade -label {Profile color} \
         -menu [menu $Options.profile]
      foreach colour $simplecolours_ {
         $Options.profile add radiobutton \
             -background $colour \
             -variable [scope itk_option(-profile_colour)] \
             -value $colour \
             -label {    } \
             -command [code $this set_profile_colour_ $colour]
      }
      add_menu_short_help $Options {Profile color} \
         {Change the colour of the profiles}

      $Options add cascade -label {Previous profile color} \
         -menu [menu $Options.lastprofile]
      foreach colour $simplecolours_ {
         $Options.lastprofile add radiobutton \
             -background $colour \
             -variable [scope itk_option(-last_profile_colour)] \
             -value $colour \
             -label {    } \
             -command [code $this set_last_profile_colour_ $colour]
      }
      add_menu_short_help $Options {Previous profile color} \
         {Change the colour of the previous profiles}

      $Options add cascade -label {Peak line color} \
         -menu [menu $Options.peakcolour]
      foreach colour $simplecolours_ {
         $Options.peakcolour add radiobutton \
             -background $colour \
             -variable [scope itk_option(-peak_colour)] \
             -value $colour \
             -label {    } \
             -command [code $this set_peak_colour_ $colour]
      }
      add_menu_short_help $Options {Peak line color} \
         {Change the colour of peak lines}

      $Options add cascade -label {Image peak line color} \
         -menu [menu $Options.imagepeakcolour]
      foreach colour $simplecolours_ {
         $Options.imagepeakcolour add radiobutton \
             -background $colour \
             -variable [scope itk_option(-image_peak_colour)] \
             -value $colour \
             -label {    } \
             -command [code $this set_image_peak_colour_ $colour]
      }
      add_menu_short_help $Options {Image peak line color} \
         {Change the colour of peak lines shown in image display}

      #  Line width options.
      $Options add cascade -label {Profile width} \
         -menu [menu $Options.profilewidth]
      foreach i {1 2 3 4} {
         $Options.profilewidth add radiobutton \
            -value $i \
            -bitmap width$i \
            -variable [scope itk_option(-profile_width)] \
            -command [code $this set_profile_width_ $i]
      }
      add_menu_short_help $Options {Profile width} \
         {Set width of lines used to draw profiles}

      $Options add cascade -label {Previous profile width} \
         -menu [menu $Options.lastwidth]
      foreach i {1 2 3 4} {
         $Options.lastwidth add radiobutton \
            -value $i \
            -bitmap width$i \
            -variable [scope itk_option(-last_profile_width)] \
            -command [code $this set_last_profile_width_ $i]
      }
      add_menu_short_help $Options {Previous profile width} \
         {Set width of lines used to draw previous profiles}

      $Options add cascade -label {Peak line width} \
         -menu [menu $Options.peakwidth]
      foreach i {1 2 3 4} {
         $Options.peakwidth add radiobutton \
            -value $i \
            -bitmap width$i \
            -variable [scope itk_option(-peak_width)] \
            -command [code $this set_peak_width_ $i]
      }
      add_menu_short_help $Options {Peak line width} \
         {Set width of peak lines drawn in graphs}

      $Options add cascade -label {Image peak line width} \
         -menu [menu $Options.imagepeakwidth]
      foreach i {1 2 3 4} {
         $Options.imagepeakwidth add radiobutton \
            -value $i \
            -bitmap width$i \
            -variable [scope itk_option(-image_peak_width)] \
            -command [code $this set_image_peak_width_ $i]
      }
      add_menu_short_help $Options {Image peak line width} \
         {Set width of peak lines drawn in main image}

      #  Set the initial corner coordinates of the rectangle.
      set_image_bounds_

      #  Create the BLT graphs that display the profiles.
      make_graphs_

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
         blt::vector destroy $xxVector_ $xiVector_ $xdVector_
         blt::vector destroy $yyVector_ $yiVector_ $ydVector_
      }
      catch {
         blt::vector destroy $last_xxVector_ $last_xdVector_
         blt::vector destroy $last_yyVector_ $last_ydVector_
      }
      catch {
         blt::vector destroy $dxVector_ $diVector_ $ddVector_
      }

      #  Remove rectangle.
      catch {
         $itk_option(-canvasdraw) remove_notify_cmd $itk_option(-rect_id)
         $itk_option(-canvasdraw) delete_object $itk_option(-rect_id)
      }

      #  Remove column and row lines.
      catch {
         if { $xline_id_ != {} } {
            $itk_option(-canvasdraw) delete_object $xline_id_
         }
         if { $yline_id_ != {} } {
            $itk_option(-canvasdraw) delete_object $yline_id_
         }
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

   #  Create the BLT graphs and vectors for displaying the profiles.
   protected method make_graphs_ {} {

      #  Create a panedwindow to display the two graphs.
      itk_component add pane {
         ::panedwindow $w_.pane -width 400 -height 600 -orient vertical
      } {
      }
      pack $itk_component(pane) -fill both -expand 1 -padx 1m -pady 1m

      itk_component add xpane {
         ::frame $w_.xpane
      }
      itk_component add ypane {
         ::frame $w_.ypane
      }
      $itk_component(pane) add $itk_component(xpane) $itk_component(ypane)

      #  Create the X graph and add it to the upper pane.
      itk_component add xgraph {
         blt::graph $itk_component(xpane).xgraph \
            -width 400 \
            -height 225 \
            -borderwidth 3 \
            -relief groove \
            -title "Mean X Profile"
      } {}
      set xgraph_ $itk_component(xgraph)
      pack $itk_component(xgraph) -fill both -expand 1 -padx 1m -pady 1m
      add_short_help $itk_component(xgraph) \
         {Graph: average values along X, {bitmap dragb1} = zoom, {bitmap b2} = restore}

      #  Readouts for this graph.
      #  Table for current values.
      itk_component add uppertableframe {
         frame $itk_component(xpane).uppertableframe \
            -relief flat -borderwidth 1
      }
      pack $itk_component(uppertableframe) -fill none -expand 0

      #  Readout coordinate from current graph.
      set lwidth 12
      set vwidth 8
      itk_component add uppercoord {
         util::LabelValue $itk_component(uppertableframe).coord \
            -text "Readout coord:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief groove \
            -anchor e
      }

      #  Readout value from current graph.
      itk_component add uppervalue {
         util::LabelValue $itk_component(uppertableframe).value \
            -text "value:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief groove \
            -anchor e
      }
      blt::blttable $itk_component(uppertableframe) $itk_component(uppercoord) \
            0,0 -fill both
      blt::blttable $itk_component(uppertableframe) $itk_component(uppervalue) \
            0,1 -fill both

      #  Display X peak coordinate.
      itk_component add xpeakcoord {
         util::LabelValue $itk_component(uppertableframe).xpeakcoord \
            -text "Peak coord:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief flat \
            -anchor e
      }

      #  X peak value.
      itk_component add xpeakvalue {
         util::LabelValue $itk_component(uppertableframe).xpeakvalue \
            -text "value:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief flat \
            -anchor e
      }

      blt::blttable $itk_component(uppertableframe) $itk_component(xpeakcoord) \
            1,0 -fill both
      blt::blttable $itk_component(uppertableframe) $itk_component(xpeakvalue) \
            1,1 -fill both

      #  Create the Y graph and add it to the lower pane.
      itk_component add ygraph {
         blt::graph $itk_component(ypane).ygraph \
            -width 400 \
            -height 225 \
            -borderwidth 3 \
            -relief groove \
            -title "Mean Y Profile"
      } {}
      set ygraph_ $itk_component(ygraph)
      pack $itk_component(ygraph) -fill both -expand 1 -padx 1m -pady 1m
      add_short_help $itk_component(ygraph) \
         {Graph: average values along Y, {bitmap dragb1} = zoom, {bitmap b2} = restore}

      #  Readouts for this graph.
      #  Table for current values.
      itk_component add lowertableframe {
         frame $itk_component(ypane).lowertableframe \
            -relief flat -borderwidth 1
      }
      pack $itk_component(lowertableframe) -fill none -expand 0

      #  Readout coordinate from current graph.
      itk_component add lowercoord {
         util::LabelValue $itk_component(lowertableframe).coord \
            -text "Readout coord:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief groove \
            -anchor e
      }

      #  Readout value from current graph.
      itk_component add lowervalue {
         util::LabelValue $itk_component(lowertableframe).value \
            -text "value:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief groove \
            -anchor e
      }
      blt::blttable $itk_component(lowertableframe) $itk_component(lowercoord) \
            0,0 -fill both
      blt::blttable $itk_component(lowertableframe) $itk_component(lowervalue) \
            0,1 -fill both

      #  Y peak coordinate.
      itk_component add ypeakcoord {
         util::LabelValue $itk_component(lowertableframe).ypeakcoord \
            -text "Peak coord:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief flat \
            -anchor e
      }

      #  Y peak value.
      itk_component add ypeakvalue {
         util::LabelValue $itk_component(lowertableframe).ypeakvalue \
            -text "value:" \
            -labelfont $itk_option(-labelfont) \
            -valuefont $itk_option(-valuefont) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -relief flat \
            -anchor e
      }

      blt::blttable $itk_component(lowertableframe) $itk_component(ypeakcoord) \
            1,0 -fill both
      blt::blttable $itk_component(lowertableframe) $itk_component(ypeakvalue) \
            1,1 -fill both

      #  Set axes labels.
      $xgraph_ yaxis configure -title {}
      $xgraph_ xaxis configure -title {X coordinate}

      $ygraph_ yaxis configure -title {Y coordinate}
      $ygraph_ xaxis configure -title {}

      #  Create vectors that contain profile coordinates.
      $xgraph_ legend config -hide 1
      $ygraph_ legend config -hide 1
      if { ! [info exists $xxVector_] && ! [info exists $xiVector_] &&
           ! [info exists $xdVector_] && ! [info exists $yyVector_] &&
           ! [info exists $yiVector_] && ! [info exists $ydVector_] } {

         set xxVector_ [blt::vector create \#auto]
         set xiVector_ [blt::vector create \#auto]
         set xdVector_ [blt::vector create \#auto]
         set yyVector_ [blt::vector create \#auto]
         set yiVector_ [blt::vector create \#auto]
         set ydVector_ [blt::vector create \#auto]

         #  Copies for displaying last extractions as well.
         set last_xxVector_ [blt::vector create \#auto]
         set last_xdVector_ [blt::vector create \#auto]
         set last_yyVector_ [blt::vector create \#auto]
         set last_ydVector_ [blt::vector create \#auto]

         #  Dummies for throwing away.
         set dxVector_ [blt::vector create \#auto]
         set diVector_ [blt::vector create \#auto]
         set ddVector_ [blt::vector create \#auto]
      }
      set symbol {}
      $xgraph_ element create elem \
         -xdata $xxVector_ -ydata $xdVector_ -symbol $symbol \
         -color $itk_option(-profile_colour)
      $ygraph_ element create elem \
         -xdata $ydVector_ -ydata $yyVector_ -symbol $symbol \
         -color $itk_option(-profile_colour)

      #  Last vectors for comparison. Only enabled when requested.
      $xgraph_ element create last_elem \
         -xdata $last_xxVector_ -ydata $last_xdVector_ -symbol $symbol \
         -color $itk_option(-last_profile_colour)
      $ygraph_ element create last_elem \
         -xdata $last_ydVector_ -ydata $last_yyVector_ -symbol $symbol \
         -color $itk_option(-last_profile_colour)

      #  Do the initial profile plot.
      add_notify_

      #  Add BLT features.
      ::Blt_ZoomStack $xgraph_
      ::Blt_ActiveLegend $xgraph_
      ::Blt_Crosshairs $xgraph_
      ::Blt_ClosestPoint $xgraph_
      bind bltCrosshairs$this <Any-Motion> [code $this dispXY_ %W %x %y]
      blt::AddBindTag $xgraph_ bltCrosshairs$this

      ::Blt_ZoomStack $ygraph_
      ::Blt_ActiveLegend $ygraph_
      ::Blt_Crosshairs $ygraph_
      ::Blt_ClosestPoint $ygraph_
      bind bltCrosshairs$this <Any-Motion> [code $this dispXY_ %W %x %y]
      blt::AddBindTag $ygraph_ bltCrosshairs$this
   }

   #  Set/reset the notification call back on the rectangle.
   protected method add_notify_ {} {
      $itk_option(-canvasdraw) remove_notify_cmd $itk_option(-rect_id)
      $itk_option(-canvasdraw) add_notify_cmd $itk_option(-rect_id) \
         [code $this notify_cmd] $itk_option(-continuous_updates)
   }

   #  Deal with notification that rectangle has changed position. If
   #  the operation is "delete" (i.e. the rectangle has been removed)
   #  then the whole toolbox is deleted, unless we're in UKIRT mode.
   #  In that case we do nothing and the interface should be woken
   #  by a call to restore when a new image (or image event) happens.
   #
   #  If the op is "realtime" (should only be used when in UKIRT mode)
   #  then we check the appropriate members for the realtime position of the
   #  rectangle and the rowcut. When the rowcut is set the X profile
   #  displayed is just from that row, not the mean profile.
   #
   public method notify_cmd {{op update}} {

      if { "$op" == "delete" } {
         configure -rect_id {}
         if { ! $itk_option(-ukirt_options) } {
            destroy $w_
         }
         return 0
      }

      #  Previous vectors become last ones.
      if { $itk_option(-show_last_profiles) } {
         $xxVector_ dup $last_xxVector_
         $xdVector_ dup $last_xdVector_
         $yyVector_ dup $last_yyVector_
         $ydVector_ dup $last_ydVector_
      }

      set rowcut {}
      if { "$op" == "realtime" } {
         catch {
            set var $itk_option(-rtdimage)
            global ::$var
            set x0_ [set ${var}(X0)]
            set x1_ [set ${var}(X1)]
            set y0_ [set ${var}(Y0)]
            set y1_ [set ${var}(Y1)]
            set rowcut [set ${var}(ROWCUT)]

            #  Nearest image pixels.
            set x0_ [expr round($x0_)]
            set y0_ [expr round($y0_)]
            set x1_ [expr round($x1_)]
            set y1_ [expr round($y1_)]
            set rowcut [expr round($rowcut)]

            #  Set bounds of rectangle, and transform to canvas coordinates.
            set_canvas_bounds_
         }
      } else {
         #  Set bounds of rectangle in image coordinates.
         set_image_bounds_
      }

      #  Get the X and Y profile distributions.
      set nvals [$itk_option(-rtdimage) xyprofile $xgraph_ $ygraph_ elem \
                    $x0_ $y0_ $x1_ $y1_ image \
                    $xxVector_ $xiVector_ $xdVector_ \
                    $yyVector_ $yiVector_ $ydVector_]
      set numXValues_ [lindex $nvals 0]
      set numYValues_ [lindex $nvals 1]

      #  If we have a rowcut, then UKIRT just wants that row as the X profile,
      #  not the collapsed version. Y profile is just emptied into dummy
      #  vectors.
      if { "$op" == "realtime" && $rowcut != {} } {
         set nvals [$itk_option(-rtdimage) xyprofile $xgraph_ $ygraph_ elem \
                       $x0_ $rowcut $x1_ $rowcut image \
                       $xxVector_ $xiVector_ $xdVector_ \
                       $dxVector_ $diVector_ $ddVector_]

         #  Change graph title to make this clear.
         $xgraph_ configure -title "X Profile at Row: $rowcut"
      } else {
         $xgraph_ configure -title "Mean X Profile"
      }

      #  Update the simple statistics. Peak value and position.
      if { ! [info exists itk_component(xpeakcoord)] } {
         return 0
      }
      $xdVector_ variable vec
      set xpeakvalue $vec(max)
      set index [lindex [$xdVector_ search $xpeakvalue] 0]
      set xpeakcoord [$xxVector_ range $index $index]

      $ydVector_ variable vec
      set ypeakvalue $vec(max)
      set index [lindex [$ydVector_ search $ypeakvalue] 0]
      set ypeakcoord [$yyVector_ range $index $index]

      $itk_component(xpeakcoord) config -value "$xpeakcoord"
      $itk_component(xpeakvalue) config -value "$xpeakvalue"
      $itk_component(ypeakcoord) config -value "$ypeakcoord"
      $itk_component(ypeakvalue) config -value "$ypeakvalue"

      #  If requested he peak column and row are shown in the main image as
      #  line graphics. When a realtime event is received, this may also set
      #  the row.
      if { $itk_option(-show_peak_lines) } {
         if { "$op" == "realtime" && $rowcut != {} } {
            draw_peak_lines_ $xpeakcoord $rowcut
         } else {
            draw_peak_lines_ $xpeakcoord $ypeakcoord
         }
      }
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

   #  Display the original X or Y position and the data value,
   #  depending on which graph we're moving around in.
   protected method dispXY_ {w x y} {

      #  Update crosshair position.
      $w crosshairs configure -position @$x,$y

      #  Update the values according to which is the current graph.
      if { $w == $xgraph_ } {

         #  Find the closest position and hence the current data value.
         #  If off the graph then do nothing.
         if { ![$w element closest $x $y result -interpolate yes -halo 10i -along x]} {
            return
         }
         $itk_component(uppercoord) config -value "$result(x)"
         $itk_component(uppervalue) config -value "$result(y)"
      } else {

         #  Find the closest position and hence the current data value.
         #  If off the graph then do nothing.
         if { ![$w element closest $x $y result -interpolate yes -halo 10i -along y]} {
            return
         }
         $itk_component(lowercoord) config -value "$result(y)"
         $itk_component(lowervalue) config -value "$result(x)"
      }
   }

   #  Add buttons to close window and make a hardcopy of the profiles.
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

      #  Logfile controls. Only for UKIRT.
      if { $itk_option(-ukirt_options) } {
         set lwidth 7
         itk_component add logfile {
            LabelFileChooser $itk_component(lframe).logfile \
               -labelwidth $lwidth \
               -text "Logfile:" \
               -anchor e \
               -textvariable [scope logfile_] \
               -value "$logfile_"
         }
         add_short_help $itk_component(logfile) \
            {Filename for saving window values, created if doesn't exist}

         #  And the comment field.
         itk_component add comment {
            util::LabelEntry $itk_component(lframe).comment \
               -labelwidth $lwidth \
               -text "Comment:" \
               -anchor e \
         }
         add_short_help $itk_component(comment) \
            {Comment to add with results in log file}

         #  Add a save/append button.
         itk_component add save {
            button $itk_component(lframe).save  \
               -text "Save/Append" \
               -width 12 \
               -command [code $this save_log]
         }
         add_short_help $itk_component(save) \
            {Append values to log file, created if doesn't exist}
      }

      #  Action buttons.
      itk_component add fix {
         gaia::StarLabelCheck $itk_component(bframe).fix \
            -text "Fix data range:" \
            -onvalue 1 -offvalue 0 \
            -variable [scope fixed_] \
            -anchor e \
            -command [code $this toggle_fix_data_range_]
      }
      add_short_help $itk_component(fix) {Fix data range to the current limits}

      itk_component add print {
         button $itk_component(bframe).print -text "Print..." \
            -command [code $this print]
      }
      add_short_help $itk_component(print) \
         {Print mean profiles to a printer or disk file}

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

      if { $itk_option(-ukirt_options) } {
         pack $itk_component(logfile) -side top -fill x -expand 1
         pack $itk_component(comment) -side left -fill x -expand 1
         pack $itk_component(save) -side right -expand 0 -fill x -padx 1m -pady 1m
      }

      pack $itk_component(fix) $itk_component(print) $itk_component(close) \
         -side left -expand 1 -padx 1m -pady 1m

      pack $itk_component(rframe) -side top -fill x
      if { $itk_option(-ukirt_options) } {
         pack $itk_component(lframe) -side top -fill x
      }
      pack $itk_component(bframe) -side bottom -fill x
   }

   #  Make a postscript copy of the profiles. This puts each profile
   #  on a separate page.
   public method print {} {
      utilReUseWidget gaia::MultiGraphPrint $w_.print \
         -graphs [list $xgraph_ $ygraph_]
   }

   #  Save current settings to a log file. Appends if the file already exists.
   public method save_log {} {
      set init 0
      if { ! [file exists "$logfile_"] } {
         set init 1
      }
      if { [catch {set fd [::open "$logfile_" a+]} msg] } {
         error_dialog $msg
         return
      }

      #  If new file then add headers.
      if { $init } {
         puts $fd "# x0 \t x1 \t y0 \t y1 \t X peak \t X peak pos \t Y peak \t Y peak pos"
      }

      #  Add the comment and time stamp.
      set comment [$itk_component(comment) get]
      if { "$comment" != "" } {
         puts $fd "# $comment"
      }
      set time [clock format [clock seconds] -format {%A %B %d %Y - %H:%M:%S}]
      puts $fd "# $time"

      #  Write values.
      set xpeakvalue [$itk_component(xpeakvalue) get]
      set xpeakcoord [$itk_component(xpeakcoord) get]
      set ypeakvalue [$itk_component(ypeakvalue) get]
      set ypeakcoord [$itk_component(ypeakcoord) get]
      puts $fd "$x0_ \t $x1_ \t $y0_ \t $y1_ \t $xpeakvalue \t $xpeakcoord \t $ypeakvalue \t $ypeakcoord"

      ::close $fd
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

   #  Toggle whether to fix the Y axes ranges to the current limits.
   protected method toggle_fix_data_range_ {} {
      if { $fixed_ } {
         $xdVector_ variable vec
         set xmin $vec(min)
         set xmax $vec(max)
         $ydVector_ variable vec
         set ymin $vec(min)
         set ymax $vec(max)

         $xgraph_ yaxis configure -min $xmin -max $xmax
         $ygraph_ xaxis configure -min $ymin -max $ymax
      } else {
         $xgraph_ yaxis configure -min {} -max {}
         $ygraph_ xaxis configure -min {} -max {}
      }
   }

   #  Draw or update the lines drawn in the main image that represent the
   #  positions of the peak values, also updates the line markers showing
   #  these positions in the graphs. When in UKIRT mode ycoord will be
   #  the peakrow value. Both coordinates should be in image coordinates.
   protected method draw_peak_lines_ {xcoord ycoord} {

      #  Update the marker lines.
      if { $xgraph_max_line_ == {} } {
         set xgraph_max_line_ \
            [$xgraph_ marker create line -outline $itk_option(-peak_colour)]
         set ygraph_max_line_ \
            [$ygraph_ marker create line -outline $itk_option(-peak_colour)]
      }
      $xgraph_ marker configure $xgraph_max_line_ \
         -coords "$xcoord -Inf $xcoord +Inf"
      $ygraph_ marker configure $ygraph_max_line_ \
         -coords "-Inf $ycoord +Inf $ycoord"

      #  Get the canvas coordinates of this column and row in the image.
      $itk_option(-rtdimage) convert coords \
         $xcoord $ycoord image column_ row_ canvas

      #  If the image lines are drawn, just need to update.
      if { $xline_id_ != {} } {
         if { [$itk_option(-canvas) gettags $xline_id_] != {} } {
            $itk_option(-canvas) coords \
               $xline_id_ $column_ $cy0_ $column_ $cy1_
            if { $yline_id_ != {} } {
               if { [$itk_option(-canvas) gettags $yline_id_] != {} } {
                  $itk_option(-canvas) coords \
                     $yline_id_ $cx0_ $row_ $cx1_ $row_
               }
            }
            return
         }
      }

      #  Else re-create the lines.
      $itk_option(-canvasdraw) set_drawing_mode line [code $this drawn_ximage_line_]
      $itk_option(-canvasdraw) create_object $column_ $column_
      $itk_option(-canvasdraw) create_done $column_ $column_

      $itk_option(-canvasdraw) set_drawing_mode line [code $this drawn_yimage_line_]
      $itk_option(-canvasdraw) create_object $row_ $row_
      $itk_option(-canvasdraw) create_done $row_ $row_
   }

   #  Toggle display of peak lines.
   protected method toggle_peak_lines_ {} {
      if { $itk_option(-show_peak_lines) } {
         #  Cause an update with will redraw.
         notify_cmd
      } else {
         #  Remove lines.
         if { $xline_id_ != {} } {
            $itk_option(-canvasdraw) delete_object $xline_id_
         }
         if { $yline_id_ != {} } {
            $itk_option(-canvasdraw) delete_object $yline_id_
         }
         set xline_id_ {}
         set yline_id_ {}
      }
   }

   #  Toggle display of the last profiles.
   protected method toggle_show_last_profiles_ {} {
      if { ! $itk_option(-show_last_profiles) } {
         $last_xxVector_ length 0
         $last_xdVector_ length 0
         $last_yyVector_ length 0
         $last_ydVector_ length 0
      }
      notify_cmd
   }

   #  Restore of x image line completed. Finish up by setting to the
   #  correct column.
   protected method drawn_ximage_line_ {id args} {
      $itk_option(-canvasdraw) set_drawing_mode anyselect
      $itk_option(-canvas) coords $id $column_ $cy0_ $column_ $cy1_
      $itk_option(-canvas) itemconfigure $id \
         -fill $itk_option(-image_peak_colour)
      set xline_id_ $id
   }

   #  Restore of y image line completed. Finish up by setting to the
   #  correct column.
   protected method drawn_yimage_line_ {id args} {
      $itk_option(-canvasdraw) set_drawing_mode anyselect
      $itk_option(-canvas) coords $id $cx0_ $row_ $cx1_ $row_
      $itk_option(-canvas) itemconfigure $id \
         -fill $itk_option(-image_peak_colour)
      set yline_id_ $id
   }

   #  Set the colour of the profiles.
   protected method set_profile_colour_ {colour} {
      configure -profile_colour $colour
      $xgraph_ element configure elem -color $colour
      $ygraph_ element configure elem -color $colour
   }

   #  Set the line width of the profiles.
   protected method set_profile_width_ {width} {
      configure -profile_width $width
      $xgraph_ element configure elem -linewidth $width
      $ygraph_ element configure elem -linewidth $width
   }

   #  Set the colour of the last profiles.
   protected method set_last_profile_colour_ {colour} {
      configure -last_profile_colour $colour
      $xgraph_ element configure last_elem -color $colour
      $ygraph_ element configure last_elem -color $colour
   }

   #  Set the line width of the last profiles.
   protected method set_last_profile_width_ {width} {
      configure -last_profile_width $width
      $xgraph_ element configure last_elem -linewidth $width
      $ygraph_ element configure last_elem -linewidth $width
   }

   #  Set the colour of the graph peak lines.
   protected method set_peak_colour_ {colour} {
      configure -peak_colour $colour
      if { $xgraph_max_line_ != {} } {
         $xgraph_ marker configure $xgraph_max_line_ -outline $colour
         $ygraph_ marker configure $ygraph_max_line_ -outline $colour
      }
   }

   #  Set the line width of the graph peak lines.
   protected method set_peak_width_ {width} {
      configure -peak_width $width
      if { $xgraph_max_line_ != {} } {
         $xgraph_ marker configure $xgraph_max_line_ -linewidth $width
         $ygraph_ marker configure $ygraph_max_line_ -linewidth $width
      }
   }

   #  Set the colour of the image peak lines.
   protected method set_image_peak_colour_ {colour} {
      configure -image_peak_colour $colour
      if { $xline_id_ != {} } {
         $itk_option(-canvas) itemconfigure $xline_id_ -fill $colour
         $itk_option(-canvas) itemconfigure $yline_id_ -fill $colour
      }
   }

   #  Set the width of the image peak lines.
   protected method set_image_peak_width_ {width} {
      configure -image_peak_width $width
      if { $xline_id_ != {} } {
         $itk_option(-canvas) itemconfigure $xline_id_ -width $width
         $itk_option(-canvas) itemconfigure $yline_id_ -width $width
      }
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

   #  Whether changes in position of rectangle are continuous.
   itk_option define -continuous_updates continuous_updates \
      Continuous_updates 1

   #  Include the UKIRT quick look facilities. Changes show peaks lines,
   #  whether the log saving is shown, what happens when the
   #  rectangle is deleted (nothing if true) and the display of the
   #  last profiles.
   itk_option define -ukirt_options ukirt_options Ukirt_Options 0 {
      configure -show_peak_lines $itk_option(-ukirt_options)
      configure -show_last_profiles $itk_option(-ukirt_options)
   }

   #  Whether to show the peak lines.
   itk_option define -show_peak_lines show_peak_lines Show_Peak_Lines 0

   #  Fonts used
   itk_option define -labelfont labelFont LabelFont TkDefaultFont
   itk_option define -valuefont valueFont ValueFont TkDefaultFont

   #  Colour for the profiles.
   itk_option define -profile_colour profile_colour Profile_Colour blue

   #  Line width for the profiles.
   itk_option define -profile_width profile_width Profile_Width 1

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

   #  Colour for the previous profiles.
   itk_option define -last_profile_colour last_profile_colour \
      Last_Profile_Colour red

   #  Line width for the previous profiles.
   itk_option define -last_profile_width last_profile_width \
         Last_Profile_Width 1

   #  Whether to display the previous profiles for reference.
   itk_option define -show_last_profiles show_last_profiles \
      Show_Last_Profiles 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  X and Y blt graphs.
   protected variable xgraph_ {}
   protected variable ygraph_ {}

   #  X profile BLT vectors.
   protected variable xxVector_ {}
   protected variable xiVector_ {}
   protected variable xdVector_ {}

   protected variable last_xxVector_ {}
   protected variable last_xdVector_ {}

   #  Y profile BLT vectors.
   protected variable yyVector_ {}
   protected variable yiVector_ {}
   protected variable ydVector_ {}

   protected variable last_yyVector_ {}
   protected variable last_ydVector_ {}

   #  Dummy vectors.
   protected variable dxVector_ {}
   protected variable diVector_ {}
   protected variable ddVector_ {}

   #  Max and min graph lines.
   protected variable xgraph_max_line_ {}
   protected variable ygraph_max_line_ {}

   #  Number of positions in X and Y vectors.
   protected variable numXValues_ 0
   protected variable numYValues_ 0

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

   #  Whether to fix data ranges.
   protected variable fixed_ 0

   #  Canvas identifier of the UKIRT column and row lines shown in image.
   protected variable xline_id_ {}
   protected variable yline_id_ {}

   #  Canvas coordinate of the UKIRT column and row shown in image.
   protected variable column_ 0
   protected variable row_ 0

   #  Name of logfile.
   protected variable logfile_ "GaiaXYProfile.log"

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
