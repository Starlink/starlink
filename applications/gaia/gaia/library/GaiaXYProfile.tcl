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

      #  Set the initial corner coordinates of the rectangle.
      lassign [$itk_option(-canvas) coords $itk_option(-rect_id)] x0_ y0_ x1_ y1_

      #  Create the BLT graphs that display the profiles.
      make_graphs_

      #  Add the control panel and buttons.
      make_buttons_
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Remove BLT vectors.
      catch {
         blt::vector destroy $xxVector_ $xiVector_ $xdVector_
         blt::vector destroy $yyVector_ $yiVector_ $ydVector_
      }

      #  Remove rectangle.
      catch {
         $itk_option(-canvasdraw) remove_notify_cmd $itk_option(-rect_id)
         $itk_option(-canvasdraw) delete_object $itk_option(-rect_id)
      }
   }

   #  Methods:
   #  --------

   #  Close the window. Always destroy, unless in UKIRT mode, in which case we
   #  just withdraw.
   public method close {} {
      if { $itk_option(-ukirt_options) } {
         catch {$itk_option(-canvasdraw) delete_object $itk_option(-rect_id)}
         configure -rect_id {}
         wm withdraw $w_
      } else {
         destroy $w_
      } 
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
      blt::table $itk_component(uppertableframe) $itk_component(uppercoord) \
            0,0 -fill both
      blt::table $itk_component(uppertableframe) $itk_component(uppervalue) \
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

      blt::table $itk_component(uppertableframe) $itk_component(xpeakcoord) \
            1,0 -fill both
      blt::table $itk_component(uppertableframe) $itk_component(xpeakvalue) \
            1,1 -fill both

      #  Create the Y graph and add it to the lower pane.
      itk_component add ygraph {
         blt::graph $itk_component(ypane).ygraph \
            -width 400 \
            -height 225 \
            -borderwidth 3 \
            -relief groove \
            -title "Mean Y Profile" \
            -invertxy 1
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
      blt::table $itk_component(lowertableframe) $itk_component(lowercoord) \
            0,0 -fill both
      blt::table $itk_component(lowertableframe) $itk_component(lowervalue) \
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

      blt::table $itk_component(lowertableframe) $itk_component(ypeakcoord) \
            1,0 -fill both
      blt::table $itk_component(lowertableframe) $itk_component(ypeakvalue) \
            1,1 -fill both

      #  Set axes labels.
      $xgraph_ yaxis configure -title {}
      $xgraph_ xaxis configure -title {Distance along X}
      $ygraph_ yaxis configure -title {}
      $ygraph_ xaxis configure -title {Distance along Y}

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
      }
      set symbol {}
      $xgraph_ element create elem -xdata $xiVector_ -ydata $xdVector_ -symbol $symbol
      $ygraph_ element create elem -xdata $yiVector_ -ydata $ydVector_ -symbol $symbol

      #  Do the initial profile plot.
      add_notify_

      #  Add BLT features.
      ::Blt_ZoomStack $xgraph_
      ::Blt_ActiveLegend $xgraph_
      ::Blt_Crosshairs $xgraph_
      ::Blt_ClosestPoint $xgraph_
      bind bltCrosshairs$this <Any-Motion> [code $this dispXY %W %x %y]
      blt::AddBindTag $xgraph_ bltCrosshairs$this

      ::Blt_ZoomStack $ygraph_
      ::Blt_ActiveLegend $ygraph_
      ::Blt_Crosshairs $ygraph_
      ::Blt_ClosestPoint $ygraph_
      bind bltCrosshairs$this <Any-Motion> [code $this dispXY %W %x %y]
      blt::AddBindTag $ygraph_ bltCrosshairs$this

      #  Initialise first values.
      notify_cmd
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
   public method notify_cmd {{op update}} {
      if { "$op" == "delete" } {
         configure -rect_id {}
         if { ! $itk_option(-ukirt_options) } {
            destroy $w_
         }
         return 0
      }
      lassign [$itk_option(-canvas) coords $itk_option(-rect_id)] x0_ y0_ x1_ y1_

      #  Get the X and Y profile distributions.
      set nvals [$itk_option(-rtdimage) xyprofile $xgraph_ $ygraph_ elem \
                    $x0_ $y0_ $x1_ $y1_ canvas \
                    $xxVector_ $xiVector_ $xdVector_ \
                    $yyVector_ $yiVector_ $ydVector_]
      set numXValues_ [lindex $nvals 0]
      set numYValues_ [lindex $nvals 1]

      $xgraph_ xaxis configure -max $numXValues_
      $ygraph_ xaxis configure -max $numYValues_

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

      #  In UKIRT mode the peak column is shown in the main image as a line
      #  graphic.
      if { $itk_option(-ukirt_options) } {
         draw_image_line_ $xpeakcoord
      }
      return 0
   }

   #  Display the original X or Y position and the data value,
   #  depending on which graph we're moving around in.
   method dispXY {w x y} {

      #  Update crosshair position.
      $w crosshairs configure -position @$x,$y
      
      #  Find the closest position and hence the current data value.
      #  If off the graph then do nothing.
      set ret 0
      if { ![$w element closest $x $y "" -interpolate 1 -halo 10000]} {
         return
      }
      lassign [$w invtransform $x $y] index value
      set index [expr int(round($index))]

      #  Update the values according to which is the current graph.
      if { $w == $xgraph_ } {
         if {$index < 0 || $index >= $numXValues_} {
            return
         }
         catch {
            set x [$xxVector_ range $index $index]
            set y [$xdVector_ range $index $index]
            $itk_component(uppercoord) config -value "$x"
            $itk_component(uppervalue) config -value "$y"
         }
      } else {
         if {$index < 0 || $index >= $numYValues_} {
            return
         }
         catch {
            set x [$yyVector_ range $index $index]
            set y [$ydVector_ range $index $index]
            $itk_component(lowercoord) config -value "$x"
            $itk_component(lowervalue) config -value "$y"
         }
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
            LabelEntry $itk_component(lframe).comment \
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
         StarLabelCheck $itk_component(bframe).fix \
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

      blt::table $itk_component(rframe) $itk_component(xmin) 0,0 -fill both
      blt::table $itk_component(rframe) $itk_component(xmax) 0,1 -fill both
      blt::table $itk_component(rframe) $itk_component(ymin) 0,2 -fill both
      blt::table $itk_component(rframe) $itk_component(ymax) 0,3 -fill both

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

      #  Do nothing if currently withdrawn.
      if { [wm state $w_] == "withdrawn" } {
         return
      }

      #  If rect_id is still drawn, just need to update. 
      if { $itk_option(-rect_id) != {} } {
         if { [$itk_option(-canvas) gettags $itk_option(-rect_id)] != {} } {
            notify_cmd
            return
         }
      }

      #  Else re-create the rectangle.
      $itk_option(-canvasdraw) set_drawing_mode rectangle [code $this restored_]
      $itk_option(-canvasdraw) create_object $x0_ $y0_
      $itk_option(-canvasdraw) create_done $x0_ $y0_
   }

   #  Restore of graphics object completed. Finish up by setting to the
   #  correct size and adding bindings.
   protected method restored_ {id args} {
      $itk_option(-canvasdraw) set_drawing_mode anyselect
      $itk_option(-canvas) coords $id $x0_ $y0_ $x1_ $y1_
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
         $ygraph_ yaxis configure -min $ymin -max $ymax
      } else {
         $xgraph_ yaxis configure -min {} -max {}
         $ygraph_ yaxis configure -min {} -max {}
      }
   }

   #  Draw or update the line drawn in the main image that represents the
   #  UKIRT peakrow. "coord" should be in image coordinates.
   protected method draw_image_line_ {coord} {

      #  Do nothing if currently withdrawn.
      if { [wm state $w_] == "withdrawn" } {
         return
      }

      #  Get the canvas coordinates of this column in the image.
      $itk_option(-rtdimage) convert coords $coord 1 image column_ dummy canvas

      #  If line_id_ is still drawn, just need to update. 
      if { $line_id_ != {} } {
         if { [$itk_option(-canvas) gettags $line_id_] != {} } {
            $itk_option(-canvas) coords $line_id_ $column_ $y0_ $column_ $y1_
            return
         }
      }

      #  Else re-create the line.
      $itk_option(-canvasdraw) set_drawing_mode line [code $this drawn_image_line_]
      $itk_option(-canvasdraw) create_object $column_ $column_
      $itk_option(-canvasdraw) create_done $column_ $column_
   }

   #  Restore of image line completed. Finish up by setting to the
   #  correct column.
   protected method drawn_image_line_ {id args} {
      $itk_option(-canvasdraw) set_drawing_mode anyselect
      $itk_option(-canvas) coords $id $column_ $y0_ $column_ $y1_
      $itk_option(-canvas) itemconfigure $id -fill red
      set line_id_ $id
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

   #  Include the UKIRT quick look facilities.
   itk_option define -ukirt_options ukirt_options Ukirt_Options 0

   #  Fonts used
   itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*
   itk_option define -valuefont valueFont ValueFont -Adobe-helvetica-medium-r-normal-*-12*

   #  Protected variables: (available to instance)
   #  --------------------

   #  X and Y blt graphs.
   protected variable xgraph_ {}
   protected variable ygraph_ {}

   #  X profile BLT vectors.
   protected variable xxVector_ {}
   protected variable xiVector_ {}
   protected variable xdVector_ {}

   # Y profile BLT vectors.
   protected variable yyVector_ {}
   protected variable yiVector_ {}
   protected variable ydVector_ {}

   #  Number of positions in X and Y vectors.
   protected variable numXValues_ 0
   protected variable numYValues_ 0

   #  Canvas coordinates of rectangle.
   protected variable x0_ 0
   protected variable x1_ 0
   protected variable y0_ 0
   protected variable y1_ 0

   #  Whether to fix data ranges.
   protected variable fixed_ 0

   #  Canvas identifier of the UKIRT column shown in image.
   protected variable line_id_ {}

   #  Canvas coordinate of the UKIRT column shown in image.
   protected variable column_ 0

   #  Name of logfile.
   protected variable logfile_ "GaiaXYProfile.log"

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
