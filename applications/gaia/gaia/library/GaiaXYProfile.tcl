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
      lassign [$itk_option(-canvas) coords $itk_option(-rect_id)] x0 y0 x1 y1

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

   #  Close the window. Always destroy.
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
         ::panedwindow $w_.pane -width 5i -height 6i -orient vertical
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
            -width 5i \
            -height 3i \
            -borderwidth 3 \
            -relief groove \
            -title "Mean X Profile"
      } {}
      set xgraph_ $itk_component(xgraph)
      pack $itk_component(xgraph) -fill both -expand 1 -padx 1m -pady 1m
      add_short_help $itk_component(xgraph) \
         {Graph: average values along X, {bitmap dragb1} = zoom, {bitmap b2} = restore}

      #  Create the Y graph and add it to the lower pane.
      itk_component add ygraph {
         blt::graph $itk_component(ypane).ygraph \
            -width 3i \
            -height 5i \
            -borderwidth 3 \
            -relief groove \
            -title "Mean Y Profile" \
            -invertxy 1
      } {}
      set ygraph_ $itk_component(ygraph)
      pack $itk_component(ygraph) -fill both -expand 1 -padx 1m -pady 1m
      add_short_help $itk_component(ygraph) \
         {Graph: average values along Y, {bitmap dragb1} = zoom, {bitmap b2} = restore}

      #  Set axes labels.
      $xgraph_ yaxis configure -title {}
      $xgraph_ xaxis configure -title {Distance along X}
      $ygraph_ yaxis configure -title {}
      $ygraph_ xaxis configure -title {Distance along Y}

      #  Create vectors that contain profile coordinates.
      #  Vector names must start with a letter.
      regsub -all {\.} v$xgraph_.xxVector _ xxVector_
      regsub -all {\.} v$xgraph_.xiVector _ xiVector_
      regsub -all {\.} v$xgraph_.xdVector _ xdVector_

      regsub -all {\.} v$ygraph_.yyVector _ yyVector_
      regsub -all {\.} v$ygraph_.yiVector _ yiVector_
      regsub -all {\.} v$ygraph_.ydVector _ ydVector_

      $xgraph_ legend config -hide 1
      $ygraph_ legend config -hide 1
      if { ! [info exists $xxVector_] && ! [info exists $xiVector_] &&
           ! [info exists $xdVector_] && ! [info exists $yyVector_] &&
           ! [info exists $yiVector_] && ! [info exists $ydVector_] } {
         blt::vector create $xxVector_ $xiVector_ $xdVector_
         blt::vector create $yyVector_ $yiVector_ $ydVector_
      }
      set symbol {}
      $xgraph_ element create elem -xdata $xiVector_ -ydata $xdVector_ -symbol $symbol
      $ygraph_ element create elem -xdata $yiVector_ -ydata $ydVector_ -symbol $symbol

      #  Do the initial profile plot.
      add_notify_
      notify_cmd

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

      # Tk frame for position and data value readout.
      itk_component add fpos {
         frame $w_.fpos -relief flat
      }

      # Tk label for coordinate position.
      itk_component add coord {
         label $itk_component(fpos).coord -width 15 -anchor w
      }

      # Tk label for data value.
      itk_component add value {
         label $itk_component(fpos).value -width 15 -anchor w
      }

      pack $itk_component(coord) $itk_component(value) \
         -fill x -expand 1 -side left
      pack $itk_component(fpos) -fill none -expand 0
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
         destroy $w_
         return 0
      }
      lassign [$itk_option(-canvas) coords $itk_option(-rect_id)] x0 y0 x1 y1

      #  Get the X and Y profile distributions.
      set nvals [$itk_option(-rtdimage) xyprofile $xgraph_ $ygraph_ elem \
                    $x0 $y0 $x1 $y1 canvas \
                    $xxVector_ $xiVector_ $xdVector_ \
                    $yyVector_ $yiVector_ $ydVector_]
      set numXValues_ [lindex $nvals 0]
      set numYValues_ [lindex $nvals 1]

      $xgraph_ xaxis configure -max $numXValues_
      $ygraph_ xaxis configure -max $numYValues_
      return 0
   }

   #  Display the original X or Y position and the data value,
   #  depending on which graph were moving around in.
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
            $itk_component(coord) config -text "X: $x"
            $itk_component(value) config -text "Data: $y"
         }
      } else {
         if {$index < 0 || $index >= $numYValues_} {
            return
         }
         catch {
            set x [$yyVector_ range $index $index]
            set y [$ydVector_ range $index $index]
            $itk_component(coord) config -text "Y: $x"
            $itk_component(value) config -text "Data: $y"
         }
      }
   }

   #  Add buttons to close window and make a hardcopy of the profiles.
   protected method make_buttons_ {} {
      itk_component add bframe {
         frame $w_.buttons -borderwidth 2 -relief groove
      }
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

      pack $itk_component(print) $itk_component(close) \
         -side left -expand 1 -padx 2m -pady 2m
      pack $itk_component(bframe) -side bottom -fill x
   }

   #  Make a postscript copy of the profiles. This puts each profile
   #  on a separate page.
   public method print {} {
      utilReUseWidget gaia::MultiGraphPrint $w_.print \
         -graphs [list $xgraph_ $ygraph_]
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

   #  Canvas coordinate of the upper left-hand corner.
   itk_option define -x0 x0 X0 0

   #  Canvas coordinate of the upper left-hand corner.
   itk_option define -y0 y0 Y0 0

   #  Canvas coordinate of the lower right-hand corner.
   itk_option define -x1 x1 X1 0

   #  Canvas coordinate of the lower right-hand corner.
   itk_option define -y1 y1 Y1 0

   #  Whether changes in position of rectangle are continuous.
   itk_option define -continuous_updates continuous_updates \
      Continuous_updates 1

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

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
