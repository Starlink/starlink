#+
#  Name:
#     GaiaXYProfile

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Display total profiles of data in the X and Y directions of a
#     rectangular region of an image.

#  Description:
#     This class creates a toolbox that displays the cumilative
#     profiles of all the data in the X and Y directions of a
#     rectangular region on the image. The region is displayed as a
#     rectangle that can be dragged around the image. The profiles are
#     updated when the rectangle is dragged around the image.
#
#     When creating an instance of this class you must supply a canvas 
#     rectangle object (rect_id).

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

#  Configuration options:

#  Methods:

#  Inheritance:
#     This object inherits no other classes.

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
      wm title $w_ "GAIA: X and Y profile ($itk_option(-number))"

      #  Add the File menu
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0

      #  Add window help.
      global gaia_dir
      add_help_button $gaia_dir/GaiaXYProfile.hlp "On Window..."

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

      #  Set the initial corner coordinates of the rectangle.
      lassign [$itk_option(-canvas) coords $itk_option(-rect_id)] x0 y0 x1 y1

      #  Create the BLT graphs that display the profiles.
      make_graphs_

   }

   #  Destructor:
   #  -----------
   destructor  {
      
      #  Remove BLT vectors.
      blt::vector destroy $xxVector_ $xyVector_ $yxVector_ $yyVector_

      #  Remove rectangle.
      $itk_option(-canvasdraw) remove_notify_cmd $itk_option(-rect_id)
      $itk_option(-canvasdraw) delete_object $itk_option(-rect_id)

   }

   #  Methods:
   #  --------

   #  Close the window. 
   public method close {} {
      destroy $w_
   }

   #  Create the BLT graphs and vectors for displaying the profiles.
   protected method make_graphs_ {} {
      global ::tcl_version

      #  Add the X and Y graphs.
      itk_component add xgraph {
         blt::graph $w_.xgraph \
            -width 5i \
            -height 3i \
            -borderwidth 3 \
            -relief groove \
            -title "X Profile"
      } {}
      set xgraph_ $itk_component(xgraph)
      pack $itk_component(xgraph) -fill both -expand 1 -padx 1m -pady 1m
      add_short_help $itk_component(xgraph) \
         {Graph: average values along X, {bitmap dragb1} = zoom, {bitmap b2} = restore}

      itk_component add ygraph {
         blt::graph $w_.ygraph \
            -width 5i \
            -height 3i \
            -borderwidth 3 \
            -relief groove \
            -title "Y Profile"
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
      regsub -all {\.} v$xgraph_.xyVector _ xyVector_
      regsub -all {\.} v$ygraph_.yxVector _ yxVector_
      regsub -all {\.} v$ygraph_.yyVector _ yyVector_

      $xgraph_ legend config -hide 1
      $ygraph_ legend config -hide 1
      if { ! [info exists $xxVector_] && ! [info exists $xyVector_] &&
           ! [info exists $yxVector_] && ! [info exists $yyVector_] } {
         blt::vector create $xxVector_ $xyVector_
         blt::vector create $yxVector_ $yyVector_
      }
      set symbol {}
      $xgraph_ element create elem -xdata $xxVector_ -ydata $xyVector_ -symbol $symbol
      $ygraph_ element create elem -xdata $yxVector_ -ydata $yyVector_ -symbol $symbol

      #  Do the initial profile plot.
      $itk_option(-canvasdraw) add_notify_cmd $itk_option(-rect_id) \
         [code $this notify_cmd] 1
      notify_cmd

      #  Add BLT features.
      ::Blt_ZoomStack $xgraph_
      ::Blt_ActiveLegend $xgraph_
      ::Blt_Crosshairs $xgraph_
      ::Blt_ClosestPoint $xgraph_
      bind bltCrosshairs <Any-Motion> [code $this dispXY %x %y]

      ::Blt_ZoomStack $ygraph_
      ::Blt_ActiveLegend $ygraph_
      ::Blt_Crosshairs $ygraph_
      ::Blt_ClosestPoint $ygraph_
      bind bltCrosshairs <Any-Motion> [code $this dispXY %x %y]

      # X graph components: Tk frame for X,Y positions.
      itk_component add xfpos {
         frame $w_.xfpos -relief flat
      }

      # Tk label for X position.
      itk_component add xxpos {
         label $itk_component(xfpos).xxpos -width 15 -anchor w
      }

      # Tk label for Y position.
      itk_component add xyval {
         label $itk_component(xfpos).xyval -width 15 -anchor w
      }

      # Tk frame for Value positions.
      itk_component add xval {
         label $itk_component(xfpos).xval -width 15 -anchor w
      }
      pack $itk_component(xxpos) $itk_component(xyval) $itk_component(xval) \
         -fill x -expand 1 -side left
      pack $itk_component(xfpos) -fill none -expand 0

      # Ygraph components: Tk frame for X,Y positions.
      itk_component add yfpos {
         frame $w_.yfpos -relief flat
      }

      # Tk label for X position.
      itk_component add yxpos {
         label $itk_component(yfpos).yxpos -width 15 -anchor w
      }

      # Tk label for Y position.
      itk_component add yyval {
         label $itk_component(yfpos).yyval -width 15 -anchor w
      }

      # Tk frame for Value positions.
      itk_component add yval {
         label $itk_component(yfpos).yval -width 15 -anchor w
      }
      pack $itk_component(xxpos) $itk_component(yyval) $itk_component(yval) \
         -fill x -expand 1 -side left
      pack $itk_component(yfpos) -fill none -expand 0
   }

   #  Deal with notification that rectangle has changed position.
   public method notify_cmd {{op update}} {
      puts "notification of rectangle change"
      if { "$op" == "delete" } {
         destroy $w_
         return 0
      }
      lassign [$itk_option(-canvas) coords $itk_option(-rect_id)] x0 y0 x1 y1
      
      #  Get the X and Y profile distributions.
      set nvals [$itk_option(-rtdimage) xyprofile $xgraph_ $ygraph_ elem \
                    $x0 $y0 $x1 $y1 canvas \
                    $xxVector_ $xyVector_ $yxVector_ $yyVector_]
      puts "nvals = $nvals"
      set numXValues_ [lindex $nvals 0]
      set numYValues_ [lindex $nvals 1]
      
      $xgraph_ xaxis configure -max $numXValues_
      $ygraph_ xaxis configure -max $numYValues_
      return 0
   }

   #  Display the X and Y position.
   method dispXY {x y} {

      #  Update crosshair position.
      $xgraph_ crosshairs configure -position @$x,$y
      $ygraph_ crosshairs configure -position @$x,$y

      #  Now update the readout.
      set ret 0
      if { ![$xgraph_ element closest $x $y "" -interpolate 1 -halo 10000]} {
         return
      }
      lassign [$xgraph_ invtransform $x $y] index value
      set index [expr int(round($index))]
      if {$index < 0 || $index >= $numXValues_} {
         return
      }
      puts "X: $x, Y: $y"
      catch {
         set x [$xxVector_ range $index $index]
         set y [$xyVector_ range $index $index]
         $itk_component(xxpos) config -text "X: $x"
         $itk_component(xyval) config -text "Y: $y"
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------
   #  Name of canvas.
   itk_option define -canvas canvas Canvas {} {}

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Name of RtdImageCtrl widget or a derived class.
   itk_option define -image image Image {} {}

   #  Name of CanvasDraw widget.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

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

   #  Protected variables: (available to instance)
   #  --------------------

   #  X and Y blt graphs.
   protected variable xgraph_ {}
   protected variable ygraph_ {}

   #  BLT vectors.
   protected variable xxVector_ {}
   protected variable xyVector_ {}
   protected variable yxVector_ {}
   protected variable yyVector_ {}

   #  Number of positions in X and Y vectors.
   protected variable numXValues_ 0
   protected variable numYValues_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
