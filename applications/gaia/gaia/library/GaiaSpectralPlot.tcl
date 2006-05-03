#+
#  Name:
#     GaiaSpectralPlot

#  Type of Module:
#     [incr Tk] widget

#  Purpose:
#     Creates a window for controlling a spectral plot.

#  Description:
#     This class creates a top level window that displays and configures
#     a spectral_plot canvas item. The spectrum displayed is a 1D section
#     of an NDF. The section can also be displayed, in a small format, on
#     another canvas, and have a reference position displayed (usually the
#     coordinate of an image slice being displayed.

#  Invocations:
#
#        GaiaSpectralPlot object_name [configuration options]
#
#     This creates an instance of a GaiaPhotom object. The return is
#     the name of the object.
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
#     See itk_option definitions below.

#  Methods:
#     See declarations.

#  Inheritance:
#     TopLevelWidget.

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     06-MAR-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaSpectralPlot {}

itcl::class gaia::GaiaSpectralPlot {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Spectral plot ($itk_option(-number))"
      wm geometry $w_ 700x300

      #  When window is closed, we want to withdraw all graphics
      #  interactions.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]

      #  Add an options menu for setting options that should probably
      #  be defined once only per-session, or infrequently.
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button spectralplot "On Window..."

      #  Add print option.
      $File add command -label Print -command [code $this print] \
         -accelerator {Control-p}
      bind $w_ <Control-p> [code $this print]
      $short_help_win_ add_menu_short_help Print \
         {Print} {Print window to file or printer}

      #  Set the close menu item.
      $File add command -label Close -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]
      $short_help_win_ add_menu_short_help $File \
         {Close} {Close this window}

      #  Use log for pliot axes.
      $Options add checkbutton \
         -label {Log X axis} \
         -variable [scope itk_option(-log_x_axis)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this apply_gridoptions]
      add_menu_short_help $Options {Log X axis}  \
         {Try to use a log scale for X axis (fails when range includes zero)}

      $Options add checkbutton \
         -label {Log Y axis} \
         -variable [scope itk_option(-log_y_axis)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this apply_gridoptions]
      add_menu_short_help $Options {Log Y axis}  \
         {Try to use a log scale for Y axis (fails when range includes zero)}

      #  Whether to show the secondary plot or not. Also requires an
      #  extra canvas.
      #set show_plot2_ 0
      #$Options add checkbutton \
      #   -label {Show thumbnail plot} \
      #   -variable [scope show_plot2_] \
      #   -onvalue 1 \
      #   -offvalue 0 \
      #   -command [code $this toggle_show_plot2_]
      #add_menu_short_help $Options {Show thumbnail plot}  \
      #   {Show a thumbnail copy of plot in main window}

      #  Choose a colour for the spectral line.
      $Options add cascade -label "Line color" \
         -menu [menu $Options.line]
      foreach {index colour} $simplecolours_ {
         $Options.line add radiobutton \
             -background $colour \
             -variable [scope linecolour_] \
             -value $colour \
             -label {    } \
             -command [code $this set_linecolour $colour]
      }
      add_menu_short_help $Options {Line color} \
         {Change the colour of the spectral line}

      #  Choose a colour for the all the axis and labels.
      $Options add cascade -label "Axes color" \
         -menu [menu $Options.axes]
      foreach {index colour} $simplecolours_ {
         $Options.axes add radiobutton \
            -background $colour \
            -variable [scope axescolour_] \
            -value $index \
            -label {    } \
            -command [code $this set_axescolour $index]
      }
      add_menu_short_help $Options {Axes color} \
          {Change the colour of the plot axes}

      #  Choose a colour for the reference line.
      $Options add cascade -label "Ref line color" \
         -menu [menu $Options.refline]
      foreach {index colour} $simplecolours_ {
         $Options.refline add radiobutton \
            -background $colour \
            -variable [scope reflinecolour_] \
            -value $colour \
            -label {    } \
            -command [code $this set_reflinecolour $colour]
      }
      add_menu_short_help $Options {Ref line color} \
          {Change the colour of the reference line}

      #  Choose a colour for the background.
      $Options add cascade -label "Background" \
         -menu [menu $Options.back]
      foreach {index colour} $fullcolours_ {
         $Options.back add radiobutton \
            -background $colour \
            -variable [scope background_] \
            -value $colour \
            -label {    } \
            -command [code $this set_background $colour]
      }
      add_menu_short_help $Options {Background} \
         {Change the background colour of the plot} \

      #  Choose a font for axes labels.
      $Options add cascade -label "Axes font" \
         -menu [menu $Options.font]
      foreach {index xname desc} $fontmap_ {
         $Options.font add radiobutton \
            -variable [scope itk_option(-axes_font)] \
            -value $index \
            -font $xname \
            -label $desc \
            -command [code $this apply_gridoptions]
      }
      add_menu_short_help $Options {Axes font} \
         {Change the font used for axes labelling} \

      #  Create the canvas. Pack inside a frame so that we can get the resize
      #  events and the new geometry to get the apparent size of canvas right.
      itk_component add canvasframe {
         frame $w_.canvasframe -relief groove -bd 4
      }
      itk_component add canvas {
         ::canvas $itk_component(canvasframe).canvas
      }
      pack $itk_component(canvasframe) -fill both -expand 1
      pack $itk_component(canvas) -fill both -expand 1

      #  Make resizes, resize the canvas and arrange for a fit.
      bind $itk_component(canvas) <Configure> [code $this resize %w %h]

      #  Set first set of grid options.
      update_gridoptions_

      #  Add the graphics menu.
      make_graphics_menu_
   }

   #  Destructor:
   #  -----------
   destructor  {
      #  No need to fail now, application is exiting.
      catch {
         reset
      }
   }

   #  Methods:
   #  --------

   #  Close down.
   public method close {} {
      reset
      wm withdraw $w_
      if { $itk_option(-close_cmd) != {} } {
         eval $itk_option(-close_cmd)
      }
   }

   #  Reopen
   public method open {} {
      wm deiconify $w_
      if { $itk_option(-open_cmd) != {} } {
         eval $itk_option(-open_cmd)
      }
   }

   #  Reset so that a new spectrum will be created.
   public method reset {} {
      if { [info exists itk_component(canvas)] && $spectrum_ != {} } {
         $itk_component(canvas) delete $spectrum_
      }
      set spectrum_ {}

      #if { $itk_option(-canvas) != {} && $spectrum2_ != {} } {
      #   $itk_option(-canvas) delete $spectrum2_
      #}
      #set spectrum2_ {}

      if { [info exists itk_component(canvas)] && $ref_line_ != {} } {
         $itk_component(canvas) delete $ref_line_
      }
      set ref_line_ {}
   }

   #  Print canvas, use a print dialog.
   public method print {} {
      busy {
         utilReUseWidget gaia::GaiaCanvasPrint $w_.print \
            -canvas $itk_component(canvas) \
            -transient 1 \
            -pagewidth 7.5i
      }
   }

   #  Display a spectrum. The data is wrapped by an instance of GaiaNDAccess.
   #
   #  The axis defines the WCS axis that should be used for the plot X axis.
   #  If autoscale is true, then the plot should be rescaled so that the
   #  spectrum fits. Otherwise the existing plot bounds are used.
   #  The alow and ahigh arguments are the range along the axis to extract
   #  and p1 and p2 the positions of the spectrum along the remaining two axes.
   public method display {accessor axis alow ahigh p1 p2 autoscale
                          {x {}} {y {}}} {

      #  Get the spectral data from the accessor.
      #  XXXX Note assumes WCS & data array axes are aligned and in same order.
      lassign [$accessor getspectrum $axis $alow $ahigh $p1 $p2 1] adr

      #  Create the main spectral_plot.
      if { $spectrum_ == {} } {
         set autoscale 1
         set spectrum_ [$itk_component(canvas) create spectral_plot \
                           pointer $adr \
                           -x 25 -y 5 -width 650 -height 200 \
                           -linecolour $linecolour_ -linewidth 1 \
                           -gridoptions $gridoptions_ \
                           -showaxes 1]
         #  Clicking on the plot deselects other graphics.
         $itk_component(canvas) bind $spectrum_ <1> \
            +[code $itk_component(draw) deselect_objects]

         #  Make a reference line.
         make_ref_line_
         set_to_ref_coord_
         fitxy
      }

      #  Create the secondary plot, put this at the given x and y.
      #if { $itk_option(-canvas) != {} && $spectrum2_ == {} && $show_plot2_ } {
      #   set autoscale 1
      #   set spectrum2_ [$itk_option(-canvas) create spectral_plot \
      #                      pointer $adr \
      #                      -x $x -y $y -width 200 -height 200 \
      #                      -linecolour $linecolour_ -linewidth 1 \
      #                      -showaxes 0 -tags $itk_option(-ast_tag) \
      #                      -fixedscale 1]
      #}

      #  When autoscaling (or just created one of the plots), set the frameset
      #  and the NDF data units.
      if { $autoscale } {
         set frameset [$accessor getaxiswcs $axis [expr $alow -1]]
         $itk_component(canvas) itemconfigure $spectrum_ -frameset $frameset
         #if { $spectrum2_ != {} } {
         #   $itk_option(-canvas) itemconfigure $spectrum2_ -frameset $frameset
         #}
         $itk_component(canvas) itemconfigure $spectrum_ \
            -dataunits [$accessor getc units] \
            -datalabel [$accessor getc label]
      }

      #  Pass in the data.
      $itk_component(canvas) coords $spectrum_ pointer $adr
      #if { $spectrum2_ != {} } {
      #   $itk_option(-canvas) coords $spectrum2_ pointer $adr
      #
      #   #  Translate the secondary plot.
      #   if { $x != {} && $y != {} } {
      #      set dx [expr $x - [$itk_option(-canvas) itemcget $spectrum2_ -x]]
      #      set dy [expr $y - [$itk_option(-canvas) itemcget $spectrum2_ -y]]
      #      $itk_option(-canvas) move $spectrum2_ $dx $dy
      #   }
      #}

      #  Finished with the spectral data.
      $accessor release $adr
   }

   #  Make the reference line item. This should be refreshed to the reference
   #  coordinate as necessary.
   protected method make_ref_line_ {} {
      if { $itk_option(-show_ref_line) } {
         lassign [$itk_component(canvas) bbox $spectrum_] x0 y0 x1 y1
         if { $x0 != {} } {
            set ref_line_ [$itk_component(canvas) create line $x0 $y0 $x0 $y1 \
                              -width 1 -fill $reflinecolour_]
         }
      }
   }

   #  Set the reference line coordinate. Use a special coords that
   #  takes a world coordinate and returns a canvas coordinate.
   public method set_ref_coord {xcoord} {
      if { $spectrum_ != {} && $ref_line_ != {} } {
         set xref_ [$itk_component(canvas) coords $spectrum_ convert $xcoord]
         set_to_ref_coord_
      }
   }

   #  Move reference line to the reference coordinate.
   public method set_to_ref_coord_ {} {
      if { $spectrum_ != {} && $ref_line_ != {} } {
         lassign [$itk_component(canvas) coords $ref_line_] x0 y0 x1 y1
         $itk_component(canvas) move $ref_line_ [expr $xref_ - $x0] 0
      }
   }

   #  Make the spectral_plot item scale to fit the full size of the canvas.
   public method fitxy { args } {
      if { $spectrum_ != {} } {
         $itk_component(canvas) scale $spectrum_ -1 -1 -1 -1

         # Resize the reference line.
         if { $ref_line_ != {} } {
            $itk_component(canvas) delete $ref_line_
            make_ref_line_
            set_to_ref_coord_
         }

         # Fudge immediate update.
         $itk_component(canvas) itemconfigure $spectrum_ -showaxes 1
      }
   }

   #  Set the anchor point for the spectrum.
   public method anchor {x y} {
      $itk_component(canvas) itemconfigure $spectrum_ -x $x -y $y
   }

   #  Resize the canvas to fit the size of enclosing frame.
   public method resize {cw ch} {
      set fh [expr [winfo height $itk_component(canvasframe)]-10]
      if { $fh < $ch } {
         $itk_component(canvas) configure -height $fh
      }
      set fw [expr [winfo width $itk_component(canvasframe)]-10]
      if { $fw < $cw } {
         $itk_component(canvas) configure -width $fw
      }
      $itk_component(canvas) configure -scrollregion "0 0 $fw $fh"
      fitxy
   }

   #  Handle a change in the show_plot2_ variable.
   #protected method toggle_show_plot2_ {} {
   #   if { ! $show_plot2_ && $spectrum2_ != {} } {
   #      #  Make spectrum disappear.
   #      if { $itk_option(-canvas) != {} } {
   #         $itk_option(-canvas) delete $spectrum2_
   #      }
   #      set spectrum2_ {}
   #   }
   #}

   #  Apply the current grid options.
   public method apply_gridoptions {} {
      if { $spectrum_ != {} } {
         update_gridoptions_
         $itk_component(canvas) itemconfigure $spectrum_ \
            -gridoptions $gridoptions_
      }
   }

   #  Add the graphics menu and populate it.
   protected method make_graphics_menu_ {} {
      itk_component add draw {
         gaia::StarCanvasDraw $w_.draw \
            -canvas $itk_component(canvas) \
            -transient 1 \
            -center 0 \
            -withdraw 1 \
            -clipping 0 \
            -shorthelpwin $itk_option(-shorthelpwin) \
            -withtoolbox 1 \
            -show_object_menu 1 \
            -ignore_tag $itk_option(-ast_tag)
      }

      # Add menu and populate it.
      set Graphics [add_menubutton "Graphics" left]
      configure_menubutton Graphics -underline 0
      $itk_component(draw) add_menuitems $Graphics
   }

   #  Update the applicable gridoptions.
   protected method update_gridoptions_ {} {
      set gridoptions_ \
         "Grid=0,\
          DrawTitle=0,\
          Colour=$axescolour_,\
          LogPlot(1)=$itk_option(-log_x_axis),\
          LogPlot(2)=$itk_option(-log_y_axis),\
          Font=$itk_option(-axes_font)"
   }

   #  Set the colour of the spectral line.
   public method set_linecolour {colour} {
      set linecolour_ $colour
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -linecolour $linecolour_
      }
   }

   #  Set the colour of the axes.
   public method set_axescolour {colour} {
      set axescolour_ $colour
      apply_gridoptions
   }

   #  Set the colour of the reference line.
   public method set_reflinecolour {colour} {
      set reflinecolour_ $colour
      if { $ref_line_ != {} } {
         $itk_component(canvas) itemconfigure $ref_line_ \
            -fill $reflinecolour_
      }
   }

   #  Set the colour of the canvas background.
   public method set_background {colour} {
      $itk_component(canvas) configure -background $colour
      set background_ $colour
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The canvas, could be local or in the main image.
   itk_option define -canvas canvas Canvas {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Tag to use for any graphics. Matches the ast_tag value used in GAIA
   #  to avoid the main canvas scaling the plot.
   itk_option define -ast_tag ast_tag Ast_Tag ast_element

   #  Command to execute when the "open" method is invoked.
   itk_option define -open_cmd open_cmd Open_Cmd {}

   #  Command to execute when the "close" method is invoked.
   itk_option define -close_cmd close_cmd Close_Cmd {}

   #  Whether to show the coordinate ref line.
   itk_option define -show_ref_line show_ref_line \
      Show_Ref_Line 1

   #  Whether to log coordintae axis.
   itk_option define -log_x_axis log_x_axis Log_X_Axis 0

   #  Whether to log data axis.
   itk_option define -log_y_axis log_y_axis Log_Y_Axis 0

   #  The font to use when drawing the axes (AST integer).
   itk_option define -axes_font axes_font Axes_Font 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  The main spectral_plot item.
   protected variable spectrum_ {}

   #  Whether to show the secondary plot.
   #protected variable show_plot2_ 0

   #  The secondary spectral_plot item.
   #protected variable spectrum2_ {}

   #  The reference coordinate, canvas coords.
   protected variable xref_ 0

   #  Reference line item.
   protected variable ref_line_ {}

   #  Colours, two sets full and simple. These are the AST colours
   #  defined in grf_tkcan, plus some extra greys for the background
   #  choice. The grey indices are fakes, others are AST ones.
   protected variable fullcolours_ {
      0 "white"
      101 "grey90" 102 "grey80" 103 "grey70" 104 "grey60" 105 "grey50"
      106 "grey40" 107 "grey30" 108 "grey20" 109 "grey10"
      1 "black"
      2 "red" 3 "green" 4 "blue" 5 "#0ff" 6 "#f0f"
      7 "#ff0" 8 "#f80" 9 "#8f0" 10 "#0f8" 11 "#08f" 12 "#80f"
      13 "#f08" 14 "#512751275127" 15 "#a8b4a8b4a8b4"
   }

   #  AST colours
   protected variable simplecolours_ {
      0 "white" 1 "black" 2 "red" 3 "green" 4 "blue" 5 "#0ff" 6 "#f0f"
      7 "#ff0" 8 "#f80" 9 "#8f0" 10 "#0f8" 11 "#08f" 12 "#80f"
      13 "#f08" 14 "#512751275127" 15 "#a8b4a8b4a8b4"
   }

   #  Current colour of the line.
   protected variable linecolour_ "blue"

   #  Current background colour.
   protected variable background_ "black"

   #  Current axes colour.
   protected variable axescolour_ 0 ;     # AST index of white

   #  Current reference line colour.
   protected variable reflinecolour_ "red"

   #  The gridoptions that are currently in use.
   protected variable gridoptions_ {}

   #  Names of the fonts that we will use and their AST indices.
   #  A text string to very briefly describe the font is also set.
   #  XXX cut and paste from GaiaAstGrid.tcl, should share.
   protected variable fontmap_ {
      0  "-adobe-helvetica-medium-r-normal--*-140-*-*-*-*-*-*" "medium"
      1  "-adobe-helvetica-medium-o-normal--*-140-*-*-*-*-*-*" "medium"
      2  "-adobe-helvetica-bold-r-normal--*-140-*-*-*-*-*-*"   "bold"
      3  "-adobe-helvetica-bold-o-normal--*-140-*-*-*-*-*-*"   "bold"
      4  "-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-*-*" "medium"
      5  "-adobe-helvetica-medium-o-normal--*-120-*-*-*-*-*-*" "medium"
      6  "-adobe-helvetica-bold-r-normal--*-120-*-*-*-*-*-*"   "bold"
      7  "-adobe-helvetica-bold-o-normal--*-120-*-*-*-*-*-*"   "bold"
      8  "-adobe-times-medium-r-normal--*-120-*-*-*-*-*-*"     "medium"
      9  "-adobe-times-medium-i-normal--*-120-*-*-*-*-*-*"     "medium"
      10 "-adobe-times-bold-r-normal--*-120-*-*-*-*-*-*"       "bold"
      11 "-adobe-times-bold-i-normal--*-120-*-*-*-*-*-*"       "bold"
      12 "-adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*"       "fixed-width"
      13 "-adobe-courier-medium-o-*-*-*-120-*-*-*-*-*-*"       "fixed-width"
      14 "-adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*"         "fixed-width"
      15 "-adobe-courier-bold-o-*-*-*-120-*-*-*-*-*-*"         "fixed-width"
      17 "-adobe-helvetica-bold-r-*-*-20-120-*-*-*-*-*-*"      "large screen"
   }

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
