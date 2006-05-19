#+
#  Name:
#     GaiaSpectralPlot

#  Type of Module:
#     [incr Tk] widget

#  Purpose:
#     Creates a window for controlling a spectral plot.

#  Description:
#     This class creates a top level window that displays and configures
#     a spectral_plot canvas item. This has a main spectrum and a reference
#     spectrum, both of which can be controlled in this tool. The spectral
#     data are provided wrapped in an instance of GaiaNDAccess, delivered
#     in the various display* methods.
#
#     The presentation of the spectra are controlled via various options in
#     menus, which include whether or not to autoscale and what colours to use
#     and provide many graphical elements that can be used to annotate the
#     plot.  It is also possible to print the current display to a postscript
#     printer or file and convert between various spectral coordinate systems.
#
#     Special positions in the spectrum can be identified using reference
#     lines (these are drawn from the top to the bottom of the plot). There
#     can be as many of these lines as needed.

#  Invocations:
#
#        GaiaSpectralPlot object_name [configuration options]
#
#     This creates an instance of a GaiaSpectralPlot object. The return is
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

#  Copyright:
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

      #  Whether to constantly update the spectrum data limits.
      $Options add checkbutton -label "Autoscale" \
         -variable [scope itk_option(-autoscale)] \
         -onvalue 1 \
         -offvalue 0
      add_menu_short_help $Options {Autoscale}  \
         {Continuously change data limits of spectral plot,
            otherwise fixed by last click (faster)}

      #  Remove the reference spectral from plot.
      $Options add command -label "Remove ref spec" \
         -command [code $this remove_reference]
      add_menu_short_help $Options {Remove ref spec}  \
         {Remove the reference spectrum from the plot}

      #  Whether to draw the X coordinates running min to max, or
      #  as they are from the "front" to "back" of cube.
      $Options add checkbutton \
         -label {X min to max} \
         -variable [scope itk_option(-xminmax)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this set_xminmax]
      add_menu_short_help $Options {X min to max}  \
         {Show X coordinates as minimum to maximum,
            otherwise cube "front" to "back" order}

      #  Use log for plot axes.
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

      #  Choose a colour for the reference spectrum.
      $Options add cascade -label "Ref spec color" \
         -menu [menu $Options.refspec]
      foreach {index colour} $simplecolours_ {
         $Options.refspec add radiobutton \
            -background $colour \
            -variable [scope refspeccolour_] \
            -value $colour \
            -label {    } \
            -command [code $this set_refspeccolour $colour]
      }
      add_menu_short_help $Options {Ref spec color} \
          {Change the colour of the reference spectrum}

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

      #  If we have a GaiaSpecCoords instance use that to create a menu for
      #  selecting a coordinate system.
      if { $itk_option(-spec_coords) != {} } {
         set SpectralCoords [add_menubutton "Coords" left]
         configure_menubutton "Coords" -underline 0
         $itk_option(-spec_coords) add_menu $SpectralCoords
      }

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

      if { [info exists itk_component(canvas)] } {
         if { [::array exists ref_lines_] } {
            foreach {id} [::array names ref_lines_] {
               $itk_component(draw) delete_object $ref_lines_($id)
            }
            unset ref_lines_
         }
      }
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

   #  Add the graphics menu and populate it.
   protected method make_graphics_menu_ {} {
      itk_component add draw {
         gaia::StarCanvasDraw $w_.draw \
            -canvas $itk_component(canvas) \
            -rtdimage $this \
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

   #  Display a spectrum. The data is wrapped by an instance of GaiaNDAccess.
   #
   #  The axis defines the WCS axis that should be used for the plot X axis.
   #  If autoscale is true, then the plot should be rescaled so that the
   #  spectrum fits. Otherwise the existing plot bounds are used (unless the
   #  local autoscale option is enabled, that takes precendence).  The alow and
   #  ahigh arguments are the range along the axis to extract and p1 and p2 the
   #  positions of the spectrum along the remaining two axes.
   public method display {accessor axis alow ahigh p1 p2 autoscale} {

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
                           -showaxes 1 -xminmax $itk_option(-xminmax)\
                           -reflinecolour $refspeccolour_]
         #  Clicking on the plot deselects other graphics.
         $itk_component(canvas) bind $spectrum_ <1> \
            +[code $itk_component(draw) deselect_objects]

         #  Make the base reference line (always have one of these).
         fitxy
         make_ref_line 1
         set_to_ref_coord_ 1
      }

      #  When autoscaling (or just created one of the plots), set the frameset
      #  and the NDF data units.
      if { $autoscale || $itk_option(-autoscale) } {
         set frameset [$accessor getaxiswcs $axis [expr $alow -1]]
         $itk_component(canvas) itemconfigure $spectrum_ -frameset $frameset
         $itk_component(canvas) itemconfigure $spectrum_ \
            -dataunits [$accessor getc units] \
            -datalabel [$accessor getc label]
      }

      #  Pass in the data.
      $itk_component(canvas) coords $spectrum_ pointer $adr

      #  Finished with the spectral data.
      $accessor release $adr
   }

   #  Display the reference spectrum.
   #
   #  All the given details should correspond to those given to the display
   #  method.
   public method display_reference {accessor axis alow ahigh p1 p2} {

      if { $spectrum_ == {} } {
         return
      }

      #  Get the spectral data from the accessor.
      lassign [$accessor getspectrum $axis $alow $ahigh $p1 $p2 1] adr

      #  Pass in the data.
      $itk_component(canvas) coords $spectrum_ refpointer $adr

      #  Finished with the spectral data.
      $accessor release $adr
   }

   #  Remove the reference spectrum, if displayed.
   public method remove_reference {} {

      if { $spectrum_ == {} } {
         return
      }
      $itk_component(canvas) coords $spectrum_ refpointer 0
   }


   #  Make a reference line item. This should be refreshed to the reference
   #  coordinate as necessary.
   public method make_ref_line {id} {
      lassign [$itk_component(canvas) bbox $spectrum_] x0 y0 x1 y1
      if { $x0 != {} } {
         remove_ref_line $id

         #  Make this create a canvasdraw column object...
         $itk_component(draw) set_drawing_mode column \
            [code $this created_ref_line_ $id]
         $itk_component(draw) create_object $x0 $y0
         $itk_component(draw) create_done $x0 $y0
      }
   }

   #  Called when the creation of a reference line is completed. Records the
   #  canvas id and sets the colour. Adds a notification call so we can see
   #  when the line is dragged or deleted.
   protected method created_ref_line_ {id cid args} {
      set ref_lines_($id) $cid
      $itk_component(canvas) itemconfigure $cid -fill $reflinecolour_

      $itk_component(draw) add_notify_cmd $cid \
         [code $this update_ref_line_ $id] 1

      #  Add an additional binding to see when the button is released. Use
      #  this to signal a drag stop.
      $itk_component(canvas) bind $cid <ButtonRelease-1> \
         "+[code $this update_ref_line_ $id released]"

      if { ! [info exists xref_($id)] } {
         set xref_($id) 0
      }
   }

   #  Called when a reference line is updated, that's moved on the canvas or
   #  deleted.
   protected method update_ref_line_ {id mode} {

      if { $mode == "move" || $mode == "released" } {
         #  Convert canvas to axis coordinate and return this if a command has
         #  been supplied. Note that the axis coordinate is return in the
         #  equivalent of NDF pixel indices for convenience.
         if { $itk_option(-ref_changed_cmd) != {} } {

            lassign [$itk_component(canvas) coords $ref_lines_($id)] x0 y0 x1 y1
            
            #  From canvas to world.
            set x0 [$itk_component(canvas) coords $spectrum_ convert 1 $x0]

            #  This is the current world coord, so record it.
            set xref_($id) $x0

            #  World to grid.
            set frameset [$itk_component(canvas) itemcget $spectrum_ -frameset]
            set ind [gaiautils::getaxiscoord $frameset $x0 0]

            eval $itk_option(-ref_changed_cmd) $id $ind $mode
         }

      } elseif { $mode == "delete" } {
         unset ref_lines_($id)
      }
   }

   #  Remove a reference line item.
   public method remove_ref_line {id} {
      if { [info exists ref_lines_($id)] } {
         $itk_component(draw) delete_object $ref_lines_($id)
      }
   }

   #  Set the coordinate of a reference line and optionally creates it.
   public method set_ref_coord {id xcoord} {
      if { $spectrum_ != {} } {
         if { ! [info exists ref_lines_($id)] } {
            make_ref_line $id
         }
         
         #  Don't move unnecessarily.
         if { $xref_($id) != $xcoord } {
            set xref_($id) $xcoord
            set_to_ref_coord_ $id
         }
      }
   }

   #  Move reference line to the reference coordinate. Reference coordinate in
   #  world coordinates.
   public method set_to_ref_coord_ {id} {
      if { $spectrum_ != {} && [info exists ref_lines_($id)] } {
         lassign [$itk_component(canvas) coords $ref_lines_($id)] x0 y0 x1 y1
         set cx [$itk_component(canvas) coords $spectrum_ convert 0 $xref_($id)]
         $itk_component(canvas) move $ref_lines_($id) [expr $cx - $x0] 0
      }
   }

   #  Make the spectral_plot item scale to fit the full size of the canvas.
   public method fitxy { args } {
      if { $spectrum_ != {} } {
         $itk_component(canvas) scale $spectrum_ -1 -1 -1 -1

         #  Resize the reference lines.
         if { [::array exists ref_lines_] } {
            foreach {id} [::array names ref_lines_] {
               $itk_component(draw) delete_object $ref_lines_($id)
               make_ref_line $id
               set_to_ref_coord_ $id
            }
         }

         # Fudge immediate update.
         $itk_component(canvas) itemconfigure $spectrum_ -showaxes 1
      }
   }

   #  Set the anchor point for the spectrum.
   public method anchor {x y} {
      $itk_component(canvas) itemconfigure $spectrum_ -x $x -y $y
   }

   #  Set the xminmax value of spectrum.
   public method set_xminmax {} {
      $itk_component(canvas) itemconfigure \
         $spectrum_ -xminmax $itk_option(-xminmax)
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

   #  Apply the current grid options.
   public method apply_gridoptions {} {
      if { $spectrum_ != {} } {
         update_gridoptions_
         $itk_component(canvas) itemconfigure $spectrum_ \
            -gridoptions $gridoptions_
      }
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
      if { [::array exists ref_lines_] } {
         foreach {id} [::array names ref_lines_] {
            $itk_component(canvas) itemconfigure $ref_lines_($id) \
               -fill $reflinecolour_
         }
      }
   }

   #  Set the colour of the reference spectrum.
   public method set_refspeccolour {colour} {
      set refspeccolour_ $colour
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -reflinecolour $refspeccolour_
      }
   }

   #  Set the colour of the canvas background.
   public method set_background {colour} {
      $itk_component(canvas) configure -background $colour
      set background_ $colour
   }

   #  "rtdimage" emulation, needed to support some actions of the
   #  StarCanvasDraw instance (items that require width and height
   #  information).
   public method scale {} {
      return 1
   }
   public method dispwidth {} {
      return [winfo width $itk_component(canvasframe)]
   }
   public method dispheight {} {
      return [winfo height $itk_component(canvasframe)]
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Does spectral plot auto-update ranges.
   itk_option define -autoscale autoscale AutoScale 0

   #  Tag to use for any graphics. Matches the ast_tag value used in GAIA
   #  to avoid the main canvas scaling the plot.
   itk_option define -ast_tag ast_tag Ast_Tag ast_element

   #  Command to execute when the "open" method is invoked.
   itk_option define -open_cmd open_cmd Open_Cmd {}

   #  Command to execute when the "close" method is invoked.
   itk_option define -close_cmd close_cmd Close_Cmd {}

   #  Whether to order X coordinates to run from min to max, or the cube
   #  order.
   itk_option define -xminmax xminmax XMinMax 1

   #  Whether to log coordintae axis.
   itk_option define -log_x_axis log_x_axis Log_X_Axis 0

   #  Whether to log data axis.
   itk_option define -log_y_axis log_y_axis Log_Y_Axis 0

   #  The font to use when drawing the axes (AST integer).
   itk_option define -axes_font axes_font Axes_Font 0

   #  A GaiaSpecCoords object for controlling the coordinate systems.
   #  Actual control is handled by the GaiaCube instance.
   itk_option define -spec_coords spec_coords Spec_Coords {}

   #  A command to invoke when a reference line is moved (by interaction by
   #  the user). The command will be trailed by the reference line id and 
   #  the new world coordinate.
   itk_option define -ref_changed_cmd ref_changed_cmd Ref_Changed_Cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  The main spectral_plot item.
   protected variable spectrum_ {}

   #  The reference coordinates, canvas coords, indexed by id.
   protected variable xref_

   #  All reference line canvas ids, indexed by a user specified number.
   protected variable ref_lines_

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

   #  Current reference spectrum colour.
   protected variable refspeccolour_ "green"

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
