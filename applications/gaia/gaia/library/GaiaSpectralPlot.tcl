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
#     Copyright (C) 2008-2009, 2012 Science and Technology Facilities Council.
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
#     MJC: Malcolm J. Currie (JAC, Hawaii)
#     {enter_new_authors_here}

#  History:
#     06-MAR-2006 (PWD):
#        Original version.
#     2012 April 20 (MJC):
#        Add standard-of-rest menu.
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

      #  Set colour of reference line. Done here as array.
      set ref_lines_colour_(1) "red"

      #  And restore properties. These apply to local variables, not
      #  configuration options, so harder work...
      set props_ [gaia::GaiaProperties::instance]
      set keys [$props_ get_named_keys GaiaSpectralPlot]
      if { $keys != {} } {
         foreach key $keys {
            set value [$props_ get_property $key]
            if { $value != {} } {
               set lkey [$props_ get_unnamed_key GaiaSpectralPlot $key]
               eval set $lkey $value
            }
         }
      }

      #  Set the top-level window title.
      wm title $w_ "GAIA: Spectral plot ($itk_option(-number))"
      wm geometry $w_ 700x300

      #  When window is closed, we want to withdraw all graphics
      #  interactions.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]

      #  Add an options menu for setting options that should probably
      #  be defined once only per-session, or infrequently.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button spectralplot "On Window..."

      #  If we have a GaiaSpecWriter instance then use that to add the
      #  various save as options to this menu. The GaiaSpecWriter has
      #  access to the original data, important to preserve as much
      #  information as possible.
      if { $itk_option(-spec_writer) != {} } {
         {*}$itk_option(-spec_writer) set_menu $File
      }

      #  Add print option.
      $File add command -label Print -command [code $this print] \
         -accelerator {Control-p}
      bind $w_ <Control-p> [code $this print]
      $short_help_win_ add_menu_short_help $File \
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

      #  Whether to display error bars.
      $Options add checkbutton \
         -label {Error bars} \
         -variable [scope use_errors_] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this set_use_errors_]
      add_menu_short_help $Options {Error bars}  \
         {If available use data variances to display error bars}

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
         -label {Positive X only} \
         -variable [scope itk_option(-xpositive)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this set_xpositive]
      add_menu_short_help $Options {Positive X only}  \
         {Only range positive X coordinates (useful when Log X axis enabled)}

      $Options add checkbutton \
         -label {Log Y axis} \
         -variable [scope itk_option(-log_y_axis)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this apply_gridoptions]
      add_menu_short_help $Options {Log Y axis}  \
         {Try to use a log scale for Y axis (fails when range includes zero)}

      $Options add checkbutton \
         -label {Positive Y only} \
         -variable [scope itk_option(-ypositive)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this set_ypositive]
      add_menu_short_help $Options {Positive Y only}  \
         {Only range positive Y coordinates (useful when Log Y axis enabled)}

      #   Display reference line coordinate.
      $Options add checkbutton \
         -label {Reference coordinate label} \
         -variable [scope show_ref_label_] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_show_ref_label_]
      add_menu_short_help $Options {Reference coordinate label}  \
         {Show the coordinate of the reference line}

      #   Display coordinate label for the extraction position.
      $Options add checkbutton \
         -label {Extraction coordinate label} \
         -variable [scope show_label_] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_show_label_]
      add_menu_short_help $Options {Extraction coordinate label}  \
         {Show the extraction coordinates of the spectrum}

      #   Show prefix as part of the coordinate label.
      $Options add checkbutton \
         -label {Full coordinate label} \
         -variable [scope show_full_label_] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_show_label_]
      add_menu_short_help $Options {Full coordinate label}  \
         {Prefix extraction coordinates of the spectrum with filename}

      #  Reset all saved attributes to their defaults.
      $Options add command -label "Reset" \
         -command [code $this reset_options_]
      add_menu_short_help $Options {Reset}  \
         {Reset any saved options to their default value}

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

      #  Choose a width for the spectral line.
      $Options add cascade -label "Line width" \
         -menu [menu $Options.width]
      foreach width {1 2 3 4 } {
         $Options.width add radiobutton \
             -variable [scope linewidth_] \
             -value $width \
             -bitmap width$width \
             -command [code $this set_linewidth $width]
      }
      add_menu_short_help $Options {Line width} \
         {Change the width of the spectral line}

      #  Choose a colour for the error bars.
      $Options add cascade -label "Error color" \
         -menu [menu $Options.errorcolour]
      foreach {index colour} $simplecolours_ {
         $Options.errorcolour add radiobutton \
             -background $colour \
             -variable [scope errorcolour_] \
             -value $colour \
             -label {    } \
             -command [code $this set_errorcolour $colour]
      }
      add_menu_short_help $Options {Error color} \
         {Change the colour of the error bars}

      #  Number of sigma for the error bars.
      $Options add cascade -label "Error length" \
         -menu [menu $Options.errorlength]
      foreach nsigma {1 2 3 4 5} {
         $Options.errorlength add radiobutton \
            -variable [scope errornsigma_] \
            -value $nsigma \
            -label $nsigma \
            -command [code $this set_errornsigma $nsigma]
      }
      add_menu_short_help $Options {Error length} \
         {Length of error bars, multiple of standard deviation}

      #  Choose a frequency for the error bars.
      $Options add cascade -label "Error frequency" \
         -menu [menu $Options.errorfreq]
      foreach freq {1 2 5 10 20 50} {
         $Options.errorfreq add radiobutton \
            -variable [scope errorfreq_] \
            -value $freq \
            -label $freq \
            -command [code $this set_errorfreq $freq]
      }
      add_menu_short_help $Options {Error frequency} \
         {Frequency for error bars, one every n is displayed}

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

      #  Choose a colour for the main reference line.
      $Options add cascade -label "Ref line color" \
         -menu [menu $Options.refline]
      foreach {index colour} $simplecolours_ {
         $Options.refline add radiobutton \
            -background $colour \
            -variable [scope ref_lines_colour_(1)] \
            -value $colour \
            -label {    } \
            -command [code $this set_ref_line_colour 1 $colour]
      }
      add_menu_short_help $Options {Ref line color} \
          {Change the colour of the image slice reference line}

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
         {Change the background colour of the plot}

      #  Choose a font for axes labels.
      $Options add cascade -label "Axes font" \
         -menu [menu $Options.font]
      foreach {index xname desc} $fontmap_ {
         $Options.font add radiobutton \
            -variable [scope axes_font_] \
            -value $index \
            -font $xname \
            -label $desc \
            -command [code $this set_axes_font $index]
      }
      add_menu_short_help $Options {Axes font} \
         {Change the font used for axes labelling} \

      #  If we have a GaiaSpecCoords instance use that to create a menu for
      #  selecting a coordinate system.
      if { $itk_option(-spec_coords) != {} } {
         set SpectralCoords [add_menubutton "Coords/StdOfRest"]
         configure_menubutton "Coords/StdOfRest" -underline 0
         {*}$itk_option(-spec_coords) add_menu $SpectralCoords
      }

      #  Interoperability. Send spectrum to other SAMP enabled
      #  applications, such as SPLAT.
      set samp_client_ [gaia::Gaia::get_samp_client]
      if { $samp_client_ != {} } {

         #  Add a new menu for SAMP-related activities.
         set interopmenu_ [add_menubutton Interop]
         set m $interopmenu_
         add_short_help $itk_component(menubar).interop \
            {Tool interoperability using SAMP}

         set send_spectrum_menu [menu $m.send_spectrum]
         add_menuitem $m command "Broadcast spectrum" \
            {Send spectrum to all registered SAMP-enabled applications} \
            -command [code $this send_spectrum {}]
         add_menuitem $m cascade "Send spectrum" \
            {Send spectrum to a specific SAMP-enabled application} \
            -menu $send_spectrum_menu

         #  Arrange for the menu to be kept up to date with the current state
         #  of the SAMP connection.
         {*}$samp_client_ reg_change_command [code $this samp_reg_changed_]
         samp_reg_changed_
         set tracker [{*}$samp_client_ cget -client_tracker]
         {*}$tracker client_change_command [code $this samp_client_changed_]
         samp_client_changed_
      }

      #  Create the canvas. Pack inside a frame so that we can get the resize
      #  events and the new geometry to get the apparent size of canvas right.
      itk_component add canvasframe {
         frame $w_.canvasframe -relief groove -bd 4
      }
      itk_component add canvas {
         ::canvas $itk_component(canvasframe).canvas
      }
      set_background $background_
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

      #  Delete any temporary files.
      if { $temp_files_ != {} } {
         $temp_files_ clear
         ::delete object $temp_files_
      }

      #  Save global properties (should do this once at application exit).
      catch {$props_ save_properties}
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

         #  Reference lines.
         if { [::array exists ref_lines_] } {
            foreach {id} [::array names ref_lines_] {
               $itk_component(draw) delete_object $ref_lines_($id)
            }
            unset ref_lines_
         }

         #  Reference ranges.
         if { [::array exists ref_ranges_] } {
            foreach {id} [::array names ref_ranges_] {
               $itk_component(draw) delete_object $ref_ranges_($id)
            }
            unset ref_ranges_
         }

         #  Labels.
         if { $label_ != {} } {
            $itk_component(canvas) delete $label_
            set label_ {}
         }
         if { $ref_label_ != {} } {
            $itk_component(canvas) delete $ref_label_
            set ref_label_ {}
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
      set Graphics [add_menubutton "Graphics"]
      configure_menubutton Graphics -underline 0
      $itk_component(draw) add_menuitems $Graphics
   }

   #  Display a spectrum extracted from a 2D region. The data is wrapped by an
   #  instance of GaiaNDAccess.
   #
   #  The extraction region is defined using a 2D ARD description and data
   #  from each region in a plane is combined to a spectral point using the
   #  specified combination method.
   #
   #  The axis defines the WCS axis that should be used for the plot X axis.
   #  If autoscale is true, then the plot should be rescaled so that the
   #  spectrum fits, unless the data range is being fixed. Otherwise the
   #  existing plot bounds are used (unless the local autoscale option
   #  is enabled, that takes precendence).  The alow and ahigh arguments
   #  are the range along the axis to extract.
   public method display_region {accessor axis alow ahigh desc meth \
                                 autoscale} {
      #  Get the spectral data from the accessor.
      #  Note assumes WCS & data array axes are aligned and in same order.
      set dataadr [get_region_spectrum_ \
                      $accessor $axis $alow $ahigh $desc $meth]
      set varadr [get_region_spectrum_errors_ \
                     $accessor $axis $alow $ahigh $desc $meth]
      display_spectrum_ $dataadr $varadr $accessor $axis $alow $autoscale
   }

   #  Get the spectral data from a region of the current component.
   protected method get_region_spectrum_ {accessor axis alow ahigh desc meth} {
      return [$accessor getregionspectrum $axis $alow $ahigh \
                 $desc $meth 1 $itk_option(-component)]
   }

   #  Extract the variance associated with a region of a spectrum. If not
   #  already displaying and available.
   protected method get_region_spectrum_errors_ {accessor axis alow ahigh desc meth} {
      set varadr 0
      if { $use_errors_ } {
         if { $itk_option(-component) != "VARIANCE" } {
            if { [$accessor exists "VARIANCE"] } {
               set mapped [$accessor ismapped "VARIANCE"]
               if { ! $mapped } {
                  $accessor map "READ" "VARIANCE"
               }
               set varadr [$accessor getregionspectrum $axis $alow $ahigh \
                              $desc $meth 1 VARIANCE]
            }
         }
      }
      return $varadr
   }

   #  Display a spectrum extracted from a point. The data is wrapped by an
   #  instance of GaiaNDAccess.
   #
   #  The axis defines the WCS axis that should be used for the plot X axis.
   #  If autoscale is true, then the plot should be rescaled so that the
   #  spectrum fits unless the data range is being fixed. Otherwise the
   #  existing plot bounds are used (unless the local autoscale option is
   #  enabled, that takes precendence).  The alow and ahigh arguments are
   #  the range along the axis to extract and p1 and p2 the positions of
   #  the spectrum along the remaining two axes.
   public method display {accessor axis alow ahigh p1 p2 autoscale} {

      #  Get the spectral data from the accessor.
      #  Note assumes WCS & data array axes are aligned and in same order.
      set dataadr [get_spectrum_ $accessor $axis $alow $ahigh $p1 $p2]
      set varadr [get_spectrum_errors_ $accessor $axis $alow $ahigh $p1 $p2]
      display_spectrum_ $dataadr $varadr $accessor $axis $alow $autoscale
   }

   #  Get the spectral data from the current component.
   protected method get_spectrum_ {accessor axis alow ahigh p1 p2} {
      return [$accessor getspectrum $axis $alow $ahigh \
                 $p1 $p2 1 $itk_option(-component)]
   }

   #  Extract the variance associated with a spectrum. If not already
   #  displaying and available.
   protected method get_spectrum_errors_ {accessor axis alow ahigh p1 p2} {
      set varadr 0
      if { $use_errors_ } {
         if { $itk_option(-component) != "VARIANCE" } {
            if { [$accessor exists "VARIANCE"] } {
               set mapped [$accessor ismapped "VARIANCE"]
               if { ! $mapped } {
                  $accessor map "READ" "VARIANCE"
               }
               set varadr [$accessor getspectrum $axis $alow $ahigh \
                              $p1 $p2 1 VARIANCE]
            }
         }
      }
      return $varadr
   }

   #  Display a spectrum whose data are pointed at by "dataadr" and
   #  associated variances (if needed and it has any) by "varadr".
   #  The associated cube is wrapped by an instance of GaiaNDAccess (accessor)
   #  and the axis and offset along that axis, used to extract the spectrum
   #  are axis and alow. Autoscale determines how the plot limits are
   #  determined.
   protected method display_spectrum_ {dataadr varadr accessor axis alow autoscale} {

      #  Create the main spectral_plot.
      if { $spectrum_ == {} } {
         set autoscale 1
         set spectrum_ [$itk_component(canvas) create spectral_plot \
                           pointer $dataadr $varadr \
                           -x 25 -y 5 -width 650 -height 200 \
                           -linecolour $linecolour_ \
                           -linewidth $linewidth_ \
                           -gridoptions $gridoptions_ \
                           -showaxes 1 -xminmax $itk_option(-xminmax) \
                           -xpositive $itk_option(-xpositive) \
                           -ypositive $itk_option(-ypositive) \
                           -reflinecolour $refspeccolour_\
                           -fixdatarange $itk_option(-fix_data_range) \
                           -ytop $itk_option(-data_high) \
                           -ybot $itk_option(-data_low)\
                           -showerrorbars $use_errors_ \
                           -errorcolour $errorcolour_ \
                           -frequency $errorfreq_ \
                           -nsigma $errornsigma_]
         #  Clicking on the plot deselects other graphics.
         $itk_component(canvas) bind $spectrum_ <1> \
            +[code $itk_component(draw) deselect_objects]

         #  Make the base reference line (always have one of these).
         fitxy
         make_ref_line 1
         move_ref_line_ 1
      }

      #  When autoscaling and not fixing the data range (or if we have just
      #  created one of the plots), set the frameset and the NDF data units,
      #  and the offset to the start of the spectrum.
      if { $autoscale || ( $itk_option(-autoscale) &&
                           ! $itk_option(-fix_data_range) ) } {
         set frameset [$accessor getwcs]

         $itk_component(canvas) itemconfigure $spectrum_ \
            -axis $axis \
            -frameset $frameset
         $itk_component(canvas) itemconfigure $spectrum_ \
            -dataunits [$accessor getc units] \
            -datalabel [$accessor getc label] \
            -offset [expr $alow -1]
      }

      #  Pass in the data.
      $itk_component(canvas) coords $spectrum_ pointer $dataadr $varadr

      #  Finished with the spectral data.
      $accessor release $dataadr
      if { $varadr != 0 } {
         $accessor release $varadr
      }
   }

   #  Display a reference spectrum extracted from a region.
   #
   #  All the given details should correspond to those given to the
   #  displayregion method.
   public method display_region_reference {accessor axis alow ahigh desc \
                                           meth} {

      if { $spectrum_ == {} } {
         return
      }

      #  Get the spectral data from the accessor.
      set dataadr [get_region_spectrum_ \
                      $accessor $axis $alow $ahigh $desc $meth]
      set varadr [get_region_spectrum_errors_ \
                     $accessor $axis $alow $ahigh $desc $meth]

      #  Pass in the data.
      $itk_component(canvas) coords $spectrum_ refpointer $dataadr $varadr

      #  Finished with the spectral data.
      $accessor release $dataadr
      if { $varadr != 0 } {
         $accessor release $varadr
      }
   }

   #  Display a reference spectrum extracted from a point.
   #
   #  All the given details should correspond to those given to the display
   #  method.
   public method display_reference {accessor axis alow ahigh p1 p2} {

      if { $spectrum_ == {} } {
         return
      }

      #  Get the spectral data from the accessor.
      set dataadr [get_spectrum_ $accessor $axis $alow $ahigh $p1 $p2]
      set varadr [get_spectrum_errors_ $accessor $axis $alow $ahigh $p1 $p2]

      #  Pass in the data.
      $itk_component(canvas) coords $spectrum_ refpointer $dataadr $varadr

      #  Finished with the spectral data.
      $accessor release $dataadr
      if { $varadr != 0 } {
         $accessor release $varadr
      }
   }

   #  Remove the reference spectrum, if displayed.
   public method remove_reference {} {
      if { $spectrum_ == {} } {
         return
      }
      $itk_component(canvas) coords $spectrum_ refpointer 0
   }

   #  Set the value of the label. This is optional can be any string, but
   #  is expected to be the coordinates of the spectrum, when appropriate.
   #  Set to blank to remove. Note this will have any -prefix added.
   public method update_label {value} {
      if { $show_label_ } {
         if { $itk_option(-label_prefix) != {} && $show_full_label_ } {
            set value "$itk_option(-label_prefix): $value"
         }
         if { $label_ == {} } {
            $itk_component(draw) set_drawing_mode text \
               [code $this created_label_ $value]
            $itk_component(draw) create_object 0 0
            $itk_component(draw) create_done 0 0
         } else {
            $itk_component(canvas) itemconfigure $label_ -text $value
         }
      }
   }

   protected method created_label_ {value id args} {
      set label_ $id
      $itk_component(canvas) itemconfigure $label_ -anchor sw -text $value
      position_label_

      #  Make sure label is deselected after creation (selection is in
      #  create_done, so use after to call later).
      after idle "$itk_component(draw) deselect_object $id"
   }

   #  Position label just above bottom of plot.
   protected method position_label_ {} {
      lassign [$itk_component(canvas) bbox $spectrum_] x0 y0 x1 y1
      incr x0 4
      incr y1 -4
      $itk_component(canvas) coords $label_ $x0 $y1
   }

   #  Remove the label, if not wanted.
   protected method toggle_show_label_ {} {
      if { ! $show_label_ } {
         if { $label_ != {} } {
            $itk_component(canvas) delete $label_
            set label_ {}
         }
      }
      $props_ set_named_property GaiaSpectralPlot \
         show_label_ $show_label_
      $props_ set_named_property GaiaSpectralPlot \
         show_full_label_ $show_full_label_
   }

   #===========================================================================
   #  Reference lines
   #===========================================================================


   #  Make a reference line item. This should be refreshed to the reference
   #  coordinate as necessary.
   public method make_ref_line {id} {
      lassign [$itk_component(canvas) bbox $spectrum_] x0 y0 x1 y1
      if { $x0 != {} } {
         remove_ref_line $id

         #  Create a column canvasdraw object.
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
      if { ! [info exists ref_lines_colour_($id)] } {
         set ref_lines_colour_($id) "cyan"
      }
      $itk_component(canvas) itemconfigure $cid -fill $ref_lines_colour_($id)

      $itk_component(draw) add_notify_cmd $cid \
         [code $this update_ref_line_ $id] 1

      #  Add an additional binding to see when the button is released. Use
      #  this to signal a drag stop.
      $itk_component(canvas) bind $cid <ButtonRelease-1> \
         "+[code $this update_ref_line_ $id released]"

      if { ! [info exists ref_lines_coord_($id)] } {
         set ref_lines_coord_($id) 0
      }
   }

   #  Called when a reference line is updated, that's moved on the canvas or
   #  deleted.
   protected method update_ref_line_ {id mode} {

      if { $mode == "move" || $mode == "released" } {
         #  Convert canvas to axis coordinate and return this if a command has
         #  been supplied. Note that the axis coordinate is return in the
         #  equivalent of NDF pixel indices for convenience.
         if { $itk_option(-ref_line_changed_cmd) != {} } {

            lassign [$itk_component(canvas) coords $ref_lines_($id)] x0 y0 x1 y1

            #  From canvas to world.
            set x0 [$itk_component(canvas) coords $spectrum_ convert 1 $x0]

            #  This is the current world coord, so record it.
            set ref_lines_coord_($id) $x0

            #  World to grid.
            set mapping [$itk_component(canvas) itemcget $spectrum_ -mapping]
            set ind [gaiautils::getaxiscoord $mapping $x0 0]

            eval {*}$itk_option(-ref_line_changed_cmd) $id $ind $mode
         }

         #  Update coordinate label.
         update_ref_label_ $id

      } elseif { $mode == "delete" } {
         unset ref_lines_($id)
         unset ref_lines_coord_($id)
      }
   }

   #  Remove a reference line item.
   public method remove_ref_line {id} {
      if { [info exists ref_lines_($id)] } {
         $itk_component(draw) delete_object $ref_lines_($id)
      }
   }

   #  Set the coordinate of a reference line and optionally creates it.
   public method set_ref_line_coord {id xcoord} {
      if { $spectrum_ != {} } {
         if { ! [info exists ref_lines_($id)] } {
            make_ref_line $id
         }

         #  Don't move unnecessarily.
         if { $ref_lines_coord_($id) != $xcoord } {
            set ref_lines_coord_($id) $xcoord
            move_ref_line_ $id
         }
      }

      #  Update coordinate label.
      update_ref_label_ $id
   }

   #  Move reference line to the reference coordinate. Reference coordinate in
   #  world coordinates.
   public method move_ref_line_ {id} {
      if { $spectrum_ != {} && [info exists ref_lines_($id)] } {
         lassign [$itk_component(canvas) coords $ref_lines_($id)] x0 y0 x1 y1
         set cx [$itk_component(canvas) coords $spectrum_ convert 0 $ref_lines_coord_($id)]
         $itk_component(canvas) move $ref_lines_($id) [expr $cx - $x0] 0
      }
   }

   #  Set the colour of a reference line.
   public method set_ref_line_colour {id colour} {
      set ref_lines_colour_($id) $colour
      $props_ set_named_property GaiaSpectralPlot \
         "ref_lines_colour_($id)" $colour
      if { [::array exists ref_lines_] && [info exists ref_lines_($id)] } {
         $itk_component(canvas) itemconfigure $ref_lines_($id) \
            -fill $ref_lines_colour_($id)
      }
   }

   #  Remove the coordinate label, if not wanted.
   protected method toggle_show_ref_label_ {} {
      if { ! $show_ref_label_ } {
         if { $ref_label_ != {} } {
            $itk_component(canvas) delete $ref_label_
            set ref_label_ {}
         }
      }
      $props_ set_named_property GaiaSpectralPlot \
         show_ref_label_ $show_ref_label_
   }

   #  Update the reference coordinate label.
   protected method update_ref_label_ {id} {
      if { $show_ref_label_ } {
         set value $ref_lines_coord_($id)

         #  Format for this axis.
         set frameset [$itk_component(canvas) itemcget $spectrum_ -frameset]
         set axis [$itk_component(canvas) itemcget $spectrum_ -axis]
         set value [gaiautils::astformat $frameset $axis $value]

         if { $ref_label_ == {} } {
            $itk_component(draw) set_drawing_mode text \
               [code $this created_ref_label_ $value]
            $itk_component(draw) create_object 0 0
            $itk_component(draw) create_done 0 0
         } else {
            $itk_component(canvas) itemconfigure $ref_label_ -text $value
         }
      }
   }

   protected method created_ref_label_ {value id args} {
      set ref_label_ $id
      $itk_component(canvas) itemconfigure $ref_label_ -anchor sw -text $value
      position_ref_label_

      #  Make sure label is deselected after creation (selection is in
      #  create_done, so use after to call later).
      after idle "$itk_component(draw) deselect_object $id"
   }

   #  Position label just below top of plot.
   protected method position_ref_label_ {} {
      lassign [$itk_component(canvas) bbox $spectrum_] x0 y0 x1 y1
      incr x0 4
      incr y0 8
      $itk_component(canvas) coords $ref_label_ $x0 $y0
   }

   #===========================================================================
   #  Reference ranges
   #===========================================================================

   #  Make a reference range item. This should be refreshed with new
   #  coordinates as necessary.
   public method make_ref_range {id} {
      lassign [$itk_component(canvas) bbox $spectrum_] x0 y0 x1 y1
      if { $x0 != {} } {
         remove_ref_range $id

         #  Create an xrange canvasdraw object.
         $itk_component(draw) set_drawing_mode xrange \
            [code $this created_ref_range_ $id]
         $itk_component(draw) create_object $x0 $y0
         $itk_component(draw) create_done $x0 $y0
      }
   }


   #  Called when the creation of a reference range is completed. Records the
   #  canvas id and sets the colour. Adds a notification call so we can see
   #  when the range is dragged or deleted.
   protected method created_ref_range_ {id cid args} {
      set ref_ranges_($id) $cid
      if { ! [info exists ref_ranges_colour_($id)] } {
         set ref_ranges_colour_($id) "magenta"
      }
      $itk_component(canvas) itemconfigure $cid -fill $ref_ranges_colour_($id)

      $itk_component(draw) add_notify_cmd $cid \
         [code $this update_ref_range_ $id] 1

      #  Add an additional binding to see when the button is released. Use
      #  this to signal a drag stop.
      $itk_component(canvas) bind $cid <ButtonRelease-1> \
         "+[code $this update_ref_range_ $id released]"

      if { ! [info exists ref_ranges_coord_($id)] } {
         set ref_ranges_coord_($id) "0 0"
      }

      #  Deselect and select to get grips in right places, but do it after
      #  the draw component has finished.
      after idle [code $itk_component(draw) deselect_object $cid]
      after idle [code $itk_component(draw) select_object $cid]
   }

   #  Called when a reference range is updated, that's moved on the canvas or
   #  deleted.
   protected method update_ref_range_ {id mode} {

      if { $mode == "move" || $mode == "resize" || $mode == "released" } {
         #  Convert canvas to axis coordinates and return this if a command has
         #  been supplied. Note that the axis coordinates are return in the
         #  equivalent of NDF pixel indices for convenience.
         if { $itk_option(-ref_range_changed_cmd) != {} } {

            lassign [$itk_component(canvas) bbox $ref_ranges_($id)] x0 y0 x1 y1

            #  From canvas to world.
            set x0 [$itk_component(canvas) coords $spectrum_ convert 1 $x0]
            set x1 [$itk_component(canvas) coords $spectrum_ convert 1 $x1]

            #  These are the current world coords, so record them.
            set ref_ranges_coord_($id) "$x0 $x1"

            #  World to grid.
            set mapping [$itk_component(canvas) itemcget $spectrum_ -mapping]
            set ind0 [gaiautils::getaxiscoord $mapping $x0 0]
            set ind1 [gaiautils::getaxiscoord $mapping $x1 0]

            eval $itk_option(-ref_range_changed_cmd) $id $ind1 $ind0 $mode
         }
      } elseif { $mode == "delete" } {
         unset ref_ranges_($id)
         unset ref_ranges_coord_($id)
      }
   }

   #  Remove a reference range item.
   public method remove_ref_range {id} {
      if { [info exists ref_ranges_($id)] } {
         $itk_component(draw) delete_object $ref_ranges_($id)
      }
   }

   #  Set the coordinates of a reference range and optionally creates it.
   public method set_ref_range_coord {id x0 x1} {
      if { $spectrum_ != {} } {
         if { ! [info exists ref_ranges_($id)] } {
            make_ref_range $id
         }

         #  Don't move unnecessarily.
         if { $ref_ranges_coord_($id) != "$x0 $x1" } {
            set ref_ranges_coord_($id) "$x0 $x1"
            move_ref_range_ $id
         }
      }
   }

   #  Move reference range to the reference coordinates. Reference coordinates
   #  are in world coordinates.
   public method move_ref_range_ {id} {
      if { $spectrum_ != {} && [info exists ref_ranges_($id)] } {
         lassign [$itk_component(canvas) bbox $spectrum_] x0 y0 x1 y1
         lassign $ref_ranges_coord_($id) x0 x1

         set cx0 [$itk_component(canvas) coords $spectrum_ convert 0 $x0]
         set cx1 [$itk_component(canvas) coords $spectrum_ convert 0 $x1]

         #  A reference range is a 3 piece segmented line draw as |-|...
         set centy [expr ($y0+$y1)/2]
         set dy [expr abs($y1+$y0)*0.2]
         set y0 [expr $centy+$dy]
         set y1 [expr $centy-$dy]

         $itk_component(canvas) coords $ref_ranges_($id) \
            $cx0 $y0 $cx0 $y1 $cx0 $centy $cx1 $centy $cx1 $y0 $cx1 $y1
      }
   }

   #  Set the colour of a reference range.
   public method set_ref_range_colour {id colour} {
      set ref_ranges_colour_($id) $colour
      if { [::array exists ref_ranges_] && [info exists ref_ranges_($id)] } {
         $itk_component(canvas) itemconfigure $ref_ranges_($id) \
            -fill $ref_ranges_colour_($id)
      }
   }

   #===========================================================================

   #  Make the spectral_plot item scale to fit the full size of the canvas.
   public method fitxy { args } {
      if { $spectrum_ != {} } {
         $itk_component(canvas) scale $spectrum_ -1 -1 -1 -1

         #  Resize the reference lines.
         if { [::array exists ref_lines_] } {
            foreach {id} [::array names ref_lines_] {
               set oldval $ref_lines_coord_($id)
               $itk_component(draw) delete_object $ref_lines_($id)
               set ref_lines_coord_($id) $oldval
               make_ref_line $id
               move_ref_line_ $id
            }
         }

         #  Resize the reference ranges.
         if { [::array exists ref_ranges_] } {
            foreach {id} [::array names ref_ranges_] {
               set oldval $ref_ranges_coord_($id)
               $itk_component(draw) delete_object $ref_ranges_($id)
               set ref_ranges_coord_($id) $oldval
               make_ref_range $id
               move_ref_range_ $id
            }
         }

         #  Reposition label.
         if { $label_ != {} } {
            position_label_
         }

         # Fudge immediate update.
         $itk_component(canvas) itemconfigure $spectrum_ -showaxes 1
      }
   }

   #  Set the anchor point for the spectrum.
   public method anchor {x y} {
      $itk_component(canvas) itemconfigure $spectrum_ -x $x -y $y
   }

   #  Set whether to display error bars. Depends on data having variances.
   public method set_use_errors_ {} {
      $props_ set_named_property GaiaSpectralPlot use_errors_ $use_errors_
      $itk_component(canvas) itemconfigure \
         $spectrum_ -showerrorbars $use_errors_

      #  May need to re-extract spectrum as errors may not be extracted yet.
      #  Don't store this state locally so get pass to GaiaCubeSpectrum.
      if { $last_use_errors_ != $use_errors_ && $use_errors_ } {
         if { $itk_option(-gaiacubespectrum) != {} } {
            $itk_option(-gaiacubespectrum) reextract
         }
      }
      set last_use_errors_ $use_errors_
   }

   #  Set the xminmax value of spectrum.
   public method set_xminmax {} {
      $itk_component(canvas) itemconfigure \
         $spectrum_ -xminmax $itk_option(-xminmax)
   }

   #  Set the xpostive value of spectrum.
   public method set_xpositive {} {
      $itk_component(canvas) itemconfigure \
         $spectrum_ -xpositive $itk_option(-xpositive)
   }

   #  Set the ypositive value of spectrum.
   public method set_ypositive {} {
      $itk_component(canvas) itemconfigure \
         $spectrum_ -ypositive $itk_option(-ypositive)
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

      #  Make the spectral plot scale to fit canvas and also re-draw itself.
      #  That is needed to update the coordinate system.
      $itk_component(canvas) scale $spectrum_ -1 -1 -1 -1
      $itk_component(canvas) itemconfigure $spectrum_ -showaxes 1

      #  Wait until the above completes before re-positioning graphics.
      after idle [code $this fitxy]
   }

   #  Apply the current grid options.
   public method apply_gridoptions {} {
      if { $spectrum_ != {} } {

         #  Check if the log settings are sensible, if not issue a warning,
         #  leave these set as autoscaling might change the limits.
         if { $itk_option(-log_x_axis) == 1 && ! $itk_option(-xpositive) } {
            lassign [$itk_component(canvas) coords $spectrum_ limits] x0 y0 x1 y1
            if { $x0 <= 0.0 || $x1 <= 0.0 } {
               warning_dialog "Spectral coordinates include zero or less so \
                               will not use logarithmic scaling" $w_
            }
         }
         if { $itk_option(-log_y_axis) == 1 && ! $itk_option(-ypositive) } {
            lassign [$itk_component(canvas) coords $spectrum_ limits] x0 y0 x1 y1
            if { $y0 <= 0.0 || $y1 <= 0.0 } {
               warning_dialog "The range of data values include zero or \
                               less so will not use logarithmic scaling" $w_
            }
         }

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
          Font=$axes_font_"
   }

   #  Set the colour of the spectral line.
   public method set_linecolour {colour} {
      set linecolour_ $colour
      $props_ set_named_property GaiaSpectralPlot linecolour_ $colour
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -linecolour $linecolour_
      }
      if { $itk_option(-colour_changed_cmd) != {} } {
         eval $itk_option(-colour_changed_cmd) "spectrum" $colour
      }
   }

   #  Set the width of the spectral line.
   public method set_linewidth {width} {
      set linewidth_ $width
      $props_ set_named_property GaiaSpectralPlot linewidth_ $width
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -linewidth $linewidth_
      }
   }

   #  Set the colour of the error bars.
   public method set_errorcolour {colour} {
      set errorcolour_ $colour
      $props_ set_named_property GaiaSpectralPlot errorcolour_ $colour
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -errorcolour $errorcolour_
      }
   }

   #  Set length of the error bars.
   public method set_errornsigma {nsigma} {
      set errornsigma_ $nsigma
      $props_ set_named_property GaiaSpectralPlot errornsigma_ $nsigma
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -nsigma $errornsigma_
      }
   }

   #  Set frequency of the error bars.
   public method set_errorfreq {freq} {
      set errorfreq_ $freq
      $props_ set_named_property GaiaSpectralPlot errorfreq_ $freq
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -frequency $errorfreq_
      }
   }

   #  Set the colour of the axes.
   public method set_axescolour {colour} {
      set axescolour_ $colour
      $props_ set_named_property GaiaSpectralPlot axescolour_ $colour
      apply_gridoptions
   }

   #  Set the font of the axes.
   public method set_axes_font {font} {
      set axes_font_ $font
      $props_ set_named_property GaiaSpectralPlot axes_font_ $font
      apply_gridoptions
   }

   #  Set the colour of the reference spectrum.
   public method set_refspeccolour {colour} {
      set refspeccolour_ $colour
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -reflinecolour $refspeccolour_
      }
      $props_ set_named_property GaiaSpectralPlot refspeccolour_ $colour
      if { $itk_option(-colour_changed_cmd) != {} } {
         eval $itk_option(-colour_changed_cmd) "reference" $colour
      }
   }

   #  Set the colour of the canvas background.
   public method set_background {colour} {
      $itk_component(canvas) configure -background $colour
      set background_ $colour
      $props_ set_named_property GaiaSpectralPlot background_ $colour
   }

   #  Reset all the saved options to their default values. KEEP this up
   #  to date.
   protected method reset_options_ {} {
      set_linecolour "blue"
      set_errorcolour "red"
      set_errornsigma 1
      set_errorfreq 1
      set_linewidth 1
      set_axescolour 0
      set_axes_font 0
      set_refspeccolour "green"
      set_background "black"
      set_ref_line_colour 1 "red"
      set show_ref_label_ 0
      set show_label_ 0
      set show_full_label_ 0
      toggle_show_label_
      toggle_show_ref_label_
   }

   #  Return the canvas id of the spectral plot.
   public method get_spectrum {} {
      return $spectrum_
   }

   #  "rtdimage" emulation.
   #
   #  Needed to support some actions of the StarCanvasDraw instance
   #  (items that require width and height information).
   public method scale {} {
      return 1
   }
   public method dispwidth {} {
      return [winfo width $itk_component(canvasframe)]
   }
   public method dispheight {} {
      return [winfo height $itk_component(canvasframe)]
   }

   #  SAMP support.

   #  Send spectrum to be displayed in another application.
   public method send_spectrum {recipient_id} {
      if { [catch {
         set sender [gaia::Gaia::get_samp_sender]
         if { $sender != {} && $itk_option(-spec_writer) != {} } {

            if { $temp_files_ == {} } {
               set temp_files_ [gaia::GaiaTempName \#auto \
                                   -prefix "GaiaTempSampSpec" \
                                   -type ".fits" -exists 0]
            }
            set filename [$temp_files_ get_name]
            {*}$itk_option(-spec_writer) write_as_fits $filename

            set shortname [{*}$itk_option(-spec_writer) get_shortname]
            if { $shortname == {} } {
               set shortname "$filename"
            }
            set dataunit [$itk_component(canvas) itemcget $spectrum_ -dataunit]
            set frameset [$itk_component(canvas) itemcget $spectrum_ -frameset]
            set axis [$itk_component(canvas) itemcget $spectrum_ -axis]
            set coordunit [gaiautils::astget $frameset "unit($axis)"]

            $sender send_spectrum $filename $shortname \
               $coordunit $dataunit $recipient_id
         }
      } msg]} {
         puts "selection send error: $msg"
      }
   }

   #  Called when the SAMP connection goes up or down.
   protected method samp_reg_changed_ {} {

      #  Configure the menu items all enabled/disabled according to whether
      #  there is a SAMP connection.
      set state disabled
      if { $samp_client_ != {} && [{*}$samp_client_ is_registered] } {
         set state normal
      }
      set nitem [$interopmenu_ index last]
      for { set item 0 } { $item < $nitem } { incr item } {
         $interopmenu_ entryconfigure $item -state $state
      }
   }

   #  Called when external applications register or unregister with SAMP.
   protected method samp_client_changed_ {} {

      #  Configure specific application menu so that it has one entry for
      #  each of the currently registered SAMP applications capable of
      #  receiving spectra.
      set specific_menu $interopmenu_.send_spectrum
      $specific_menu delete 0 last
      if { [{*}$samp_client_ is_registered] } {
         set tracker [{*}$samp_client_ cget -client_tracker]
         set mtype "spectrum.load.ssa-generic"
         set subscribed_clients [{*}$tracker get_subscribed_clients $mtype]
         foreach client_id $subscribed_clients {
            set client_name [{*}$tracker get_name $client_id]
            add_menuitem $specific_menu command "Send to $client_name" \
               "Send spectrum to $client_name" \
               -command [code $this send_spectrum $client_id]
         }
      }
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  The component of the data that we're to display. Usually DATA,
   #  but could be VARIANCE or QUALITY.
   itk_option define -component component Component "DATA"

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

   #  Whether to log coordinate axis.
   itk_option define -log_x_axis log_x_axis Log_X_Axis 0

   #  Whether to just use positive X coordinates (used with log_x_axis).
   itk_option define -xpositive xpositive Xpositive 0

   #  Whether to just use positive Y coordinates (used with log_y_axis).
   itk_option define -ypositive ypositive Ypositive 0

   #  Whether to log data axis.
   itk_option define -log_y_axis log_y_axis Log_Y_Axis 0

   #  A GaiaCubeSpectrum instance, optional. Required if toggling
   #  errorbars is to work without requiring an image click.
   itk_option define -gaiacubespectrum gaiacubespectrum GaiaCubeSpectrum {}

   #  A GaiaSpecCoords object for controlling the coordinate systems.
   #  Actual control is handled by the GaiaCube instance.
   itk_option define -spec_coords spec_coords Spec_Coords {}

   #  A GaiaSpecCoords object for controlling the coordinate systems.
   #  Actual control is handled by the GaiaCube instance.
   itk_option define -spec_sor spec_sor Spec_Coords {}

   #  A GaiaSpecWriter object for controlling saving spectra to disk.
   #  This is done by re-visiting the cube, which we do not keep locally,
   #  but this menu naturally belongs here.
   itk_option define -spec_writer spec_writer Spec_Writer {}

   #  A command to invoke when a reference line is moved (by interaction by
   #  the user). The command will be trailed by the reference line id and
   #  the new world coordinate.
   itk_option define -ref_line_changed_cmd ref_line_changed_cmd Ref_Line_Changed_Cmd {}

   #  A command to invoke when a reference range is moved (by interaction by
   #  the user). The command will be trailed by the reference range id and
   #  the new world coordinates.
   itk_option define -ref_range_changed_cmd ref_range_changed_cmd Ref_Range_Changed_Cmd {}

   #  Command to execute when the colour of the spectral line is changed.
   #  The result will be appended by "spectrum" or "reference" and the
   #  new colour.
   itk_option define -colour_changed_cmd colour_changed_cmd Colour_Change_Cmd {}

   #  Fix the spectral plot data range. Otherwise use min/max.
   itk_option define -fix_data_range fix_data_range Fix_Data_Range 0 {
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -fixdatarange $itk_option(-fix_data_range)
      }
   }

   #  Upper and lower data limits.
   itk_option define -data_high data_high Data_High 1 {
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -ytop $itk_option(-data_high)
      }
   }

   itk_option define -data_low data_low Data_Low 0 {
      if { $spectrum_ != {} } {
         $itk_component(canvas) itemconfigure $spectrum_ \
            -ybot $itk_option(-data_low)
      }
   }

   #  A prefix for the label. Usually dataset name etc.
   itk_option define -label_prefix label_prefix Label_Prefix {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  The main spectral_plot item.
   protected variable spectrum_ {}

   #  The label.
   protected variable label_ {}

   #  Whether to show the label or not.
   protected variable show_label_ 0
   protected variable show_full_label_ 0

   #  The font to use when drawing the axes (AST integer).
   protected variable axes_font_ 0

   #  Whether to show the reference label or not.
   protected variable show_ref_label_ 0

   #  Whether to show reference coordinate label item.
   protected variable ref_label_ {}

   #  All reference line canvas ids, indexed by a user specified number.
   protected variable ref_lines_

   #  The reference line coordinates, world coords, indexed as ref_lines_
   protected variable ref_lines_coord_

   #  Reference line colours , indexed as ref_lines_.
   protected variable ref_lines_colour_

   #  All reference range canvas ids, indexed by a user specified number.
   protected variable ref_ranges_

   #  The reference range coordinates, world coords, indexed as ref_ranges_
   protected variable ref_ranges_coord_

   #  Reference range colours, indexed as ref_ranges_.
   protected variable ref_ranges_colour_

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

   #  Current width of the line.
   protected variable linewidth_ 1

   #  Current background colour.
   protected variable background_ "black"

   #  Current axes colour.
   protected variable axescolour_ 0 ;     # AST index of white

   #  Current reference spectrum colour.
   protected variable refspeccolour_ "green"

   #  The gridoptions that are currently in use.
   protected variable gridoptions_ {}

   #  Names of the fonts that we will use and their AST indices.
   #  A text string to very briefly describe the font is also set.
   protected variable fontmap_ $::gaia::astfontmap

   #  SampClient object used for SAMP connections.
   protected variable samp_client_

   #  Interoperability menu.
   protected variable interopmenu_

   #  Object for generating and managing temporary files.
   protected variable temp_files_ {}

   #  Global properties handler.
   protected variable props_ {}

   #  Whether to display errorbars.
   protected variable use_errors_ 0
   protected variable last_use_errors_ 0

   #  Current colour of the error bars.
   protected variable errorcolour_ "red"

   #  Frequency of the error bars (one every ...).
   protected variable errorfreq_ 1

   #  Number of standard deviations for error bar length.
   protected variable errornsigma_ 1

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Counter for creating unique names.
   common count_ 0

#  End of class definition.
}
