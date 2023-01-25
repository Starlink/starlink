#+
#  Name:
#     GaiaCubeSpectrum

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Controls for spectral display from a cube.

#  Description:
#     This class creates a panel of controls for determining how the
#     spectrum is extracted from a cube. The extraction can be from a point
#     and or ARD regions.

#  Invocations:
#
#        GaiaCubeSpectrum object_name [configuration options]
#
#     This creates an instance of a GaiaCubeSpectrum object. The return is
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
#     See itk_option definitions below.

#  Methods:
#     See individual method declarations below.

#  Inheritance:
#     util::TopLevelWidget

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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
#     10-JUL-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCubeSpectrum {}

itcl::class gaia::GaiaCubeSpectrum {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      itk_component add tframe {
         frame $w_.tframe
      }

      #  Whether to enable spectral extraction.
      itk_component add extraction {
         gaia::StarLabelCheck $itk_component(tframe).extraction \
            -text "Spectrum extraction:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $itk_option(-labelwidth) \
            -variable [scope itk_option(-extraction)] \
            -command [code $this toggle_extraction_]
      }
      add_short_help $itk_component(extraction) {Display extracted spectrum}

      #  Do a re-extraction, without clicking on the image.
      itk_component add reextract {
         button $itk_component(tframe).reextract -text "Re-extract" \
            -command [code $this reextract]
      }
      add_short_help $itk_component(reextract) \
         {Re-extract spectrum using new limits}

      pack $itk_component(tframe) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(extraction) -side left -expand 0 -anchor w
      pack $itk_component(reextract) -side left -expand 0 -anchor c

      #  Whether to show the extraction limits as a range object on the
      #  spectral plot.
      itk_component add showrange {
         gaia::StarLabelCheck $w_.showrange \
            -text "Show limits on plot:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $itk_option(-labelwidth) \
            -variable [scope itk_option(-show_ref_range)] \
            -command [code $this toggle_show_ref_range_]
      }
      pack $itk_component(showrange) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(showrange) \
         {Show extent of spectral extraction on plot with a reference range
            figure}

      itk_component add bounds {
         gaia::GaiaSpectralPlotRange $w_.bounds \
            -gaiacube $itk_option(-gaiacube) \
            -ref_id $itk_option(-ref_id) \
            -text1 {Lower index:} \
            -text2 {Upper index:} \
            -value1 $itk_option(-lower_limit) \
            -value2 $itk_option(-upper_limit) \
            -show_ref_range $itk_option(-show_ref_range) \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -coord_update_cmd [code $this set_limits_]
      }
      pack $itk_component(bounds) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(bounds) \
         {Lower and upper indices used for extraction}

      #  Setting of the data range.
      itk_component add fixdatarange {
         gaia::StarLabelCheck $w_.fixdatarange \
            -text "Fix data range:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $itk_option(-labelwidth) \
            -variable [scope itk_option(-fix_data_range)] \
            -command [code $this toggle_fix_data_range_]
      }
      pack $itk_component(fixdatarange) \
         -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(fixdatarange) \
         {Fix data range to the values given, otherwise use min/max}

      #  The displayed data range. Cannot really say what the expected
      #  range is, so these are just free format fields, expecting a floating
      #  point value.
      itk_component add datalow {
         util::LabelEntry $w_.datalow \
            -text "Lower data limit:" \
            -value 0 \
            -labelwidth $itk_option(-labelwidth) \
            -textvariable [scope itk_option(-data_low)] \
            -validate real \
            -command [code $this set_lower_data_limit_]
      }
      pack $itk_component(datalow) \
         -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(datalow) \
         {Lower data limit for spectral plot, press return to apply}

      itk_component add datahigh {
         util::LabelEntry $w_.datahigh \
            -text "Upper data limit:" \
            -value 1 \
            -labelwidth $itk_option(-labelwidth) \
            -textvariable [scope itk_option(-data_high)] \
            -validate real \
            -command [code $this set_upper_data_limit_]
      }
      pack $itk_component(datahigh) \
         -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(datahigh) \
         {Upper data limit for spectral plot, press return to apply}

      #  Gray out as appropriate.
      toggle_fix_data_range_

      #  Stop point tracking, can be confusing then also using regions.
      itk_component add pointtracking {
         gaia::StarLabelCheck $w_.pointtracking \
            -text "Point tracking:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $itk_option(-labelwidth) \
            -variable [scope itk_option(-point_tracking)] \
            -command [code $this toggle_point_tracking_]
      }
      pack $itk_component(pointtracking) \
         -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(pointtracking) \
         {Do point extraction of spectrum, otherwise not}

      #  Add region controls.
      add_region_controls_

      #  Add reference spectrum controls. Just "set" and "clear".
      itk_component add rframe {
         frame $w_.rframe
      }
      itk_component add reflabel {
         label $itk_component(rframe).reflabel -text "Reference:" \
            -width $itk_option(-labelwidth) -anchor w
      }
      itk_component add setref {
         button $itk_component(rframe).setref -text "Set" \
            -command [code $this set_reference]
      }
      itk_component add clearref {
         button $itk_component(rframe).clearref -text "Clear" \
            -command [code $this clear_reference]
      }

      pack $itk_component(rframe) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(reflabel) -side left -expand 0 -ipadx 1m
      pack $itk_component(setref) $itk_component(clearref) \
         -side left -expand 0 -anchor w -ipadx 1m -padx 1m
      add_short_help $itk_component(setref) \
         {Make current spectrum the reference}
      add_short_help $itk_component(clearref) \
         {Clear the reference spectrum}

      #  Add controls to send a spectrum to SPLAT.
      itk_component add sframe {
         frame $w_.sframe
      }
      itk_component add splatlabel {
         label $itk_component(sframe).splatlabel -text "SPLAT-VO:" \
            -width $itk_option(-labelwidth) -anchor w
      }

      itk_component add splatreplace {
         button $itk_component(sframe).splatreplace -text "Send: replace" \
            -command [code $this send_to_splat_ 0]
      }
      itk_component add splatcompare {
         button $itk_component(sframe).splatcompare -text "Send: add" \
            -command [code $this send_to_splat_ 1]
      }

      pack $itk_component(sframe) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(splatlabel) -side left -expand 0 -ipadx 1m
      pack $itk_component(splatreplace) -side left -expand 0 -anchor w \
         -ipadx 1m -padx 1m
      pack $itk_component(splatcompare) -side left -expand 0 -anchor w \
         -ipadx 1m -padx 1m

      #  If SPLAT isn't available this is greyed out.
      global env
      if { [info exists env(SPLAT_DIR)] } {
         set splat_dir_ $env(SPLAT_DIR)
      } else {
         $itk_component(splatcompare) configure -state disabled
         $itk_component(splatreplace) configure -state disabled
      }

      add_short_help $itk_component(splatcompare) \
         {Send extracted spectrum to SPLAT-VO and add to plot}
      add_short_help $itk_component(splatreplace) \
         {Send extracted spectrum to SPLAT-VO and remove existing spectra}

      #  Set initial bindings.
      toggle_extraction_

      #  Create an instance of GaiaSpecWriter. This is shared with the
      #  plot.
      set spec_writer_ [GaiaSpecWriter \#auto \
                           -cubespectrum [code $this] \
                           -gaiacube $itk_option(-gaiacube)]
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Delete any temporary files.
      if { $temp_files_ != {} } {
         $temp_files_ clear
         ::delete object $temp_files_
      }

      close
      if { $toolbox_ != {} } {
         ::delete object $toolbox_
      }
   }

   #  Methods:
   #  --------

   #  Handle a close. This disables any interactions and graphics and closes
   #  the plot.
   public method close {} {

      #  Close the plot.
      close_plot

      #  Also remove bindings.
      remove_bindings_
   }

   #  Close the plot. Do this when the display becomes invalid (cube changes).
   public method close_plot {} {
      if { $spectrum_ != {} && [winfo exists $spectrum_] } {
         $spectrum_ close
      }

      #  Remove all markers and forget previous extractions.
      remove_position_markers_
      remove_regions_
      set last_cxcy_ {}
      set last_region_ {}
   }

   #  Open, undo some of the actions of close.
   public method open {} {
      toggle_extraction_
   }

   #  Toggle whether we're extracting spectra or not.
   protected method toggle_extraction_ {} {
      if { $itk_option(-extraction) } {
         add_bindings_
      } else {
         remove_bindings_
         remove_position_markers_
      }
   }

   #  Toggle whether point tracking is being done. Can be confusing when
   #  this is used at the same time as regions, but not always.
   protected method toggle_point_tracking_ {} {
      if { $itk_option(-point_tracking) } {
         #  Setup normal bindings.
         add_bindings_
      } else {
         #  Remove normal bindings.
         remove_bindings_
         remove_position_markers_
      }
   }

   #  Configure canvas so we get any clicks on the image and can display
   #  the associated point spectra.
   protected method add_bindings_ {} {

      $canvas_ bind $rtdimage_ <1> \
         [code $this display_point_spectrum_ localstart %x %y]

      $canvas_ bind $rtdimage_  <B1-Motion> \
         [code $this display_point_spectrum_ localdrag %x %y]
   }

   #  Remove bindings from main canvas for spectral plot.
   protected method remove_bindings_ {} {
      catch {
         $canvas_ bind $rtdimage_ <1> {}
         $canvas_ bind $rtdimage_ <B1-Motion> {}
      }
   }

   #  If not available create the spectral plot, otherwise activate it.
   protected method make_plot_ {} {
      if { $spectrum_ == {} } {
         #  Need to create a spectrum plot. Note show short help in this
         #  window to save real estate.
         set cube $itk_option(-gaiacube)
         set spectrum_ \
            [GaiaSpectralPlot $w_.specplot \
                -number [$itk_option(-gaia) cget -number] \
                -gaiacubespectrum $this \
                -spec_coords $itk_option(-spec_coords) \
                -spec_sor $itk_option(-spec_sor) \
                -spec_writer [code $spec_writer_] \
                -ref_line_changed_cmd [code $cube ref_line_moved_] \
                -ref_range_changed_cmd [code $cube ref_range_moved_] \
                -colour_changed_cmd [code $this spec_colour_changed_] \
                -component $itk_option(-component) \
                -shorthelpwin $itk_option(-gaia) \
                -transient $itk_option(-transient_spectralplot) \
                -fix_data_range $itk_option(-fix_data_range) \
                -data_high $itk_option(-data_high) \
                -data_low $itk_option(-data_low) \
                -label_prefix [$cube get_ndfname]]

         #  Make this a transient of main window, not this one.
         if { $itk_option(-transient_spectralplot) } {
            wm transient $spectrum_ $itk_option(-gaia)
         }
      } else {
         #  Already have a plot, re-display if withdrawn.
         if { [wm state $spectrum_] == "withdrawn" } {
            $spectrum_ open
            $spectrum_ configure -label_prefix \
               [$itk_option(-gaiacube) get_ndfname]
         }
      }
   }

   #  Display a spectrum in the local plot. Action can be "localstart" or
   # "localdrag", to start a spectral display (sets the initial scale of a
   # drag), or update during a drag.
   protected method display_point_spectrum_ {action cx cy} {

      #  Retain coordinates for a possible update.
      set last_cxcy_ "$cx $cy"
      set last_region_ {}

      #  Convert click coordinates from canvas coords to grid coords.
      set ccx [$canvas_ canvasx $cx]
      set ccy [$canvas_ canvasy $cy]
      $rtdimage_ convert coords $ccx $ccy canvas iix iiy image

      #  Make the spectral plot available.
      make_plot_

      #  Re-create the marker for the image position.
      if { $position_mark_ == {} } {
         create_position_marker_ $ccx $ccy
      }

      #  Correct spectral extraction bounds to grid indices.
      set lb $itk_option(-lower_limit)
      set ub $itk_option(-upper_limit)
      set alow [$itk_option(-gaiacube) axis_pixel2grid $lb]
      set ahigh [$itk_option(-gaiacube) axis_pixel2grid $ub]
      if { $alow > $ahigh } {
         set tmp $alow
         set alow $ahigh
         set ahigh $tmp
      }

      #  Make sure ix and iy are integers (zoomed images).
      set ix [expr round($iix)]
      set iy [expr round($iiy)]

      #  Move position marker on the image.
      $canvas_ coords $position_mark_ $ccx $ccy

      #  Also autoscale when single click, so that we are not fixed for
      #  all time.
      set cubeaccessor [$itk_option(-gaiacube) get_cubeaccessor]
      set axis [$itk_option(-gaiacube) get_axis]
      if { $action == "localstart" } {
         busy {
            $spectrum_ display $cubeaccessor $axis $alow $ahigh $ix $iy 1
         } 0

         #  Set first-time position of the main reference line. This is
         #  the position of the current image slice.
         set coord [$itk_option(-gaiacube) get_plane_coord 0 0]
         if { $coord != {} } {
            update;                     # Make sure plot is created first.
            set_spec_ref_coord 1 $coord
         }

         #  And reposition any graphics, if the extraction bounds have
         #  changed.
         if { $lb != $llb_ || $ub != $lub_ } {
            $spectrum_ fitxy
         }
      } else {
         busy {
            $spectrum_ display $cubeaccessor $axis $alow $ahigh $ix $iy 0
         } 0
      }

      #  Display the current coordinate for reference.
      catch {
         lassign [$rtdimage_ astpix2cur $iix $iiy] ra dec
         $spectrum_ update_label "$ra, $dec"
      }

      #  Eval the notification command, if have one.
      if { $itk_option(-notify_cmd) != {} } {
         if { $action == "localstart" } {
            eval $itk_option(-notify_cmd) p 1 \"$ix $iy\"
         } else {
            eval $itk_option(-notify_cmd) p 0 \"$ix $iy\"
         }
      }

      #  Record extraction bounds, these are checked.
      set llb_ $lb
      set lub_ $ub
   }

   #  Set the position of the point extracted spectrum, if extraction is
   #  enabled. The position is in image coordinates (not pixel). Note
   #  if spectrum is enabled, but we're not extraction a position, that is
   #  started. The init argument should be set true when the extraction
   #  bounds should be re-initialised (a localstart).
   public method set_point_position {init ix iy} {
      if { $spectrum_ != {} } {
         $rtdimage_ convert coords $ix $iy image sx sy screen
         if { $last_cxcy_ == {} || $init } {
            display_point_spectrum_ "localstart" $sx $sy
         } else {
            display_point_spectrum_ "localdrag" $sx $sy
         }
      }
   }

   #  Get the position of the point extracted spectrum, if extraction is
   #  enabled. The position is in image coordinates (not pixel). If
   #  not extracting or using a region "" is returned.
   public method get_point_position {} {
      if { $last_cxcy_ != {} } {

         #  Convert click coordinates from canvas coords to grid coords.
         lassign $last_cxcy_ cx cy
         set ccx [$canvas_ canvasx $cx]
         set ccy [$canvas_ canvasy $cy]
         $rtdimage_ convert coords $ccx $ccy canvas ix iy image

         #  Make grid coordinates integer to pick centre of pixel.
         set ix [expr round($ix)]
         set iy [expr round($iy)]

         return [list $ix $iy]
      }
      return ""
   }

   #  Get the ARD region of the extracted spectrum, if extraction is
   #  enabled. If not extracting or using a point "" is returned.
   public method get_region_position {} {
      if { $last_region_ != {} } {
         return $last_region_
      }
      return ""
   }

   #  Set the ARD region of the extracted spectrum. The basic types may not
   #  match so this can fail. Just handle that silently and hope the message
   #  gets through, init should be set true when the data limits should be
   #  autoscaled (following a click).
   public method set_region_position {init region} {
      if { $spectrum_ != {} } {
         if { [catch {$toolbox_ set_selected_description $region} ] } {

            #  Simple set failed, look for a suitable shape or create one.
            if { [$toolbox_ match_description $region] } {
               $toolbox_ set_selected_description $region
            } else {
               #  Create new shape.
               $toolbox_ parse_description $region
            }

            #  And extract the spectrum.
            display_region_spectrum_ localstart $region
         } else {
            if { $init } {
               display_region_spectrum_ localstart $region
            } else {
               display_region_spectrum_ localdrag $region
            }
         }
      }
   }

   #  Display a region spectrum in the local plot. Action can be "localstart"
   #  or "localdrag", to start a spectral display (sets the initial scale of a
   #  drag), or update during a drag.
   protected method display_region_spectrum_ {action region} {

      #  Retain region for a possible update.
      set last_region_ $region
      set last_cxcy_ {}

      #  Make the spectral plot available.
      make_plot_

      #  Correct spectral extraction bounds to grid indices.
      set lb $itk_option(-lower_limit)
      set ub $itk_option(-upper_limit)
      set alow [$itk_option(-gaiacube) axis_pixel2grid $lb]
      set ahigh [$itk_option(-gaiacube) axis_pixel2grid $ub]
      if { $alow > $ahigh } {
         set tmp $alow
         set alow $ahigh
         set ahigh $tmp
      }

      #  Also autoscale when doing a localstart.
      set cubeaccessor [$itk_option(-gaiacube) get_cubeaccessor]
      set axis [$itk_option(-gaiacube) get_axis]
      if { $action == "localstart" } {
         busy {
            $spectrum_ display_region $cubeaccessor $axis $alow $ahigh \
               $region $combination_type_ 1
         } 0

         #  Set first-time position of the main reference line. This is
         #  the position of the current image slice.
         set coord [$itk_option(-gaiacube) get_plane_coord 0 0]
         if { $coord != {} } {
            update;                     # Make sure plot is created first.
            set_spec_ref_coord 1 $coord
         }

         #  And reposition any graphics, if the extraction bounds have
         #  changed.
         if { $lb != $llb_ || $ub != $lub_ } {
            $spectrum_ fitxy
         }
      } else {
         busy {
            $spectrum_ display_region $cubeaccessor $axis $alow $ahigh \
               $region $combination_type_ 0
         } 0
      }

      #  Cannot display the current coordinate, so make sure it is empty.
      $spectrum_ update_label ""

      #  Eval the notification command, if have one. Note contract assumes
      #  a single character string, so remove any newlines (polygon).
      if { $itk_option(-notify_cmd) != {} } {
         regsub -all {\n} $region { } region
         if { $action == "localstart" } {
            eval $itk_option(-notify_cmd) r 1 \"$region\"
         } else {
            eval $itk_option(-notify_cmd) r 0 \"$region\"
         }
      }

      #  Record extraction bounds, these are checked.
      set llb_ $lb
      set lub_ $ub
   }

   #  Create an NDF section-like name to use to identify the current spectrum.
   public method sectioned_name {} {
      if { $spectrum_ == {} || [last_extracted_type] == "none" } {
         return ""
      }
      set ndfname [$itk_option(-gaiacube) get_ndfname]
      set spectype [last_extracted_type]
      if { $spectype == "point" } {
         lassign $last_cxcy_ cx cy

         #  Convert click coordinates from canvas coords to grid coords.
         set ccx [$canvas_ canvasx $cx]
         set ccy [$canvas_ canvasy $cy]
         $rtdimage_ convert coords $ccx $ccy canvas iix iiy image

         #  Correct to pixel indices.
         lassign [$itk_option(-gaiacube) image_grid2pixel $iix $iiy] ix iy

         #  Create the right section. Use extraction bounds on the
         #  spectral axis.
         set lb $itk_option(-lower_limit)
         set ub $itk_option(-upper_limit)
         set range "$lb:$ub"
         set axis [$itk_option(-gaiacube) get_axis]
         set close_section [$itk_option(-gaiacube) get_close_section]

         if { $axis == 1 } {
            set section "($range,$ix,${iy}${close_section}"
         } elseif { $axis == 2 } {
            set section "($ix,$range,${iy}${close_section})"
         } else {
            set section "($ix,$iy,${range}${close_section}"
         }

      } else {

         #  Region, clean up any leading spaces and keep to a sensible
         #  length, finally remove any embedded newlines (polygons).
         set cr [string range [string trim $last_region_] 0 80]
         regsub -all {\n} $cr { } section
      }

      #  Return proper section name for possible NDFs.
      if { $spectype == "point"
           && [$itk_option(-gaiacube) get_type] == ".sdf" } {
         return "${ndfname}${section}"
      } else {
         return "${ndfname}:${section}"
      }
   }

   #  Return the type of the spectrum currently extracted. Result is
   #  one of "point", "region" or "none".
   public method last_extracted_type {} {
      if { $last_cxcy_ != {} } {
         return "point"
      }
      if { $last_region_ != {} } {
         return "region"
      }
      return "none"
   }

   #  Re-extract the spectrum. Quick button to avoid clicking on the image
   #  (which can be unzoomed so accuracy is difficult).
   public method reextract {} {
      if { $spectrum_ != {} } {
         set spectype [last_extracted_type]
         if { $spectype == "point"  } {
            eval display_point_spectrum_ localstart $last_cxcy_
         } elseif { $spectype == "region" } {
            display_region_spectrum_ localstart $last_region_
         }
      } else {
         info_dialog "No spectrum has been extracted" $w_
      }
   }

   #  Send the currently extracted spectrum to SPLAT for display.
   #  Use an NDF so that the maximum information is retained.
   #  If compare is 1 then just add the spectrum, otherwise remove
   #  any previously sent spectra from our plot.
   protected method send_to_splat_ { compare } {
      if { $spectrum_ == {} || [last_extracted_type] == "none" } {
         info_dialog "No spectrum has been extracted" $w_
         return
      }

      #  If not already done, prepare for sending messages to SPLAT.
      #  Trap any messages to a text window if requested and return
      #  any errors as an error not in a dialog (gives full stack trace).
      if { $splat_disp_ == {} } {
         set use_error_dialog 1
         if { $itk_option(-show_splatdisp) } {
            set use_error_dialog 0
         }
         set splat_disp_ [GaiaForeignExec \#auto \
                             -application $splat_dir_/splatdisp \
                             -show_output $itk_option(-show_splatdisp) \
                             -show_traceback $itk_option(-show_splatdisp) \
                             -use_error_dialog $use_error_dialog]
      }

      if { $temp_files_ == {} } {
         set temp_files_ [gaia::GaiaTempName \#auto \
                             -prefix "GaiaTempSpec" -type ".sdf"\
                             -exists 0]
      }
      set filename [$temp_files_ get_name]
      $spec_writer_ write_as_ndf $filename

      # If in CYGWIN environment convert filename to windows format.
      # SPLAT is a windows application. Otherwise get absolute name
      # for SPLAT to locate, if needed.
      if { [string match {CYGWIN*} $::tcl_platform(os)] } {
         set filename [exec cygpath -wa $filename]
      } else {
         if { [::file pathtype $filename] != "absolute" } {
            set filename "[pwd]/$filename"
         }
      }

      if { $compare } {
         $splat_disp_ runwith $filename 0 false
      } else {
         $splat_disp_ runwith $filename 0 true
      }
   }

   #  Extract and display a position as a reference spectrum. The
   #  plot must already exist.
   public method display_point_reference_spectrum {cx cy} {

      #  Must already have a plot, re-display if withdrawn.
      if { $spectrum_ == {} } {
         return
      }
      $spectrum_ open

      #  Convert coordinates from canvas coords to grid coords.
      set ccx [$canvas_ canvasx $cx]
      set ccy [$canvas_ canvasy $cy]
      $rtdimage_ convert coords $ccx $ccy canvas iix iiy image

      #  Correct extraction bounds to grid indices.
      set lb $itk_option(-lower_limit)
      set ub $itk_option(-upper_limit)
      set alow [$itk_option(-gaiacube) axis_pixel2grid $lb]
      set ahigh [$itk_option(-gaiacube) axis_pixel2grid $ub]
      if { $alow > $ahigh } {
         set tmp $alow
         set alow $ahigh
         set ahigh $tmp
      }

      #  Make sure ix and iy are integers (zoomed images).
      set ix [expr round($iix)]
      set iy [expr round($iiy)]

      #  Display it as a reference spectrum.
      set cubeaccessor [$itk_option(-gaiacube) get_cubeaccessor]
      set axis [$itk_option(-gaiacube) get_axis]
      busy {
         $spectrum_ display_reference $cubeaccessor $axis $alow $ahigh $ix $iy
      } 0

      #  Re-create the marker for the image position, if needed.
      if { $ref_position_mark_ == {} } {
         create_ref_position_marker_ $ccx $ccy
      }

      #  Move position marker on the image.
      $canvas_ coords $ref_position_mark_ $ccx $ccy
   }

   #  Extract and display a region as a reference spectrum. The
   #  plot must already exist.
   public method display_region_reference_spectrum {region} {

      #  Must already have a plot, re-display if withdrawn.
      if { $spectrum_ == {} } {
         return
      }
      $spectrum_ open

      #  Correct extraction bounds to grid indices.
      set lb $itk_option(-lower_limit)
      set ub $itk_option(-upper_limit)
      set alow [$itk_option(-gaiacube) axis_pixel2grid $lb]
      set ahigh [$itk_option(-gaiacube) axis_pixel2grid $ub]
      if { $alow > $ahigh } {
         set tmp $alow
         set alow $ahigh
         set ahigh $tmp
      }

      #  Display it as a reference spectrum.
      set cubeaccessor [$itk_option(-gaiacube) get_cubeaccessor]
      set axis [$itk_option(-gaiacube) get_axis]
      busy {
         $spectrum_ display_region_reference $cubeaccessor $axis $alow $ahigh \
            $region $combination_type_
      } 0

      #  Reference position marker doesn't apply.
      if { $ref_position_mark_ != {} } {
         remove_position_markers_
      }
   }

   #  Set the current spectrum (that is the last one extracted) as the
   #  reference spectrum.
   public method set_reference {} {
      if { $spectrum_ != {} } {
         set spectype [last_extracted_type]
         if { $spectype == "point" } {
            eval display_point_reference_spectrum $last_cxcy_
         } elseif { $spectype == "region" } {
            display_region_reference_spectrum $last_region_
         }
      }
   }

   #  Clear the reference spectrum.
   public method clear_reference {} {
      if { $spectrum_ != {} } {
         $spectrum_ remove_reference
      }
   }

   #  Handle a change in one of the colours used by the spectral plot. These
   #  are propagated to the markers and ARD regions.
   protected method spec_colour_changed_ {what colour} {
      if { $what == "spectrum" } {
         $toolbox_ configure -selected_colour $colour
         set position_mark_colour_ $colour
         if { $position_mark_ != {} } {
            $canvas_ itemconfigure $position_mark_ -outline $colour
         }
      } elseif { $what == "reference" } {
         set ref_position_mark_colour_ $colour
         if { $ref_position_mark_ != {} } {
            $canvas_ itemconfigure $ref_position_mark_ -outline $colour
         }
      }
   }

   #  Create the main spectral line position marker.
   protected method create_position_marker_ { cx cy } {

      #  Note fixscale so that always same size, regardless of zoom.
      set position_mark_ [$canvas_ create rtd_mark $cx $cy -type cross \
                             -scale 1 -fixscale 1 -size 7 \
                             -outline $position_mark_colour_]

      #  Bindings to move and select this.
      $canvas_ bind $position_mark_ <1> \
         [code $this display_point_spectrum_ localstart %x %y]

      $canvas_ bind $position_mark_  <B1-Motion> \
         [code $this display_point_spectrum_ localdrag %x %y]
   }

   #  Create the reference spectral position marker.
   protected method create_ref_position_marker_ { cx cy } {

      #  Note fixscale so that always same size, regardless of zoom.
      set ref_position_mark_ [$canvas_ create rtd_mark $cx $cy -type cross \
                                 -scale 1 -fixscale 1 -size 7 \
                                 -outline $ref_position_mark_colour_]

      #  Bindings so that main position mark moves when over this.
      $canvas_ bind $ref_position_mark_ <1> \
         [code $this display_point_spectrum_ localstart %x %y]

      $canvas_ bind $ref_position_mark_  <B1-Motion> \
         [code $this display_point_spectrum_ localdrag %x %y]
   }

   #  Remove the position markers.
   protected method remove_position_markers_ {} {
      if { [winfo exists $canvas_] } {
         if { $position_mark_ != {} } {
            $canvas_ delete $position_mark_
            set position_mark_ {}
         }
         if { $ref_position_mark_ != {} } {
            $canvas_ delete $ref_position_mark_
            set ref_position_mark_ {}
         }
      }
   }


   #  Add controls for choosing a region shape.
   protected method add_region_controls_ {} {
      set toolbox_ [StarArdTool \#auto \
                       -rtdimage $rtdimage_ \
                       -canvasdraw $canvasdraw_ \
                       -canvas $canvas_ \
                       -maxcol 9 \
                       -selected_colour $position_mark_colour_ \
                       -deselected_colour "red" \
                       -notify_created_cmd [code $this region_created_]\
                       -notify_started_cmd [code $this region_started_]]

      #  Just use sensible types, no Pixel.
      set types [{*}$toolbox_ get_known_types]
      regsub "Pixel" $types "" clean_types
      {*}$toolbox_ known_types $clean_types

      #  Pack buttons like a labelled object.
      itk_component add bframe {
         frame $w_.bframe
      }
      itk_component add blabel {
         label $itk_component(bframe).label -text "Define region:" \
            -width $itk_option(-labelwidth) -anchor w
      }

      set buttons_ [{*}$toolbox_ make_types_frame $itk_component(bframe).tools]
      add_short_help $buttons_ {Choose a region, then draw on main image}

      pack $itk_component(bframe) -side top -fill x -expand 1 -ipadx 1m
      pack $itk_component(blabel) -side left -expand 0 -ipadx 1m
      pack $buttons_ -side left -expand 0 -anchor w -ipadx 1m

      #  Choice of combination method.
      itk_component add combination {
         util::LabelMenu $w_.combination \
            -labelwidth $itk_option(-labelwidth) \
            -text "Combination method:" \
            -variable [scope combination_type_]
      }
      pack $itk_component(combination) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(combination) \
         {Region data combination method}

      $itk_component(combination) add \
         -label Mean \
         -value mean \
         -command [code $this set_combination_type_ "mean"]
      $itk_component(combination) add \
         -label Median \
         -value median \
         -command [code $this set_combination_type_ "median"]
   }

   #  Just read a description from a file.
   public method read_ard_file {file} {
      $toolbox_ read_description $file
   }

   #  Set the combination type used for regions.
   protected method set_combination_type_ {value} {
      set combination_type_ $value
   }

   #  Method to deal with the creation of a region.
   protected method region_created_ {index id} {

      #  Configure underlying object to pass on movements and resizes, when
      #  the object is selected or deselected and return coordinates in GRID.
      $toolbox_ apply_cmd $index configure \
         -notify_update_cmd [code $this region_update_] \
         -use_origins 0 \
         -notify_selected_cmd [code $this update_region_selection]

      #  Display the first extraction.
      display_region_spectrum_ localstart [$toolbox_ get_selected_description 1]

      #  Bindings are disabled when object creation starts, so reenable.
      toggle_extraction_
   }

   #  Method to deal with the start of region creation.
   protected method region_started_ {} {

      #  Stop point tracking from interfering with interaction.
      remove_bindings_
   }

   #  Deal with notification that a region has been updated. Need to extract
   #  the current ARD description and display the extracted spectrum.
   protected method region_update_ {} {
      display_region_spectrum_ localdrag [$toolbox_ get_selected_description 1]
   }

   #  Region has been selected or deselected. Only react to selected which
   #  means extract spectrum with autoscaling.
   public method update_region_selection {state} {
      if { $state == "selected" } {
         display_region_spectrum_ localstart \
            [$toolbox_ get_selected_description 1]
      }
   }

   #  Remove all the regions.
   protected method remove_regions_ {} {
      $toolbox_ clear
   }

   #  Return a list of the current extraction limits, if they differ from the
   #  maximum and minimum bounds. Otherwise return an empty list.
   public method get_set_limits {} {
      set from [$itk_component(bounds) cget -from]
      set to [$itk_component(bounds) cget -to]
      if { $from != $itk_option(-lower_limit) ||
           $to != $itk_option(-upper_limit) } {
         return [list $itk_option(-lower_limit) $itk_option(-upper_limit)]
      }
      return {}
   }

   #  Set the minimum and maximum possible bounds of the extraction.
   public method set_bounds {plane_min plane_max} {

      set from [$itk_component(bounds) cget -from]
      set to [$itk_component(bounds) cget -to]
      $itk_component(bounds) configure -from $plane_min -to $plane_max

      #  If the min/max have changed, reset the extraction range (different
      #  axis or unrelated cube).
      if { $plane_min != $from || $plane_max != $to } {
         apply_limits $plane_min $plane_max
      } else {
         apply_limits $itk_option(-lower_limit) $itk_option(-upper_limit)
      }
   }

   #  Handle the change in the extraction reference range (user interaction by
   #  dragging or resizing range shown in plot).
   public method ref_range_moved {id coord1 coord2 action} {

      #  Inhibit feedback to graphics reference range, before applying the new
      #  bounds.
      if { $action == "move" } {
         set oldvalue [$itk_component(bounds) cget -show_ref_range]
         $itk_component(bounds) configure -show_ref_range 0
      }

      #  Update the bounds.
      if { $coord1 < $coord2 } {
         apply_limits $coord1 $coord2
      } else {
         apply_limits $coord2 $coord1
      }

      if { $action == "move" } {
         $itk_component(bounds) configure -show_ref_range $oldvalue
      }
   }

   #  Apply an extraction range, that's set it and display the changes.
   #  Use this instead of configure -lower_limit -upper_limit, after
   #  construction.
   public method apply_limits {lower upper} {
      $itk_component(bounds) configure -value1 $lower -value2 $upper
      set_limits_ $lower $upper
   }

   #  Set the extraction limits. Same as configure lower_limit & upper_limit.
   protected method set_limits_ {lower upper} {
      configure -lower_limit $lower -upper_limit $upper
   }

   #  Toggle the display of the reference range.
   protected method toggle_show_ref_range_ {} {
      $itk_component(bounds) configure \
         -show_ref_range $itk_option(-show_ref_range)
      if { $itk_option(-show_ref_range) } {
         $itk_option(-gaiacube) make_ref_range $itk_option(-ref_id)
         $itk_option(-gaiacube) set_ref_range_colour \
            $itk_option(-ref_id) "orange"
         $itk_component(bounds) configure \
            -value1 $itk_option(-lower_limit) \
            -value2 $itk_option(-upper_limit)
      } else {
         $itk_option(-gaiacube) remove_ref_range $itk_option(-ref_id)
      }
   }

   #  Make a reference range in the spectral plot, if shown. Use the given
   #  identifier.
   public method make_ref_range {id} {
      if { $spectrum_ != {} } {
         $spectrum_ make_ref_range $id
      }
   }

   #  Remove a reference range from the spectral plot, if shown. Use the
   #  given identifier.
   public method remove_ref_range {id} {
      if { $spectrum_ != {} } {
         $spectrum_ remove_ref_range $id
      }
   }

   #  Set the colour of the identified reference range.
   public method set_ref_range_colour {id colour} {
      if { $spectrum_ != {} } {
         $spectrum_ set_ref_range_colour $id $colour
      }
   }

   #  Update the spectral plot if the coordinate system has been changed.
   #  Re-extracts the last spectrum.
   public method coord_system_changed {} {
      if { $spectrum_ != {} } {
         set spectype [last_extracted_type]
         if { $spectype == "point"  } {
            eval display_point_spectrum_ localstart $last_cxcy_
         } elseif { $spectype == "region" } {
            display_region_spectrum_ localstart $last_region_
         }
      }
   }

   #  Set the position of a reference line shown in the plot.
   public method set_spec_ref_coord {id coord} {
      if { $spectrum_ != {} && $coord != {} } {
         $spectrum_ set_ref_line_coord $id $coord
      }
   }

   #  Set the position of a reference range shown in the plot.
   public method set_spec_ref_range_coord {id coord1 coord2} {
      if { $spectrum_ != {} && $coord1 != {} && $coord2 != {} } {
         $spectrum_ set_ref_range_coord $id $coord1 $coord2
      }
   }

   #  Return the name of the GaiaSpectralPlot instance.
   public method get_spectrum {} {
      if { $spectrum_ != {} } {
         return [code $spectrum_]
      }
      return {}
   }

   #  Return various useful coordinates. The centre of image, the coordinates
   #  of the extraction and the offset of the extraction from the image
   #  centre, all in world coordinates (usually RA and Dec). Note the
   #  coordinates of the extraction are for the whole pixel, not a sub-pixel
   #  canvas position.
   public method get_last_coords {} {
      set xra ""
      set xdec ""

      set ra ""
      set dec ""
      set dra ""
      set ddec ""

      set refra ""
      set refdec ""
      set drefra ""
      set drefdec ""

      if { $last_cxcy_ != {} } {

         #  Convert click coordinates from canvas coords to grid coords.
         lassign $last_cxcy_ cx cy
         set ccx [$canvas_ canvasx $cx]
         set ccy [$canvas_ canvasy $cy]
         $rtdimage_ convert coords $ccx $ccy canvas iix iiy image

         #  Make grid coordinates integer to pick centre of pixel.
         set iix [expr round($iix)]
         set iiy [expr round($iiy)]

         #  Centre of image, image coordinates.
         set icx [expr 0.5+0.5*[$rtdimage_ width]]
         set icy [expr 0.5+0.5*[$rtdimage_ height]]

         #  Offsets from the image centre (not distances). This needs
         #  the positions in degrees which are then offset along each
         #  of the sky axes.
         catch {
            $rtdimage_ convert coords $icx $icy image cra cdec deg
            $rtdimage_ convert coords $iix $iiy image pra pdec deg
            set dra [angdiff_ $pra $cra]
            set ddec [angdiff_ $pdec $cdec]

            #  Offset from centre, arcsecs.
            set dra [format "%f" [expr $dra*3600.0*cos($pdec*$PI_/180.0)]]
            set ddec [format "%f" [expr $ddec*3600.0]]
         } msg

         #  Distance from the source position. Take a guess at these. Try the
         #  SkyRef positions from the image, failing that see if we have a
         #  SpecFrame use RefRA and RefDec.
         set cubeaccessor [$itk_option(-gaiacube) get_cubeaccessor]
         set axis [$itk_option(-gaiacube) get_axis]
         if { [$cubeaccessor isaxisframetype $axis "specframe"] ||
              [$cubeaccessor isaxisframetype $axis "dsbspecframe"] } {

            catch {
               set rra [$rtdimage_ astget "SkyRef(1)"]
               set rdec [$rtdimage_ astget "SkyRef(2)"]

               if { $rra == "" || $rra == 0.0 } {
                  set frameset [$cubeaccessor getwcs]
                  set specframe [gaiautils::getaxis $frameset $axis]
                  lassign [gaiautils::astgetrefpos $specframe] rra rdec
                  gaiautils::astannul $specframe
               } else {
                  #  Radians to degrees.
                  set rra [expr $rra*180.0/$PI_]
                  set rdec [expr $rdec*180.0/$PI_]
               }

               #  Are axes interchanged?
               if { [$rtdimage_ astget "astime(2)"] } {
                  set raindex 2
                  set decindex 1
               } else {
                  set raindex 1
                  set decindex 2
               }

               #  Get extraction position in degrees (again), and work out
               #  the offsets to this reference position. If the coordinate
               #  system is measuring offset sky coordinates then we handle
               #  those differently.
               if { $raindex == 2 } {
                  lassign [$rtdimage_ astpix2wcs $iix $iiy 1 0] pdec pra
               } else {
                  lassign [$rtdimage_ astpix2wcs $iix $iiy 1 0] pra pdec
               }

               if { [$rtdimage_ astget "SkyRefIs"] != "Ignored" } {
                  set drefra [angdiff_ $pra 0.0]
                  set drefdec [angdiff_ $pdec 0.0]
               } else {
                  set drefra [angdiff_ $pra $rra]
                  set drefdec [angdiff_ $pdec $rdec]
               }
               set drefra [format "%f" \
                              [expr $drefra*3600.0*cos($pdec*$PI_/180.0)]]
               set drefdec [format "%f" [expr $drefdec*3600.0]]

               #  Format reference RA and Dec.
               set skyframe [gaiautils::astskyframe "System=FK5,Digits=9"]
               set refra \
                  [gaiautils::astformat $skyframe $raindex [expr $rra*$PI_/180.0]]
               set refdec \
                  [gaiautils::astformat $skyframe $decindex [expr $rdec*$PI_/180.0]]
               gaiautils::astannul $skyframe
            } msg
         }

         #  Image centre in world coordinates.
         lassign [$rtdimage_ astpix2cur $icx $icy] ra dec

         #  World coordinates of extraction pixel.
         lassign [$rtdimage_ astpix2cur $iix $iiy] xra xdec
      }
      return [list $ra $dec $xra $xdec $dra $ddec \
                   $refra $refdec $drefra $drefdec]
   }

   #  Handle change in the value of -fix_data_range.
   protected method toggle_fix_data_range_ {} {
      if { $spectrum_ != {} } {
         $spectrum_ configure -fix_data_range $itk_option(-fix_data_range)

         #  If switched on match any existing limits.
         if { $itk_option(-fix_data_range) } {
            set_upper_data_limit_ $itk_option(-data_high)
            set_lower_data_limit_ $itk_option(-data_low)
         }
      }
      set state disabled
      if { $itk_option(-fix_data_range) } {
         set state normal
      }
      $itk_component(datahigh) configure -state $state
      $itk_component(datalow) configure -state $state
   }

   #  Set the upper data limit.
   protected method set_upper_data_limit_ { value } {
      if { $value != {} && $itk_option(-data_low) != {} } {
         configure -data_high $value
         set_data_limits_
      }
   }

   #  Set the lower data limit.
   protected method set_lower_data_limit_ { value } {
      if { $value != {} && $itk_option(-data_high) != {} } {
         configure -data_low $value
         set_data_limits_
      }
   }

   #  Match the data limits to the current values.
   protected method set_data_limits_ {} {
      if { $spectrum_ != {} } {
         $spectrum_ configure -data_high $itk_option(-data_high) \
                              -data_low $itk_option(-data_low)
      }
   }

   #  Difference between two angles normalised into the range +/- 180.
   protected method angdiff_ {a1 a2} {
      set ang [expr $a1-$a2]
      set diff [expr fmod($ang,360.0)]
      if { $diff >= 180.0 } {
         set diff [expr $diff-360.0]
      } elseif { $diff <= -180.0 } {
         set diff [expr $diff+360.0]
      }
      return $diff
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Whether to extract spectra, or not.
   itk_option define -extraction extraction Extraction 1

   #  The related GaiaCube instance.
   itk_option define -gaiacube gaiacube GaiaCube {}

   #  The related Gaia instance.
   itk_option define -gaia gaia Gaia {} {
      if { $itk_option(-gaia) != {} } {
         set image_ [$itk_option(-gaia) get_image]
         set rtdimage_ [$image_ get_image]
         set canvasdraw_ [$image_ component draw]
         set canvas_ [$image_ get_canvas]
      }
   }

   #  Component of the cube that we're extracting. Usually DATA but could
   #  be VARIANCE or QUALITY.
   itk_option define -component component Component "DATA" {
      if { $spectrum_ != {} } {
         $spectrum_ configure -component $itk_option(-component)
      }
   }

   #  Whether window is transient, or not.
   itk_option define -transient_spectralplot transient_spectralplot \
      Transient_Spectralplot 1

   #  The identifier of the reference range.
   itk_option define -ref_id ref_id Ref_Id 1

   #  Whether to show the reference range.
   itk_option define -show_ref_range show_ref_range Show_Ref_Range 0

   #  Extraction bounds.
   itk_option define -lower_limit lower_limit Lower_Limit 0
   itk_option define -upper_limit upper_limit Upper_Limit 0

   #  Width of labels.
   itk_option define -labelwidth labelwidth LabelWidth 20

   #  Width of values.
   itk_option define -valuewidth valuewidth ValueWidth 20

   #  Enable/disable point tracking and extraction.
   itk_option define -point_tracking point_tracking Point_Tracking 1

   #  GaiaSpecCoords instance used by related GaiaCube.
   itk_option define -spec_coords spec_coords Spec_Coords {}

   #  GaiaSpecStdOfRest instance used by related GaiaCube.
   itk_option define -spec_sor spec_sor Spec_Coords {}

   #  Fix the spectral plot data range. Otherwise use min/max.
   itk_option define -fix_data_range fix_data_range Fix_Data_Range 0

   #  Upper and lower data limits.
   itk_option define -data_high data_high Data_High 1
   itk_option define -data_low data_low Data_Low 0

   #  Whether to show the output from splatdisp.
   itk_option define -show_splatdisp show_splatdisp Show_SplatDisp 0

   #  Command to execute when a spectrum is extracted or the extraction has
   #  been updated. Trailed by "p" or "r" for point, 0 or 1 to indicate if a
   #  locatstart or locatdrag has been done, and the extraction position, or
   #  region description as a single argument.
   itk_option define -notify_cmd notify_cmd Notify_Cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  The spectrum plot item.
   protected variable spectrum_ {}

   #  GaiaSpecWriter instance used by related GaiaSpectralPlot.
   protected variable spec_writer_ {}

   #  Maximum and minimum possible value for plane.
   protected variable plane_max_ 0
   protected variable plane_min_ 0

   #  Useful components of GAIA.
   protected variable image_ {}
   protected variable rtdimage_ {}
   protected variable canvasdraw_ {}
   protected variable canvas_ {}

   #  Toolbox in charge of ARD regions.
   protected variable toolbox_ {}

   #  The ARD buttons.
   protected variable buttons_ {}

   #  Last cx and cy values used to open a spectrum.
   protected variable last_cxcy_ {}

   #  Last region used to open a spectrum.
   protected variable last_region_ {}

   #  Last lower and upper extraction bounds. When these change any
   #  reference graphics should be updated.
   protected variable llb_ ""
   protected variable lub_ ""

   #  The SPLAT home directory.
   protected variable splat_dir_ {}

   #  Task controller for splatdisp command.
   protected variable splat_disp_ {}

   #  The position marker that corresponds to the spectrum.
   protected variable position_mark_ {}

   #  The position marker that corresponds to the reference spectrum.
   protected variable ref_position_mark_ {}

   #  Colours of the markers (and selected ARD regions).
   protected variable position_mark_colour_ "blue"
   protected variable ref_position_mark_colour_ "green"

   #  The region data combination type.
   protected variable combination_type_ "mean"

   #  Object to manage temporary file names.
   protected variable temp_files_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Counter for temporary files.
   common count_ 0

   common PI_ 3.14159265358979323846264338328

#  End of class definition.
}
