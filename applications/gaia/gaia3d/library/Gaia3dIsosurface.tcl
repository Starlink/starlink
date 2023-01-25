#+
#  Name:
#     Gaia3dIsosurface

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class for displaying various iso-surfaces of a data-cube.

#  Description:
#     This class renders iso-surfaces of a supported data-cube using
#     the facilities of the VTK library.

#  Invocations:
#
#        Gaia3dIsosurface object_name [configuration options]
#
#     This creates an instance of a Gaia3dIsosurface object. The return is
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

#  Copyright:
#     Copyright (C) 2007-2009 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     27-JUN-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual Gaia3dIsosurface {}

itcl::class gaia3d::Gaia3dIsosurface {

   #  Inheritances:
   #  -------------
   inherit gaia3d::Gaia3dTool

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA3D: Iso-surfaces ($itk_option(-number))"

      #  Make it a decent size (packing doesn't work).
      wm geometry  $w_ 800x800+0+0

      #  Add window help.
      add_help_button isosurface "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Save configuration to a file.
      set File [get_menu "File"]
      $File insert 1 command \
         -label {Save configuration...} \
         -command [code $this write_config_file] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this write_config_file]
      $short_help_win_ add_menu_short_help $File \
         {Save configuration...}\
         {Write the current configuration to a text file}

      #  Read configuration from a file.
      $File insert 2 command \
         -label {Read configuration...} \
         -command [code $this read_config_file] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_config_file]
      $short_help_win_ add_menu_short_help $File \
         {Read configuration...}\
         {Read previous configuration back from a text file}


      #  An option to sort any iso-surfaces into intensity ascending order.
      set Options [get_menu "Options"]
      $Options add checkbutton \
         -label {Sort levels} \
         -variable [scope sort_levels_] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Sort levels} \
         {Render iso-surfaces in intensity sorted order}

      #  Add an option to use same colour for all surfaces.
      $Options add checkbutton \
         -label {Use single colour} \
         -variable [scope single_colour_] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Use single colour} \
         {Draw all iso surfaces with the same colour}

      #  Add an option to use same opacity for all surfaces.
      $Options add checkbutton \
         -label {Use single opacity} \
         -variable [scope single_opacity_] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Use single opacity} \
         {Draw all iso-surfaces with the same opacity}

      #  Auto update.
      $Options add checkbutton \
         -label {Auto update} \
         -variable [scope auto_update_] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Auto update} \
         {Re-draw iso-surfaces after any change}
   }

   #  Destructor:
   #  -----------
   destructor  {
      #  Release all ColourLabelMenu objects.
      if { [info exists colour_menu_] } {
         for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
            delete object $colour_menu_($i)
         }
      }
   }

   #  Methods:
   #  --------

   #  Add the controls for the iso-surface attributes.
   protected method add_tool_controls_ {} {

      #  Get pane for levels and attributes.
      $itk_component(tab) add -label Levels

      #  Add controls for line attributes.
      add_att_controls_ [$itk_component(tab) childsite 0]

      #  Get pane for levels generation controls.
      $itk_component(tab) add -label Generate

      #  Add controls for level generation.
      add_gen_controls_ [$itk_component(tab) childsite 1]

      #  Add a button to clear all contour levels.
      itk_component add clear {
         button $itk_component(actionframe).clear -text {Clear levels} \
            -command [code $this clear_levels]
      }
      add_short_help $itk_component(clear) {Clear all levels and contours}
      pack $itk_component(clear) -side left -expand 1 -pady 3 -padx 3

      #  Display a window pane.
      $itk_component(tab) select 0
   }

   #  Add the controls for the contour levels and attributes.
   protected method add_att_controls_ {w} {

      itk_component add attrule {
         gaia::LabelRule $w.attrule -text "Contour levels & attributes:"
      }
      pack $itk_component(attrule) -side top -fill x

      #  Use a scrolled frame to get all these in a small amount of
      #  real estate.
      itk_component add atframe {
         ::iwidgets::scrolledframe $w.atframe \
            -width 75 \
            -height 200 \
            -hscrollmode dynamic \
            -vscrollmode dynamic
      }
      pack $itk_component(atframe) -fill both -expand 1
      set parent [$itk_component(atframe) childsite]

      #  Add headings.
      itk_component add athead1 {
         label $parent.value -text "Level"
      }
      itk_component add athead2 {
         label $parent.colour -text "Colour"
      }
      itk_component add athead3 {
         label $parent.opacity -text "Opacity"
      }

      grid $itk_component(athead1) $itk_component(athead2) \
         $itk_component(athead3)

      #  Set up the default colours (wrapped at maximum number, avoiding
      #  white and black, pens 0 and 1.
      for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
         set index [expr 2+int(fmod($i,13))]
         set coldefault_($i) [gaia::AstColours::lookup_colour $index]
      }

      #  Now add the controls for the actual values.
      for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {

         #  Entry widget for the contour values.
         itk_component add value$i {
            util::LabelEntry $parent.value$i \
               -validate real \
               -text "$i:" \
               -labelwidth 3 \
               -command [code $this set_level_ $i]
         }

         #  Menu for selecting the colour.
         itk_component add colour$i {
            util::LabelMenu $parent.colour$i \
               -relief raised
         }

         #  Now add all the colours to it.
         set colour_menu_($i) \
            [gaia::ColourLabelMenu \#auto $itk_component(colour$i) \
                -change_cmd [code $this set_colour_ $i] \
                -show_custom 0]

         #  Set to next colour in list.
         $itk_component(colour$i) configure -value $coldefault_($i)

         #  Add menu for selecting the opacity.
         itk_component add opacity$i {
            util::LabelMenu $parent.opacity$i \
               -relief raised
         }

         #  Now add the range of opacities to it.
         for {set j 0.0} {$j <= 1.0} {set j [expr $j+0.1]} {
            $itk_component(opacity$i) add \
               -label $j \
               -value $j \
               -command [code $this set_opacity_ $i]
         }
         set value [expr max(0.2,1.0-$i*0.2)]
         $itk_component(opacity$i) configure -value $value

         #  Need to make geometries up to date, otherwise a user define
         #  BorderWidth property seems to leave all widgets size 1.
         update idletasks

         #  Add these to the grid.
         grid $itk_component(value$i) $itk_component(colour$i) \
              $itk_component(opacity$i)
      }
   }

   #  Add a customized colour to the menus. Use an index if
   #  supplied. Otherwise create one.
   public method add_custom_colour { new_colour {index -1} } {
      if { $index == -1 } {
         #  Slight trick here is to get a valid index first and then use
         #  it for all additional entries
         set index [$colour_menu_(0) add_custom_colour $new_colour $index]
         for {set i 1} {$i < $itk_option(-maxcnt)} {incr i} {
            $colour_menu_($i) add_custom_colour $new_colour $index
         }
      } else {
         for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
            $colour_menu_($i) add_custom_colour $new_colour $index
         }
      }
      Gaia3dTool::add_custom_colour $new_colour $index
   }

   #  Get the level and attributes as a single string.
   public method get_levels_and_atts {} {
      set atts {}
      for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
         set value [$itk_component(value$i) get]
         if { $value != {} } {
            set colour [$itk_component(colour$i) get]
            set opacity [$itk_component(opacity$i) get]
            lappend atts "$value $colour $opacity"
         }
      }
      return $atts
   }

   #  Get the levels and attributes as a list for a contour.
   public method get_level_and_atts {index} {
      set value [$itk_component(value$index) get]
      if { $value != {} } {
         set colour [$itk_component(colour$index) get]
         set opacity [$itk_component(opacity$index) get]
         return [list $value $colour $opacity]
      }
      return ""
   }

   #  Clear the contours levels and attributes, or just the levels.
   public method clear_levels { {all 1} } {
      if { $all } {
         for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
            $itk_component(value$i) configure -value {}
            $itk_component(colour$i) configure -value $coldefault_($i)
            $itk_component(opacity$i) configure -value 0.5
         }
      } else {
         for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
            $itk_component(value$i) configure -value {}
         }
      }
      clear_scene 0
   }

   #  Level generation commands.
   protected method add_gen_controls_ {w} {

      #  Add section header.
      itk_component add genrule {
         gaia::LabelRule $w.genrule -text "Contour level generation:"
      }
      pack $itk_component(genrule) -side top -fill x

      #  Number of contours to generate.
      itk_component add ncont {
         util::LabelEntryScale $w.ncont \
            -text "Number:" \
            -labelwidth 14 \
            -valuewidth 3 \
            -increment 1  \
            -resolution 1 \
            -anchor w \
            -show_arrows 1 \
            -show_scale 1 \
            -from 1 \
            -to $itk_option(-maxcnt) \
            -fix_range 1 \
            -validate integer \
            -value 3
      }
      pack $itk_component(ncont) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(ncont) \
         {Number of contour levels to generate}

      #  Type of generation.
      itk_component add ctype {
         util::LabelMenu $w.ctype \
            -relief raised \
            -text {Algorithm:} \
            -labelwidth 14 \
            -valuewidth 20 \
      }
      pack $itk_component(ctype) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(ctype) \
         {Algorithm to use for contour generation}
      foreach type {automatic linear magnitude percentiles} {
         $itk_component(ctype) add \
            -label $type \
            -value $type \
            -command [code $this ctype_changed_]
      }

      #  Starting value.
      itk_component add start {
         util::LabelEntry $w.start \
            -validate real \
            -text "Start:" \
            -labelwidth 14 \
            -valuewidth 20
      }
      pack $itk_component(start) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(start) \
         {Starting point for level generation}

      #  Increment.
      itk_component add incre {
         util::LabelEntry $w.incre \
            -validate real \
            -text "Increment:" \
            -labelwidth 14 \
            -valuewidth 20 \
            -command [code $this generate_contours_]
      }
      pack $itk_component(incre) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(incre) \
         {Increment between generated levels}

      #  Percentile list.
      itk_component add percent {
         util::LabelEntry $w.percent \
            -text "Percentiles:" \
            -labelwidth 14 \
            -valuewidth 20 \
            -command [code $this generate_contours_]
      }
      pack $itk_component(percent) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(percent) \
         {List of percentiles (space separated)}

      #  Set initial state.
      $itk_component(ctype) configure -value automatic
      ctype_changed_

      #  Button to generate contours.
      itk_component add generate {
         button $w.gen \
            -text "Generate" \
            -command [code $this generate_contours_]
      }
      pack $itk_component(generate) -side top -ipadx 1m -ipady 1m -pady 4m
      add_short_help $itk_component(generate) \
         {Generate contours}
   }

   #  Configure entry fields when the generation type changes.
   protected method ctype_changed_ {} {
      set method [$itk_component(ctype) get]
      if { $method == "automatic" || $method == "percentiles" } {
         set state disabled
      } else {
         set state normal
      }
      $itk_component(start) configure -state $state
      $itk_component(incre) configure -state $state
      if { $method == "percentiles" } {
	 $itk_component(percent) configure -state normal
	 $itk_component(ncont) configure -state disabled
      } else {
	 $itk_component(percent) configure -state disabled
	 $itk_component(ncont) configure -state normal
      }
   }

   #  Generate contours levels. These are based on the image view of the
   #  cube, not the full data.
   protected method generate_contours_ {args} {
      set method [$itk_component(ctype) get]
      set ncont [$itk_component(ncont) get]
      set ncont [min $itk_option(-maxcnt) $ncont]
      set start [$itk_component(start) get]
      set incre [$itk_component(incre) get]
      set percent [$itk_component(percent) get]
      if { $method != "automatic" && $method != "percentiles" &&
           ( $start == {} || $incre == {} ) } {
         info_dialog "Please enter values for start and increment"
         return
      } elseif { $method == "percentiles" && $percent == {} } {
         info_dialog "Please enter values for percentiles"
         return
      }
      clear_levels 0
      if { $method == "magnitude" } {
         for {set i 0} {$i < $ncont} {incr i} {
            set value [expr $start*pow(10.0,-0.4*$i*$incre)]
            $itk_component(value$i) configure -value [format "%g" $value]
         }
      } elseif { $method == "linear" } {
         for {set i 0} {$i < $ncont} {incr i} {
            set value [expr $start+$i*$incre]
            $itk_component(value$i) configure -value [format "%g" $value]
         }
      } else {

	 #  Automatic, or percentiles. Need to use some of the data of the
         #  cube. Speed this up by using the current image.
         set rtdimage $itk_option(-rtdimage)
	 if { $method == "automatic" } {
	    set min [$rtdimage min]
	    set max [$rtdimage max]
	    set incre [expr double($max-$min)/double($ncont+1)]
            #  Run backwards, for correct use of opacities.
	    set start [expr $min+($incre*$ncont)]
	    for {set i 0} {$i < $ncont} {incr i} {
               set value [expr $start-($i*$incre)]
	       $itk_component(value$i) configure -value [format "%g" $value]
	    }
	 } else {

	    #  Percentiles.
	    set i 0
	    foreach level [$rtdimage percentiles $percent] {
	       $itk_component(value$i) configure -value $level
	       incr i
	       if { $i >= $itk_option(-maxcnt) } {
		  break
	       }
	    }
	 }
      }

      #  Switch to the levels pane.
      $itk_component(tab) select 0
   }

   #  Level has been set (user interaction), check if redraw is needed.
   protected method set_level_ {index value} {
      if { $auto_update_ } {
         draw
      }
   }

   #  Set the colour of all contours if using a single colour.
   #  Check if redraw is needed.
   protected method set_colour_ {index colindex} {
      if { $single_colour_ } {
         set colour [gaia::AstColours::lookup_colour $colindex]
         for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
            $itk_component(colour$i) configure -value $colour
         }
      }
      if { $auto_update_ } {
         draw
      }
   }

   #  Set the opacity of all contours if using a single value.
   #  Check if redraw is needed.
   protected method set_opacity_ {index} {
      if { $single_opacity_ } {
         set opacity [$itk_component(opacity$index) get]
         for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
            if { $i != $index } {
               $itk_component(opacity$i) configure -value $opacity
            }
         }
      }
      if { $auto_update_ } {
         draw
      }
   }

   #================================================================
   #  VTK setup.
   #================================================================

   #  Draw the scene. If data has changed newdata is set and the scene
   #  has already been cleared.
   protected method draw_scene_ { newdata } {

      if { ! $contoured_ || $newdata } {

         #  Create an actor for each possible level.
         for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
            create_iso_contour_ $i
         }
      }

      #  Update colours, levels and opacities. These may need to be ordered in
      #  increasing intensity so that the rendering is correct.
      set atts [get_levels_and_atts]
      if { $sort_levels_ } {
         set atts [lsort -decreasing -real -index 0 $atts]
      }
      set index 0
      foreach lco $atts {
         eval update_iso_contour_ $index $lco
         incr index
      }

      #  Set remaining iso-surfaces to show nothing.
      for {set i $index} {$i <$itk_option(-maxcnt)} {incr i} {
         if { [info exists contour_($i)] } {
            $contour_($i) set_invisible
         }
      }

      #  Some contours are now drawn.
      set contoured_ 1
   }

   #  Create objects to manage a contour.
   protected method create_iso_contour_ {index} {
      if { $imagedata_ != {} } {
         set contour_($index) [Gaia3dVtkIso \#auto \
                                  -stencil [$imagedata_ get_stencil] \
                                  -imagedata [$imagedata_ get_imagedata] \
                                  -renwindow $renwindow_]
         $contour_($index) add_to_window
         $contour_($index) set_visible
      }
   }

   #  Update the objects managing a contour to have a given level, colour and
   #  opacity.
   protected method update_iso_contour_ {index level colour opacity} {

      #  Do nothing if the contour actor don't exist.
      if { ! [info exists contour_($index)] } {
         return
      }
      $contour_($index) set_visible
      $contour_($index) set_lco $level $colour $opacity
   }

   #  Release all iso contours. Note doesn't release data unless fullrelease
   #  is true.
   public method release_objects { fullrelease } {

      #  Clear the renderer of all contouring actors.
      if { [info exists contour_] } {
         foreach index [array names contour_] {
            $contour_($index) remove_from_window
            ::delete object $contour_($index)
         }
         unset contour_
      }

      #  Release image data if requested.
      gaia3d::Gaia3dTool::release_objects $fullrelease

      #  No contours are now drawn.
      set contoured_ 0
   }

   #=====================================================================
   #  Save restore
   #=====================================================================

   #  Save the current configuration to a file.
   public method write_config_file {} {
      set w [util::FileSelect .\#auto -title "Save configuration to a file"]
      if {[$w activate]} {
         save_config [$w get]
      }
      destroy $w
   }

   #  Restore configuration from a file.
   public method read_config_file {} {
      set w [util::FileSelect .\#auto -title "Read configuration from a file"]
      if {[$w activate]} {
         read_config [$w get]
      }
      destroy $w
   }

   #  Write the current configuration to a named file. This is written
   #  in a the format:
   #
   #     level  colour opacity
   #     level  colour opacity
   #     ...
   #
   #     parameter = value
   public method save_config {filename} {
      if { $filename != {} } {
         busy {
            #  Open the output file.
            set fid [::open $filename w]
            puts $fid "\# GAIA3D Iso-surface configuration file."
            puts $fid "\#"

            #  Add any custom colours first. These need to be
            #  available before being used.
            set custom [gaia::AstColours::describe_custom]
            if { $custom != {} } {
               puts $fid "\#  Customized colours"
               puts $fid "colours = \{$custom\}"
            }

            #  Get the level attributes.
            set levatts [get_levels_and_atts]

            puts $fid [format "\# %-26s %-10s %-10s" level colour opacity]
            foreach line $levatts {
               lassign $line level colour opacity
               puts $fid [format "  %-26s %-10s %-10s" $level $colour $opacity]
            }

            ::close $fid
         }
      }
   }

   #  Read in configuration from a file.
   public method read_config {filename} {
      if { [file readable $filename] } {
         busy {
            #  Open the file.
            set fid [open $filename r]

            #  Clear existing contours.
            catch {clear_levels}

            #  Loop over the file skipping comments and blank
            #  lines.
            set ok 1
            set count 0
            while { $ok  } {
               set llen [gets $fid line]
               if { $llen > 0 } {
                  switch -glob $line {
                     *=* {
                        eval set_parameter_ $line
                     }
                     \#* {
                        #  Comment do nothing.
                     }
                     default {
                        if { [llength $line] == 3 } {
                           eval add_contour_ $count $line
                           incr count
                        } else {
                           warning_dialog \
                              "unrecognised line in configuration file: $line"
                        }
                     }
                  }
               } elseif { $llen < 0 } {

                  # End of file.
                  set ok 0
               }
            }
            ::close $fid
         }
      }
   }

   #  Add a new contour to draw.
   protected method add_contour_ {ncont value colour opacity} {
      $itk_component(value$ncont) configure -value $value
      $itk_component(opacity$ncont) configure -value $opacity
      $itk_component(colour$ncont) configure -value $colour
   }

   #  Assign a parameter value read back from a configuration file.
   #  The equals parameter is ignored.
   protected method set_parameter_ {param equals value} {
      switch -exact $param {
         colours {
            restore_custom_ $value
         }
         default {
            warning_dialog "unrecognised configuration parameter: $param"
         }
      }
   }

   #  Restore menu custom colours.
   protected method restore_custom_ {spec} {
      foreach {index colour} "$spec" {
         add_custom_colour $colour $index
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Maximum number of contours, only works once.
   itk_option define -maxcnt maxcnt Maxcnt 10

   #  Protected variables: (available to instance)
   #  --------------------

   #  Whether any contours are drawn (controls redraw).
   protected variable contoured_ 0

   #  Default colours.
   protected variable coldefault_

   #  Whether to use a single colour for all contours.
   protected variable single_colour_ 0

   #  Whether to use a single opacity for all contours.
   protected variable single_opacity_ 0

   #  Whether to redraw when an attribute is changed.
   protected variable auto_update_ 0

   #  Colour menus.
   protected variable colour_menu_

   #  Iso-surface handlers.
   protected variable contour_

   #  Whether to sort levels into increasing order.
   protected variable sort_levels_ 1

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Number of potential contours drawn.
   common unique_ 0


#  End of class definition.
}
