#+
#  Name:
#     GaiaStartup

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Display and configure startup-level options.

#  Description:
#     This class creates a dialog window that allows the various
#     command-line configuration options to be defined and saved
#     to the GAIA "properties" file. Depending on the type of option
#     these are either applied now, or the next time that GAIA
#     starts.

#  Invocations:
#
#        GaiaStartup object_name [configuration options]
#
#     This creates an instance of a GaiaStartup object. The return is
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
#     Copyright (C) 2003-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006-2007 Particle Physics & Astronomy Research Council.
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
#     22-JAN-2003 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaStartup {}

itcl::class gaia::GaiaStartup {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Get the properties object.
      set props_ [gaia::GaiaProperties::instance]

      #  Start with defaults and any previously set values.
      set_defaults_
      match_properties_

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      # Set the top-level window title.
      wm title $w_ "GAIA: Set startup options"

      #  Add short help window
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Add the apply menu item
      $File add command -label Apply \
         -command [code $this apply] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this apply]

      #  Add the close menu item
      $File add command -label Close \
         -command [code $this close] \
         -accelerator {Control-x}
      bind $w_ <Control-x> [code $this close]

      #  Add window help.
      add_help_button startupoptions "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Tabbed window for various controls.
      itk_component add tabbedpane {
         iwidgets::tabnotebook $w_.tab -angle 0 -tabpos n \
            -height 400 -width 500
      }
      pack $itk_component(tabbedpane) -side top -fill both -expand 1

      #  Add various controls....
      $itk_component(tabbedpane) add -label Elements
      add_element_controls_ [$itk_component(tabbedpane) childsite 0]

      $itk_component(tabbedpane) add -label Control
      add_control_controls_ [$itk_component(tabbedpane) childsite 1]

      $itk_component(tabbedpane) add -label Colours
      add_colour_controls_ [$itk_component(tabbedpane) childsite 2]

      $itk_component(tabbedpane) add -label Fonts
      add_font_controls_ [$itk_component(tabbedpane) childsite 3]

      #  Reveal a page.
      $itk_component(tabbedpane) select 0

      #  Add action buttons.
      itk_component add actions {
         frame $w_.actions
      }
      pack $itk_component(actions) -side bottom -fill x

      #  Apply configuration
      itk_component add apply {
         button $itk_component(actions).apply -text Apply \
            -command [code $this apply]
      }
      add_short_help $itk_component(apply) \
         {Apply configuration, saving for next GAIA startup}
      pack $itk_component(apply) -side left -expand 1 -pady 3 -padx 3

      #  Reset window to defaults.
      itk_component add reset {
         button $itk_component(actions).reset -text Reset \
            -command [code $this reset]
      }
      add_short_help $itk_component(reset) \
         {Reset window to defaults}
      pack $itk_component(reset) -side left -expand 1 -pady 3 -padx 3

      #  Close window
      itk_component add close {
         button $itk_component(actions).close -text Close \
            -command [code $this close]
      }
      add_short_help $itk_component(close) \
         {Close window}
      pack $itk_component(close) -side left -expand 1 -pady 3 -padx 3
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Set current properties.
   protected method match_properties_ {} {
      foreach prop [$props_ get_named_keys Gaia] {
         set value [$props_ get_property $prop]
         set key [$props_ get_unnamed_key Gaia $prop]
         if { $value != {} } {
            set values_($key) $value
         }
      }
   }

   #  Apply changes
   public method apply {} {
      save_properties_
      apply_properties_
   }

   #  Close window without any other action.
   public method close {} {
      $itk_component(labelfont) withdraw
      $itk_component(textfont) withdraw
      wm withdraw $w_
   }

   #  Reset window to defaults.
   public method reset {} {
      set_defaults_
      $itk_component(minscale) configure -value $values_(min_scale)
      $itk_component(maxscale) configure -value $values_(max_scale)
   }

   #  Set defaults for widget states.
   protected method set_defaults_ {} {
      set values_(always_merge) 0
      set values_(check_for_cubes) 1
      set values_(default_cmap) real
      set values_(default_cut) 100.0
      set values_(isize) 9
      set values_(maxshift) 5.5
      set values_(default_itt) ramp
      set values_(extended_precision) 0
      set values_(float_panel) 0
      set values_(focus_follows_mouse) 0
      set values_(interop_menu) 1
      set values_(linear_cartesian) 1
      set values_(force_degrees) 0
      set values_(max_scale) 20
      set values_(min_scale) -10
      set values_(panel_orient) horizontal
      set values_(quiet_exit) 1
      set values_(scrollbars) 1
      set values_(show_hdu_chooser) 1
      set values_(transient_tools) 0
      set values_(transient_spectralplot) 1
      set values_(with_colorramp) 1
      set values_(with_pan_window) 1
      set values_(with_zoom_window) 1
      set values_(zoom_factor) 4
      set values_(pick_zoom_factor) 10
      set values_(autoscale) 0
      set values_(autofit) 0
      set values_(pixel_indices) 0

      set values_(labelfont) TkDefaultFont
      set values_(textfont) TkFixedFont
      set values_(font_scale) 0.0
      set values_(unicoderadec) 1

      set values_(blank_color) black
      set values_(image_background) black
      set values_(pos_interest_color) green
   }

   #  Update the properties object to the local values and cause a
   #  save to backing store.
   protected method save_properties_ {} {
      foreach key $options {
         $props_ set_named_property Gaia $key $values_($key)
      }
      $props_ save_properties
   }

   #  Apply any properties that can be done now, rather than just left
   #  for the next startup.
   protected method apply_properties_ {} {
      if { $itk_option(-image) != {} } {
         $itk_option(-image) configure -extended_precision \
            $values_(extended_precision)
         $itk_option(-image) configure -show_hdu_chooser \
            $values_(show_hdu_chooser)
         $itk_option(-image) configure -default_cut \
            $values_(default_cut)
         $itk_option(-image) configure -linear_cartesian \
            $values_(linear_cartesian)
         $itk_option(-image) configure -force_degrees \
            $values_(force_degrees)
         $itk_option(-image) configure -always_merge \
            $values_(always_merge)
      }
      if { $itk_option(-gaia) != {} } {
         $itk_option(-gaia) configure -transient_tools \
            $values_(transient_tools)
         $itk_option(-gaia) configure -transient_spectralplot \
            $values_(transient_spectralplot)
         $itk_option(-gaia) configure -quiet_exit \
            $values_(quiet_exit)
         $itk_option(-gaia) configure -check_for_cubes \
            $values_(check_for_cubes)
         $itk_option(-gaia) configure -isize \
            $values_(isize)
         $itk_option(-gaia) configure -maxshift \
            $values_(maxshift)
         $itk_option(-gaia) configure -autoscale \
            $values_(autoscale)
         $itk_option(-gaia) configure -autofit \
            $values_(autofit)
         $itk_option(-gaia) configure -pixel_indices \
            $values_(pixel_indices)

         $itk_option(-gaia) configure -blank_color \
            $values_(blank_color)
         $itk_option(-gaia) configure -image_background \
            $values_(image_background)
         $itk_option(-gaia) configure -pos_interest_color \
            $values_(pos_interest_color)

         $itk_option(-gaia) configure -unicoderadec \
            $values_(unicoderadec)

      }
   }

   #  Set element of values_.
   protected method set_value_ {key value} {
      set values_($key) $value
   }

   #  Add "elements" controls, i.e things you can see.
   protected method add_element_controls_ {parent} {

      #  Float panel.
      itk_component add floatpanel {
         gaia::StarLabelCheck $parent.floatpanel \
            -text "Use a floating panel:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(float_panel)]
      }
      add_short_help $itk_component(floatpanel) \
         {Create panel area in own window (requires restart)}
      pack $itk_component(floatpanel) -side top -fill x

      #  Panel orientation.
      itk_component add panelorient {
         util::LabelMenu $parent.panelorient -text "Panel orientation:" \
            -labelwidth $lwidth_ \
            -variable [scope values_(panel_orient)]
      }
      foreach value "horizontal vertical" {
         $itk_component(panelorient) add -label $value \
            -command [code $this set_value_ panel_orient $value]
      }
      add_short_help $itk_component(panelorient) \
         {Change the orientation of the control panel (requires restart)}
      pack $itk_component(panelorient) -side top -fill x

      #  Zoom window
      itk_component add withzoomwindow {
         gaia::StarLabelCheck $parent.withzoomwindow \
            -text "Display a zoom window:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(with_zoom_window)]
      }
      add_short_help $itk_component(withzoomwindow) \
         {Display a zoom window (requires restart)}
      pack $itk_component(withzoomwindow) -side top -fill x

      #  Pan window
      itk_component add withpanwindow {
         gaia::StarLabelCheck $parent.withpanwindow \
            -text "Display a panner window:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(with_pan_window)]
      }
      add_short_help $itk_component(withpanwindow) \
         {Display a panner window (requires restart)}
      pack $itk_component(withpanwindow) -side top -fill x

      #  Colorramp window
      itk_component add withcolorramp {
         gaia::StarLabelCheck $parent.withcolorramp \
            -text "Display a color ramp:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(with_colorramp)]
      }
      add_short_help $itk_component(withcolorramp) \
         {Display a color ramp at bottom of window (requires restart)}
      pack $itk_component(withcolorramp) -side top -fill x

      #  Scrollbars
      itk_component add scrollbars {
         gaia::StarLabelCheck $parent.scrollbars \
            -text "Show image scrollbars:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(scrollbars)]
      }
      add_short_help $itk_component(scrollbars) \
         {Display scrollbars around main image (requires restart)}
      pack $itk_component(scrollbars) -side top -fill x

      #  Show HDU chooser by default
      itk_component add hduchooser {
         gaia::StarLabelCheck $parent.hduchooser \
            -text "Show HDU chooser:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(show_hdu_chooser)]
      }
      add_short_help $itk_component(hduchooser) \
         {Show the HDU chooser, by default, when loading multiextension images}
      pack $itk_component(hduchooser) -side top -fill x

      #  Display the SAMP interop menu.
      itk_component add interopmenu {
         gaia::StarLabelCheck $parent.interopmenu \
            -text "Show Interop menu:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(interop_menu)]
      }
      add_short_help $itk_component(interopmenu) \
         {Show the main Interop menu for SAMP interactions}
      pack $itk_component(interopmenu) -side top -fill x

      #  Minimum zoom scale.
      itk_component add minscale {
         util::LabelEntryScale $parent.minscale \
            -text {Minimum zoom:} \
            -labelwidth $lwidth_ \
            -valuewidth 4 \
            -from -50 \
            -to 1 \
            -increment 1 \
            -show_arrows 1 \
            -resolution 1 \
            -anchor w \
            -value $values_(min_scale) \
            -command [code $this set_value_ min_scale]
      }
      add_short_help $itk_component(minscale) \
         {Minimum zoom scale (-ve for smaller, requires restart)}
      pack $itk_component(minscale) -side top -fill x

      #  Maximum zoom scale.
      itk_component add maxscale {
         util::LabelEntryScale $parent.maxscale \
            -text {Maximum zoom:} \
            -labelwidth $lwidth_ \
            -valuewidth 4 \
            -from 1 \
            -to 50 \
            -increment 1 \
            -show_arrows 1 \
            -resolution 1 \
            -anchor w \
            -value $values_(max_scale) \
            -command [code $this set_value_ max_scale]
      }
      add_short_help $itk_component(maxscale) \
         {Maximum zoom scale (requires restart)}
      pack $itk_component(maxscale) -side top -fill x

   }

   #  Add "control" controls, i.e things you can configure.
   protected method add_control_controls_ {parent} {
      #  Focus follows mouse
      itk_component add focusfollowsmouse {
         gaia::StarLabelCheck $parent.focusfollowsmouse \
            -text "Focus follows mouse:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(focus_follows_mouse)]
      }
      add_short_help $itk_component(focusfollowsmouse) \
         {Entry fields focus follows mouse position (requires restart)}
      pack $itk_component(focusfollowsmouse) -side top -fill x

      # Transient tools
      itk_component add transienttools {
         gaia::StarLabelCheck $parent.transienttools \
            -text "Make toolboxes transient:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(transient_tools)]
      }
      add_short_help $itk_component(transienttools) \
         {Make toolboxes remain above main window (requires restart)}
      pack $itk_component(transienttools) -side top -fill x

      # Transient spectral plot.
      itk_component add transientplot {
         gaia::StarLabelCheck $parent.transientplot \
            -text "Make spectral plot transient:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(transient_spectralplot)]
      }
      add_short_help $itk_component(transientplot) \
         {Make spectral plot window remain above main window (requires restart)}
      pack $itk_component(transientplot) -side top -fill x

      # Exit without asking.
      itk_component add quietexit {
         gaia::StarLabelCheck $parent.quietexit \
            -text "Exit without prompt:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(quiet_exit)]
      }
      add_short_help $itk_component(quietexit) \
         {Make "Exit" option ask before closing GAIA}
      pack $itk_component(quietexit) -side top -fill x

      #  Extended precision.
      itk_component add precision {
         gaia::StarLabelCheck $parent.precision \
            -text "Display milli-arcsecs:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(extended_precision)]
      }
      add_short_help $itk_component(precision) \
         {Display milli-arcsecond resolution in readouts}
      pack $itk_component(precision) -side top -fill x

      #  Linear cartesian projection.
      itk_component add cartesian {
         gaia::StarLabelCheck $parent.cartesian \
            -text "Linear CAR projections:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(linear_cartesian)]
      }
      add_short_help $itk_component(cartesian) \
         {Assume any FITS CAR projections are simple linear mapping}
      pack $itk_component(cartesian) -side top -fill x

      #  Force display of degrees.
      itk_component add forcedegrees {
         gaia::StarLabelCheck $parent.forcedegrees \
            -text "Display decimal degrees:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(force_degrees)]
      }
      add_short_help $itk_component(forcedegrees) \
         {Force the display of decimal degrees in main window}
      pack $itk_component(forcedegrees) -side top -fill x

      #  How to merge MEF headers.
      itk_component add alwaysmerge {
         gaia::StarLabelCheck $parent.alwaysmerge \
            -text "Always merge MEF headers:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(always_merge)]
      }
      add_short_help $itk_component(alwaysmerge) \
         {Always merge primary into extension headers for full WCS}
      pack $itk_component(alwaysmerge) -side top -fill x

      #  Check for cubes.
      itk_component add checkforcubes {
         gaia::StarLabelCheck $parent.checkforcubes \
            -text "Check for cubes:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(check_for_cubes)]
      }
      add_short_help $itk_component(checkforcubes) \
         {Check any opened files for cubes, if found open in cube toolbox}
      pack $itk_component(checkforcubes) -side top -fill x

      #  Autoscale images to fit window.
      itk_component add autoscale {
         gaia::StarLabelCheck $parent.autoscale \
            -text "Auto scale:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(autoscale)]
      }
      add_short_help $itk_component(autoscale) \
         {Auto scale images to fit window, disables zoom}
      pack $itk_component(autoscale) -side top -fill x

      #  Autofit images to window (once).
      itk_component add autofit {
         gaia::StarLabelCheck $parent.autofit \
            -text "Auto fit:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(autofit)]
      }
      add_short_help $itk_component(autofit) \
         {Auto fit new images to window, keeps zoom}
      pack $itk_component(autofit) -side top -fill x

      #  Display pixel indices not grid coordinates.
      itk_component add pixelindices {
         gaia::StarLabelCheck $parent.pixelindices \
            -text "Pixel indices:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(pixel_indices)]
      }
      add_short_help $itk_component(pixelindices) \
         {Display NDF pixel indices as X,Y not grid coordinates}
      pack $itk_component(pixelindices) -side top -fill x

      #  Zoom factor used in zoom window.
      itk_component add zoomfactor {
         util::LabelEntryScale $parent.zoomfactor \
            -text {Zoom factor:} \
            -labelwidth $lwidth_ \
            -valuewidth 4 \
            -from 2 \
            -to 16 \
            -increment 1 \
            -show_arrows 1 \
            -resolution 1 \
            -anchor w \
            -value $values_(zoom_factor) \
            -command [code $this set_value_ zoom_factor]
      }
      add_short_help $itk_component(zoomfactor) \
         {Zoom factor of zoomed window (requires restart)}
      pack $itk_component(zoomfactor) -side top -fill x

      #  Zoom factor used in pick window.
      itk_component add pickzoomfactor {
         util::LabelEntryScale $parent.pickzoomfactor \
            -text {Pick zoom factor:} \
            -labelwidth $lwidth_ \
            -valuewidth 4 \
            -from 2 \
            -to 16 \
            -increment 1 \
            -show_arrows 1 \
            -resolution 1 \
            -anchor w \
            -value $values_(pick_zoom_factor) \
            -command [code $this set_value_ pick_zoom_factor]
      }
      add_short_help $itk_component(pickzoomfactor) \
         {Default zoom factor of pick window (requires restart)}
      pack $itk_component(pickzoomfactor) -side top -fill x

      #  Default percentage cut used for new files.
      itk_component add defaultcut {
         util::LabelEntryScale $parent.defaultcut \
            -text {Default cut:} \
            -labelwidth $lwidth_ \
            -valuewidth 4 \
            -from 50 \
            -to 100 \
            -increment 0.5 \
            -show_arrows 1 \
            -resolution 0.5 \
            -anchor w \
            -value $values_(default_cut) \
            -command [code $this set_value_ default_cut]
      }
      add_short_help $itk_component(defaultcut) \
         {Default percentage cut used for new files}
      pack $itk_component(defaultcut) -side top -fill x

      #  Search box size used for centroiding.
      itk_component add isize {
         util::LabelEntryScale $parent.isize \
            -text {Centroid search box:} \
            -labelwidth $lwidth_ \
            -valuewidth 4 \
            -from 3 \
            -to 21 \
            -increment 1 \
            -show_arrows 1 \
            -resolution 1 \
            -anchor w \
            -value $values_(isize) \
            -command [code $this set_value_ isize]
      }
      add_short_help $itk_component(isize) \
         {Size of search box used when centroiding}
      pack $itk_component(isize) -side top -fill x

      #  Maximum shift allowed when centroiding.
      itk_component add maxshift {
         util::LabelEntryScale $parent.maxshift \
            -text {Centroid max shift:} \
            -labelwidth $lwidth_ \
            -valuewidth 4 \
            -from 3.5 \
            -to 21.5 \
            -increment 1 \
            -show_arrows 1 \
            -resolution 0.5 \
            -anchor w \
            -value $values_(maxshift) \
            -command [code $this set_value_ maxshift]
      }
      add_short_help $itk_component(maxshift) \
         {Maximum shift from initial position when centroiding}
      pack $itk_component(maxshift) -side top -fill x
   }

   #  Add "colour" controls.
   protected method add_colour_controls_ {parent} {

      #  Default colormap
      itk_component add defaultcmap {
         util::LabelMenu $parent.cmap -text "Default colormap:" \
            -labelwidth $lwidth_ \
            -variable [scope values_(default_cmap)]
      }
      set cmap_files [$itk_option(-image) cmap list]
      foreach map $cmap_files {
         set map [file rootname $map]
         $itk_component(defaultcmap) add -label $map \
            -command [code $this set_value_ default_cmap $map]
      }
      add_short_help $itk_component(defaultcmap) \
         {Default colourmap (requires restart)}
      pack $itk_component(defaultcmap) -side top -fill x -expand 0

      #  Blank pixel colour.
      itk_component add blankcolour {
         util::LabelMenu $parent.blankcolour \
            -text "Blank colour:" \
            -labelwidth $lwidth_ \
            -variable [scope values_(blank_color)]
      }
      foreach colour $colours_ {
         $itk_component(blankcolour) add \
            -label {    } \
            -value $colour \
            -background $colour \
            -command [code $this set_value_ blank_color $colour]
      }
      add_short_help $itk_component(blankcolour) \
         {Colour for blank pixels (requires restart)}
      pack $itk_component(blankcolour) -side top -fill x -expand 0
      $itk_component(blankcolour) configure -value $values_(blank_color)

      #  Image background colour.
      itk_component add backgroundcolour {
         util::LabelMenu $parent.backgroundcolour \
            -text "Background colour:" \
            -labelwidth $lwidth_ \
            -variable [scope values_(image_background)]
      }
      foreach colour $colours_ {
         $itk_component(backgroundcolour) add \
            -label {    } \
            -value $colour \
            -background $colour \
            -command [code $this set_value_ image_background $colour]
      }
      add_short_help $itk_component(backgroundcolour) \
         {Colour for main image background (requires restart)}
      pack $itk_component(backgroundcolour) -side top -fill x -expand 0
      $itk_component(backgroundcolour) configure \
         -value $values_(image_background)

      #  Position of interest colour.
      itk_component add posinterestcolour {
         util::LabelMenu $parent.posinterestcolour \
            -text "Position of interest colour:" \
            -labelwidth $lwidth_ \
            -variable [scope values_(pos_interest_color)]
      }
      foreach colour $colours_ {
         $itk_component(posinterestcolour) add \
            -label {    } \
            -value $colour \
            -background $colour \
            -command [code $this set_value_ pos_interest_color $colour]
      }
      add_short_help $itk_component(posinterestcolour) \
         {Colour for interop position of interest (requires restart)}
      pack $itk_component(posinterestcolour) -side top -fill x -expand 0
      $itk_component(posinterestcolour) configure \
         -value $values_(pos_interest_color)

   }

   #  Add "font" controls.
   protected method add_font_controls_ {parent} {

      #  Fonts, label and text.
      itk_component add labelfont {
         gaia::LabelFontChooser $parent.labelfont \
            -text "Label font:" \
            -chooser_title "Label font" \
            -labelwidth 10 \
            -value $values_(labelfont) \
            -textvariable [scope values_(labelfont)]
      }
      add_short_help $itk_component(labelfont) \
         {Font used for labels (requires restart)}
      pack $itk_component(labelfont) -side top -fill x -expand 0

      itk_component add textfont {
         gaia::LabelFontChooser $parent.textfont \
            -text "Text font:" \
            -chooser_title "Text font" \
            -chooser_fixed_width 1 \
            -labelwidth 10 \
            -value $values_(textfont) \
            -textvariable [scope values_(textfont)]
      }
      add_short_help $itk_component(textfont) \
         {Font used for fixed width text (requires restart)}
      pack $itk_component(textfont) -side top -fill x -expand 0

      #  Font scale. Only scales non-pixel fonts, but adjusts padding anyway.
      itk_component add fontscale {
         util::LabelEntryScale $parent.fontscale \
            -text {Font scale:} \
            -labelwidth 10 \
            -valuewidth 4 \
            -from 0.0 \
            -to 4.0 \
            -increment 0.1 \
            -show_arrows 1 \
            -resolution 0.05 \
            -anchor w \
            -value $values_(font_scale) \
            -command [code $this set_value_ font_scale]
      }
      add_short_help $itk_component(fontscale) \
         {Scale factor for non-pixel fonts, 0 for default}
      pack $itk_component(fontscale) -side top -fill x

      #  Whether to use unicode RA and Dec labels. Sometimes these are not available.
      #  Focus follows mouse
      itk_component add unicoderadec {
         gaia::StarLabelCheck $parent.unicoderadec \
            -text "Use unicode RA and Dec symbols:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth_ \
            -variable [scope values_(unicoderadec)]
      }
      add_short_help $itk_component(unicoderadec) \
         {RA and Dec labels display unicode alpha and delta symbols}
      pack $itk_component(unicoderadec) -side top -fill x
   }

   #  Check if an option is currently supported. Use to test if
   #  a stored values should be part of an objects configuration
   #  (maybe untrue if this is an older version).
   public proc check_option {option} {
      if { [lsearch -exact $options $option] == -1 } {
         return 0
      }
      return 1
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of GaiaImageCtrl widget that some options may be applied to
   #  directly.
   itk_option define -image image Image {} {}

   #  Name of Gaia widget that is our parent.
   itk_option define -gaia gaia Gaia {} {}

   #  Directory for colormap and ITT files
   itk_option define -cmap_dir cmap_dir Cmap_dir "" {
      if {"$itk_option(-cmap_dir)" == ""} {
         global ::rtd_library
         set itk_option(-cmap_dir) "$rtd_library/colormaps"
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  The GaiaProperties object. Just one instance of this.
   protected variable props_ {}

   #  Offered colours of the main background and blank pixels.
   protected variable colours_ {
      white
      grey90 grey80 grey70 grey60 grey50 grey40 grey30 grey20 grey10
      black
      red green blue cyan magenta yellow
   }

   #  General label width.
   protected variable lwidth_ 25

   #  Values shared by widgets -- indexed by (fieldname).
   protected variable values_

   #  Common variables: (shared by all instances)
   #  -----------------

   #  List of options that are handled.
   common options {
      always_merge
      autofit
      autoscale
      blank_color
      check_for_cubes
      default_cmap
      default_cut
      default_itt
      extended_precision
      float_panel
      focus_follows_mouse
      font_scale
      force_degrees
      image_background
      pos_interest_color
      interop_menu
      isize
      labelfont
      linear_cartesian
      max_scale
      maxshift
      min_scale
      panel_orient
      pick_zoom_factor
      pixel_indices
      quiet_exit
      scrollbars
      show_hdu_chooser
      textfont
      transient_spectralplot
      transient_tools
      unicoderadec
      with_colorramp
      with_pan_window
      with_zoom_window
      zoom_factor
   }

#  End of class definition.
}
