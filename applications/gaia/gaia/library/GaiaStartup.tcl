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
      set props_ [GaiaProperties::instance]

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
      set File [add_menubutton "File" left]
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

      #  Add various controls....
      add_controls_

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
         set values_($this,$key) $value
      }
   }

   #  Apply changes
   public method apply {} {
      save_properties_
      apply_properties_
   }

   #  Close window without any other action.
   public method close {} {
      wm withdraw $w_
   }

   #  Reset window to defaults.
   public method reset {} {
      set_defaults_
      $itk_component(minscale) configure -value $values_($this,min_scale)
      $itk_component(maxscale) configure -value $values_($this,max_scale)
   }

   #  Set defaults for widget states.
   protected method set_defaults_ {} {
      set values_($this,extended_precision) 0
      set values_($this,float_panel) 0
      set values_($this,with_zoom_window) 1
      set values_($this,with_pan_window) 1
      set values_($this,with_colorramp) 1
      set values_($this,focus_follows_mouse) 1
      set values_($this,scrollbars) 1
      set values_($this,transient_tools) 0
      set values_($this,quiet_exit) 1
      set values_($this,min_scale) -10
      set values_($this,max_scale) 20
      set values_($this,zoom_factor) 4
      set values_($this,default_cmap) real
      set values_($this,default_itt) ramp
   }

   #  Update the properties object to the local values and cause a
   #  save to backing store.
   protected method save_properties_ {} {
      foreach key "extended_precision float_panel with_zoom_window \
                   with_pan_window with_colorramp focus_follows_mouse \
                   scrollbars transient_tools quiet_exit min_scale max_scale \
                   zoom_factor default_cmap default_itt" {
         $props_ set_named_property Gaia $key $values_($this,$key)
      }
      $props_ save_properties
   }

   #  Apply any properties that can be done now, rather than just left
   #  for the next startup.
   protected method apply_properties_ {} {
      if { $itk_option(-image) != {} } {
         $itk_option(-image) configure -extended_precision \
            $values_($this,extended_precision)
      }
      if { $itk_option(-gaia) != {} } {
         $itk_option(-gaia) configure -transient_tools \
            $values_($this,transient_tools)
         $itk_option(-gaia) configure -quiet_exit \
            $values_($this,quiet_exit)
      }
   }

   #  Set element of values_.
   protected method set_value_ {key value} {
      set values_($this,$key) $value
   }

   #  Add controls for the startup properties.
   protected method add_controls_ {} {

      set lwidth 25

      # Float panel.
      itk_component add floatpanel {
         StarLabelCheck $w_.floatpanel \
            -text "Use a floating panel:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope values_($this,float_panel)]
      }
      add_short_help $itk_component(floatpanel) \
         {Create panel area in own window (requires restart)}
      pack $itk_component(floatpanel) -side top -fill x

      # Zoom window
      itk_component add withzoomwindow {
         StarLabelCheck $w_.withzoomwindow \
            -text "Display a zoom window:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope values_($this,with_zoom_window)]
      }
      add_short_help $itk_component(withzoomwindow) \
         {Display a zoom window (requires restart)}
      pack $itk_component(withzoomwindow) -side top -fill x

      # Pan window
      itk_component add withpanwindow {
         StarLabelCheck $w_.withpanwindow \
            -text "Display a panner window:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope values_($this,with_pan_window)]
      }
      add_short_help $itk_component(withpanwindow) \
         {Display a panner window (requires restart)}
      pack $itk_component(withpanwindow) -side top -fill x

      # Colorramp window
      itk_component add withcolorramp {
         StarLabelCheck $w_.withcolorramp \
            -text "Display a color ramp:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope values_($this,with_colorramp)]
      }
      add_short_help $itk_component(withcolorramp) \
         {Display a color ramp at bottom of window (requires restart)}
      pack $itk_component(withcolorramp) -side top -fill x

      # Focus follows mouse
      itk_component add focusfollowsmouse {
         StarLabelCheck $w_.focusfollowsmouse \
            -text "Focus follows mouse:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope values_($this,focus_follows_mouse)]
      }
      add_short_help $itk_component(focusfollowsmouse) \
         {Entry fields focus follows mouse position (requires restart)}
      pack $itk_component(focusfollowsmouse) -side top -fill x

      # Scrollbars
      itk_component add scrollbars {
         StarLabelCheck $w_.scrollbars \
            -text "Show image scrollbars:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope values_($this,scrollbars)]
      }
      add_short_help $itk_component(scrollbars) \
         {Display scrollbars around main image (requires restart)}
      pack $itk_component(scrollbars) -side top -fill x

      # Transient tools
      itk_component add transienttools {
         StarLabelCheck $w_.transienttools \
            -text "Make toolboxes transient:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope values_($this,transient_tools)]
      }
      add_short_help $itk_component(transienttools) \
         {Make toolboxes remain above main window (requires restart)}
      pack $itk_component(transienttools) -side top -fill x

      # Exit without asking.
      itk_component add quietexit {
         StarLabelCheck $w_.quietexit \
            -text "Exit without prompt:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope values_($this,quiet_exit)]
      }
      add_short_help $itk_component(quietexit) \
         {Make "Exit" option ask before closing GAIA}
      pack $itk_component(quietexit) -side top -fill x

      #  Extended precision.
      itk_component add precision {
         StarLabelCheck $w_.precision \
            -text "Display milli-arcsecs:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope values_($this,extended_precision)]
      }
      add_short_help $itk_component(precision) \
         {Display milli-arcsecond resolution in readouts}
      pack $itk_component(precision) -side top -fill x

      #  Minimum zoom scale.
      itk_component add minscale {
         LabelEntryScale $w_.minscale \
            -text {Minimum zoom:} \
            -labelwidth $lwidth \
            -valuewidth 4 \
            -from -30 \
            -to 1 \
            -increment 1 \
            -show_arrows 1 \
            -resolution 1 \
            -anchor w \
            -value $values_($this,min_scale) \
            -command [code $this set_value_ min_scale]
      }
      add_short_help $itk_component(minscale) \
         {Minimum zoom scale (-ve for smaller)}
      pack $itk_component(minscale) -side top -fill x

      #  Maximum zoom scale.
      itk_component add maxscale {
         LabelEntryScale $w_.maxscale \
            -text {Maximum zoom:} \
            -labelwidth $lwidth \
            -valuewidth 4 \
            -from 1 \
            -to 30 \
            -increment 1 \
            -show_arrows 1 \
            -resolution 1 \
            -anchor w \
            -value $values_($this,max_scale) \
            -command [code $this set_value_ max_scale]
      }
      add_short_help $itk_component(maxscale) \
         {Maximum zoom scale (requires restart)}
      pack $itk_component(maxscale) -side top -fill x

      #  Zoom factor used in zoom window.
      itk_component add zoomfactor {
         LabelEntryScale $w_.zoomfactor \
            -text {Zoom factor:} \
            -labelwidth $lwidth \
            -valuewidth 4 \
            -from 2 \
            -to 16 \
            -increment 1 \
            -show_arrows 1 \
            -resolution 1 \
            -anchor w \
            -value $values_($this,zoom_factor) \
            -command [code $this set_value_ zoom_factor]
      }
      add_short_help $itk_component(zoomfactor) \
         {Zoom factor of zoomed window (requires restart)}
      pack $itk_component(zoomfactor) -side top -fill x

      #  Default colormap
      itk_component add defaultcmap {
         LabelMenu $w_.cmap -text "Default colormap:" \
            -labelwidth $lwidth \
            -variable [scope values_($this,default_cmap)]
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

      #  Default itt
      itk_component add defaultitt {
         LabelMenu $w_.itt -text "Default itt:" \
            -labelwidth $lwidth \
            -variable [scope values_($this,default_itt)]
      }
      set itt_files [$itk_option(-image) itt list]
      foreach map $itt_files {
         set map [file rootname $map]
         $itk_component(defaultitt) add -label $map \
            -command [code $this set_value_ default_itt $map]
      }
      add_short_help $itk_component(defaultitt) \
         {Default intensity transfer table (requires restart)}
      pack $itk_component(defaultitt) -side top -fill x -expand 0

      #  Make defaults show
      set values_($this,default_cmap) $values_($this,default_cmap)
      set values_($this,default_itt) $values_($this,default_itt)

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

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Values shared by widgets -- indexed by ($this,fieldname).
   common values_

#  End of class definition.
}
