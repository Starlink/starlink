#+
#  Name:
#     Gaia3dTool

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a super-class for tools that render volumes.

#  Description:
#     This class provides common features used by toolboxes that render
#     data-cubes using VTK, such as the basic window setup data access and
#     drawing control. Methods that add tool and scene specific controls are
#     not defined and should be implemented.

#  Invocations:
#
#        Gaia3dTool object_name [configuration options]
#
#     This creates an instance of a Gaia3dVolume object. The return is
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
#     24-JUL-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual Gaia3dTool {}

itcl::class gaia3d::Gaia3dTool {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      package require vtk
      package require vtkinteraction

      #  Evaluate any options.
      eval itk_initialize $args

      #  Restore properties. These all apply to local variables, not
      #  configuration options, so harder work...
      set props_ [gaia::GaiaProperties::instance]
      set keys [$props_ get_named_keys Gaia3dTool]
      if { $keys != {} } {
         foreach key $keys {
            set value [$props_ get_property $key]
            if { $value != {} } {
               set lkey [$props_ get_unnamed_key Gaia3dTool $key]
               eval set $lkey $value
            }
         }
      }

      #  Make it a decent size (packing doesn't work).
      wm geometry  $w_ 800x800

      #  Add short help window.
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Add the print menu items for hardcopies.
      $File add command -label "Print..." \
         -command [code $this hardcopy] \
         -accelerator {Control-p}
      bind $w_ <Control-p> [code $this hardcopy]

      #  Set the close menu item.
      $File add command -label Close \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add the view menu
      set view_menu_ [add_menubutton "View"]
      configure_menubutton View -underline 0

      #  Create the extra cubes toolbox.
      add_menuitem $view_menu_ command "Other cubes..." \
         {Render isosurfaces of other cubes into the scene} \
         -command [code $this make_extratoolbox]

      #  Create the light controls toolbox.
      add_menuitem $view_menu_ command "Lighting..." \
         {Change the lighting of the scene} \
         -command [code $this make_lightingtoolbox]

      #  Add the options menu
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0

      #  Add an option to add a customized colour.
      $Options add command \
         -label {Add custom colour...} \
         -command [code $this choose_custom_colour]
      $short_help_win_ add_menu_short_help $Options \
         {Add custom colour...} \
         {Choose a new colour for menus}

      #  Reset all saved attributes to their defaults.
      $Options add command -label "Reset" \
         -command [code $this reset_options_]
      add_menu_short_help $Options {Reset}  \
         {Reset any saved options to their default value}

      #  Add option to choose the image plane colour map.
      set submenu [menu $Options.imagecolour]
      $Options add cascade -label "Image plane colour map" -menu $submenu
      colourmaps_fill_ $submenu

      #  Add option to choose a type of opacity for the image plane.
      set submenu [menu $Options.imageopacity]
      $Options add cascade -label "Image plane opacity" -menu $submenu
      opacity_fill_ $submenu

      #  Add option to choose a colour for the spectral line/region.
      set spectral_colour_menu_ [menu $Options.spectralcolour]
      $Options add cascade -label "Spectral colour" \
         -menu $spectral_colour_menu_
      spectral_colour_fill_

      #  AST axes, text scale and a colour.
      set submenu [menu $Options.textscale]
      $Options add cascade -label "Annotated text scale" -menu $submenu
      textscale_fill_ $submenu

      set ast_textcolour_menu_ [menu $Options.textcolour]
      $Options add cascade -label "Annotated text colour" \
         -menu $ast_textcolour_menu_
      textcolour_fill_

      #  Use the spectral extraction limits to clip cube data.
      $Options add checkbutton \
         -label {Apply extraction limits} \
         -variable [scope apply_extraction_limits_] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Apply extraction limits} \
         {Use the spectral extraction limits of cube toolbox to clip data}

      #  Interactor mode, joystick by default, so offer trackerball.
      $Options add checkbutton \
         -label {Trackerball interactions} \
         -variable [scope interaction_mode_] \
         -onvalue trackerball \
         -offvalue joystick \
         -command [code $this set_interaction_mode_]
      $short_help_win_ add_menu_short_help $Options \
         {Trackerball interactions} \
         {Mouse interaction mode, joystick (off) or trackerball (on)}

      #  Backingstore on or off (off as faster for interactions).
      $Options add checkbutton \
         -label {Use backingstore} \
         -variable [scope backingstore_on_] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this set_backingstore_mode_]
      $short_help_win_ add_menu_short_help $Options \
         {Use backingstore} \
         {Use a backingstore to refresh scene, faster for simple refreshes}

      #  Create a panedwindow to define the basic layout between the
      #  graphics display and controls in a left-right split.
      itk_component add pane {
         ::panedwindow $w_.pane -width 5i -height 6i -orient horizontal
      } {
         #  This is needed. Not sure why.
      }

      #  Add the renderer widget in the right-hand pane.
      set renwindow_ [gaia3d::Gaia3dVtkWindow $w_.renwindow \
                         -backingstore_on $backingstore_on_]
      $renwindow_ set_interaction_mode $interaction_mode_
      add_short_help $renwindow_ {See help for interactions}

      #  Create the button bar
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window.
      itk_component add close {
         button $itk_component(actionframe).close -text Close \
            -command [code $this close]
      }
      add_short_help $itk_component(close) {Close window}

      #  Do the rendering.
      itk_component add draw {
         button $itk_component(actionframe).draw -text {Draw} \
            -command [code $this draw]
      }
      add_short_help $itk_component(draw) {Draw the 3D volume}

      #  Pack all the components into place.
      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5
      pack $itk_component(pane) -fill both -expand 1 -padx 1m -pady 1m
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(draw) -side left -expand 1 -pady 3 -padx 3
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Free the Plot3D. Don't do this as the renderer is destructing
      #  so it's not safe.
      #  free_plot3d_

      #  Release any scene VTK objects and the image data.
      catch {release_objects 1}

      #  Simple axes. These can be just set visible/invisible, so only remove
      #  on a full release.
      if { $simpleaxes_ != {} } {
         catch {::delete object simpleaxes_}
         set simpleaxes_ {}
      }

      #  Outline. This can be just set visible/invisible, so only remove
      #  on a full release.
      if { $outline_ != {} } {
         catch{::delete object outline_}
         set outline_ {}
      }

      #  Save global properties.
      catch {$props_ save_properties}
   }

   #  Methods:
   #  --------

   #  Called after construction is completed. Finish UI.
   protected method init {} {

      #  Populate the controls part of display (left-hand pane).
      add_controls_

      #  Add this and the render window to the panedwindow.
      $itk_component(pane) add $itk_component(controls) $renwindow_
   }

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Close this window, kill it if needed, otherwise withdraw. Also
   #  clear the display. Retains the image data.
   public method close {} {
      fullclose 0
   }

   #  Close this window, also releases the data if fullrelease is true.
   public method fullclose { fullrelease } {
      catch {release_objects $fullrelease}
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Create a hardcopy.
   public method hardcopy {} {
      utilReUseWidget gaia3d::Gaia3dVtkPrintTool $w_.hardcopy \
         -renwindow $renwindow_
   }

   #  Public redraw method.
   public method redraw { {override 0} } {
      if { $override } {
         draw
      }
   }

   #  Add generic controls for the data handling.
   protected method add_controls_ {} {

      #  Frame for all controls.
      set lwidth 14
      itk_component add controls {
         ::frame $w_.controls
      }
      itk_component add rule {
         gaia::LabelRule $itk_component(controls).rule -text "Controls:"
      }
      pack $itk_component(rule) -side top -fill x

      #  Add tab table to the top of the controls frame. This should contain
      #  the various pages of tool-specific controls and should be used by the
      #  add_tool_controls_ overrides to add further controls.
      itk_component add tab {
         ::iwidgets::tabnotebook $itk_component(controls).tab \
            -angle 0 -equaltabs 0 -tabpos n -width 300
      }
      pack $itk_component(tab) -fill both -expand 1

      #  Tool specific controls.
      add_tool_controls_

      #  Add CUPID import controls.
      add_cupid_controls_

      #  Add AST attribute controls.
      add_ast_controls_

      #  Switch on bad pixel replacement, or not.
      itk_component add replacebad {
         gaia::StarLabelCheck $itk_component(controls).replacebad \
            -text "Replace bad:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope checkbad_] \
            -command [code $this changed_bad_]
      }
      pack $itk_component(replacebad) -fill x -expand 0 -ipady 5
      add_short_help $itk_component(replacebad) \
         {Replace BAD values in cube before rendering}

      itk_component add replacevalue {
         util::LabelEntry $itk_component(controls).replacevalue \
            -text "Replacement value:" \
            -labelwidth $lwidth \
            -validate real \
            -orient vertical \
            -value 0.0 \
            -command [code $this changed_bad_]
      }
      pack $itk_component(replacevalue) -fill x -expand 0
      add_short_help $itk_component(replacevalue) \
         {Value to replace BAD values in cube with}

      #  Image plane visibility.
      itk_component add showimageplane {
         gaia::StarLabelCheck $itk_component(controls).showimageplane \
            -text "Image plane:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope show_image_plane_] \
            -command [code $this changed_show_image_plane_]
      }
      pack $itk_component(showimageplane) -fill x -expand 0 -ipady 5
      add_short_help $itk_component(showimageplane) \
         {Show current image slice (interactive)}

      #  Spectral line visibility.
      itk_component add showspectralline {
         gaia::StarLabelCheck $itk_component(controls).showspectralline \
            -text "Spectral line:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope show_spectral_line_] \
            -command [code $this changed_show_spectral_line_]
      }
      pack $itk_component(showspectralline) -fill x -expand 0 -ipady 5
      add_short_help $itk_component(showspectralline) \
         {Show spectral extraction line (requires image plane for interaction)}

      #  Simple axes directions.
      itk_component add showsimpleaxes {
         gaia::StarLabelCheck $itk_component(controls).showsimpleaxes \
            -text "Direction axes:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope show_simple_axes_] \
            -command [code $this changed_show_simple_axes_]
      }
      pack $itk_component(showsimpleaxes) -fill x -expand 0 -ipady 5
      add_short_help $itk_component(showsimpleaxes) \
         {Show directions axes for world coordinates (always visible)}

      #  Bounding box (aka outline).
      itk_component add showoutline {
         gaia::StarLabelCheck $itk_component(controls).showoutline \
            -text "Bounding box:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope show_outline_] \
            -command [code $this changed_show_outline_]
      }
      pack $itk_component(showoutline) -fill x -expand 0 -ipady 5
      add_short_help $itk_component(showoutline) \
         {Show an outline around the cube (bounding box)}

      #  AST annotated axes.
      itk_component add showastaxes {
         gaia::StarLabelCheck $itk_component(controls).showastaxes \
            -text "Annotated axes:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope show_ast_axes_] \
            -command [code $this changed_show_ast_axes_]
      }
      pack $itk_component(showastaxes) -fill x -expand 0 -ipady 5
      add_short_help $itk_component(showastaxes) \
         {Show annotated axes around cube (RA and Dec labelling)}

      #  Apply the CUPID pixelmask. Note the stencil of the Gaia3dVtkCubeData
      #  should be used as the input connection for subclasses when this is
      #  active.
      itk_component add applymask {
         gaia::StarLabelCheck $itk_component(controls).applymask \
            -text "Apply mask:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope applymask_] \
            -command [code $this changed_applymask_]
      }
      pack $itk_component(applymask) -fill x -expand 0 -ipady 5
      add_short_help $itk_component(applymask) \
         {Apply CUPID mask to select data regions}
   }

   #  Get a list of the standard colourmaps, sorted in alphabetic order
   #  and associated with a short-form name (no .lasc).
   public method get_colourmaps {} {
      set slist [lsort -dictionary [gvtk::cmap list]]
      set cmaps ""
      foreach cmap $slist {
         set name [file rootname $cmap]
         lappend cmaps $cmap $name
      }
      return $cmaps
   }

   #  Fill the colourmaps menu. Use columns to keep length under
   #  control.
   protected method colourmaps_fill_ {menu} {
      set cmaps [get_colourmaps]
      set n 0
      foreach {cmap name} $cmaps {
         if { $n > 20 } {
            set cbreak 1
            set n 0
         } else {
            set cbreak 0
         }
         incr n
         $menu add radiobutton \
            -value $cmap \
            -label $name \
            -variable [scope plane_colourmap_] \
            -columnbreak $cbreak \
            -command [code $this changed_image_plane_colourmap_]
      }
   }

   #  Fill the opacity menu with three states, opaque, see-through and
   #  clear. Full control of the opacity itself isn't used as the scene
   #  rendering order would require this to go before the main content, but
   #  that obscures things (as the plane merges into the scene, rather than
   #  being apart from it). This is less confusing...
   protected method opacity_fill_ {menu} {
      $menu add radiobutton \
         -value 1.0 \
         -label "opaque" \
         -variable [scope plane_opacity_] \
         -command [code $this changed_image_plane_opacity_]
      $menu add radiobutton \
         -value 0.9 \
         -label "see-through" \
         -variable [scope plane_opacity_] \
         -command [code $this changed_image_plane_opacity_]
      $menu add radiobutton \
         -value 0.0 \
         -label "clear" \
         -variable [scope plane_opacity_] \
         -command [code $this changed_image_plane_opacity_]
   }

   #  Fill the textscale menu.
   protected method textscale_fill_ {menu} {
      foreach i "0.25 0.5 0.75 1 1.5 2 3 4 5 6 7 8 9 10 11 12 15 20 50" {
         $menu add radiobutton \
            -value $i \
            -label $i \
            -variable [scope ast_textscale_] \
            -command [code $this changed_ast_textscale_]
      }
   }

   #  Fill the text colour menu.
   protected method textcolour_fill_ {} {
      set count [gaia::AstColours::standard_count]
      for {set i 0} {$i < $count} {incr i} {
         set colour [gaia::AstColours::lookup_colour $i]
         $ast_textcolour_menu_ add radiobutton \
            -value $i \
            -label {   } \
            -background $colour \
            -variable [scope ast_textcolour_] \
            -command [code $this changed_ast_textcolour_]
      }
   }

   #  Fill the spectral colour menu. Use AST colours, but not bound to them.
   protected method spectral_colour_fill_ {} {
      set count [gaia::AstColours::standard_count]
      for {set i 0} {$i < $count} {incr i} {
         set colour [gaia::AstColours::lookup_colour $i]
         $spectral_colour_menu_ add radiobutton \
            -value $colour \
            -label {   } \
            -background $colour \
            -variable [scope spectral_colour_] \
            -command [code $this changed_spectral_colour_]
      }
   }

   #  Add the controls specific to this rendering job (contours, data limits
   #  etc).
   protected method add_tool_controls_ {} {
      error "Implement an add_tool_controls_ method"
   }

   #  Controls for CUPID catalogue options.
   protected method add_cupid_controls_ {} {

      $itk_component(tab) add -label CUPID
      set w [$itk_component(tab) childsite CUPID]

      itk_component add catframe {
         frame $w.catframe
      }
      pack $itk_component(catframe) -side top -fill x -expand 0

      itk_component add catrule {
         gaia::LabelRule $itk_component(catframe).catrule \
            -text "Catalogue:"
      }
      pack $itk_component(catrule) -side top -fill x -expand 1

      set lwidth 21

      #  Render any CUPID catalogues opened in the GAIA cube toolbox.
      itk_component add showcupidcat {
         gaia::StarLabelCheck $itk_component(catframe).showcupidcat \
            -text "Display catalogues:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope show_cupid_cat_] \
            -command [code $this changed_show_cupid_cat_]
      }
      pack $itk_component(showcupidcat) -fill x -expand 0 -ipadx 1m
      add_short_help $itk_component(showcupidcat) \
         {Show CUPID catalogue clumps, if any catalogues are available}

      #   Just show the selected rows.
      itk_component add showcupidselected {
         gaia::StarLabelCheck $itk_component(catframe).showcupidselected \
            -text "Only show selected rows:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope show_cupid_selected_] \
            -command [code $this changed_show_cupid_selected_]
      }
      pack $itk_component(showcupidselected) -fill x -expand 0 -ipadx 1m
      add_short_help $itk_component(showcupidselected) \
         {Only display rows selected in CUPID catalogues}

      #  Quick import.
      itk_component add cupidimport {
         button $itk_component(catframe).import \
            -text "Import" \
            -command [code $this import_cupid_catalogue_]
      }
      pack $itk_component(cupidimport) -side top -ipadx 1m -pady 1m
      add_short_help $itk_component(cupidimport) {Import another catalogue}

      #  Pixel masks.
      itk_component add pixelmask {
         gaia3d::Gaia3dCupidMasks $w.pixelmask \
            -filter_types $itk_option(-filter_types) \
            -options_menu $view_menu_
      }
      pack $itk_component(pixelmask) -side top -fill x -expand 0
      add_short_help $itk_component(pixelmask) {Display pixel masks}
   }


   #  Controls for CUPID catalogue options.
   protected method add_ast_controls_ {} {

      $itk_component(tab) add -label {Axes attributes}
      set w [$itk_component(tab) childsite {Axes attributes}]

      itk_component add astframe {
         frame $w.astframe
      }
      pack $itk_component(astframe) -side top -fill x -expand 0

      itk_component add astrule {
         gaia::LabelRule $itk_component(astframe).astrule \
            -text "AST attributes for axes:"
      }
      pack $itk_component(astrule) -side top -fill x -expand 1

      itk_component add asttext {
         gaia::ScrollText $itk_component(astframe).text
      }
      pack $itk_component(asttext) -side top -fill both -expand 1

      set lwidth 21
   }

   #  Gather AST attributes. If these change we need to re-generate the plot.
   protected method gather_axes_attributes_ {} {
      set content [$itk_component(asttext) get all]
      set result ""
      foreach line $content {
         if { $line != {} } {
            append result "$line,"
         }
      }
      if { $result != $astatts_ } {
         set astatts_ $result
         free_plot3d_
      }
   }

   #  Control extra cubes isosurface toolbox.
   public method make_extratoolbox {} {

      #  If the window exists then just raise it.
      if { [info exists itk_component(extratoolbox) ] &&
           [winfo exists $itk_component(extratoolbox) ] } {
         wm deiconify $itk_component(extratoolbox)
         raise $itk_component(extratoolbox)
      } else {
         busy {
            itk_component add extratoolbox {
               gaia3d::Gaia3dExtraIsosurface $w_.\#auto \
                  -rtdimage $itk_option(-rtdimage) \
                  -number $itk_option(-number) \
                  -filter_types $itk_option(-filter_types)
            }
         }
      }
   }

   #  Control scene lighting.
   public method make_lightingtoolbox {} {

      #  If the window exists then just raise it.
      if { [info exists itk_component(lightingtoolbox) ] &&
           [winfo exists $itk_component(lightingtoolbox) ] } {
         wm deiconify $itk_component(lightingtoolbox)
         raise $itk_component(lightingtoolbox)
      } else {
         busy {
            itk_component add lightingtoolbox {
               gaia3d::Gaia3dVtkLights $w_.\#auto \
                  -renwindow $renwindow_
            }
         }
      }
   }

   #  Choose and then add a custom colour to the menus.
   public method choose_custom_colour {} {
      set new_colour [gaia::ColourLabelMenu::choose_custom_colour]
      if { $new_colour != {} } {
         add_custom_colour $new_colour -1
      }
   }

   #  Add a customized colour to the menus. Use an index if supplied.
   #  Otherwise create one. If overridden must call this method.
   public method add_custom_colour { new_colour {index -1} } {

      #  AST text labels colour. Note need to handle in Grf3d.
      set i [gaia3d::Grf3dColours::add_custom_colour $index $new_colour]
      $ast_textcolour_menu_ add radiobutton \
         -value $i \
         -label {   } \
         -background $new_colour \
         -variable [scope ast_textcolour_] \
         -command [code $this changed_ast_textcolour_]

      #  Spectral line/region colour.
      $spectral_colour_menu_ add radiobutton \
         -value $new_colour \
         -label {   } \
         -background $new_colour \
         -variable [scope spectral_colour_] \
         -command [code $this changed_spectral_colour_]
   }

   #  Set variable to get data update next read. Usually when
   #  bad value handling changed. "args" is ignored.
   protected method changed_bad_ { args } {
      set changed_bad_ 1
   }

   #  Set variable to get data update next read. Usually when
   #  mask value handling changed. "args" is ignored.
   protected method changed_applymask_ {args} {
      set changed_mask_ 1
   }

   #  Called when variable that controls visibility of the image plane
   #  is changed. When the plane is translucent we must re-establish the
   #  prop order, with the plane at the back. So this is done the
   #  hard way.
   protected method changed_show_image_plane_ {} {
      if { $drawn_ } {
         if { $plane_ != {} } {
            clear_scene 0
            draw
         }
      }
   }

   #  Called when the colourmap used by the image plane changes is to be
   #  changed.
   protected method changed_image_plane_colourmap_ {} {
      $props_ set_named_property Gaia3dTool plane_colourmap_ $plane_colourmap_
      if { $plane_ != {} } {
         $plane_ configure -colourmap $plane_colourmap_
         $plane_ update
         $renwindow_ render
      }
   }

   #  Called when the spectral line or region colour is changed.
   protected method changed_spectral_colour_ {} {
      $props_ set_named_property Gaia3dTool spectral_colour_ $spectral_colour_
      if { $line_ != {} } {
         $line_ configure -colour $spectral_colour_
         $renwindow_ render
      }
   }

   #  Called when the textscale is changed.
   protected method changed_ast_textscale_ {} {
      $props_ set_named_property Gaia3dTool ast_textscale_ $ast_textscale_
      if { $drawn_ != {} } {
         draw
      }
   }

   #  Called when the textscale is changed.
   protected method changed_ast_textcolour_ {} {
      $props_ set_named_property Gaia3dTool ast_textcolour_ $ast_textcolour_
      if { $drawn_ != {} } {
         draw
      }
   }

   #  Called when the opacity of the image plane changes.
   protected method changed_image_plane_opacity_ {} {
      $props_ set_named_property Gaia3dTool plane_opacity_ $plane_opacity_
      if { $plane_ != {} } {
         $plane_ set_opacity $plane_opacity_
         $renwindow_ render
      }
   }

   #  Called when variable that controls visibility of the spectral line
   #  is changed. When the line is translucent we must re-establish the
   #  prop order, with the line at the back. So this is done the
   #  hard way by completely redrawing the scene.
   protected method changed_show_spectral_line_ {} {
      if { $drawn_ } {
         if { $line_ != {} } {
            if { $show_spectral_line_ } {
               clear_scene 0
               draw
            } else {
               $line_ set_invisible
               $renwindow_ render
            }
         }
      }
   }

   #  Called when variable that controls visibility of the simple
   #  direction axes is changed.
   protected method changed_show_simple_axes_ {} {
      if { $drawn_ } {
         if { $simpleaxes_ != {} } {
            if { $show_simple_axes_ } {
               $simpleaxes_ set_visible
            } else {
               $simpleaxes_ set_invisible
            }
            $renwindow_ render
         } else {
            draw
         }
      }
   }

   #  Called when variable that controls visibility of the outline
   #  is changed.
   protected method changed_show_outline_ {} {
      if { $drawn_ } {
         if { $outline_ != {} } {
            if { $show_outline_ } {
               $outline_ set_visible
            } else {
               $outline_ set_invisible
            }
            $renwindow_ render
         } else {
            draw
         }
      }
   }

   #  Called when variable that controls visibility of the AST axes is
   #  changed.
   protected method changed_show_ast_axes_ {} {
      if { $drawn_ } {
         draw
      }
   }

   #  Called when variable that controls visibility of the CUPID catalogue
   #  clumps. When the clumps are translucent we must re-establish the
   #  prop order, with the clumps at the back. Also we match the objects
   #  displayed in the catalogue, which might have changed. So this is done
   #  the hard way by completely redrawing the scene.
   protected method changed_show_cupid_cat_ {} {
      if { $drawn_ } {
         if { $cupid_cat_ != {} } {
            if { $show_cupid_cat_ } {
               clear_scene 0
               draw
            } else {
               $cupid_cat_ set_invisible
               $renwindow_ render
            }
         }
      }
   }

   #  Import a CUPID catalogue. Uses the cube toolbox control.
   protected method import_cupid_catalogue_ {} {
      $itk_option(-gaiacube) make_cupid_importer
   }

   #  Change if selected CUPID clumps are shown, or all.
   protected method changed_show_cupid_selected_ {} {
      if { $drawn_ } {
         draw
      }
   }

   #  Start reporting the position of the cursor in the image plane, if
   #  tracking.
   protected method start_report_position_ {} {
      if { [$plane_ has_position] } {
         $textwcs_ set_visible
         report_position_ 1
      }
   }

   #  Report the position of the cursor in the image plane, if tracking.
   #  Also updates the spectral extraction in GAIA, if that's enabled.
   #  If init is true then a request to autorange the data extraction limits
   #  is made.
   protected method report_position_ { {init 0} } {
      if { [$plane_ has_position] } {
         lassign [$plane_ get_position_and_delta] ix iy iz dx dy dz
         incr ix
         incr iy
         incr iz

         #  Position the extraction region.
         if { $line_ != {} } {
            $line_ set_position $ix $iy $iz $dx $dy $dz
         }

         #  Report the position in world coords.
         set value [$plane_ get_value]
         set coords \
            [gaiautils::asttrann [$imagedata_ get_wcs] 1 [list $ix $iy $iz] 1]
         $textwcs_ set_text "coords: $coords -> value: $value"

         #  Track the position in GAIA.
         line_moved_ $init
      }
   }

   #  Stop reporting the position of the cursor in the image plane.
   protected method end_report_position_ {} {
      $textwcs_ set_invisible
   }

   #  Set the mouse interaction mode.
   protected method set_interaction_mode_ {} {
      $props_ set_named_property Gaia3dTool \
         interaction_mode_ $interaction_mode_
      $renwindow_ set_interaction_mode $interaction_mode_
   }

   #  Set the backingstore mode.
   protected method set_backingstore_mode_ {} {
      $props_ set_named_property Gaia3dTool backingstore_on_ $backingstore_on_
      $renwindow_ configure -backingstore_on $backingstore_on_
   }

   #  Reset all the saved options to their default values. KEEP this up
   #  to date.
   protected method reset_options_ {} {

      set plane_colourmap_ "ramp.lasc"
      changed_image_plane_colourmap_

      set plane_opacity_ 1.0
      changed_image_plane_opacity_

      set spectral_colour_ [gaia::AstColours::lookup_colour 5]
      changed_spectral_colour_

      set ast_textscale_ 5
      changed_ast_textscale_

      set ast_textcolour_ 1
      changed_ast_textcolour_

      set interaction_mode_ "joystick"
      set_interaction_mode_

      set backingstore_on_ 0
      set_backingstore_mode_
   }


   #================================================================
   #  Interaction with GAIA.
   #================================================================

   #  To GAIA....

   #  Plane has moved. Set the cube display in GAIA. Note index is zero based
   #  grid value.
   protected method image_plane_moved_ {index} {
      set plane [$imagedata_ grid2pixel \
                    [$itk_option(-gaiacube) get_axis] [expr $index+1]]
      $itk_option(-gaiacube) set_display_plane $plane
   }

   #  User has selected a new axis and plane. Go there.
   protected method image_plane_snapped_ {axis index} {
      if { [$itk_option(-gaiacube) get_axis] != $axis } {

         #  Extracted spectrum is now invalid remove from scene.
         if { $line_ != {} && $show_spectral_line_ } {
            set show_spectral_line_ 0
            $line_ set_invisible
         }

         #  Move GAIA to this axis.
         $itk_option(-gaiacube) set_axis $axis
      }
      image_plane_moved_ $index
   }

   #  The spectral line has moved. Track this in GAIA if needed. If init
   #  is true then a request to autorange the data extraction limits is made.
   protected method line_moved_ { init } {
      if { $line_ != {} && $show_spectral_line_ } {
         lassign [$line_ get_description] type desc
         set last_line_type_ "$type"
         set last_line_desc_ "$desc"
         if { $type == "p" } {
            eval $itk_option(-gaiacube) set_point_spectrum_position $init $desc
         } else {
            $itk_option(-gaiacube) set_region_spectrum_position $init $desc
         }
      }
   }

   #  Get the position of the spectral line.
   public method get_spectral_line {} {
      if { $show_spectral_line_ && $line_ != {} } {
         set pos [$itk_option(-gaiacube) get_point_spectrum_position]
         if { $pos != {} } {
            return "p $pos"
         }
         set region [$itk_option(-gaiacube) get_region_spectrum_position]
         if { $region != {} } {
            return "r $region"
         }
      }
      return {}
   }


   #  From GAIA (or local)...

   #  Move image plane to given pixel coordinate.
   public method set_display_plane {plane} {
      if { $plane_ != {} && $show_image_plane_ } {
         set index [$imagedata_ pixel2grid \
                       [$itk_option(-gaiacube) get_axis] $plane]
         incr index -1

         #  Avoid unnecessary updates.
         if { [$plane_ get_slice_index] != $index } {
            $renwindow_ set_rate_to_desired
            catch {
               $plane_ set_slice_index $index
               $renwindow_ render
            }
            $renwindow_ set_rate_to_still
         }
      }
   }

   #  Change the display axis. Reorient image plane and spectral line.
   #  Spectral position is invalid so removed.
   public method set_display_axis {axis} {
      if { $line_ != {} && $show_spectral_line_ } {
         $line_ configure -axis $axis
         $line_ fit_to_data
         set show_spectral_line_ 0
         $line_ set_invisible
         $renwindow_ render
      }
      if { $plane_ != {} && $show_image_plane_ } {
         $plane_ configure -axis $axis
         $renwindow_ render
      }
   }

   #  Set position of spectral line, when moved by GAIA.
   public method set_spectral_line {type args} {
      if { $show_spectral_line_ && $line_ != {} } {

         #  Avoid unnecessary updates.
         if { $last_line_type_ != "$type" || $last_line_desc_ != "$args" } {

            #  Fast LOD updates for volumes.
            $renwindow_ set_rate_to_desired
            catch {
               if { $type == "p" } {
                  set ix [expr [lindex $args 0] -1]
                  set iy [expr [lindex $args 1] -1]
                  $line_ set_description p "$ix $iy"
               } else {
                  $line_ set_description $type $args
               }
               $renwindow_ render
            }
            $renwindow_ set_rate_to_still
         }
      }
   }

   #  Set the CUPID catalogue importer. This has handles to all the
   #  CUPID catalogues.
   public method set_cupid_importer {cupidimporter} {
      set importer_ $cupidimporter
      if { $cupid_cat_ != {} } {

         #  Fast LOD updates for volumes.
         $renwindow_ set_rate_to_desired
         $cupid_cat_ set_importer $importer_
         $renwindow_ render
         $renwindow_ set_rate_to_still
      }
   }

   #================================================================
   #  VTK setup.
   #================================================================

   #  Do the drawing of the scene.
   public method draw {} {
      set drawn_ 1
      busy {
         #  Access the main cube image data.
         set datachange [get_vtk_data_array_]

         #  Clear the scene for a data change.
         if { $datachange != 0 } {
            clear_scene 0

            #  And set the camera to be nearly along the Z axis (facing like
            #  image, but with side-view), if the data file changed. Otherwise
            #  the limits or bad pixel handling changed, want to keep the
            #  scene position in that case.
            if { $datachange == 1 } {
               lassign [$imagedata_ get_dims] nx ny nz
               $renwindow_ set_camera $nx $ny [expr $nz*5]
            }
         }

         #  Add the spectral line prop, needs to go before other props so gets
         #  the interaction first, plus like the image plane will only render
         #  correctly when drawn before the volume (which will be
         #  translucent). Note not true for translucent plane and line.
         if { $line_ == {} } {
            set line_ [gaia3d::Gaia3dArdPrismProxy \#auto \
                          -dataset [$imagedata_ get_imagedata] \
                          -renwindow $renwindow_ \
                          -align_to_axis 1 \
                          -colour $spectral_colour_ \
                          -axis [$itk_option(-gaiacube) get_axis]]
            $line_ add_to_window

            #  See if GAIA has position, if not just use a dummy position.
            lassign [get_spectral_line] type pos
            if { $type != {} } {
               set_spectral_line $type $pos
            } else {
               set_spectral_line p 0 0
            }
         } else {
            #  Axis may have changed.
            $line_ configure -axis [$itk_option(-gaiacube) get_axis]
         }

         if { $show_spectral_line_ } {
            #  Set initial position.
            set result [$itk_option(-gaiacube) get_point_spectrum_position]
            if { $result != "" } {
               eval set_spectral_line p $result
            }
            $line_ set_visible
         } else {
            $line_ set_invisible
         }

         #  Add the image plane prop.
         if { $plane_ == {} } {
            set plane_ \
               [gaia3d::Gaia3dVtkImagePlane \#auto \
                   -renwindow $renwindow_ \
                   -rtdimage $itk_option(-rtdimage) \
                   -dataset [$imagedata_ get_imagedata] \
                   -opacity $plane_opacity_ \
                   -colourmap $plane_colourmap_ \
                   -axis [$itk_option(-gaiacube) get_axis] \
                   -move_cmd [code $this image_plane_moved_] \
                   -snap_cmd [code $this image_plane_snapped_] \
                   -interact_cmd [code $this report_position_] \
                   -start_interact_cmd [code $this start_report_position_] \
                   -end_interact_cmd [code $this end_report_position_]]

            $plane_ add_to_window

            #  Use a text display for the WCS coordinates.
            set textwcs_ [gaia3d::Gaia3dVtkOverlayText \#auto \
                             -renwindow $renwindow_]
            $textwcs_ add_to_window
         }

         if { $show_image_plane_ } {
            #  Set the initial plane.
            set_display_plane [$itk_option(-gaiacube) get_display_plane]

            $plane_ set_visible
            $textwcs_ set_visible
         } else {
            $plane_ set_invisible
            $textwcs_ set_invisible
         }

         #  Draw the CUPID regions if requested. Lazily create this object
         #  as it always renders immediately.
         if { $cupid_cat_ == {} && $show_cupid_cat_ } {
            set cupid_cat_ [gaia3d::Gaia3dCupidPrism \#auto \
                               -wcs [$imagedata_ get_wcs] \
                               -renwindow $renwindow_ \
                               -align_to_axis 1 \
                               -axis [$itk_option(-gaiacube) get_axis]\
                               -selected $show_cupid_selected_]
            $cupid_cat_ set_importer $importer_
            $cupid_cat_ add_to_window
         } else {
            if { $cupid_cat_ != {} } {
               $cupid_cat_ configure -selected $show_cupid_selected_
               if { $show_cupid_cat_ } {
                  #  Complete re-rendering so that new or removed regions are
                  #  revealed and removed.
                  $cupid_cat_ set_importer $importer_
               } else {
                  $cupid_cat_ set_invisible
               }
            }
         }

         #  Draw AST axes if requested. XXX allow attributes...
         #  Remove all existing props (redrawing can be needed even if the
         #  scene hasn't changed due to text re-orientation and when we have
         #  it, attribute changes).
         clear_plot3d_
         if { $show_ast_axes_ } {

            #  If first time, or the cube has changed, get a plot.
            if { $plot_ == {} } {
               create_plot3d_
            }
            grid_plot3d_
         }

         #  Do the work of rendering the fuller scene using the main
         #  cube. Note use ProgressEvents when possible to update the UI from
         #  time to time, so although blocked it might not be too bad.
         draw_scene_ $datachange

         #  Now draw any additional cubes.
         if { [info exists itk_component(extratoolbox) ] &&
              [winfo exists $itk_component(extratoolbox) ] } {
            $itk_component(extratoolbox) render $renwindow_ \
               [$imagedata_ get_wcs]
         }

         #  And draw the masks if that is also required.
         $itk_component(pixelmask) render $renwindow_

         #  2D Widgets go last. Add a orientation marker that is always
         #  visible to determine the directions.
         if { $show_simple_axes_ } {
            if { $simpleaxes_ == {} } {
               set simpleaxes_ [gaia3d::Gaia3dVtkAxes \#auto \
                                   -renwindow $renwindow_]
               $simpleaxes_ add_to_window
            }
            $simpleaxes_ configure -wcs [$imagedata_ get_wcs]
            $simpleaxes_ set_visible
         } else {
            if { $simpleaxes_ != {} } {
               $simpleaxes_ set_invisible
            }
         }

         if { $show_outline_ } {
            if { $outline_ == {} } {
               set outline_ [gaia3d::Gaia3dVtkOutline \#auto \
                                -renwindow $renwindow_]
               $outline_ add_to_window
            }
            $outline_ configure -dataset [$imagedata_ get_imagedata]
            $outline_ set_visible
         } else {
            if { $outline_ != {} } {
               $outline_ set_invisible
            }
         }

         #  Do rendering.
         $renwindow_ render

         #  If this is a new data file reset camera to make sure the
         #  volume and everything else is withing clipping range.
         #  NOTE must do this after the first full render in case the
         #  window has been resized and some attributes are not initialised.
         if { $datachange == 1 } {
            $renwindow_ reset_camera
            $renwindow_ render
         }
      }
   }

   #  Create a Plot3D for the current cube.
   protected method create_plot3d_ {} {
      free_plot3d_
      set grf_context_ [gvtk::grfinit [$renwindow_ get_renderer]]
      set plot_ [gvtk::astplot [$imagedata_ get_wcs] [$imagedata_ get_dims] \
                    "colour(markers)=5,size(markers)=20"]
   }

   #  Free the current Plot3D.
   protected method free_plot3d_ {} {
      if { $plot_ != {} } {
         catch {gaiautils::astannul $plot_}
         set plot_ {}
      }

      #  Also release the context. A new one will be created for the new plot.
      if { $grf_context_ != {} } {
         catch {gvtk::grffreecontext $grf_context_}
         set grf_context_ {}
      }
   }

   #  Clear the Plot3D of all actors. Need to make sure this our context.
   protected method clear_plot3d_ {} {
      if { $grf_context_ != {} } {
         set_plot3d_
         gvtk::grfclear
      }
   }

   #  Set the Plot3D. Really restore the context.
   protected method set_plot3d_ {} {
      if { $grf_context_ != {} } {

         #  Re-establish the context so we get our graphics back for this
         #  plot, not those of another renderer.
         gvtk::grfsetcontext $grf_context_
      }
   }

   #  Draw a grid using the Plot3D.
   protected method grid_plot3d_ {} {
      if { $plot_ != {} } {

         #  Gather any user attributes. Need new plot if any attributes have
         #  changed.
         gather_axes_attributes_
         if { $plot_ == {} } {
            create_plot3d_
         }

         #  And UI preferences.
         set atts "colour(numlab)=$ast_textcolour_"
         append atts ",colour(textlab)=$ast_textcolour_,"
         append atts "$astatts_"

         gvtk::astgrid $plot_ $ast_textscale_ $atts
      }
   }

   #  Create and update the scene as necessary. If the data has been
   #  read or changed datachange will be set to 1 or 2. The scene will be
   #  rendered immediately after this call.
   protected method draw_scene_ { datachange } {
      error "Implement a draw_scene_ method"
   }

   #  Access the cube data and wrap this into an instance of vtkArrayData.
   #  Only done if the image data has changed or the extraction limits have
   #  changed. Returns 1 when the data has changed and 2 when the limits have
   #  or bad pixel replacement has changed and 0 otherwise.
   protected method get_vtk_data_array_ {} {

      set cubeaccessor_ [$itk_option(-gaiacube) get_cubeaccessor]

      #  Get the extraction limits. Only support this along the "spectral"
      #  axis at present.
      set changed_limits 0
      if { $apply_extraction_limits_ } {
         set limits [$itk_option(-gaiacube) get_extraction_limits]
      } else {
         set limits {}
      }
      if { $limits != $limits_ } {
         set changed_limits 1
      }
      set limits_ $limits

      #  See if the WCS has changed. This can happen if a system or units
      #  change is made.
      set changed_wcs 0
      set prop_changes [$cubeaccessor_ getpropchanges]
      if { $wcs_prop_changes_ != $prop_changes } {
         set wcs_prop_changes_ $prop_changes
         set changed_wcs 1
      }

      #  Check name of cube data, if changed re-access, or
      #  bad value handling changed.
      set newname [$cubeaccessor_ cget -dataset]

      #  Has the data changed?
      set result 0
      if { $newname != {} && $cubename_ != $newname } {
         set result 1
         set cubename_ $newname
      } else {
         if { $changed_bad_ || $changed_limits || $changed_wcs ||
              $changed_mask_ } {
            set result 2
         } else {
            if { $applymask_ } {
               #  Maybe the mask has changed some property that will
               #  regenerate it.
               if { [$itk_component(pixelmask) changed 0] } {
                  set result 2
               }
            }
         }
      }

      #  Read data if needed.
      if { $result != 0 } {
         set changed_bad_ 0
         set changed_mask_ 0

         #  Create object to access the cube data.
         if { $imagedata_ == {} } {
            set imagedata_ [uplevel \#0 gaia3d::Gaia3dVtkCubeData \#auto]
         }

         #  Data changed, so new Plot3D required.
         free_plot3d_

         #  Configure data access object so that we check for BAD pixels and
         #  replace if requested.
         $imagedata_ configure \
            -cubeaccessor $cubeaccessor_ \
            -checkbad $checkbad_ \
            -nullvalue [$itk_component(replacevalue) get] \
            -pixelmask $itk_component(pixelmask) \
            -applymask $applymask_

         #  If we have limits for the spectral axis, update them so that only
         #  part of the date is read (forces a copy however).
         if { $changed_limits } {
            if { $limits != {} } {
               #  Limits are in pixel coordinates.
               set l1 \
                  [$itk_option(-gaiacube) axis_pixel2grid [lindex $limits 0]]
               set l2 \
                  [$itk_option(-gaiacube) axis_pixel2grid [lindex $limits 1]]
               set ll [expr min($l1,$l2)-1]
               set ul [expr max($l1,$l2)]
               set axis [$itk_option(-gaiacube) get_axis]
               $imagedata_ set_axis_limits $axis "$ll $ul"
               $itk_component(pixelmask) set_axis_limits $axis "$ll $ul"
            } else {
               #  No limits, so back to full cube.
               $imagedata_ configure -limits {}
               $itk_component(pixelmask) clear_axis_limits
            }
         }

         #  Finally ready to read the data.
         $imagedata_ access

         #  Access any masks. These can only be applied when WCS is available.
         #  So the connection of the masks to imagedata_ must be deferred.
         $itk_component(pixelmask) access [$imagedata_ get_wcs]
         $imagedata_ applymasks
      }
      return $result
   }

   #  Clear the scene so that nothing is displayed, by deleting any scene
   #  objects and re-rendering. Also releases the image data if fullrelease
   #  is true.
   public method clear_scene { fullrelease } {
      release_objects $fullrelease

      #  Do rendering to display a clear scene.
      $renwindow_ render
   }

   #  Release all VTK objects used in the scene, also release the image
   #  data if fullrelease is true. Extend in derived classes. Note
   #  use clear_scene during interactive clearing (this is designed
   #  for release when the window is closed).
   public method release_objects { fullrelease } {
      if { $fullrelease && $imagedata_ != {} } {
         ::delete object $imagedata_
         set imagedata_ {}
         set cubename_ {}
      }

      if { $plane_ != {} } {
         ::delete object $plane_
         set plane_ {}
         ::delete object $textwcs_
         set textwcs_ {}
      }

      if { $line_ != {} } {
         ::delete object $line_
         set line_ {}
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of GaiaCube instance that's opened the cube we're rendering.
   itk_option define -gaiacube gaiacube GaiaCube {} {}

   #  Name of the associated rtdimage widget (displaying slice).
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Filters for selecting files.
   itk_option define -filter_types filter_types Filter_types {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Wrapped VTK objects.
   protected variable imagedata_ {}
   protected variable plane_ {}
   protected variable textwcs_ {}
   protected variable simpleaxes_ {}
   protected variable outline_ {}

   #  The extraction line or ARD region.
   protected variable line_ {}

   #  Rendering window.
   protected variable renwindow_ {}

   #  Full name of the cube. Used to check if data has changed.
   protected variable cubename_ {}

   #  Accessor for the current cube (cached).
   protected variable cubeaccessor_ {}

   #  Limits of spectral extraction.
   protected variable limits_ {}

   #  Whether to check for blank pixels.
   protected variable checkbad_ 0

   #  Whether user preference for handling bad values has changed.
   protected variable changed_bad_ 0

   #  Whether to show the image plane.
   protected variable show_image_plane_ 0

   #  Whether to show the spectral line.
   protected variable show_spectral_line_ 0

   #  Whether to show the simple orientation axes.
   protected variable show_simple_axes_ 0

   #  Whether to show the outline.
   protected variable show_outline_ 0

   #  Whether to show the AST axes.
   protected variable show_ast_axes_ 0

   #  Whether to show the CUPID catalogues.
   protected variable show_cupid_cat_ 0

   #  The Plot3D instance used to draw axes (and markers).
   protected variable plot_ {}
   protected variable grf_context_ {}

   #  Whether scene has been drawn. Use to avoid selecting axes etc. causing
   #  a redraw without pressing "Draw".
   protected variable drawn_ 0

   #  Colourmap of the image plane.
   protected variable plane_colourmap_ "ramp.lasc"

   #  The image plane opacity.
   protected variable plane_opacity_ 1.0

   #  The colour of the spectral line or region.
   protected variable spectral_colour_ [gaia::AstColours::lookup_colour 5]

   #  Menu for spectral line or region colours.
   protected variable spectral_colour_menu_ {}

   #  The AST axes text scale.
   protected variable ast_textscale_ 5

   #  The colour of the AST axes text. AST index.
   protected variable ast_textcolour_ 1

   #  Menu for axes text colour.
   protected variable ast_textcolour_menu_ {}

   #  Interaction mode.
   protected variable interaction_mode_ "joystick"

   #  Whether to clip data to the extraction limits.
   protected variable apply_extraction_limits_ 1

   #  The last line description sent to GAIA.
   protected variable last_line_type_ {}
   protected variable last_line_desc_ {}

   #  Backingstore interaction mode.
   protected variable backingstore_on_ 0

   #  Changed counter for accessor WCS. Should be updated when WCS changes.
   protected variable wcs_prop_changes_ 0

   #  Global properties handler.
   protected variable props_ {}

   #  CUPID catalogues handler.
   protected variable cupid_cat_ {}

   #  Whether to show only selected objects.
   protected variable show_cupid_selected_ 0

   #  The importer for the CUPID catalogue (gaia::GaiaCupidImporter).
   protected variable importer_ {}

   #  The "View" menu.
   protected variable view_menu_ {}

   #  Whether to apply the CUPID mask to pick out data.
   protected variable applymask_ 0

   #  Whether user preference for handling masks has changed.
   protected variable changed_mask_ 0

   #  Last set of AST attributes applied. If these change then a new 
   #  plot is required.
   protected variable astatts_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
