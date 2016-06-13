#+
#  Name:
#     GaiaCube

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Control the display of a cube as a series of images in GAIA.

#  Description:
#     This class opens a 3D data cube and then displays image shaped sections
#     of it in an associated window.

#  Invocations:
#
#        GaiaCube object_name [configuration options]
#
#     This creates an instance of a GaiaCube object. The return is
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

#  Copyright:
#     Copyright (C) 2004-2005 Central Laboratory of the Research Councils
#     Copyright (C) 2006-2007 Particle Physics & Astronomy Research Council
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     MJC: Malcolm J. Currie (JAC, Hawaii)
#     {enter_new_authors_here}

#  History:
#     08-OCT-2004 (PWD):
#        Original version.
#     2012 April 20 (MJC):
#        Added support for standards of rest.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCube {}

itcl::class gaia::GaiaCube {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      set lwidth 18
      set vwidth 10

      #  Unique tag for coordinate label.
      set coord_label_tag_ $w_

      #  Object to manage temporary cubes.
      set tmpfiles_ [gaia::GaiaTempName \#auto]

      #  Set window properties.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]
      wm title $w_ "Display image sections of a cube ($itk_option(-number))"

      #  Add short help window
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Add item to read an ARD region from a file.
      add_menuitem $File command {Read ARD region...} \
         {Read an ARD region from a file and extract a spectrum} \
         -command [code $this read_ard_region_]

      #  Add the close menu item.
      add_menuitem $File command Close \
         {Close this window} \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add the View menu.
      set View [add_menubutton "View"]
      configure_menubutton View -underline 0

      #  View the cube FITS headers.
      add_menuitem $View command  "Fits header..." \
         {View the FITS header cards of the data-cube} \
         -command [code $this show_fitsheaders_] \
         -accelerator {Control-f}
      bind $w_ <Control-f> [code $this show_fitsheaders_]

      #  3D toolboxes.
      add_menuitem $View cascade "3D visualisation... " \
         {Visualise data-cube using volume or iso surface rendering} \
         -menu [menu $View.visualisation]

      add_menuitem $View.visualisation command "Iso surfaces... " \
         {Visualise data-cube using iso surface rendering} \
         -command [code $this make_toolbox isosurface 0]

      add_menuitem $View.visualisation command "Volume rendering... " \
         {Visualise data-cube using volume rendering} \
         -command [code $this make_toolbox volume 0]

      #  MEF selection.
      add_menuitem $View command "Select HDU..." \
         {Browse all the data components (MEF or NDF)} \
         -command [code $this show_hdu_list_]

      #  CUPID catalogue.
      add_menuitem $View command "Import CUPID catalogue..." \
         {Import a CUPID catalogue for display over the plane} \
         -command [code $this make_cupid_importer]

      #  Add the Options menu.
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0

      #  If HDS file mapping is not available, then set value off.
      set canmmap 1
      if { [hds::gtune MAP] != "1" } {
         configure -usemmap 0
         set canmmap 0
      }

      #  Whether to mmap cube or load directly into memory, usually
      #  mmap'd. Control-l forces direct load, if not already done.
      $Options add checkbutton -label "Memory map cube data" \
         -command [code $this load_cube_ 0] \
         -accelerator {Control-l} \
         -variable [scope itk_option(-usemmap)] \
         -onvalue 1 \
         -offvalue 0
      bind $w_ <Control-l> [code $this configure -usemmap 1]
      add_menu_short_help $Options {Memory map cube data}  \
         {Access cube data using mmap(), otherwise read fully into RAM}

      #  If memory mapping not available, then also disable this option.
      if { ! $canmmap } {
         $Options entryconfigure {Memory map cube data} -state disabled
         set itk_option(-usemmap) 0
      }

      #  Whether to constantly update the image cuts.
      $Options add checkbutton -label "Autocut" \
         -variable [scope itk_option(-autocut)] \
         -onvalue 1 \
         -offvalue 0
      add_menu_short_help $Options {Autocut}  \
         {Continuously change set the cuts of the image slices, data limits}

      #  Choice of cube component, only for NDFs.
      set component_menu_ [menu $Options.component]
      $Options add cascade -label "NDF data component" -menu $component_menu_
      foreach type {DATA VARIANCE ERROR QUALITY} {
         $component_menu_ add radiobutton \
            -variable [scope component_] \
            -value $type \
            -label $type \
            -command [code $this component_changed_]
      }

      #  Slave toolbox. Send slice and spectral extraction updates to this.
      #  Note content is dynamic and refreshed each time.
      set submenu [menu $Options.slave]
      $Options add cascade -label "Slaves" -menu $submenu
      $submenu config -postcommand [code $this update_slave_menu_ $submenu]

      #  Submenus to offer font + colour controls for label in main window.
      $Options add cascade -label "Label colour" -menu [menu $Options.labelcolor]
      foreach i $colours_ {
         $Options.labelcolor add radiobutton \
            -value $i \
            -command [code $this configure -labelcolor $i] \
            -variable [scope itk_option(-labelcolor)] \
            -background $i
      }
      add_menu_short_help $Options "Label colour" \
         {Set colour for coordinate label in main window}

      $Options add cascade -label "Label font" -menu [menu $Options.font]
      foreach i $fonts_ {
         $Options.font add radiobutton \
            -value $i \
            -label {abc} \
            -command [code $this configure -labelfont $i] \
            -variable [scope itk_option(-labelfont)] \
            -font $i
      }
      add_menu_short_help $Options "Label font" \
         {Set font for coordinate label in main window}

      #  Add the coordinate selection menu (use global variable so we can
      #  share this).
      set SpectralCoords [add_menubutton "Coords/StdOfRest"]
      configure_menubutton "Coords/StdOfRest" -underline 0
      $SpectralCoords add command \
         -label "Change coordinates or standard or rest..." \
         -command [code $this show_builtin_toolbox_]

      #  Spectral and time coordinates handler.
      set spec_coords_ [uplevel \#0 GaiaSpecCoords \#auto]
      $spec_coords_ configure -change_cmd [code $this coords_changed_]
      $spec_coords_ add_menu $SpectralCoords

      #  Spectral standard-of-rest handler.
      set spec_sor_ [uplevel \#0 GaiaSpecCoords \#auto]
      $spec_sor_ configure -change_cmd [code $this coords_changed_]

      #  Add the "Go" menu to switch back and forth between cubes that have
      #  been loaded.
      set GoMenu [add_menubutton "Go" \
                  "Go: menu with shortcuts to view cubes previously viewed"]
      configure_menubutton "Go" -underline 0
      set history_ [GaiaCubeHistory \#auto -gaia_cube $this]
      $GoMenu config -postcommand [code $history_ update_history_menu $GoMenu]

      #  Add window help.
      add_help_button cube "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Name of input dataset.
      itk_component add cube {
         LabelFileChooser $w_.cube \
            -text "Input cube:" \
            -textvariable [scope itk_option(-cube)] \
            -command [code $this configure -cube] \
            -filter_types $itk_option(-filter_types) \
            -chooser_title "Select cube"
      }
      pack $itk_component(cube) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(cube) \
         {Name of the input data file, must be a cube}

      #  Control for selecting the axis we move along.
      itk_component add axis {
         LabelMenu $w_.axis \
            -text "Axis:" \
            -labelwidth $lwidth
      }
      foreach {label value} { one 1 two 2 three 3 } {
         $itk_component(axis) add \
            -command [code $this set_step_axis_ $value] \
            -label $label \
            -value $value
      }
      $itk_component(axis) configure -value 3

      pack $itk_component(axis) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(axis) \
         {Axis to step along}

      #  Slider that moves along the chosen axis.
      itk_component add index {
         GaiaSpectralPlotLine $w_.index \
            -gaiacube [code $this] \
            -ref_id 1 \
            -text {Index of plane:} \
            -value $plane_ \
            -show_type 1 \
            -show_ref_line 1 \
            -show_increment 1 \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -coord_update_cmd [code $this set_display_plane] \
            -drag_update_cmd [code $this update_wcs]
      }
      set ref_line_controls_(1) $itk_component(index)
      pack $itk_component(index) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(index) \
         {Index of the image plane to display (along current axis)}

      #  Whether to show a label with the coordinate in the main window.
      itk_component add showcoordlabel {
         StarLabelCheck $w_.showcoordlabel \
            -text "Show coordinate label:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope itk_option(-show_coord_label)] \
            -command [code $this toggle_show_coord_label_]
      }
      pack $itk_component(showcoordlabel) \
         -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(showcoordlabel) \
         {Display a label showing the coordinate in the main window}

      #  Add tab window for choosing either the helper controls.
      itk_component add tabnotebook {
         iwidgets::tabnotebook $w_.tab -equaltabs 0 \
            -angle 0 -tabpos n -width 460 -height 500
      }
      pack $itk_component(tabnotebook) -fill both -expand 1

      $itk_component(tabnotebook) add -label Spectrum
      set spectrumTab [$itk_component(tabnotebook) childsite 0]

      $itk_component(tabnotebook) add -label Animation
      set animationTab [$itk_component(tabnotebook) childsite 1]

      $itk_component(tabnotebook) add -label Collapse
      set collapseTab [$itk_component(tabnotebook) childsite 2]

      $itk_component(tabnotebook) add -label Chanmap
      set chanmapTab [$itk_component(tabnotebook) childsite 3]

      $itk_component(tabnotebook) add -label Rebin
      set rebinTab [$itk_component(tabnotebook) childsite 4]

      $itk_component(tabnotebook) add -label Filter
      set filterTab [$itk_component(tabnotebook) childsite 5]

      $itk_component(tabnotebook) add -label Baseline
      set baselineTab [$itk_component(tabnotebook) childsite 6]

      #  Spectrum section.
      itk_component add sruler {
         LabelRule $spectrumTab.sruler -text "Spectrum controls:"
      }
      pack $itk_component(sruler) -side top -fill x

      #  Scrolled frame for the specrum window. MJC complained.
      itk_component add sf1 {
         iwidgets::scrolledframe $spectrumTab.sf1 \
            -width 400 \
            -height 500 \
            -scrollmargin 0 \
            -relief flat \
            -vscrollmode dynamic \
            -hscrollmode dynamic
      } {
         #  Weird interaction with usual.
         rename -labelfont -labelfont labelfont LabelFont
      }
      set sf1win [$itk_component(sf1) childsite]

      set ref_ids 1
      itk_component add spectrum {
         GaiaCubeSpectrum $sf1win.spectrum \
            -gaiacube [code $this] \
            -gaia $itk_option(-gaia) \
            -ref_id $ref_ids \
            -lower_limit $plane_ \
            -upper_limit $plane_ \
            -show_ref_range 0 \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_] \
            -spec_sor [code $spec_sor_] \
            -component [get_component_] \
            -transient_spectralplot $itk_option(-transient_spectralplot) \
            -notify_cmd [code $this spectrum_moved_]
      }
      set ref_range_controls_($ref_ids) $itk_component(spectrum)
      pack $itk_component(spectrum) -side top -fill both -ipadx 1m -ipady 1m
      pack $itk_component(sf1) -side top -fill both -ipadx 1m -ipady 1m
      ::update idletasks

      #  Animation section.

      itk_component add aruler {
         LabelRule $animationTab.aruler -text "Animation controls:"
      }
      pack $itk_component(aruler) -side top -fill x

      incr ref_ids
      itk_component add animation {
         GaiaCubeAnimation $animationTab.animation \
            -gaiacube [code $this] \
            -ref_id $ref_ids \
            -lower_limit $plane_ \
            -upper_limit $plane_ \
            -show_ref_range 0 \
            -labelwidth $lwidth \
            -valuewidth $vwidth
      }
      set ref_range_controls_($ref_ids) $itk_component(animation)
      pack $itk_component(animation) -side top -fill both -ipadx 1m -ipady 1m

      #  Collapse section.

      itk_component add cruler {
         LabelRule $collapseTab.cruler -text "Collapse controls:"
      }
      pack $itk_component(cruler) -side top -fill x

      incr ref_ids
      itk_component add collapse {
         GaiaCubeCollapse $collapseTab.collapse \
            -gaiacube [code $this] \
            -ref_id $ref_ids \
            -lower_limit $plane_ \
            -upper_limit $plane_ \
            -show_ref_range 0 \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
      }
      set ref_range_controls_($ref_ids) $itk_component(collapse)
      pack $itk_component(collapse) -side top -fill both -ipadx 1m -ipady 1m

      #  Chanmap section.

      itk_component add chanmapruler {
         LabelRule $chanmapTab.chanmapruler -text "Channel map controls:"
      }
      pack $itk_component(chanmapruler) -side top -fill x

      incr ref_ids
      itk_component add chanmap {
         GaiaCubeChanmap $chanmapTab.chanmap \
            -gaiacube [code $this] \
            -ref_id $ref_ids \
            -lower_limit $plane_ \
            -upper_limit $plane_ \
            -show_ref_range 0 \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
      }
      set ref_range_controls_($ref_ids) $itk_component(chanmap)
      pack $itk_component(chanmap) -side top -fill both -ipadx 1m -ipady 1m

      #  Rebin section.

      itk_component add rebinruler {
         LabelRule $rebinTab.rebinruler -text "Rebin cube controls:"
      }
      pack $itk_component(rebinruler) -side top -fill x

      itk_component add rebin {
         GaiaCubeRebin $rebinTab.rebin \
            -gaiacube [code $this] \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
      }
      pack $itk_component(rebin) -side top -fill both -ipadx 1m -ipady 1m

      #  Filter section.

      itk_component add filterruler {
         LabelRule $filterTab.filterruler -text "Filter cube controls:"
      }
      pack $itk_component(filterruler) -side top -fill x

      itk_component add filter {
         GaiaCubeFilter $filterTab.filter \
            -gaiacube [code $this] \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
      }
      pack $itk_component(filter) -side top -fill both -ipadx 1m -ipady 1m

      #  Baseline subtraction section. Must be the last.

      itk_component add baselineruler {
         LabelRule $baselineTab.baselineruler \
            -text "Baseline subtraction controls:"
      }
      pack $itk_component(baselineruler) -side top -fill x

      set baseline_id_ [incr ref_ids]
      itk_component add baseline {
         GaiaCubeBaseline $baselineTab.baseline \
            -gaiacube [code $this] \
            -ref_id $baseline_id_ \
            -lower_limit $plane_ \
            -upper_limit $plane_ \
            -show_ref_range 0 \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
      }
      set ref_range_controls_($baseline_id_) $itk_component(baseline)
      pack $itk_component(baseline) -side top -fill both -expand 1 \
         -ipadx 1m -ipady 1m

      #  Close window.
      itk_component add close {
         button $w_.close -text Close \
            -command [code $this close]
      }
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(close) {Close window}

      #  Reveal first page of controls.
      $itk_component(tabnotebook) select 0
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Release the cube.
      catch {release}

      #  Stop interactions.
      catch {halt}

      #  Delete the NDF image slice.
      if { $section_name_ != {} } {
         catch {::file delete $section_name_} msg
      }

      #  Delete any registered temporary files.
      $tmpfiles_ clear
      ::delete object $tmpfiles_
      set tmpfiles_ {}

      #  Release memory used for image slice.
      if { $last_slice_adr_ != 0 } {
         catch {$cubeaccessor_ release $last_slice_adr_}
      }

      #  Delete GaiaCubeHistory.
      if { $history_ != {} } {
         catch {::delete object $history_}
      }

      #  Name handler.
      if { $namer_ != {} } {
         catch {::delete object $namer_}
      }
   }

   #  Methods:
   #  --------

   #  Close window.
   public method close {} {
      wm withdraw $w_
      halt
   }

   #  Halt all interactive operations.
   public method halt {} {

      #  Close spectrum plot.
      $itk_component(spectrum) close

      #  Halt any animations.
      $itk_component(animation) stop

      #  If displaying a channel map remove the markers.
      $itk_component(chanmap) close

      #  Permanently remove the coordinate label.
      delete_coord_label_
      configure -show_coord_label 0

      #  Destroy FITS headers window.
      if { $fitsheaders_ != {} } {
         catch {
            destroy $fitsheaders_
            set fitsheaders_ {}
         }
      }
   }

   #  Undo some of the effects of close. Do not reopen cube, that would
   #  be expensive and reset too much. Do that using release close/halt.
   public method open {} {
      $itk_component(spectrum) open
   }

   #  Release the cube, making ready for new cube or object destruction.
   #  Don't use this unless that's what you want. Returns 1 if a cube
   #  was open.
   public method release {} {
      if { $cubeaccessor_ != {} } {
         set isopen [$cubeaccessor_ close]
         set cubeaccessor_ {}
         set itk_option(-cube) {}
         set ndfname_ {}
         $history_ clear_last_cube

         #  Clear the iso and volume renderers.
         renderers_clear_ 1
         return $isopen
      }
      return 0
   }

   #  Open a new cube, restoring the current extraction limits, if possible.
   public method open_keeplimits {filename} {
      set itk_option(-cube) $filename
      set_chosen_cube_ 1
   }

   #  Open the currently chosen file as a cube. If requested attempt to
   #  preserve the extraction limits through the change, even if the cube
   #  dimensions change.
   protected method set_chosen_cube_ {{keeplimits 0}} {

      if { $namer_ == {} } {
         set namer_ [GaiaImageName \#auto]
      }
      $namer_ configure -imagename $itk_option(-cube)

      if { ! [$namer_ exists] } {
         if { $itk_option(-cube) != {} } {
            error_dialog "No such file: $itk_option(-cube)" $w_
            $history_ record_last_cube
         }
         return
      }

      #  Add last cube  to the back list, including temporary ones.
      $history_ record_last_cube

      #  Always start with the DATA component (essential).
      set component_ "DATA"
      set last_component_ "DATA"
      if { [info exists itk_component(spectrum)] } {
         $itk_component(spectrum) configure -component "DATA"
      }

      if { $cubeaccessor_ == {} } {
         set cubeaccessor_ [uplevel \#0 GaiaNDAccess \#auto]
      } elseif { $keeplimits } {

         #  Existing cube accessor and keeping limits regardless, so we
         #  need to save some properties before letting this go.
         save_limits_
      }

      $namer_ absolute
      set ndfname_ [$namer_ ndfname 0]
      set fullname [$namer_ fullname]
      set type_ [$namer_ type]

      $cubeaccessor_ configure -dataset "$fullname"

      set bounds_ [$cubeaccessor_ getbounds 0]
      set ndims [expr [llength $bounds_]/2]

      #  Allow fourth dimension, as long as it is redundant.
      if { $ndims == 4 } {
         if { [expr [lindex $bounds_ 7] - [lindex $bounds_ 6]] == 0 } {
            set close_section_ ",1)"
            set bounds_ [lrange $bounds_ 0 5]
            set ndims 3
         }
      } else {
         #  Sections just close with parenthesis.
         set close_section_ ")"
      }
      if { $ndims != 3 } {
         error_dialog \
            "Not a cube, must have 3 significant dimensions (has $ndims)"
         $history_ record_last_cube
         return
      }

      #  Clear the iso and volume renderers before changing data.
      renderers_clear_ 1

      #  Map in all data, this makes it immediately available within
      #  application
      load_cube_ 1

      #  Retain the TITLE value for creating new object values.
      set object_ [$cubeaccessor_ getc "TITLE"]

      #  Set axis and display the plane for first time. Try to pick
      #  out a spectral axis.
      if { [$cubeaccessor_ isaxisframetype 3 "specframe"] } {
         set axis_ 2
         set axis 3
      } elseif { [$cubeaccessor_ isaxisframetype 2 "specframe"] } {
         set axis_ 1
         set axis 2
      } elseif { [$cubeaccessor_ isaxisframetype 1 "specframe"] } {
         set axis_ 2
         set axis 1
      } else {
         set axis_ 2
         set axis 3
      }

      #  And label the menu items helpfully.
      $itk_component(axis) clear
      set axisdesc [$cubeaccessor_ axisdescriptions]
      set i 0
      foreach {label value} { one 1 two 2 three 3 } {
         set desc [lindex $axisdesc $i]
         incr i
         if { $desc != "unknown" } {
            set label "$label : $desc"
         }
         $itk_component(axis) add \
            -command [code $this set_step_axis_ $value] \
            -label $label \
            -value $value
      }

      #  Set up object to control units. Do this before axis change
      #  so that cube is available for units query.
      $spec_coords_ configure -accessor $cubeaccessor_
      $spec_sor_ configure -accessor $cubeaccessor_

      #  Now apply axis change.
      $spec_coords_ configure -axis $axis
      $itk_component(axis) configure -value $axis
      set_step_axis_ $axis

      #  Now apply axis change.
      $spec_sor_ configure -axis $axis
      $itk_component(axis) configure -value $axis
      set_step_axis_ $axis

      #  If the spectral plot is open, then close it. It will be
      #  re-created on next image click.
      $itk_component(spectrum) close_plot

      #  If keeping the limits, regardless, then attempt the restore.
      if { $keeplimits } {
         restore_limits_
      }

      #  Add cube to history (and previous cube to back list).
      $history_ add_history $itk_option(-cube)

      #  Update FITS headers, if viewed.
      maybe_update_fitsheaders_

      #  Disable or enable NDF component menu.
      configure_component_menu_

      #  Update the coordinate selection window, if open.
      if { [winfo exists $w_.domainchooser] } {
         $w_.domainchooser image_changed
      }
   }

   #  Load the cube data, using the current mmap mode. Only happens when force
   #  is true, or if a change the mmap mode of the cube is needed.
   #  Controlling the mmap mode can force file i/o to be is used to load all
   #  the cube into malloc'd memory, which speeds the spectral
   #  readout. Otherwise mmap is used, which may give an initially slow
   #  spectral readout for large cubes.
   protected method load_cube_ { force } {
      if { $cubeaccessor_ == {} } {
         return
      }
      if { [$cubeaccessor_ cget -usemmap] != $itk_option(-usemmap) || $force } {
         busy {
            $cubeaccessor_ unmap
            $cubeaccessor_ configure -usemmap $itk_option(-usemmap)
            set adr [$cubeaccessor_ map "READ" "DATA"]
         }
      }
   }

   #  Set the axis we step along.
   #
   #  If setplane is true (the default) then a side-effect of this is to
   #  create the dummy NDF that will be actually manipulated in the main
   #  display window and position on the central plane. A dummy NDF is used
   #  for FITS and NDF files as this is the simplest way to make sure that the
   #  toolboxes can also access the data in an efficient manner. When a
   #  toolbox accesses this file it will need to save the NDF first to make
   #  sure that the data values are up to date.
   protected method set_step_axis_ {value {setplane 1}} {
      if { $value != $axis_ && $bounds_ != {} } {
         set axis_ $value
         set plane_min_ [lindex $bounds_ [expr (${axis_}-1)*2]]
         set plane_max_ [lindex $bounds_ [expr (${axis_}-1)*2+1]]

         $itk_component(index) configure -from $plane_min_ -to $plane_max_
         if { $setplane } {
            $itk_component(index) configure -value $plane_min_
         }

         foreach type "spectrum animation collapse chanmap baseline" {
            $itk_component($type) set_bounds $plane_min_ $plane_max_
         }

         #  Update the display of label and units in index component.
         $itk_component(index) update_coords_type $plane_

         #  Set up object to control units.
         $spec_coords_ configure -axis $axis_ -accessor $cubeaccessor_

         #  Set an initial plane, if requested.
         if { $setplane } {
            set plane_ $plane_min_
            set_display_plane [expr ($plane_max_ + $plane_min_)/2] 1
         } else {
            #  Otherwise update the displayed coordinate for the plane.
            $itk_component(index) configure -value $plane_
         }

         #  Update the 3D renderers.
         renderers_set_display_axis_
      }
   }

   #  Handle a change in the coordinate system. This requires all the local
   #  coordinates to be updated. Same as changing the axis, but without an
   #  actual change.
   protected method coords_changed_ {} {
      $itk_component(spectrum) coord_system_changed

      set value $axis_
      incr axis_
      set_step_axis_ $value 0

      #  Change catalogues to match coordinate system.
      if { [winfo exists $w_.cupidimporter] } {
         $w_.cupidimporter attach_coords
      }
   }

   #  Handle a change in the standard of rest.  This requires all the local
   #  coordinates to be updated. Same as changing the axis, but without an
   #  actual change.
   protected method sor_changed_ {} {
      $itk_component(spectrum) sor_changed

      set value $axis_
      incr axis_
      set_step_axis_ $value 0

      #  Change catalogues to match coordinate system.
      if { [winfo exists $w_.cupidimporter] } {
         $w_.cupidimporter attach_coords
      }
   }

   #  Get the plane being displayed.
   public method get_display_plane {} {
      return $plane_
   }

   #  Set the plane to display and display it. When regen is true a new
   #  image NDF is displayed, otherwise just the NDF image data is updated.
   public method set_display_plane { newvalue {regen 0} } {

      #  Do nothing, if the plane remains the same.
      if { ( $newvalue == $plane_ && ! $regen ) || $ndfname_ == {} } {
         return
      }

      if { $newvalue >= $plane_max_ } {
         set plane_ $plane_max_
      } elseif { $newvalue <= $plane_min_ } {
         set plane_ $plane_min_
      } else {
         set plane_ $newvalue
      }

      #  If regenerating create the dummy NDF. Note this is also needed when
      #  an image has replaced the dummy NDF (collapses, chanmaps etc.).
      #  Check this is by querying the object name, it should change.
      if { ! $regen } {
         set newobject [$itk_option(-rtdimage) object]
         if { $newobject != $fullobject_ } {
            set regen 1
         }
      }

      if { $regen } {

         #  Set name of the image displayed image section.
         set oldname $section_name_
         set section_name_ [gaia::GaiaTempName::make_name \
                               "GaiaTempCubeSection" [incr count_] ".sdf"]

         #  And create the dummy image NDF. Will have axis removed from
         #  the WCS and be the size and type of cube in other axes.
         set comp [get_component_]
         set imageaccessor [$cubeaccessor_ createimage $section_name_ $axis_ $comp]

         #  Map in the data component to initialise it. Use BAD
         #  to avoid NDF bad flag from being set false.
         $imageaccessor map "WRITE/BAD"

         #  Close before it can be displayed by the rtdimage.
         $imageaccessor close

         #  Display this for the first time.
         display $section_name_ 0

         #  Make sure everything is up-to-date (new data has been accepted by
         #  the RtdImage) so that the data replacement happens on the file
         #  we've just loaded (can get out of sync when unexpected errors
         #  autoloading Tcl scripts occur, which defer the newImage acceptance
         #  callbacks). Unset ndfname_ so that this method isn't called
         #  during the update.
         set temp $ndfname_
         set ndfname_ {}
         ::update
         set ndfname_ $temp

         #  Cube may have been waiting for clear.
         if { $cubeaccessor_ == {} } {
            return
         }

         #  Now delete old image slice (waited until released).
         if { $oldname != {} } {
            catch {::file delete $oldname} msg
            if { $msg != {} } {
               puts stderr \
                  "WARNING: Failed to delete old image section: $msg"
            }
         }

         #  Enable any bindings etc. in the spectral extraction tool.
         $itk_component(spectrum) open

         #  Remove any channel map markers.
         $itk_component(chanmap) close
      }

      #  Now copy this plane from the cube and update the image.
      #  Correct plane to grid indices.
      if { $axis_ == 1 } {
         set zo [lindex $bounds_ 0]
      } elseif { $axis_ == 2 } {
         set zo [lindex $bounds_ 2]
      } else {
         set zo [lindex $bounds_ 4]
      }
      set zp [expr round($plane_+1-$zo)]

      #  Access the image data for this plane and replace the displayed copy.
      set comp [get_component_]
      set adr [$cubeaccessor_ getimage $axis_ $zp 1 $comp]
      lassign [$cubeaccessor_ getinfo $adr] realadr nel type
      $itk_option(-rtdimage) replaceimagedata $realadr

      #  If regenerating or asked bring autocuts etc. into sync.
      if { $regen || $itk_option(-autocut) } {
         $itk_option(-rtdimage) autocut -percent 98
      }

      #  Set the object description of this slice to include the
      #  cube slice. Get it back for comparison (can be truncated)"
      $itk_option(-rtdimage) object "[slice_display_name_] ($object_)"
      set fullobject_ [$itk_option(-rtdimage) object]
      $rtdctrl_info_ updateValues

      #  Release memory from last time and save pointer.
      if { $last_slice_adr_ != 0 } {
         $cubeaccessor_ release $last_slice_adr_
      }
      set last_slice_adr_ $adr

      #  Make sure position show in slide matches this (note feedback is
      #  avoided as $newvalue != $plane_).
      $itk_component(index) configure -value $plane_

      #  Send event to GAIA3D toolboxes, if up and running.
      renderers_set_display_plane_

      #  Send event to any slave toolboxes.
      slave_set_display_plane_

      #  Update the coordinate label, if needed.
      update_coord_label_

      #  Change the displayed catalogue overlays for CUPID.
      set_cupid_coord 1

      #  Update everything to make sure we don't get partial image refreshes.
      update idletasks
   }

   #  Set the position of a reference line shown in the plot and update
   #  any CUPID catalogues with the new reference coordinate.
   public method set_spec_ref_coord {id coord} {
      $itk_component(spectrum) set_spec_ref_coord $id $coord
   }

   #  Update the CUPID reference position to match the extracted slice
   #  and update any displayed catalogues if propagate is true.
   public method set_cupid_coord {propagate} {
      if { [winfo exists $w_.cupidimporter] } {

         #  Need the position of the slice in cube coordinates. That
         #  coordinate system needs to be attached to the catalogue
         #  and we need to transform into the coordinates of the
         #  catalogue. Assumes importer has been told about any coordinate
         #  system changes.
         set coord [get_plane_coord 0 0 0]
         set coord [$w_.cupidimporter set_coord $coord]
         if { $propagate } {
            $w_.cupidimporter replot
         }
      } else {
         set ::cupid(COORD) 0
      }
   }

   #  Set the position of a reference range shown in the plot.
   public method set_spec_ref_range_coord {id coord1 coord2} {
      $itk_component(spectrum) set_spec_ref_range_coord $id $coord1 $coord2
   }

   #  Handle the change in a spectral reference line, when done within the
   #  plot (user interaction by dragging line).
   protected method ref_line_moved_ {id coord action} {

      #  Convert coord from grid indices to pixel indices.
      set coord [axis_grid2pixel $coord]

      #  Apply value to the right control, but avoiding new actions being
      #  triggered, unless this is release when we want to reposition to the
      #  exact control feedback.
      if { $action == "move" } {
         set oldvalue [$ref_line_controls_($id) cget -show_ref_line]
         $ref_line_controls_($id) configure -show_ref_line 0
      }
      $ref_line_controls_($id) configure -value $coord

      #  And get the associated action to run.
      eval [$ref_line_controls_($id) cget -coord_update_cmd] $coord

      #  If action is released also do the drag_update_cmd.
      if { $action == "released" } {
         eval [$ref_line_controls_($id) cget -drag_update_cmd]
      }
      if { $action == "move" } {
         $ref_line_controls_($id) configure -show_ref_line $oldvalue
      }
   }

   #  Handle the change in a spectral reference range, when done within the
   #  plot (user interaction by dragging line).
   protected method ref_range_moved_ {id coord1 coord2 action} {

      #  Convert coord from grid indices to pixel indices.
      set coord1 [axis_grid2pixel $coord1]
      set coord2 [axis_grid2pixel $coord2]

      #  Set the limits of the control and get it to configure itself.
      set cid $id
      if { $id > $baseline_id_ } {
         set cid $baseline_id_
      }
      $ref_range_controls_($cid) configure \
         -lower_limit $coord1 -upper_limit $coord2
      $ref_range_controls_($cid) ref_range_moved $id $coord1 $coord2 $action
   }

   #  Update the dummy NDF WCS so that it matches the current spectral
   #  coordinates. This should be done only when necessary (end of animation,
   #  slice slider drag etc.).
   public method update_wcs {} {
      if { $cubeaccessor_ != {} } {
         set index [axis_pixel2grid $plane_]
         set frameset [$cubeaccessor_ getimagewcs $axis_ $index]
         if { $frameset != 0 } {
            $itk_option(-rtdimage) astreplace $frameset
         }
      }
   }

   #  Create a description of the slice. Note use short name so should
   #  probably not be used to access any NDF sections.
   protected method slice_display_name_ {} {
      if { $axis_ == 1 } {
         set section "($plane_,,$close_section_"
      } elseif { $axis_ == 2 } {
         set section "(,$plane_,$close_section_"
      } else {
         set section "(,,${plane_}${close_section_}"
      }
      return ${itk_option(-cube)}${section}
   }

   #  Display an image. Note these cannot be temporary in the normal sense,
   #  that messes up the interaction. We deal with temporary images locally.
   public method display {name {istemp 0} } {
      $itk_option(-gaia) configure -check_for_cubes 0
      $itk_option(-gaia) open $name
      if { $istemp } {
         register_temp_file $name
      }
      $itk_option(-gaia) configure -check_for_cubes $check_for_cubes_
   }

   #  Register a temporary file. These will be deleted along with this
   #  object.
   public method register_temp_file {filename} {
      $tmpfiles_ add_name $filename
   }

   #  Get the coordinate of the current plane along the current axis.
   public method get_plane_coord {{formatted 1} {trail 0} {readable 0}} {
      return [get_coord $plane_ $formatted $trail $readable]
   }

   #  Get the coordinate of an index along the current axis.
   public method get_coord {index {formatted 1} {trail 0} {readable 0}} {
      #  Need index as a pixel coordinate.
      set pcoord [expr $index - 0.5]
      if { $axis_ == 1 } {
         set section [list $pcoord 1 1]
      } elseif { $axis_ == 2 } {
         set section [list 1 $pcoord 1]
      } else {
         set section [list 1 1 $pcoord]
      }
      set coord {}
      catch {
         set coord [$cubeaccessor_ getcoord \
                       $axis_ $section $formatted $trail $readable]
      }
      return $coord
   }

   #  Convert grid indices to pixel indices along image axes.
   public method image_grid2pixel {x y} {
      if { $axis_ == 1 } {
         set xo [lindex $bounds_ 2]
         set yo [lindex $bounds_ 4]
      } elseif { $axis_ == 2 } {
         set xo [lindex $bounds_ 0]
         set yo [lindex $bounds_ 4]
      } else {
         set xo [lindex $bounds_ 0]
         set yo [lindex $bounds_ 2]
      }

      #  Correct to pixel indices.
      set ix [expr round($x-1+$xo)]
      set iy [expr round($y-1+$yo)]
      return "$ix $iy"
   }

   #  Convert grid index to pixel index along the selected axis.
   public method axis_grid2pixel { value } {
      if { $axis_ == 1 } {
         set zo [lindex $bounds_ 0]
      } elseif { $axis_ == 2 } {
         set zo [lindex $bounds_ 2]
      } else {
         set zo [lindex $bounds_ 4]
      }
      return [expr round($value-1+$zo)]
   }

   #  Convert pixel indices to grid indices along image axes.
   public method image_pixel2grid {x y} {
      if { $axis_ == 1 } {
         set xo [lindex $bounds_ 2]
         set yo [lindex $bounds_ 4]
      } elseif { $axis_ == 2 } {
         set xo [lindex $bounds_ 0]
         set yo [lindex $bounds_ 4]
      } else {
         set xo [lindex $bounds_ 0]
         set yo [lindex $bounds_ 2]
      }

      #  Correct to grid indices.
      set ix [expr round($x+1-$xo)]
      set iy [expr round($y+1-$yo)]
      return "$ix $iy"
   }

   #  Convert pixel index to grid index along the selected axis.
   public method axis_pixel2grid { value } {
      if { $axis_ == 1 } {
         set zo [lindex $bounds_ 0]
      } elseif { $axis_ == 2 } {
         set zo [lindex $bounds_ 2]
      } else {
         set zo [lindex $bounds_ 4]
      }
      return [expr round($value+1-$zo)]
   }

   #  Should be called when the data component has changed.
   protected method component_changed_ {} {

      if { $cubeaccessor_ != {} } {

         #  The ERROR component is the VARIANCE component in fact, that just
         #  applies when mapping.
         set component [get_component_]

         if { [$cubeaccessor_ exists $component] } {

            #  Only the DATA component is mapped by default, so make sure
            #  we access the others too.
            if { $component_ == "ERROR" } {
               $cubeaccessor_ configure -maperrors 1
            } else {
               $cubeaccessor_ configure -maperrors 0
            }
            if { $component != "DATA" } {
               set adr [$cubeaccessor_ map "READ" $component]
            }

            #  Cause an update of the image.
            set_display_plane $plane_ 1
         } else {

            #  Undo changes and remain with current component.
            info_dialog "No $component_ component in cube" $w_
            if { $component_ != $last_component_ } {
               set component_ $last_component_
            }
            return
         }
      }

      #  Get extracted spectra to display this component.
      if { [info exists itk_component(spectrum)] } {
         $itk_component(spectrum) configure -component [get_component_]
      }

      #  Fallback when component doesn't exist.
      set last_component_ $component_
   }

   #  Enable or disable items in NDF component menu depending on whether we
   #  have an NDF cube or not.
   protected method configure_component_menu_ {} {
      if { $type_ == ".sdf" } {
         set state normal
      } else {
         set state disabled
      }
      foreach type {DATA VARIANCE ERROR QUALITY} {
         $component_menu_ entryconfigure $type -state $state
      }
   }


   #  Read a region from an ARD file.
   protected method read_ard_region_ {} {
      utilReUseWidget util::FileSelect $w_.ardselect -title "Choose ARD file"
      if {[$w_.ardselect activate]} {
         $itk_component(spectrum) read_ard_file [$w_.ardselect get]
      }
   }

   #  Add or remove the coordinate label in the main window.
   protected method toggle_show_coord_label_ {args} {

      #  Delete the current label.
      delete_coord_label_

      #  Stop now if not drawing.
      if { ! $itk_option(-show_coord_label) } {
         return
      }

      #  Determine a position inside the view, if we don't already have a
      #  reference position.
      if { $xref_ == {} } {
         lassign [calc_label_pos_] xref_ yref_
      }

      #  Create the text item.
      set id [$itk_option(-canvas) create text $xref_ $yref_ \
                 -text "" \
                 -anchor center \
                 -justify left \
                 -fill $itk_option(-labelcolor) \
                 -font $itk_option(-labelfont) \
                 -tags $coord_label_tag_ \
                 -width 0]

      #  Add bindings to move this about.
      $itk_option(-canvas) bind $id <1> [code eval $this record_mark_ %x %y]
      $itk_option(-canvas) bind $id <B1-Motion> [code eval $this move_label_ %x %y]

      #  Set label value.
      update_coord_label_
   }

   #  Update the coordinate label shown in the main window, if enabled.
   protected method update_coord_label_ {} {
      if { $itk_option(-show_coord_label) } {
         $itk_option(-canvas) itemconfigure $coord_label_tag_ \
            -text [get_plane_coord 1 1 1]
      }
   }

   #  Delete the coordinate label, but first record position, if available
   #  (used to restore).
   protected method delete_coord_label_ {} {
      lassign [$itk_option(-canvas) coords $coord_label_tag_] x y
      if { $x != {} } {
         set xref_ $x
         set yref_ $y
      }
      $itk_option(-canvas) delete $coord_label_tag_
   }

   #  Record the position when <1> is pressed.
   protected method record_mark_ {x y} {
      set xref_ [$itk_option(-canvas) canvasx $x]
      set yref_ [$itk_option(-canvas) canvasy $y]
   }

   #  Move the label relative to the marked position (need this method as
   #  coords positions relative to the anchor).
   protected method move_label_ {x y} {
      set x [$itk_option(-canvas) canvasx $x]
      set y [$itk_option(-canvas) canvasy $y]
      set dx [expr $x-$xref_]
      set dy [expr $y-$yref_]
      $itk_option(-canvas) move $coord_label_tag_ $dx $dy
      set xref_ $x
      set yref_ $y
   }

   #  Get a position to anchor coordinate label. Middle of X near top of Y.
   protected method calc_label_pos_ {} {
      set w [winfo width $itk_option(-canvas)]
      set h [winfo height $itk_option(-canvas)]
      set x0 [$itk_option(-canvas) canvasx 0]
      set y0 [$itk_option(-canvas) canvasy 0]
      set dw [expr $w*0.5]
      set dh [expr $h*0.05]
      set x [expr $x0+$dw]
      set y [expr $y0+$dh]
      return [list $x $y]
   }


   #  ==========================================
   #  Utility methods for various helper classes
   #  ==========================================

   #  Make a reference range in the spectral plot, if shown. Use the given
   #  identifier.
   public method make_ref_range {id} {
      $itk_component(spectrum) make_ref_range $id
   }

   #  Remove a reference range from the spectral plot, if shown. Use the given
   #  identifier.
   public method remove_ref_range {id} {
      $itk_component(spectrum) remove_ref_range $id
   }

   #  Set the colour of the identified reference range.
   public method set_ref_range_colour {id colour} {
      $itk_component(spectrum) set_ref_range_colour $id $colour
   }

   #  Get the name of the cube (in NDF format).
   public method get_ndfname {} {
      return $ndfname_
   }

   #  Get the type of the cube.
   public method get_type {} {
      return $type_
   }

   #  Get the current axis.
   public method get_axis {} {
      return $axis_
   }

   #  Set the current axis.
   public method set_axis {value} {
      $itk_component(axis) configure -value $value
      set_step_axis_ $value
   }

   #  Allow support classes access to the cubeaccessor. This access
   #  should be considered readonly.
   public method get_cubeaccessor {} {
      return $cubeaccessor_
   }

   #  Get the current axis.
   public method get_close_section {} {
      return $close_section_
   }

   #  Set the spectral extraction range. Tools that display collapsed images
   #  will probably want to do this.
   public method set_extraction_range {lower upper} {
      $itk_component(spectrum) ref_range_moved 1 $lower $upper none
   }

   #  Return object for controlling the spectral coordinates.
   public method get_spec_coords {} {
      return $spec_coords_
   }

   #  Return object for controlling the standards of rest.
   public method get_spec_sor {} {
      return $spec_sor_
   }

   #  Return the spectral extraction limits in pixel indices. Empty string if
   #  not set (i.e. at bounds of data).
   public method get_extraction_limits {} {
      return [$itk_component(spectrum) get_set_limits]
   }

   #  =================
   #  Methods involved in "saving" the current extraction, animation
   #  etc. limits.
   #  =================

   protected method save_limits_ {} {
      catch {unset limits_}

      if { $cubeaccessor_ != {} } {

         #  Save a copy of the current frameset, that defines the world
         #  coordinates.
         set wcs [$cubeaccessor_ getwcs]
         set limits_(frame) [gaiautils::getframe $wcs "current"]

         #  Axis all the limits apply too.
         set limits_(axis) $axis_

         #  Save all limits in world coordinates. Only done when these
         #  are different to bounds.
         foreach type "spectrum animation collapse chanmap" {
            set bounds [$itk_component($type) get_set_limits]
            if { $bounds != {} } {
               set limits_($type,0) [get_coords [lindex $bounds 0]]
               set limits_($type,1) [get_coords [lindex $bounds 1]]
            }
         }

         #  Baseline has several bounds. Need to visit them all.
         set limits_(baseline,nr) [$itk_component(baseline) cget -nranges]
         for {set i 0} {$i < $limits_(baseline,nr)} {incr i} {
            set bounds [$itk_component(baseline) get_set_limits $i]
            if { $bounds != {} } {
               set limits_(baseline,$i,0) [get_coords [lindex $bounds 0]]
               set limits_(baseline,$i,1) [get_coords [lindex $bounds 1]]
            }
         }
      }
   }

   protected method restore_limits_ {} {
      if { [info exists limits_] && $cubeaccessor_ != {} } {

         #  If not same axis, then don't bother. The other axis limits are
         #  faked.
         if { $limits_(axis) == $axis_ } {

            #  Connect the old frame defining the old coordinate system to the
            #  current WCS.
            set wcs [$cubeaccessor_ getwcs]

            #  Use an inverted WCS so we connect to BASE, not just CURRENT.
            gaiautils::astset $wcs "Invert=1"
            set convwcs [gaiautils::astconvert $limits_(frame) $wcs ""]
            gaiautils::astset $wcs "Invert=0"

            if { $convwcs != 0 } {

               set axis [expr $axis_-1]

               foreach type "spectrum animation collapse chanmap" {
                  if { [info exists limits_($type,0)] } {

                     set li [gaiautils::asttrann $convwcs 1 $limits_($type,0)]
                     set ui [gaiautils::asttrann $convwcs 1 $limits_($type,1)]

                     #  Convert to integers.
                     set li [expr int([lindex $li $axis])]
                     set ui [expr int([lindex $ui $axis])]

                     #  And set.
                     $itk_component($type) apply_limits $li $ui
                  }
               }

               #  Restore baselines.
               for {set i 0} {$i < $limits_(baseline,nr)} {incr i} {
                  if { [info exists limits_(baseline,$i,0)] } {
                     set li [gaiautils::asttrann $convwcs 1 \
                                $limits_(baseline,$i,0)]
                     set ui [gaiautils::asttrann $convwcs 1 \
                                $limits_(baseline,$i,1)]

                     #  Convert to integers.
                     set li [expr int([lindex $li $axis])]
                     set ui [expr int([lindex $ui $axis])]

                     #  And set.
                     $itk_component(baseline) apply_limits $i $li $ui
                  }
               }
            }
         }
      }
   }

   #  Get the coordinates of an index along the current axis. Returns
   #  a list of 3 or 4 values, depending on cube dimensions.
   public method get_coords {index} {

      #  Need index as a pixel coordinate.
      set pcoord [expr $index - 0.5]
      if { $close_section_ != ")" } {
         if { $axis_ == 1 } {
            set section [list $pcoord 1 1 1]
         } elseif { $axis_ == 2 } {
            set section [list 1 $pcoord 1 1]
         } else {
            set section [list 1 1 $pcoord 1]
         }
      } else {
         if { $axis_ == 1 } {
            set section [list $pcoord 1 1]
         } elseif { $axis_ == 2 } {
            set section [list 1 $pcoord 1]
         } else {
            set section [list 1 1 $pcoord]
         }
      }
      set coords {}
      catch {
         set wcs [$cubeaccessor_ getwcs]
         set coords [gaiautils::asttrann $wcs 1 $section]
      }
      return $coords
   }

   #  Deal with movement of the spectrum. Listeners in the 3D and other slave
   #  toolboxes may need updating, init should be set when the data limits
   #  should be autoranged (initial click).
   protected method spectrum_moved_ {type init desc} {
      renderers_spectrum_moved_ $type $desc
      slave_spectrum_moved_ $type $init $desc
   }

   #  Get the component, normally component_ but will be VARIANCE if
   #  component_ is ERROR.
   protected method get_component_ {} {
      if { $component_ == "ERROR" } {
         return "VARIANCE"
      }
      return $component_
   }

   #  ============
   #  FITS headers
   #  ============

   #  Create and display the FITS headers dialog window.
   protected method show_fitsheaders_ {} {
      if { $fitsheaders_ == {} } {
         set fitsheaders_ $w_.fitsheaders_
      }
      utilReUseWidget GaiaFITSHeader $fitsheaders_ -accessor $cubeaccessor_
      $fitsheaders_ activate
   }

   #  If displaying the FITS headers update to show headers from current cube.
   protected method maybe_update_fitsheaders_ {} {
      if { $fitsheaders_ != {} } {
         if { [winfo viewable $fitsheaders_] } {
            $fitsheaders_ activate
         }
      }
   }

   #  =============
   #  HDU selection
   #  =============
   protected method show_hdu_list_ {} {
      if { $cubeaccessor_ != {} } {
         if { $hdu_list_ == {} } {
            set hdu_list_ $w_.hdulist
         }
         utilReUseWidget GaiaCubeHduChooser $hdu_list_ -gaiacube $this
      }
   }

   # ================
   # 3D visualisation
   # ================

   #  Make one of the GAIA3D toolboxes, or deiconify.
   public method make_toolbox {type {clone 0} } {

      #  Do nothing if no cube is displayed, unless allowed.
      if { $cubeaccessor_ != {} && [$cubeaccessor_ cget -dataset] != "" } {
         set basename $type
         if { $clone } {
            #  Request to create a clone (i.e. another) toolbox. Make
            #  extended name for this.
            set basename "$basename[incr tool_clones_]"
         }

         #  If the window exists then just raise it.
         if { [info exists itk_component($basename) ] &&
              [winfo exists $itk_component($basename) ] } {
            wm deiconify $itk_component($basename)
            raise $itk_component($basename)
         } else {
            busy {
               make_${type}_toolbox $basename $clone
            }

            #  Establish the CUPID catalogue handler. If already active.
            if { [winfo exists $w_.cupidimporter] } {
               renderers_set_cupid_importer_
            }
         }
      }
   }

   #  ISO surface rendering toolbox.
   public method make_isosurface_toolbox {name {cloned 0}} {
      if { [check_for_gaia3d_] } {
         itk_component add $name {
            gaia3d::Gaia3dIsosurface $w_.\#auto \
               -gaiacube $w_ \
               -rtdimage $itk_option(-rtdimage) \
               -number $itk_option(-number) \
               -clone_cmd [code $this make_toolbox isosurface 1] \
               -really_die $cloned \
               -filter_types $itk_option(-filter_types)
         }
      } else {
         warning_dialog \
            "Cannot render cubes as the GAIA3D extension is not available" $w_
      }
   }

   #  Volume rendering toolbox.
   public method make_volume_toolbox {name {cloned 0}} {
      if { [check_for_gaia3d_] } {
         itk_component add $name {
            gaia3d::Gaia3dVolume $w_.\#auto \
               -gaiacube $w_ \
               -rtdimage $itk_option(-rtdimage) \
               -number $itk_option(-number) \
               -clone_cmd [code $this make_toolbox volume 1] \
               -really_die $cloned \
               -filter_types $itk_option(-filter_types)
         }
      } else {
         warning_dialog \
            "Cannot render cubes as the GAIA3D extension is not available" $w_
      }
   }

   #  Messages to GAIA3D.
   #  ===================

   #  Clear the scenes displayed by the renderers. Do this if cube data
   #  changes. If full is true then imagedata is released immediately
   #  rather than waiting for the next data to load.
   protected method renderers_clear_ { full } {
      foreach name "isosurface volume" {
         if { [info exists itk_component($name) ] } {
            if { [winfo exists $itk_component($name) ] } {
               $itk_component($name) clear_scene $full
            }
         }
      }
   }

   #  Update renderers to display a new plane.
   protected method renderers_set_display_plane_ {} {
      foreach name "isosurface volume" {
         if { [info exists itk_component($name) ] } {
            if { [winfo exists $itk_component($name) ] } {
               $itk_component($name) set_display_plane $plane_
            }
         }
      }
   }

   #  Update renderers to display a new axis
   protected method renderers_set_display_axis_ {} {
      foreach name "isosurface volume" {
         if { [info exists itk_component($name) ] } {
            if { [winfo exists $itk_component($name) ] } {
               $itk_component($name) set_display_axis $axis_
               $itk_component($name) set_display_plane $plane_
            }
         }
      }
   }

   #  Update renderers to move the spectrum extraction visualisation.
   #   XXX handle reference spectrum XXX.
   protected method renderers_spectrum_moved_ {type desc} {
      foreach name "isosurface volume" {
         if { [info exists itk_component($name) ] } {
            if { [winfo exists $itk_component($name) ] } {
               eval $itk_component($name) set_spectral_line $type $desc
            }
         }
      }
   }

   #  Set the CUPID catalogue importer.
   protected method renderers_set_cupid_importer_ {} {
      foreach name "isosurface volume" {
         if { [info exists itk_component($name) ] } {
            if { [winfo exists $itk_component($name) ] } {
               eval $itk_component($name) set_cupid_importer $w_.cupidimporter
            }
         }
      }
   }

   #  Communication from GAIA3D.
   #  ==========================

   #  Set the position of the spectrum, if extraction has been started.
   #  Coordinates are a position on the image slice, init should be set
   #  true when the data limts need resetting (initial click).
   public method set_point_spectrum_position {init ix iy} {
      $itk_component(spectrum) set_point_position $init $ix $iy
   }

   #  Get the position of the spectrum, if extraction has been started.
   #  Coordinates are a position on the image slice. If not extracting
   #  then "" is returned.
   public method get_point_spectrum_position {} {
      return [$itk_component(spectrum) get_point_position]
   }

   #  Get the region used to extract the spectrum, if region extraction has
   #  been started. Coordinates are a position on the image slice. If not
   #  extracting then "" is returned.
   public method get_region_spectrum_position {} {
      return [$itk_component(spectrum) get_region_position]
   }

   #  Set the local current region to a description from GAIA3D. The updated
   #  description will include any changes (shifts), init should be set true
   #  when the data limts need resetting (initial click).
   public method set_region_spectrum_position {init desc} {
      $itk_component(spectrum) set_region_position $init $desc
   }

   #  GAIA3D utilities.
   #  =================

   #  Check for the presence of GAIA3D. Only done once per-session.
   #  VTK is part of the standard build, so should be picked up.
   protected method check_for_gaia3d_ {} {
      if { $have_gaia3d_ == -1 } {
         set have_gaia3d_ 0
         if { [package versions Gaia3d] != "" } {
            if { [ catch {
               package require vtk
               package require Gaia3d
               set have_gaia3d_ 1 } msg ] } {
               info_dialog "Failed to load GAIA3D: $msg"
            }
         }
      }
      return $have_gaia3d_
   }

   #  Communication with other Cube toolboxes.
   #  ========================================

   #  Update the Options->slaves menu to show the currently available
   #  cube toolboxes.
   protected method update_slave_menu_ {menu} {

      #  Clear existing items.
      $menu delete 0 end

      #  Purge the slaves_ array of any toolboxes that no longer exist,
      #  or have been withdrawn.
      if { [info exists slaves_] } {
         foreach w [array names slaves_] {
            if { ! [winfo exists $w] || [wm state $w] == "withdrawn" } {
               unset slaves_($w)
            }
         }
      }

      #  Get list of cube toolboxes. Only show active ones. Note slaves_
      #  is indexed by the widget name, not the object name (no leading ::).
      set list [::itcl::find objects -class gaia::GaiaCube]
      foreach tb $list {
         if { $tb != $this } {
            set w [string trimleft $tb ":"]
            if { [wm state $w] != "withdrawn" } {
               set i [$tb cget -number]
               $menu add checkbutton \
                  -onvalue 1 \
                  -offvalue 0 \
                  -variable [scope slaves_($w)] \
                  -label $i \
                  -command [code $this slave_changed_ $w]
            }
         }
      }
   }

   #  Send message to slave toolboxes to match our slice position.
   protected method slave_set_display_plane_ {} {
      if { [info exists slaves_] } {
         foreach w [array names slaves_] {
            if { [winfo exists $w] && [wm state $w] != "withdrawn" } {
               $w set_display_plane $plane_
            } else {
               unset slaves_($w)
            }
         }
      }
   }

   #  Send message to enabled slave toolboxes to match our spectral extraction
   #  position (note delay by 10ms to eat up repeats).
   protected method slave_spectrum_moved_ {type init desc} {
      if { [info exists slaves_] } {
         foreach w [array names slaves_] {
            if { $slaves_($w) } {
               if { [winfo exists $w] && [wm state $w] != "withdrawn" } {
                  if { $type == "p" } {
                     lassign $desc ix iy
                     after 10 "$w set_point_spectrum_position $init $ix $iy"
                  } else {
                     after 10 "$w set_region_spectrum_position $init $desc"
                  }
               } else {
                  unset slaves_($w)
               }
            }
         }
      }
   }

   #  Clear another toolbox making it definitely not our slave.
   public method clear_slave {w} {
      if { [info exists slaves_] && [info exists slaves_($w)] } {
         set slaves_($w) 0
      }
   }

   #  Called when the status of a slave toolbox is changed in the menu.
   #  Send message to the selected toolbox that we cannot be a slave
   #  for it, if selected as our slave.
   protected method slave_changed_ {w} {
      if { $slaves_($w) } {
         if { [winfo exists $w] } {
            $w clear_slave $w_
         }
      }
   }

   #  ===========================
   #  Coordinate domain selection
   #  ===========================
   protected method show_builtin_toolbox_ {} {
      if { $cubeaccessor_ != {} } {
         utilReUseWidget GaiaCubeAstDomain $w_.domainchooser \
            -gaiacube $this \
            -notify_cmd [code $this coords_changed_]
      }
   }

   #  ================
   #  CUPID catalogues
   #  ================

   #  Import a CUPID catalogue.
   public method import_cupid_cat {cat} {
      make_cupid_importer
      $w_.cupidimporter open $cat
   }

   #  Create the CUPID catalogue importer. May import more than one, but only
   #  one importer for each instance of this. Any 3D uses query the importer
   #  for catalogues.
   public method make_cupid_importer {} {
      utilReUseWidget GaiaCupidImporter $w_.cupidimporter -gaiacube $this
      renderers_set_cupid_importer_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the input cube. NDF or FITS specification.
   itk_option define -cube cube Cube {} {
      set_chosen_cube_
   }

   #  Name of the Gaia instance we're controlling.
   itk_option define -gaia gaia Gaia {} {
      set check_for_cubes_ [$itk_option(-gaia) cget -check_for_cubes]
      set rtdctrl_ [$itk_option(-gaia) get_image]
      set rtdctrl_info_ [$rtdctrl_ component info]
   }

   #  The canvas.
   itk_option define -canvas canvas Canvas {}

   #  The rtdimage instance.
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Filters for selecting files.
   itk_option define -filter_types filter_types Filter_types {}

   #  Whether to autoscale the cuts as every image slice is displayed.
   itk_option define -autocut autocut AutoCut 0

   #  Whether to use file mapping (quick startup), or direct io (fast
   #  spectral display).
   itk_option define -usemmap usemmap UseMmap 1

   #  Whether spectral plot window should be a transient, or not.
   itk_option define -transient_spectralplot transient_spectralplot \
      Transient_Spectralplot 1

   #  Whether to show a label in the main window.
   itk_option define -show_coord_label show_coord_label Show_Coord_Label 0

   #  Colour of coordinate label in main window.
   itk_option define -labelcolor labelcolor LabelColor blue {
      toggle_show_coord_label_
   }

   #  Font of the coordinate label in the main window.
   itk_option define -labelfont labelfont LabelFont TkDefaultFont {
      toggle_show_coord_label_
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Data access object for the cube.
   protected variable cubeaccessor_ {}

   #  The component to display/extract. Usually DATA but could be VARIANCE,
   #  ERROR or QUALITY. Keep last component value as a fallback.
   protected variable component_ "DATA"
   protected variable last_component_ "DATA"

   #  The bounds of the cube, 3 pairs of upper and lower values.
   protected variable bounds_ {}

   #  The name of the dataset, as an NDF specification (handle HDU and slices).
   protected variable ndfname_ {}

   #  The filetype of the dataset, will be ".sdf" for NDFs.
   protected variable type_ {}

   #  The current plane along the current axis.
   protected variable plane_ 1

   #  Maximum and minimum possible value for plane.
   protected variable plane_max_ 0
   protected variable plane_min_ 0

   #  The current axis.
   protected variable axis_ 1

   #  Check for cubes setting of GAIA.
   protected variable check_for_cubes_ 1

   #  The name for the dummy NDF, with updatable image section.
   protected variable section_name_ ""

   #  Memory used for last slice. Free this when not needed.
   protected variable last_slice_adr_ 0

   #  The terminator characters for closing a section. May specify
   #  a final redundant axis.
   protected variable close_section_ ")"

   #  The object value set in the cube (used in slice object).
   protected variable object_ {}

   #  The object value set in the main display. Used to check we still
   #  have control of the dummy slice.
   protected variable fullobject_ {}

   #  GaiaImageCtrl object used in the main window.
   protected variable rtdctrl_ {}

   #  GaiaImagePanel object used in the main window. Forced to update to
   #  reveal the slice information.
   protected variable rtdctrl_info_ {}

   #  Object for controlling the spectral coordinates.
   protected variable spec_coords_ {}

   #  Object for controlling the spectral standard of rest.
   protected variable spec_sor_ {}

   #  Array of index-based controls for the reference lines. These are indexed
   #  by their ref_id.
   protected variable ref_line_controls_

   #  Array of index-based controls for the reference ranges. These are indexed
   #  by their ref_id.
   protected variable ref_range_controls_

   #  The ref_id value of the baseline controls. Must be last.
   protected variable baseline_id_ ""

   #  Data associated with the state of all the limits. Used to restore
   #  between cubes with different sizes.
   protected variable limits_

   #  Name of GaiaFITSHeader instance, only set when active.
   protected variable fitsheaders_ {}

   #  Number for creating toolbox clones.
   protected variable tool_clones_ 0

   #  Collection of managed temporary files.
   protected variable tmpfiles_ {}

   #  Object controlling the Go menu and cube history items.
   protected variable history_ {}

   #  The slave toolboxes.
   protected variable slaves_

   #  HDU selection toolbox.
   protected variable hdu_list_ {}

   #  Name handling object.
   protected variable namer_ {}

   #  Tag for the coordinate label.
   protected variable coord_label_tag_ ""

   #  Reference positions for the coord label in main window.
   protected variable xref_ {}
   protected variable yref_ {}

   #  Possible colours.
   protected variable colours_ {
      white
      grey90 grey80 grey70 grey60 grey50 grey40 grey30 grey20 grey10
      black
      red green blue cyan magenta yellow
   }

   #  Possible fonts for drawing label.
   protected variable fonts_ {
      -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-120-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*
      -adobe-courier-medium-r-*-*-*-140-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-140-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-140-*-*-*-*-*-*
      -adobe-courier-medium-r-*-*-*-180-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-180-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-180-*-*-*-*-*-*
      -adobe-courier-medium-r-*-*-*-240-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-240-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-240-*-*-*-*-*-*
   }

   #  Menu used for NDF component selection.
   protected variable component_menu_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The temporary image count.
   protected common count_ 0

   #  Is GAIA3D available, -1 means not checked.
   protected common have_gaia3d_ -1

#  End of class definition.
}
