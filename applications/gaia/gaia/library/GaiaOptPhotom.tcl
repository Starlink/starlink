#+
#  Name:
#     GaiaOptPhotom

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Creates an instance of a toolbox for controlling optimal
#     photometry.

#  Description:
#     This routine defines a class of object that creates a toolbox
#     for performing optimal photometry.
#
#     Many objects can be identified at once and all are remeasured
#     together (say after a change in frame zero point). The
#     information about the objects can be saved (as can the results)
#     and redisplayed over another image. The sky estimate for an
#     object can be made either from an annular region or from a set
#     of other apertures association with an object (each object
#     is associated with its own sky regions).
#
#     The photometry is actually done by the PHOTOM application
#     AUTOPHOTOM and the output results from an analysis can be used
#     to rerun the same measurements on other frames. Full control
#     over the parameters of this application is also available.
#
#     There are two fundamentally different modes used in this
#     routine, the first use magnitudes for the output results
#     and the second straight counts (or flux). To create an instance
#     of this class you must supply a boolean value as a second
#     argument. This is 1 (true) for magnitudes and 0 (false) for
#     counts.

#  Invocations:
#
#        GaiaOptPhotom object_name magnitudes [configuration options]
#
#     This creates an instance of a GaiaOptPhotom object. The return is
#     the name of the object. The magnitudes flag is a boolean which
#     sets the interface for calculating magnitudes or counts.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.
#
#     The following configuration options are mandatory when
#     creating an object instance.
#
#        -canvasdraw -image -canvas -rtdimage

#  Configuration options:
#
#        -canvasdraw
#
#     Sets the name of the StarCanvasDraw object used to control the
#     graphics content.
#
#        -image
#
#     Sets the name of the GaiaImageCtrl used.
#
#        -canvas
#
#     Sets the name of the canvas used to display the image and graphics.
#
#        -rtdimage
#
#     Sets the name of the rtdimage object used to display the image.
#
#        -annulus boolean
#
#     Controls if the sky regions are determined from annuli about the
#     objects.
#
#        -psfannulus boolean
#
#     Controls if the sky regions are determined from annuli about the
#     psf object.
#
#        -linewidth integer
#
#     Sets the width of any lines drawn.

#  Methods:
#     public:
#        close
#           Closes the toolbox, checking if the measurements have been
#           saved first.
#        define_object
#           Creates a marker on the canvas when mouse button 1 is
#           pressed over the canvas.
#        define_sky
#           Creates an aperture on the canvas when mouse button 1 is
#           pressed over the canvas. This method can only be used when
#           annular sky regions are not in use.
#        measure_objects
#           Performs the photometry of the objects defined on the
#           canvas. This is activated by a button, or by changing the
#           frame zero point and pressing return.
#        read_file [filename] [update]
#           Reads a set of photometry measurements back from a
#           file. The filename is not given it is chosen from an
#           existing list using a selection dialog. Any existing
#           objects are updated using the new values if update
#           is true.
#        save_objects [filename]
#           Saves the measurments into a file. If not given the file
#           name is given using a selection dialog.
#        view
#           Invoked when a view of all the measurements is required.
#
#     private:
#        created_object
#           Invoked after an object is selected.
#        measured_objects
#           Invoked after the objects have been measured.
#        sky_method_changed
#           Invoked when the sky region definition method has changed

#  Inheritance:
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 1999-2005 Central Laboratory of the Research Councils.
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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     27-MA7-1999 (PWD):
#        Original version.
#     24-MAY-2000 (PWD):
#        Added changes so that user can disable exit confirmation.
#     02-MAY-2003 (PWD):
#        Added option to make measurements immediately.
#     26-APR-2006 (PWD):
#        Added -image option to support volatile cube slices.
#     {enter_further_changes_here}
#-

#.

itk::usual GaiaOptPhotom {}

itcl::class gaia::GaiaOptPhotom {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {mags args} {
      global ::tcl_version

      #  Evaluate any options.
      eval itk_initialize $args

      #  Add short_help window.
      make_short_help

      #  Record the choice of magnitudes or counts (default is true)
      #  and set the top-level window description.
      if { ! $mags } {
         set usemags_ 0
         wm title $w_ \
            "GAIA: Optimal photometry -- counts ($itk_option(-number))"
      } else {
         set usemags_ 1
         wm title $w_ \
            "GAIA: Optimal photometry -- magnitudes ($itk_option(-number))"
      }

      #  Add tab window for revealing the object details,
      #  measurement parameters and possibly all results.
      itk_component add TabNoteBook {
	  iwidgets::tabnotebook $w_.tab \
	      -angle 0 -tabpos n -width 350 -height 450
      }

      #  Add pane for current object details.
      $itk_component(TabNoteBook) add -label Object
      set child_(details) [$itk_component(TabNoteBook) childsite 0]

      #  Add pane for defining the "PSF" object.
      $itk_component(TabNoteBook) add -label PSF
      set child_(psf) [$itk_component(TabNoteBook) childsite 1]

      #  Add pane for measurement parameters.
      $itk_component(TabNoteBook) add -label Parameters
      set child_(params) [$itk_component(TabNoteBook) childsite 2]

      #  Add pane for viewing all measurements together (slower).
      $itk_component(TabNoteBook) add -label Results
      set child_(results) [$itk_component(TabNoteBook) childsite 3]

      #  Add controls for viewing all measurements (do this now to get
      #  name).
      set view_($this) 0
      itk_component add ViewAll {
         checkbutton $child_(results).viewall  \
            -text {View all measurements:} \
            -variable [scope view_($this)] \
            -onvalue 1 \
            -offvalue 0 \
            -command [code $this view]
      }
      pack $itk_component(ViewAll) -side top -fill x
      add_short_help $itk_component(ViewAll) \
         {Display all measurements in scrollable window (slow)}

      #  Create a GaiaPhotomList object to deal with the PSF
      #  object details (actually there is only one of these).
      set psf_list_ [GaiaPhotomList \#auto \
                        -show_list 0 \
                        -details $child_(psf).psfdetails \
                        -canvasdraw $itk_option(-canvasdraw) \
                        -canvas $itk_option(-canvas) \
                        -rtdimage $itk_option(-rtdimage) \
                        -annulus $itk_option(-psfannulus) \
                        -linewidth $itk_option(-linewidth) \
                        -notify_created_cmd [code $this created_psf]\
                        -usemags $usemags_ \
                        -phottype optimal \
                        -psf 1 \
                        -semimajor 5.0 \
                        -allow_resize 0 \
			-coupled 1 \
                        -notify_changed_cmd [code $this changed_psf]]

      #  Create the GaiaPhotomList object to deal with the details of
      #  the objects that are being measured.
      set object_list_ [GaiaPhotomList \#auto \
                           -scrollbox $child_(results).box \
                           -details $child_(details).details \
                           -canvasdraw $itk_option(-canvasdraw) \
                           -canvas $itk_option(-canvas) \
                           -rtdimage $itk_option(-rtdimage) \
                           -annulus $itk_option(-annulus) \
                           -linewidth $itk_option(-linewidth) \
                           -notify_created_cmd [code $this created_object] \
                           -usemags $usemags_ \
                           -phottype optimal \
                           -psf 0 \
                           -allow_resize 0 \
			   -coupled 1 \
			   -notify_changed_cmd [code $this changed_object]]

      #  Create a GaiaPhotomDetails object to display the values
      #  of the PSF object.
      itk_component add PSFDetails {
         GaiaPhotomDetails $child_(psf).psfdetails \
            -positions_cmd [code $this sky_method_changed psf] \
            -usemags $usemags_ \
            -object_list [code $psf_list_]
      }
      add_short_help $itk_component(PSFDetails) {Details of the PSF object}

      #  Create a GaiaPhotomDetails object to display the values
      #  of the selected object.
      itk_component add ObjectDetails {
         GaiaPhotomDetails $child_(details).details \
            -positions_cmd [code $this sky_method_changed object] \
            -usemags $usemags_ \
            -object_list [code $object_list_]
      }
      add_short_help $itk_component(ObjectDetails) \
         {Details of the currently selected object}

      #  Create a GaiaPhotomExtras object to deal with any additional
      #  parameters for AUTOPHOTOM.
      itk_component add Extras {
         GaiaPhotomExtras $child_(params).extras -phottype optimal
      }

      #  Add an options menu for setting options that should probably
      #  be defined once only per-session, or infrequently.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0
      set Colours [add_menubutton "Colours"]
      configure_menubutton Colours -underline 0

      #  Add window help.
      add_help_button optimal "On Window..."

      #  Add the option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      add_menu_short_help $File {New window} {Create a new toolbox}

      #  Save measurements to a file.
      $File add command \
         -label {Save measurements...} \
         -command [code $this save_objects] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this save_objects]
      add_menu_short_help $File {Save measurements...} \
         {Save current measurements to a selected file}

      #  Read measurements from a file.
      $File add command \
         -label {Read measurements...} \
         -command [code $this read_file] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_file]
      add_menu_short_help $File {Read measurements...} \
         {Read existing measurements from a selected file}

      #  Set the exit menu items.
      $File add command -label Exit -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]
      add_menu_short_help $File {Exit} {Close this window}

      #  Determine how sky positions will be indicated.
      set skymethod_($this) 1
      $Options add checkbutton \
         -label {Use annular sky regions} \
         -variable [scope skymethod_($this)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this sky_method_changed object]
      add_menu_short_help $Options {Use annular sky regions}  \
         {Toggle to define sky in detached apertures for objects}

      set psfskymethod_($this) 1
      $Options add checkbutton \
         -label {Use PSF annular sky regions} \
         -variable [scope psfskymethod_($this)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this sky_method_changed psf]
      add_menu_short_help $Options {Use annular sky regions}  \
         {Toggle to define sky in detached apertures for PSF star}

      #  Whether to measure aperture immediately.
      $Options add checkbutton \
         -label {Measure immediately} \
         -variable [scope auto_measure_] \
         -onvalue 1 \
         -offvalue 0
      add_menu_short_help $Options {Measure immediately} \
         {Perform measurements immediately after aperture creation}

      #  Control of various colours.
      make_colours_menu_ $Colours

      #  Sky zero point and frame exposure time.
      if { $usemags_ } {
         itk_component add Skymag {
            util::LabelEntry $w_.skymag \
               -text {Frame zero point (mags) :} \
               -value $skymag_ \
               -labelwidth 25 \
               -command [code $this sky_zero_changed]
         }
         add_short_help $itk_component(Skymag) \
            {Magnitude assigned to sky level}
      }

      #  Name of the results file.
      itk_component add Results {
         LabelFileChooser $w_.results \
            -labelwidth 8 \
            -text "Results:" \
	    -value "GaiaPhotomLog.Dat"
      }
      add_short_help $itk_component(Results) \
         {Name of file that "Save" and "Append" buttons use}

      #  Create a button bar with options for defining either an
      #  object or sky regions, not for PSF so keep on object pane.
      itk_component add Define {
         frame $child_(details).define
      }

      #  Define an object position.
      itk_component add DefineObject {
         button $itk_component(Define).object \
            -text {Select object} \
            -width 20 \
            -highlightthickness 3 \
            -command [code $this define_object]
      }
      $itk_component(DefineObject) configure -highlightbackground black
      add_short_help $itk_component(DefineObject) \
      {Select object to measure; press button-1 over image}

      #  Define sky regions (only used if skymethod is set to regions).
      itk_component add DefineSky  {
         button $itk_component(Define).sky \
            -text {Define sky aperture} \
            -width 20 \
            -command [code $this define_sky] \
            -state disabled
      }
      add_short_help $itk_component(DefineSky) \
         {Press and then drag out aperture on image}
      toggle_sky_button_

      #  Copy selected objects, again not for PSF.
      itk_component add Copy {
         button $itk_component(Define).copy \
            -text {Copy object} \
            -width 20 \
            -command [code $object_list_ copy]
      }
      add_short_help $itk_component(Copy) \
         {Create copy of current/last object details}

      #  Measure all objects and close window.
      itk_component add MeasureFrame {frame $w_.meas}
      itk_component add Measure {
         button $itk_component(MeasureFrame).measure \
            -text {Calculate results} \
            -width 20 \
            -command [code $this measure_objects]
      }
      add_short_help $itk_component(Measure) \
         {Calculate photometry of all objects}

      #  Close window button.
      itk_component add Close {
         button $itk_component(MeasureFrame).close \
            -text {Close}  \
            -width 20 \
            -command [code $this close]
      }
      add_short_help $itk_component(Close) {Close this window}

      #  Save measurements to the results file.
      itk_component add SaveFrame {frame $w_.save}
      itk_component add Save {
	  button $itk_component(SaveFrame).save \
	      -text {Save} \
	      -width 20 \
	      -command [code $this save_results]
      }
      add_short_help $itk_component(Save) \
         {Save current measurements to results file}

      #  Append measurements to the results files.
      itk_component add Append {
	  button $itk_component(SaveFrame).append \
	      -text {Append} \
	      -width 20 \
	      -command [code $this append_results]
      }
      add_short_help $itk_component(Append) \
         {Append current measurements to results file}

      #  Create a button bar with options for defining either a PSF
      #  object.
      itk_component add PSFdef {
         frame $child_(psf).define
      }

      #  Define an object position.
      itk_component add DefinePSF {
         button $itk_component(PSFdef).object \
            -text {Select PSF object} \
            -width 20 \
            -highlightthickness 3 \
            -command [code $this define_psf]
      }
      $itk_component(DefinePSF) configure -highlightbackground black
      add_short_help $itk_component(DefinePSF) \
         {Select object for PSF determination}

      #  Define sky regions (only used if skymethod is set to regions).
      itk_component add DefinePSFSky  {
         button $itk_component(PSFdef).sky \
            -text {Define sky aperture} \
            -width 20 \
            -command [code $this define_psf_sky] \
            -state disabled
      }
      add_short_help $itk_component(DefinePSFSky) \
         {Press and then drag out aperture on image}
      toggle_psf_sky_button_

      #  Pack up window.
      if { $usemags_ } {
         pack $itk_component(Skymag) -side top -fill x -pady 2
      }
      pack $itk_component(MeasureFrame) -side bottom -fill x
      pack $itk_component(Close) -side right -pady 2 -padx 2 -expand true
      pack $itk_component(Measure) -side left -pady 2 -padx 2 -expand true
      pack $itk_component(Results) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(TabNoteBook) -fill both -expand 1
      pack $itk_component(ObjectDetails) -fill x  -pady 2 -padx 2
      pack $itk_component(PSFDetails) -fill x -pady 2 -padx 2
      pack $itk_component(Extras) -fill x -pady 2 -padx 2

      pack $itk_component(Define) -side top -fill x  -pady 2 -padx 2
      pack $itk_component(Copy) -side bottom -pady 2 -padx 2 -expand true
      pack $itk_component(DefineObject) -side left -expand true -pady 2 -padx 2
      pack $itk_component(DefineSky) -side right -expand true -pady 2 -padx 2

      pack $itk_component(PSFdef) -side top -fill x  -pady 2 -padx 2
      pack $itk_component(DefinePSF) -side left -expand true -pady 2 -padx 2
      pack $itk_component(DefinePSFSky) -side right -expand true -pady 2 -padx 2

      pack $itk_component(SaveFrame) -side top -fill x
      pack $itk_component(Save) -side right -pady 2 -padx 2 -expand true
      pack $itk_component(Append) -side right -pady 2 -padx 2 -expand true

      #  Reveal first page of controls.
      $itk_component(TabNoteBook) select 0

      #  Create image name control object.
      set namer_ [GaiaImageName \#auto]
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $object_list_ != {} } {
         delete object $object_list_
      }
      if { $psf_list_ != {} } {
         delete object $psf_list_
      }
      if { $autophotom_ != {} } {
         catch {$autophotom_ delete_sometime}
         set autophotom_ {}
      }
      if { $namer_ != {} } {
	 catch {object delete $namer_}
      }
   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Define an object. This uses the current aperture size, sky
   #  method etc.
   method define_object {} {
      $itk_component(DefineObject) configure -state disabled
      $object_list_ create_object
   }

   #  Define an PSF object. This uses the current aperture size, sky
   #  method etc.
   method define_psf {} {
      $itk_component(DefinePSF) configure -state disabled
      $psf_list_ create_object
   }

   #  Define a region of sky.
   method define_sky {} {
      $itk_component(DefineSky) configure -state disabled
      $object_list_ configure -allow_resize 1
      $object_list_ create_sky_region
      $object_list_ configure -allow_resize 0
   }

   #  Define a region of sky for PSF object.
   method define_psf_sky {} {
      $itk_component(DefinePSFSky) configure -state disabled
      $psf_list_ configure -allow_resize 1
      $psf_list_ create_sky_region
      $psf_list_ configure -allow_resize 0
   }

   #  Close window checking first if measurements have been
   #  saved.
   method close {} {

      #  Check if any new measurements have been made, if so then
      #  offer not to quit.
      if { [$object_list_ cget -modified] } {
         if { ! $itk_option(-quiet_exit) } {
            OptionDialog $w_.dialog \
               -title {Unsaved apertures} \
               -text {There are unsaved apertures, are you sure you want to quit?} \
               -buttons [list Yes No] \
               -option_text {Do not ask this question again} \
               -option_state 0 \
               -option_cmd {::option add *GaiaOptPhotom.quiet_exit}
            set answer [$w_.dialog activate]
         } else {
            set answer 0
         }
         if { ! $answer } {
            delete object $this
         }
      } else {
         delete object $this
      }
   }

   #  Read and display positions from a PHOTOM file.
   method read_file {{filename ""} {update 0}} {
      if { $filename == "" } {
	 set w [util::FileSelect .\#auto -title "Choose PHOTOM file"]
	 if {[$w activate]} {
            set filename [$w get]
	    $psf_list_ read_file $filename $update
            $object_list_ configure -semimajor [$psf_list_ cget -semimajor]
	    $object_list_ read_file $filename $update
	 }
	 destroy $w
      } else {
	 $psf_list_ read_file $filename $update
         $object_list_ configure -semimajor [$psf_list_ cget -semimajor]
	 $object_list_ read_file $filename $update
      }
   }

   #  Measure the current objects. Note we need the PSF object first.
   method measure_objects { {silent 0} } {
      if { [$psf_list_ write_file "GaiaPhotomIn.Dat"] } {
         if { [$object_list_ append_file "" "GaiaPhotomIn.Dat"] } {

            if { $autophotom_ == {} } {
               #  Start autophotom application.
               global env
               set autophotom_ [GaiaApp \#auto -application \
                                   $env(PHOTOM_DIR)/autophotom \
                                   -notify [code $this measured_objects]]
            }
            if { $usemags_ } {
               set skymag_ [$itk_component(Skymag) get]
            } else {
               set skymag_ 0
            }
            #set image [$itk_option(-rtdimage) cget -file]
            set image [$itk_option(-rtdimage) fullname]
            if { $image != "" } {
	       $namer_ configure -imagename $image
	       set image [$namer_ ndfname]

               #  Get any additional values from itk_component(Extras)
               if { [winfo exists $itk_component(Extras)] } {
                  set more [$itk_component(Extras) getstate]
               } else {
                  set more ""
               }

               #  Make sure that the disk image is up to date. Only relevant
               #  for volatile images (from cubes).
               $itk_option(-image) save_if_volatile

               #  And run the command.
               if { $usemags_ } {
                  set ok "true"
               } else {
                  set ok "false"
               }
               blt::busy hold $w_
               update idletasks
               catch {file delete GaiaPhotomOut.Dat}
               $autophotom_ runwiths \
                  "in=$image \
                   infile=GaiaPhotomIn.Dat \
                   outfile=GaiaPhotomOut.Dat \
                   optima=true \
                   skymag=$skymag_ \
                   usemags=$ok \
                  $more"
            } else {
               if { ! $silent } {
                  error_dialog "No image is displayed"
               }
            }
         } else {
            if { ! $silent } {
               error_dialog "You need to define some object positions"
            }
         }
      } else {
         if { ! $silent } {
            error_dialog "You need to define a PSF object"
         }
      }
   }

   #  Save the measurements to the results file.
   method save_results {} {
       set name [$itk_component(Results) get]
       save_objects $name
   }

   #  Append the measurements to the results file.
   method append_results {} {
       set name [$itk_component(Results) get]
       append_objects $name
   }

   #  Save the measurements to a file.
   method save_objects {{filename ""}} {
      if { $filename == "" } {
	 set w [util::FileSelect .\#auto -title "Write PHOTOM file"]
	 if {[$w activate]} {
            set filename [$w get]
	    $psf_list_ write_file $filename
	    $object_list_ append_file "" $filename
	 }
	 destroy $w
      } else {
         $psf_list_ write_file $filename
	 $object_list_ append_file "" $filename
      }
   }

   #  Append the measurements to a file. Adds a comment containing the
   #  image name.
   method append_objects {{filename ""}} {
     set comment "[$itk_option(-rtdimage) fullname]"
     if { $filename == "" } {
	 set w [util::FileSelect .\#auto -title "Write PHOTOM file"]
	 if {[$w activate]} {
	    $psf_list_ append_file $comment [$w get]
	    $object_list_ append_file "" [$w get]
	 }
	 destroy $w
      } else {
         $psf_list_ append_file $comment $filename
	 $object_list_ append_file "" $filename
      }
   }

   #  Notification that the define_object method has been
   #  completed. This allows us to set the state of the sky region
   #  button for plain objects.
   private method created_object {} {
      toggle_sky_button_
      $itk_component(DefineObject) configure -state normal
      if { $auto_measure_ } {
         measure_objects 1
      }
   }

   #  Toggle the object define sky button to reflect the current state.
   private method toggle_sky_button_ {} {
      if { $skymethod_($this) } {
         $itk_component(DefineSky) configure -state disabled
      } else {
         $itk_component(DefineSky) configure -state normal
      }
   }

   #  Notification that the define_object method has been
   #  completed for a PSF object.
   private method created_psf {} {
      toggle_psf_sky_button_
      $itk_component(DefinePSF) configure -state normal
      if { $auto_measure_ } {
         measure_objects 1
      }
   }

   #  Toggle the PSF define sky button to reflect the current state.
   private method toggle_psf_sky_button_ {} {
      if { $psfskymethod_($this) } {
         $itk_component(DefinePSFSky) configure -state disabled
      } else {
         $itk_component(DefinePSFSky) configure -state normal
      }
   }

   #  Notification that the PSF or normal objects have changed shape
   #  (we just allow the radius to be modified). Objects in other
   #  list need to reflect this change.
   private method changed_psf {} {
      if { $propagate_ } {
	 set propagate_ 0
	 $object_list_ configure -semimajor [$psf_list_ cget -semimajor]
	 $object_list_ config_all major [$psf_list_ cget -semimajor]
	 set propagate_ 1
      }
   }
   private method changed_object {} {
      if { $propagate_ } {
	 set propagate_ 0
	 $psf_list_ configure -semimajor [$object_list_ cget -semimajor]
	 $psf_list_ config_all major [$object_list_ cget -semimajor]
	 set propagate_ 1
      }
   }

   #  The measurements are made, read in the results.
   private method measured_objects {} {
      blt::busy release $w_
      if { [file exists GaiaPhotomOut.Dat] } {
         $psf_list_ read_file GaiaPhotomOut.Dat 1
         $object_list_ configure -semimajor [$psf_list_ cget -semimajor]
         $object_list_ read_file GaiaPhotomOut.Dat 1
      }
   }

   #  Sky method changed so reconfigure. If optional args is set then
   #  this is not called from menu button and args is the sky method
   #  string to which the button should be set. Note that when
   #  skymethod is not annulus the button state isn't changed until a
   #  new object is created (see created_object method).
   private method sky_method_changed {type args} {
      if { $type == "object" } {
         if { $args != {} } {
            if { [lindex $args 0] == "annulus" } {
               set skymethod_($this) 1
            } else {
               set skymethod_($this) 0
            }
         } else {
            if { $skymethod_($this) } {
               configure -annulus 1
            } else {
               configure -annulus 0
            }
         }
         toggle_sky_button_
      } else {
         if { $args != {} } {
            if { [lindex $args 0] == "annulus" } {
               set psfskymethod_($this) 1
            } else {
               set psfskymethod_($this) 0
            }
         } else {
            if { $psfskymethod_($this) } {
               configure -psfannulus 1
            } else {
               configure -psfannulus 0
            }
         }
         toggle_psf_sky_button_
      }
   }

   #  View all measurements in new window (controlled by
   #  GaiaPhotomList).
   method view {{value ""}} {
      if { $value != "" } {
	 set view_($this) $value
      }
      $object_list_ configure -show_list $view_($this)
   }

   #  Sky zero point may have changed, remeasure objects if so.
   private method sky_zero_changed {value} {
      if { $value != $skymag_ } {
         measure_objects
      }
   }

   #  Add a menu for controlling the aperture colours.
   protected method make_colours_menu_ {m} {

      #  Object & PSF colours.
      $m add cascade -label Objects -menu [menu $m.objcol]
      $m add cascade -label PSF -menu [menu $m.psfcol]
      set objmen $m.objcol
      set psfmen $m.psfcol

      set psf {}
      foreach men "$m.objcol $m.psfcol" {

         #  Add the menus
         foreach {label name} {{Selected colour}   selcol
            {Deselected colour} descol
            {Selected sky colour} selskycol
            {Deselected sky colour} desskycol} {
            $men add cascade -label $label -menu [menu $men.$name]
         }

         foreach i $itk_option(-colors) {
            $men.selcol add radiobutton \
               -value $i \
               -background $i \
               -command [code $this configure -${psf}selected_colour $i] \
               -variable [scope itk_option(-${psf}selected_colour)]
         }
         configure -${psf}selected_colour $itk_option(-${psf}selected_colour)
         foreach i $itk_option(-colors) {
            $men.descol add radiobutton \
               -value $i \
               -background $i \
               -command [code $this configure -${psf}deselected_colour $i] \
               -variable [scope itk_option(-${psf}deselected_colour)]
         }
         configure -${psf}deselected_colour $itk_option(-${psf}deselected_colour)
         foreach i $itk_option(-colors) {
            $men.selskycol add radiobutton \
               -value $i \
               -background $i \
               -command [code $this configure -${psf}selected_sky_colour $i] \
               -variable [scope itk_option(-${psf}selected_sky_colour)]
         }
         configure -${psf}selected_sky_colour $itk_option(-${psf}selected_sky_colour)
         foreach i $itk_option(-colors) {
            $men.desskycol add radiobutton \
               -value $i \
               -background $i \
               -command [code $this configure -${psf}deselected_sky_colour $i] \
               -variable [scope itk_option(-${psf}deselected_sky_colour)]
         }
         configure -${psf}deselected_sky_colour $itk_option(-${psf}deselected_sky_colour)
         set psf psf_
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of a StarCanvasDraw widget to use to control objects.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {
      if { $object_list_ != {} } {
         $object_list_ configure -canvasdraw $itk_option(-canvasdraw)
         $psf_list_ configure -canvasdraw $itk_option(-canvasdraw)
      }
   }

   #  Name of GaiaImageCtrl
   itk_option define -image image Image {}

   #  Name of canvas.
   itk_option define -canvas canvas Canvas {} {
      if { $object_list_ != {} } {
         $object_list_ configure -canvas $itk_option(-canvas)
         $psf_list_ configure -canvas $itk_option(-canvas)
      }
   }

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {
      if { $object_list_ != {} } {
         $object_list_ configure -rtdimage $itk_option(-rtdimage)
         $psf_list_ configure -rtdimage $itk_option(-rtdimage)
      }
   }

   #  Whether annulus region is in use.
   itk_option define -annulus annulus Annulus 1 {
      if { $object_list_ != {} } {
         $object_list_ configure -annulus $itk_option(-annulus)
      }
   }
   itk_option define -psfannulus psfannulus PsfAnnulus 1 {
      if { $psf_list_ != {} } {
         $psf_list_ configure -annulus $itk_option(-psfannulus)
      }
   }

   #  Width of lines.
   itk_option define -linewidth linewidth LineWidth 1 {
      if { $object_list_ != {} } {
         $object_list_ configure -linewidth $itk_option(-linewidth)
         $psf_list_ configure -linewidth $itk_option(-linewidth)
      }
   }

   #  Colours.
   itk_option define -selected_colour selected_colour Selected_colour {white} {
      if { $object_list_ != {} } {
         $object_list_ configure -selected_colour $itk_option(-selected_colour)
      }
   }
   itk_option define -deselected_colour deselected_colour Deselected_colour {green} {
      if { $object_list_ != {} } {
         $object_list_ configure -deselected_colour $itk_option(-deselected_colour)
      }
   }
   itk_option define -selected_sky_colour selected_sky_colour Selected_skycolour {yellow} {
      if { $object_list_ != {} } {
         $object_list_ configure -selected_sky_colour $itk_option(-selected_sky_colour)
      }
   }
   itk_option define -deselected_sky_colour deselected_sky_colour Deselected_skycolour {blue} {
      if { $object_list_ != {} } {
         $object_list_ configure -deselected_sky_colour $itk_option(-deselected_sky_colour)
      }
   }

   itk_option define -psf_selected_colour psf_selected_colour Psf_selected_colour {magenta} {
      if { $psf_list_ != {} } {
         $psf_list_ configure -selected_colour $itk_option(-psf_selected_colour)
      }
   }
   itk_option define -psf_deselected_colour psf_deselected_colour Psf_deselected_colour {magenta} {
      if { $psf_list_ != {} } {
         $psf_list_ configure -deselected_colour $itk_option(-psf_deselected_colour)
      }
   }
   itk_option define -psf_selected_sky_colour psf_selected_sky_colour Psf_selected_skycolour {red} {
      if { $psf_list_ != {} } {
         $psf_list_ configure -selected_sky_colour $itk_option(-psf_selected_sky_colour)
      }
   }
   itk_option define -psf_deselected_sky_colour psf_deselected_sky_colour Psf_deselected_skycolour {red} {
      if { $psf_list_ != {} } {
         $psf_list_ configure -deselected_sky_colour $itk_option(-psf_deselected_sky_colour)
      }
   }

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  Possible colours.
   itk_option define -colors colors Colors {
       red green blue cyan magenta yellow white grey90 grey40 grey10 black
   }

   #  Quietly exit without asking about saving results.
   itk_option define -quiet_exit quiet_exit Quiet_exit 0

   #  Protected variables: (only available to instance)
   #  --------------------

   #  Whether to work in magnitudes or counts.
   protected variable usemags_ 1

   #  Object controlling list of PSF object properties.
   protected variable psf_list_ {}

   #  Object controlling list of known photometry objects.
   protected variable object_list_ {}

   #  Name of autophotom application.
   protected variable autophotom_ {}

   #  Value of skymag when last known.
   protected variable skymag_ 50

   #  Whether to allow propagation. Stop when setting values (in
   #  GaiaPhotomLists) that may trigger a recall.
   protected variable propagate_ 1

   #  Object to deal with image names.
   protected variable namer_ {}

   #  Whether apertures are automatically measured when created, or not.
   protected variable auto_measure_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Methods of estimating sky (these are global arrays visible
   #  in this namespace only and indexed by $this to resolve between
   #  different instances).
   common skymethod_
   common psfskymethod_

   #  Whether to view all measurements or not.
   common view_

#  End of class definition.
}
