#+
#  Name:
#     GaiaApPhotom

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Creates an instance of a toolbox for controlling aperture
#     photometry.

#  Description:
#     This routine defines a class of object that creates a toolbox
#     for performing aperture photometry. The apertures can be placed
#     and resized interactively and can be elliptical or circular.
#
#     Many apertures can be placed at once and all are remeasured
#     together (say after a change in frame zero point). The
#     information about the apertures can be saved (as can the
#     results) and redisplayed over another image. The sky estimate
#     for an aperture can be made either from an annular region or from a
#     set of other apertures association with an aperture (each
#     aperture maintains its own sky regions).
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
#        GaiaApPhotom object_name magnitudes [configuration options]
#
#     This creates an instance of a GaiaApPhotom object. The return is
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
#        -canvas
#
#     Sets the name of the canvas used to display the image and graphics.
#
#        -image
#
#     Sets the name of the GaiaImageCtrl object used to display the
#     image.
#
#        -rtdimage
#
#     Sets the name of the rtdimage object used to display the
#     image.
#
#        -annulus (boolean)
#
#     Controls if the sky regions are determined from annuli about the
#     apertures.
#
#        -shape (circle|ellipse)
#
#     Controls the shape of the apertures when drawn.
#
#        -linewidth (integer)
#
#     Sets the width of any lines drawn.
#
#        -coupled (0|1)
#
#     Whether apertures are drawn at same size and keep so.

#  Methods:
#     public:
#        close
#           Closes the toolbox, checking if the measurements have been
#           saved first.
#        define_object
#           Creates an aperture on the canvas when mouse button 1 is
#           pressed over the canvas.
#        define_sky
#           Creates an aperture on the canvas when mouse button 1 is
#           pressed over the canvas. This define a sky region, rather
#           than an object aperture. It can only be used when annular
#           sky regions are not in use.
#        measure_objects
#           Performs the photometry of the apertures defined on the
#           canvas. This is activated by a button, or by changing the
#           frame zero point and pressing return.
#        read_file [filename] [update]
#           Reads a set of photometry apertures and measurments back
#           from a file. The filename is not given it is chosen from
#           an existing list using a selection dialog. Any existing
#           apertures are updated using the new values if update
#           is true.
#        read_positions [filename] [update]
#           Reads a set of object positions from a file in "id x y"
#           format and creates objects for each position.
#        save_objects [filename]
#           Saves the apertures and their measurments into a file.
#           If not given the file name is given using a selection
#           dialog.
#        view
#           Invoked when a view of all the aperture is required.
#
#     private:
#        created_object
#           Invoked after an aperture is created.
#        measured_objects
#           Invoked after the apertures have been measured.
#        set_shape
#           Sets the shape configuration option.
#        sky_method_changed
#           Invoked when the sky region definition method has changed

#  Inheritance:
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
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
#     12-MAR-1996 (PWD):
#        Original version.
#     8-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     21-NOV-1996 (PWD):
#        Output in magnitudes is now optional.
#     16-JUL-1997 (PWD):
#        Modified so that files which are read in do re-create
#        any associated sky apertures.
#     30-MAR-1998 (PWD):
#        Modified to read file when given name.
#     18-MAY-1998 (PWD):
#        Added support for image exposure times.
#     21-MAY-1999 (PWD):
#        Substantial rework of interface to add panes (to save
#        real-estate and a pop-up window) and the ability to append to
#        a named file.
#     24-MAY-2000 (PWD):
#        Added changes so that user can disable exit confirmation.
#     02-MAY-2003 (PWD):
#        Added automatic measurement of apertures.
#     26-APR-2006 (PWD):
#        Added -image to handle volatile cube data.
#     {enter_further_changes_here}
#-

#.

itk::usual GaiaApPhotom {}

itcl::class gaia::GaiaApPhotom {

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
            "GAIA: Aperture photometry -- counts ($itk_option(-number))"
      } else {
         set usemags_ 1
         wm title $w_ \
            "GAIA: Aperture photometry -- magnitudes ($itk_option(-number))"
      }

      #  Add tab window for revealing the aperture details,
      #  measurement parameters and possible all results.
      itk_component add TabNoteBook {
          iwidgets::tabnotebook $w_.tab \
              -angle 0 -tabpos n -width 350 -height 450
      }

      #  Add pane for current aperture details.
      $itk_component(TabNoteBook) add -label Aperture
      set child_(details) [$itk_component(TabNoteBook) childsite 0]

      #  Add pane for measurement parameters.
      $itk_component(TabNoteBook) add -label Parameters
      set child_(params) [$itk_component(TabNoteBook) childsite 1]

      #  Add pane for viewing all measurements together (slower).
      $itk_component(TabNoteBook) add -label Results
      set child_(results) [$itk_component(TabNoteBook) childsite 2]

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

      #  Create a GaiaPhotomDetails object to display the values
      #  of the selected object.
      itk_component add ObjectDetails {
         GaiaPhotomDetails $child_(details).details \
            -positions_cmd [code $this sky_method_changed] \
            -usemags $usemags_
      }
      add_short_help $itk_component(ObjectDetails) \
         {Details of the currently selected aperture}

      #  Create the GaiaPhotomList object to deal with the details of
      #  the objects that are being measured.
      set object_list_ [GaiaPhotomList \#auto \
                           -scrollbox $child_(results).box \
                           -details $itk_component(ObjectDetails) \
                           -canvasdraw $itk_option(-canvasdraw) \
                           -canvas $itk_option(-canvas) \
                           -rtdimage $itk_option(-rtdimage) \
                           -annulus $itk_option(-annulus) \
                           -linewidth $itk_option(-linewidth) \
                           -notify_created_cmd [code $this created_object]\
                           -usemags $usemags_ \
                           -phottype "aperture" \
                           -psf 0 \
                           -allow_resize 1 \
                           -coupled $itk_option(-coupled)]

      #  Now inform details widget of this name!
      $itk_component(ObjectDetails) configure -object_list [code $object_list_]

      #  Create a GaiaPhotomExtras object to deal with any additional
      #  parameters for autophotom.
      itk_component add Extras {
         if { $usemags_ } {
            GaiaPhotomExtras $child_(params).extras
         } else {
            # For counts we assume gaussian sky is more plausible than
            # photon statistics
            GaiaPhotomExtras $child_(params).extras -photon_errors "gaussian sky"
         }
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
      add_help_button aperture "On Window..."

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

      #  Read simple positions from a file.
      $File add command \
         -label {Read positions...} \
         -command [code $this read_positions] \
         -accelerator {Control-p}
      bind $w_ <Control-p> [code $this read_positions]
      add_menu_short_help $File {Read positions...} \
         {Read object positions from a positions file}

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
         -command [code $this sky_method_changed]
      add_menu_short_help $Options {Use annular sky regions}  \
         {Toggle to define sky in detached apertures}

      #  Get shape of apertures (sky and object must be the same).
      set shape_($this) 1
      $Options add checkbutton \
         -label {Use circular apertures} \
         -variable [scope shape_($this)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this set_shape]
      $itk_component(ObjectDetails) set_for_circles 1
      add_menu_short_help $Options {Use circular apertures}  \
         {Toggle to get elliptical apertures}

      #  Whether apertures are created and maintained as the same size.
      $Options add checkbutton \
         -label {Keep apertures same size} \
         -variable [scope itk_option(-coupled)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this set_coupled_]
      add_menu_short_help $Options {Keep apertures same size} \
         {Toggle to keep all apertures the same size}

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
      #  object or sky regions.
      itk_component add Define {
         frame $w_.define
      }

      #  Define an object aperture.
      itk_component add DefineObject {
         button $itk_component(Define).object \
            -text {Define object aperture} \
            -width 20 \
            -highlightthickness 3 \
            -command [code $this define_object]
      }
      $itk_component(DefineObject) configure -highlightbackground black
      add_short_help $itk_component(DefineObject) \
         {Press and then drag out aperture on image}

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

      #  Copy selected objects.
      itk_component add MeasureFrame {frame $w_.copy}
      itk_component add Copy {
         button $itk_component(MeasureFrame).copy \
            -text {Copy aperture} \
            -width 20 \
            -command [code $object_list_ copy]
      }
      add_short_help $itk_component(Copy) \
         {Create copy of current/last aperture}

      #  Measure all objects.
      itk_component add Measure {
         button $itk_component(MeasureFrame).measure \
            -text {Calculate results} \
            -width 20 \
            -command [code $this measure_objects]
      }
      add_short_help $itk_component(Measure) \
         {Calculate photometry of all apertures}

      #  Close window button.
      itk_component add CloseFrame {frame $w_.close}
      itk_component add Close {
         button $itk_component(CloseFrame).close \
            -text {Close}  -command [code $this close]
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

      #  Pack up window.
      pack $itk_component(Close) -side bottom -pady 2 -padx 2
      pack $itk_component(CloseFrame) -side bottom -fill x -pady 2 -padx 2
      if { $usemags_ } {
         pack $itk_component(Skymag) -side top -fill x -pady 2
      }
      pack $itk_component(Results) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(TabNoteBook) -fill both -expand 1
      pack $itk_component(ObjectDetails) -fill both -expand true \
          -pady 2 -padx 2
      pack $itk_component(Extras) -fill both -expand true -pady 2 -padx 2

      pack $itk_component(Define) -side top -fill x  -pady 2 -padx 2
      pack $itk_component(DefineObject) -side left -expand true -pady 2 -padx 2
      pack $itk_component(DefineSky) -side left -expand true -pady 2 -padx 2

      pack $itk_component(MeasureFrame) -side top -fill x
      pack $itk_component(Measure) -side right -pady 2 -padx 2 -expand true
      pack $itk_component(Copy) -side right -pady 2 -padx 2 -expand true

      pack $itk_component(SaveFrame) -side top -fill x
      pack $itk_component(Save) -side right -pady 2 -padx 2 -expand true
      pack $itk_component(Append) -side right -pady 2 -padx 2 -expand true

      #  Reveal first page of controls.
      $itk_component(TabNoteBook) select 0

      #  Create object to control image names.
      set namer_ [GaiaImageName \#auto]
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $object_list_ != {} } {
         delete object $object_list_
         set object_list_ {}
      }
      if { $autophotom_ != {} } {
         catch {$autophotom_ delete_sometime}
         set autophotom_ {}
      }
      if { $namer_ != {} } {
         catch {delete object $namer_}
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

   #  Define a region of sky.
   method define_sky {} {
      $itk_component(DefineSky) configure -state disabled
      if { $itk_option(-coupled) } {
         $object_list_ configure -allow_resize 1
      }
      $object_list_ create_sky_region
      if { $itk_option(-coupled) } {
         $object_list_ configure -allow_resize 0
      }
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
               -option_cmd {::option add *GaiaApPhotom.quiet_exit}
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
            $object_list_ read_file [$w get] $update
         }
         destroy $w
      } else {
         $object_list_ read_file $filename $update
      }
   }

   #  Read positions from a positions file.
   method read_positions {{filename ""} {update 0}} {
      if { $filename == "" } {
         set w [util::FileSelect .\#auto -title "Choose positions file"]
         if {[$w activate]} {
            $object_list_ read_positions_file [$w get] $update
         }
         destroy $w
      } else {
         $object_list_ read_file $filename $update
      }
   }

   #  Measure the current objects.
   method measure_objects {args} {
      if { [$object_list_ write_file "GaiaPhotomIn.Dat"] } {
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
            if { $usemags_ } {
               set ok "true"
            } else {
               set ok "false"
            }

            #  If args are given then set the command to be run when
            #  the application returns.
            if { $args != "" } {
               set complete_cmd_ "$args"
            }

            #  Make sure that the disk image is up to date. Only relevant for
            #  volatile images (from cubes).
            $itk_option(-image) save_if_volatile

            #  And run the command.
            blt::busy hold $w_
            update idletasks
            catch {file delete GaiaPhotomOut.Dat}
            # XXX changes here  .... eval
            $autophotom_ runwiths "\
               in=$image \
               infile=GaiaPhotomIn.Dat \
               outfile=GaiaPhotomOut.Dat \
               optima=false \
               skymag=$skymag_ \
               usemags=$ok \
               $more"

         } else {
            error_dialog "No image is displayed"
         }
      } else {
         error_dialog "You need to define some apertures"
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
            $object_list_ write_file [$w get]
         }
         destroy $w
      } else {
         $object_list_ write_file $filename
      }
   }

   #  Append the measurements to a file. Adds a comment containing the
   #  image name.
   method append_objects {{filename ""}} {
     set comment "[$itk_option(-rtdimage) fullname]"
     if { $filename == "" } {
         set w [util::FileSelect .\#auto -title "Write PHOTOM file"]
         if {[$w activate]} {
            $object_list_ append_file $comment [$w get]
         }
         destroy $w
      } else {
         $object_list_ append_file $comment $filename
      }
   }

   #  Notification that the define_object method has been
   #  completed. This allows us to set the state of the sky region
   #  button.
   private method created_object {} {
      toggle_sky_button_
      $itk_component(DefineObject) configure -state normal
      if { $auto_measure_ } {
         measure_objects
      }
   }

   #  Toggle the define sky button to reflect the current state.
   private method toggle_sky_button_ {} {
      if { $skymethod_($this) } {
         $itk_component(DefineSky) configure -state disabled
      } else {
         $itk_component(DefineSky) configure -state normal
      }
   }

   #  The aperture are measured, read in the results.
   private method measured_objects {} {
      blt::busy release $w_
      if { [file exists GaiaPhotomOut.Dat] } {
         $object_list_ read_file GaiaPhotomOut.Dat 1
      }
      if { $complete_cmd_ != {} } {
         eval $complete_cmd_
         set complete_cmd_ {}
      }
   }

   #  Sky method changed so reconfigure. If optional args is set then
   #  this is not called from menu button and args is the sky method
   #  string to which the button should be set. Note that when
   #  skymethod is not annulus the button state isn't changed until a
   #  new object is created (see created_object method).
   private method sky_method_changed {args} {
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
   }

   #  View all measurements in new window (controlled by
   #  GaiaPhotomList).
   method view {{value ""}} {
      if { $value != "" } {
         set view_($this) $value
      }
      $object_list_ configure -show_list $view_($this)
   }

   #  Set shape configuration option and disabled unwanted controls.
   private method set_shape {} {
      if { $shape_($this) } {
         configure -shape circle
      } else {
         configure -shape ellipse
      }
      $itk_component(ObjectDetails) set_for_circles $shape_($this)
   }

   #  Sky zero point may have changed, remeasure objects if so.
   private method sky_zero_changed {value} {
      if { $value != $skymag_ } {
         measure_objects
      }
   }

   #  Add a menu for controlling the aperture colours.
   protected method make_colours_menu_ {m} {

      #  Create the submenus.
      foreach {label name} {
         {Selected colour} selcol
         {Deselected colour} descol
         {Selected sky colour} selskycol
         {Deselected sky colour} desskycol} {
         $m add cascade -label $label -menu [menu $m.$name]
      }

      foreach i $itk_option(-colors) {
         $m.selcol add radiobutton \
            -value $i \
            -background $i \
            -command [code $this configure -selected_colour $i] \
            -variable [scope itk_option(-selected_colour)]
      }
      configure -selected_colour $itk_option(-selected_colour)
      foreach i $itk_option(-colors) {
         $m.descol add radiobutton \
            -value $i \
            -background $i \
            -command [code $this configure -deselected_colour $i] \
            -variable [scope itk_option(-deselected_colour)]
         }
      configure -deselected_colour $itk_option(-deselected_colour)
      foreach i $itk_option(-colors) {
         $m.selskycol add radiobutton \
            -value $i \
            -background $i \
            -command [code $this configure -selected_sky_colour $i] \
            -variable [scope itk_option(-selected_sky_colour)]
      }
      configure -selected_sky_colour $itk_option(-selected_sky_colour)
      foreach i $itk_option(-colors) {
         $m.desskycol add radiobutton \
            -value $i \
            -background $i \
            -command [code $this configure -deselected_sky_colour $i] \
            -variable [scope itk_option(-deselected_sky_colour)]
      }
      configure -deselected_sky_colour $itk_option(-deselected_sky_colour)
   }

   #  Set the coupled variable to reflect the current value.
   protected method set_coupled_ {args} {
      configure -coupled $itk_option(-coupled)
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of a StarCanvasDraw widget to use to control objects.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {
      if { $object_list_ != {} } {
         $object_list_ configure -canvasdraw $itk_option(-canvasdraw)
      }
   }

   #  Name of GaiaImageCtrl widget.
   itk_option define -image image Image {}

   #  Name of canvas.
   itk_option define -canvas canvas Canvas {} {
      if { $object_list_ != {} } {
         $object_list_ configure -canvas $itk_option(-canvas)
      }
   }

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {
      if { $object_list_ != {} } {
         $object_list_ configure -rtdimage $itk_option(-rtdimage)
      }
   }

   #  Whether annulus region is in use.
   itk_option define -annulus annulus Annulus 1 {
      if { $object_list_ != {} } {
         $object_list_ configure -annulus $itk_option(-annulus)
      }
   }

   #  Shape of aperture.
   itk_option define -shape shape Shape circle {
      if { $object_list_ != {} } {
         $object_list_ configure -shape $itk_option(-shape)
      }
   }

   #  Width of lines.
   itk_option define -linewidth linewidth LineWidth 1 {
      if { $object_list_ != {} } {
         $object_list_ configure -linewidth $itk_option(-linewidth)
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

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  Possible colours.
   itk_option define -colors colors Colors {
       red green blue cyan magenta yellow white grey90 grey40 grey10 black
   }

   #  Whether apertures are keep the same size.
   itk_option define -coupled coupled Coupled 0 {
      if { $object_list_ != {} } {
         $object_list_ configure -coupled $itk_option(-coupled)
         if { $itk_option(-coupled) } {
            $object_list_ configure -allow_resize 0
         } else {
            $object_list_ configure -allow_resize 1
         }
      }
   }

   #  Quietly exit without asking about saving results.
   itk_option define -quiet_exit quiet_exit Quiet_exit 0

   #  Whether aperures are keep same size.

   #  Protected variables: (only available to instance)
   #  --------------------

   #  Whether to work in magnitudes or counts.
   protected variable usemags_ 1

   #  Object controlling list of known photometry objects.
   protected variable object_list_ {}

   #  Name of autophotom application.
   protected variable autophotom_ {}

   #  Value of skymag when last known.
   protected variable skymag_ 50

   #  Command to perform when command completes.
   protected variable complete_cmd_ {}

   #  Name of image control object.
   protected variable namer_ {}

   #  Whether apertures are automatically measured when created, or not.
   protected variable auto_measure_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Methods of estimating sky (this is a global array visible
   #  in this namespace only and indexed by $this to resolve between
   #  different instances).
   common skymethod_

   #  Shape of aperture.
   common shape_

   #  Whether to view all measurements or not.
   common view_

#  End of class definition.
}
