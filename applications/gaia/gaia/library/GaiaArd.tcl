#+
#  Name:
#     GaiaArd

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Provides a top-level widget for controlling ARD descriptions on
#     a canvas.

#  Description:
#     This class creates a top-level frame which it populates with
#     buttons and menus for controlling the creation, modification,
#     saving and restoring ARD regions.
#
#     It also runs the applications ARDSTAT and ARDMASK to get
#     statistics on the image regions and extract or remove parts.
#
#     The regions can be converted and save as an IVOA FITS MOC.

#  Invocations:
#
#        GaiaArd object_name [configuration options]
#
#     This creates an instance of a GaiaArd object. The return is
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
#
#        -canvasdraw
#
#     Sets the name of the StarCanvasDraw object used to control the
#     graphics content.
#
#        -image
#
#     Sets the name of the GaiaImageCtrl object used.
#
#        -canvas
#
#     Sets the name of the canvas used to display the image and graphics.
#
#        -rtdimage
#
#     Sets the name of the rtdimage object used to display the image.
#
#        -gaia
#
#     Name of Gaia widget that parents this widget (used for
#     clone creation).
#
#         -replace (boolean)
#
#     Controls whether new images are displayed in a new window
#     or the existing window.
#
#         -number (integer)
#
#     Identifying number for toolbox (shown in () in window title).
#     This should be the clone number of the invoking Gaia widget.

#  Methods:
#
#        close
#
#     Closes the window created by this class and performs any
#     tidying up.
#
#         save_file [filename]
#
#     Saves the current ARD description to a file. The file, if not
#     given, is chosen using a file selection dialog.
#
#         read_file [filename]
#
#     Reads an ARD description from a file. The file, if not given,
#     is chosen using a file selection dialog. Note that the
#     description must be "simple" (i.e. consist of
#     REGION(parameters,..) statements only).
#
#         stats mode
#
#     Performs statistics for either all the regions or just those
#     which are selected.
#
#         measured_stats_
#
#     Private method to deal with completion of statistics
#     measurements.
#
#        blank mode
#
#     Blank out (i.e. set invalid) either all the regions or just the
#     selected regions. The result is displayed in either an GAIA
#     window or in the current window.
#
#        extract mode
#
#     Extract either all or just the selected regions. The result of
#     this operation is a new image that is displayed either in a
#     new window or in the current window.
#
#        crop
#
#     Removes any excess blank regions from around the image and then
#     displays the results (either in a new window or the current
#     one).
#
#        modified_image
#
#     Manages the display of modified images. These are either
#     displayed in a new window clone or in the existing window.
#
#        replace_changed_
#
#     Handles the modification of the option to display modified
#     images in a new window or the existing one.

#  Inheritance:
#     This widget inherits TopLevelWidget.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council
#     Copyright (C) 2009 Science and Technology Facilities Council
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
#     ALLAN: Allan Brighton (ESO)
#     {enter_new_authors_here}

#  History:
#     14-MAY-1996 (PWD):
#        Original version.
#     14-JUN-1996 (PWD):
#        Removed much of the functionality to StarArdTool to
#        generalise things for better reuse.
#     4-JUL-1996 (PWD):
#        Converted to itcl2.0, made StarArdTool into a "has a"
#        relationship to get around inability to mix itcl/itk
#        inheritance.
#     28-NOV-1996 (PWD):
#        Added ARDMASK options.
#     10-JUN-1997 (PWD):
#        Finished prologue.
#     24-Mar-1998 (ALLAN)
#        Changed "rect" to "rectangle", to resolve conflict with rtd bitmap
#        name.
#     24-APR-1998 (ALLAN)
#        Pass command line arguments to "clone" rather than use "after 500".
#     12-NOV-1998 (PWD):
#        Added clear stats window button.
#     4-MAY-1999 (PWD):
#        Merged Allan's changes.
#     28-JUN-1999 (PWD):
#        Added ability to save log window to text file. Renamed to
#        GaiaArd. Added short_help.
#     26-APR-2006 (PWD):
#        Added -image option to support volatile cube slices.
#     11-JAN-2019 (PWD):
#        Added FITS MOC support.
#     {enter_further_changes_here}
#-

#.

itk::usual GaiaArd {}

itcl::class gaia::GaiaArd {

   #  Inheritances:
   #  -------------

   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Image regions ($itk_option(-number))"

      #  Create the ARD toolbox that does most of the work.
      set Toolbox_ [StarArdTool \#auto \
                       -canvasdraw $itk_option(-canvasdraw) \
                       -canvas $itk_option(-canvas)\
                       -rtdimage $itk_option(-rtdimage)\
                       -maxcol 9]

      #  Add the File and Options menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button ardusage "On Window..."

      #  Add short help window.
      make_short_help

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Save measurements to a file.
      $File add command \
         -label {Save ARD description...} \
         -command [code $this save_file] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this save_file]
      $short_help_win_ add_menu_short_help $File \
         {Save ARD description...} \
         {Save the current ARD regions to a file}

      #  Read measurements from a file.
      $File add command \
         -label {Read ARD description...} \
         -command [code $this read_file] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_file]
      $short_help_win_ add_menu_short_help $File \
         {Read ARD description...} \
         {Read a simple ARD description from a file}

      #  Save regions to a FITS MOC.
      $File add command \
         -label {Save to FITS MOC...} \
         -command [code $this save_fitsmoc] \
         -accelerator {Control-f}
      bind $w_ <Control-s> [code $this save_fitsmoc]
      $short_help_win_ add_menu_short_help $File \
         {Save to FITS MOC...} \
         {Save the current ARD regions to FITS MOC file}

      #  Set the exit menu item.
      $File add command -label Exit \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Display new images in existing window.
      if { ! [info exists replace_] } {
         set replace_ $itk_option(-replace)
      } else {
         set itk_option(-replace) $replace_
      }
      $Options add checkbutton \
         -label {Display new images in existing window} \
         -variable [scope replace_] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this replace_changed_]

      #  Auto crop extracted region images.
      $Options add checkbutton \
         -label {Autocrop extracted images} \
         -variable [scope autoautocrop_] \
         -onvalue 1 \
         -offvalue 0 \

      #  Labels for toolbox.
      itk_component add label {frame $w_.labels}
      itk_component add lrect {
         label $itk_component(label).lrect -bitmap rectangle -anchor center
      }
      itk_component add lcircle {
         label $itk_component(label).lcircle -bitmap circle -anchor center
      }
      itk_component add ltext {
         label $itk_component(label).ltext -text {ARD Regions} -anchor center
      }
      itk_component add lpoly {
         label $itk_component(label).lpoly -bitmap poly -anchor center
      }
      itk_component add lellipse {
         label $itk_component(label).lellipse -bitmap ellipse -anchor center
      }

      #  Frame for control buttons.
      itk_component add action {frame $w_.action}
      blt::blttable $itk_component(action)

      #  Button to close window.
      itk_component add close {
         button $itk_component(action).close -text Close \
            -command [code $this close]
      }
      add_short_help $itk_component(close) \
         {Close window}

      #  Create buttons for creating regions interactively (this
      #  cannot be recognised as an itk component widget, so just keep
      #  it plain).
      set Buttonbox_ [$Toolbox_ make_types_frame $w_.tools]

      #  Add a button for reporting the statistics of all the regions
      #  on the image or just the selected region.
      itk_component add wholestats {
         button $itk_component(action).whole \
            -text {Stats all} \
            -command [code $this stats all]
      }
      add_short_help $itk_component(wholestats) \
         {Get stats for all regions}
      itk_component add selectedstats {
         button $itk_component(action).selected \
            -text {Stats selected} \
            -command [code $this stats selected]
      }
      add_short_help $itk_component(selectedstats) \
         {Get stats for just the selected regions}
      itk_component add clearstats {
         button $itk_component(action).clear \
            -text {Clear stats} \
            -command [code $this stats clear]
      }
      add_short_help $itk_component(clearstats) \
         {Clear the stats results window}
      itk_component add savestats {
         button $itk_component(action).save \
            -text {Save stats} \
            -command [code $this save_stats_]
      }
      add_short_help $itk_component(savestats) \
         {Save the stats results to the named file}

      #  Add an entry widget to display the results.
      itk_component add statsresults {
         Scrollbox $w_.statsresults -exportselection 1 -singleselect 0
      }
      add_short_help $itk_component(statsresults) \
         {Results of stats measurements}

      #  And a file for saving the results.
      itk_component add logfile {
         gaia::LabelFileChooser $w_.logfile \
            -labelwidth 14 \
            -text "Stats results file:" \
            -textvariable [scope logfile_] \
            -value "$logfile_"
      }
      add_short_help $itk_component(logfile) \
         {File name for saving contents of stats window}


      #  Add buttons for extracting and masking out the current
      #  regions.
      itk_component add extractwhole {
         button $itk_component(action).exwhole \
            -text {Extract all} \
            -command [code $this extract all]
      }
      add_short_help $itk_component(extractwhole) \
         {Extract all regions into a new image}
      itk_component add extractselected {
         button $itk_component(action).exselect \
            -text {Extract selected} \
            -command [code $this extract selected]
      }
      add_short_help $itk_component(extractselected) \
         {Extract select regions into a new image}
      itk_component add maskwhole {
         button $itk_component(action).maskwhole \
            -text {Blank all} \
            -command [code $this blank all]
      }
      add_short_help $itk_component(maskwhole) \
         {Create a new image with all regions blanked}
      itk_component add maskselected {
         button $itk_component(action).maskselect \
            -text {Blank selected} \
            -command [code $this blank selected]
      }
      add_short_help $itk_component(maskselected) \
         {Create a new image with selected regions blanked}
      itk_component add autocrop {
         button $itk_component(action).autocrop \
            -text {Auto crop} \
            -command [code $this crop]
      }
      add_short_help $itk_component(autocrop) \
         {Create a new image with all blank edge regions removed}

      #  Pack everything into place.
      pack $itk_component(lrect) $itk_component(lcircle) \
              $itk_component(ltext) $itk_component(lpoly) \
              $itk_component(lellipse) -side left -fill x -expand 1
      pack $itk_component(label) -side top -fill x -pady 5 -padx 5
      pack $Buttonbox_ -side top -fill x -pady 5 -padx 5
      pack $itk_component(action) -side bottom -fill x -pady 5 -padx 5
      blt::blttable $itk_component(action) \
         $itk_component(extractwhole)    0,0 -fill x -pady 3 -padx 3 \
         $itk_component(extractselected) 0,1 -fill x -pady 3 -padx 3 \
         $itk_component(maskwhole)       0,2 -fill x -pady 3 -padx 3 \
         $itk_component(maskselected)    0,3 -fill x -pady 3 -padx 3 \
         $itk_component(autocrop)        0,4 -fill x -pady 3 -padx 3 \
         $itk_component(selectedstats)   1,0 -fill x -pady 3 -padx 3 \
         $itk_component(wholestats)      1,1 -fill x -pady 3 -padx 3 \
         $itk_component(clearstats)      1,2 -fill x -pady 3 -padx 3 \
         $itk_component(savestats)       1,3 -fill x -pady 3 -padx 3 \
         $itk_component(close)           1,4 -fill x -pady 3 -padx 3
      pack $itk_component(logfile) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(statsresults) -fill both -expand 1 -side top

      #  Create control object for image names.
      set namer_ [gaia::GaiaImageName \#auto]

   }

   #  Destructor:
   #  -----------
   destructor {
      if { $Toolbox_ != {} } {
         delete object $Toolbox_
      }
      if { $ardstat_ != {} } {
         catch {$ardstat_ delete_sometime}
         set ardstat_ {}
      }
      if { $ardmask_ != {} } {
         catch {$ardmask_ delete_sometime}
         set ardmask_ {}
      }
      if { $autocrop_ != {} } {
         catch {$autocrop_ delete_sometime}
         set autocrop_ {}
      }
      if { $namer_ != {} } {
         catch {delete object $namer_}
      }

      #  Remove all temporary files (non-image).
      if { $tempfiles_ != {} } {
         $tempfiles_ clear
         catch {delete object $tempfiles_}
      }
      if { $tempimages_ != {} } {
         catch {delete object $tempimages_}
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

   #  Close window.
   public method close {} {
      destroy $w_
   }

   #  Make sure backing store image is update if displayed data is volatile
   #  (usual case for slices from cubes). Also need to restore graphics, if
   #  image is reopened.
   protected method save_if_volatile {} {
      if { [$itk_option(-rtdimage) volatile] } {

         #  Save description so we can restore it.
         set tmpfile [make_tmpname_]
         $Toolbox_ save_description $tmpfile

         #  Update image.
         $itk_option(-image) save_if_volatile

         #  Restore description.
         $Toolbox_ read_description $tmpfile
      }
   }

   #  Determine a name for a new temporary file, non-image.
   protected method make_tmpname_ {} {
      if { $tempfiles_ == {} } {
         set tempfiles_ \
            [gaia::GaiaTempName \#auto -prefix "GaiaArdIn" -type ".Dat" \
                -exists 0]
      }
      return [$tempfiles_ get_name]
   }

   #  Determine a name for a new temporary image.
   protected method make_tmpimage_ {} {
      if { $tempimages_ == {} } {
         set tempimages_ \
            [gaia::GaiaTempName \#auto -prefix "GaiaArdImg" -type ".sdf" \
               -exists 0]
      }
      return [$tempimages_ get_name]
   }

   #  Save ARD description to a FITS MOC.
   public method save_fitsmoc {{filename ""}} {
      if { $filename == "" } {
         $Toolbox_ save_moc
      } else {
         $Toolbox_ save_fitsmoc $filename
      }
   }

   #  Save ARD description to a file.
   public method save_file {{filename ""}} {
      if { $filename == "" } {
         $Toolbox_ save_file
      } else {
         $Toolbox_ save_description $filename
      }
   }

   #  Read an ARD description (in LessARD format) from a file.
   public method read_file {{filename ""}} {
      if { $filename == "" } {
         $Toolbox_ read_file
      } else {
         $Toolbox_ read_description $filename
      }
   }

   #  Display or clear statistics for regions, "args" is a command to
   #  run when the task really completes.
   public method stats {mode args} {
      if { $mode == "clear" } {
         $itk_component(statsresults) clear 0 end
         return
      }

      #  First save the current description to a file
      set tmpfile [make_tmpname_]
      if { $mode == "all" } {
         set ok [$Toolbox_ save_description $tmpfile]
      } else {
         set ok [$Toolbox_ save_selected_description $tmpfile]
      }
      if { $ok } {
         #  Now startup the Ardstat application.
         if { $ardstat_ == {} } {
            global gaia_dir
            set ardstat_ [GaiaApp \#auto -application \
                             $gaia_dir/ardstat \
                             -notify [code $this measured_stats_] \
                             -show_output $itk_component(statsresults)]
         }

         #  Get the name of the current image.
         set image [$itk_option(-rtdimage) fullname]
         if { $image != "" } {
            $namer_ configure -imagename $image
            set image [$namer_ ndfname]

            #  Set command to run on completion.
            if { $args != "" } {
               set complete_cmd_ $args
            }

            #  Make sure that the disk image is up to date.
            save_if_volatile

            #  And ARDSTAT on the image and file.
            blt::busy hold $w_
            update idletasks
            $ardstat_ runwiths \
               "in=$image simple=f full=t oneline=t region=^${tmpfile}"

         } else {
            error_dialog "No image is displayed"
         }
      } else {
         if { $mode == "all" } {
               error_dialog "No regions are defined"
         } else {
            error_dialog "No regions are selected"
         }
      }
   }

   #  When the statistics of a region are measured do nothing but add
   #  a blank line the scrollbox is updated automatically.
   private method measured_stats_ {} {
      blt::busy release $w_
      $itk_component(statsresults) insert end " "
      $itk_component(statsresults) see end

      #  If necessary do the completion command.
      if { $complete_cmd_ != {} } {
         eval $complete_cmd_
         set complete_cmd_ {}
      }
   }

   #  Save the stats window to the named file.
   protected method save_stats_ {} {
      if { $logfile_ != {} } {
         busy {
            set fid [::open $logfile_ w]
            puts $fid "\# GAIA ARD region stats file."
            puts $fid "\#"
            set size [$itk_component(statsresults) size]
            for {set i 0} {$i < $size} {incr i} {
               puts $fid [$itk_component(statsresults) get $i]
            }
            ::close $fid
         }
      }
   }

   #  Blank out regions and display in a new clone. "args" if given,
   #  is a command to execute when task really finishes.
   public method blank {mode args} {

      #  First save the current description to a file.
      set tmpfile [make_tmpname_]
      if { $mode == "all" } {
         set ok [$Toolbox_ save_description $tmpfile]
      } else {
         set ok [$Toolbox_ save_selected_description $tmpfile]
      }
      if { $ok } {

         #  Now startup the Ardmask application.
         if { $ardmask_ == {} } {
            global env
            set ardmask_ [GaiaApp \#auto -application \
                             $env(KAPPA_DIR)/ardmask \
                             -notify [code $this modified_image_]]
         }

         #  Get the name of the current image.
         set image [$itk_option(-rtdimage) fullname]
         if { $image != "" } {
            $namer_ configure -imagename $image
            set image [$namer_ ndfname]

            #  Create a temporary file name.
            set tmpimage_ [make_tmpimage_]

            #  Set command to run on completion.
            if { $args != "" } {
               set complete_cmd_ $args
            }

            #  Make sure that the disk image is up to date.
            save_if_volatile

            #  And run ARDMASK on the image and file.
            blt::busy hold $w_
            $ardmask_ runwiths "in=$image ardfile=$tmpfile out=$tmpimage_"
         } else {
            error_dialog "No image is displayed"
         }
      } else {
         if { $mode == "all" } {
            error_dialog "No regions are defined"
         } else {
            error_dialog "No regions are selected"
         }
      }
   }

   #  Extract regions and display in a new clone.
   public method extract {mode} {

      #  First save the current description to a file.
      set tmpfile [make_tmpname_]
      if { $mode == "all" } {
         set ok [$Toolbox_ save_description $tmpfile]
      } else {
         set ok [$Toolbox_ save_selected_description $tmpfile]
      }
      if { $ok } {

         #  Need to invert the sense of the ARD region for extraction.
           set f [::open $tmpfile r]
           set contents [::read $f]
           ::close $f
           set f [::open $tmpfile w]
           puts -nonewline $f {.NOT.(}
           puts -nonewline $f $contents
           puts $f {)}
           ::close $f

           #  Now startup the Ardmask application.
           if { $ardmask_ == {} } {
              global env
              set ardmask_ [GaiaApp \#auto -application \
                               $env(KAPPA_DIR)/ardmask \
                               -notify [code $this maybe_autocrop_image_]]
           }

           #  Get the name of the current image.
           set image [$itk_option(-rtdimage) fullname]
           if { $image != "" } {
              $namer_ configure -imagename $image
              set image [$namer_ ndfname]

              #  Create a temporary file name.
              set tmpimage_ [make_tmpimage_]

              #  Make sure that the disk image is up to date.
              save_if_volatile

              #  And run ARDMASK on the image and file.
              blt::busy hold $w_
              $ardmask_ runwiths "in=$image ardfile=$tmpfile out=$tmpimage_"
           } else {
              error_dialog "No image is displayed"
           }
        } else {
           if { $mode == "all" } {
              error_dialog "No regions are defined"
           } else {
              error_dialog "No regions are selected"
           }
        }
   }

   #  Autocrop the given temporary image, if autocrop automatically.
   private method maybe_autocrop_image_ {} {

      if { ! $autoautocrop_ } {
         modified_image_
         return
      }

      #  Check that extracted image is available.
      set file ""
      if { ! [file readable $tmpimage_] } {
         if { ! [file readable ${tmpimage_}.sdf] } {
            blt::busy release $w_
            return
         }
         set file ${tmpimage_}.sdf
      } else {
         set file $tmpimage_
      }

      #  And crop that before display.
      if { $autocrop_ == {} } {
         global gaia_dir
         set autocrop_ [GaiaApp \#auto -application \
                           $gaia_dir/autocrop \
                           -notify [code $this modified_image_]]
      }

      #  Create a second temporary file name.
      set tmpimage_ [make_tmpimage_]

      #  And run autocrop.
      blt::busy hold $w_
      set disposeimage_ $file
      $autocrop_ runwiths "in=$file out=$tmpimage_"
   }

   #  Crop the displayed image removing any padding pixels.
   public method crop {} {

      #  Now startup the autocrop application.
      if { $autocrop_ == {} } {
         global gaia_dir
         set autocrop_ [GaiaApp \#auto -application \
                           $gaia_dir/autocrop \
                           -notify [code $this modified_image_]]
      }

      #  Get the name of the current image.
      set image [$itk_option(-rtdimage) fullname]
      if { $image != "" } {
         $namer_ configure -imagename $image
         set image [$namer_ ndfname]

         #  Create a temporary file name.
         set tmpimage_ [make_tmpimage_]

         #  Make sure that the disk image is up to date.
         save_if_volatile

         #  And run autocrop on the image and file.
         blt::busy hold $w_
         $autocrop_ runwiths "in=$image out=$tmpimage_"
      } else {
         error_dialog "No image is displayed"
      }
   }

   #  Display the extracted/blanked image. This is either in a clone
   #  or in the existing display.
   private method modified_image_ {} {

      #  If a file for disposal is named, do that.
      if { $disposeimage_ != {} } {
         catch {::file delete $disposeimage_}
         set disposeimage_ {}
      }

      set file ""
      if { ! [file readable $tmpimage_] } {
         if { ! [file readable ${tmpimage_}.sdf] } {
            blt::busy release $w_
            return
         }
         set file ${tmpimage_}.sdf
      } else {
         set file $tmpimage_
      }
      if { $file != "" } {
         if { $itk_option(-replace) } {

            # Replace the existing image
            $itk_option(-gaia) open $file
            $itk_option(-gaia) configure -temporary 1
         } else {

            # Create new clone window.
            $itk_option(-gaia) newimage_clone $file -temporary 1
         }
      }

      #  If given do the completed command.
      if { $complete_cmd_ != {} } {
         eval $complete_cmd_
         set complete_cmd_ {}
      }
      blt::busy release $w_
   }

   #  Deal with replace option when changed by Option checkbutton.
   protected method replace_changed_ {} {
      configure -replace $replace_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of a StarCanvasDraw widget to use to control objects.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {}

   #  Name of GaiaImageCtrl object.
   itk_option define -image image Image {} {}

   #  Name of canvas.
   itk_option define -canvas canvas Canvas {} {}

   #  Name of starrtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Name of Gaia widget that parents this widget (used for
   #  clone creation).
   itk_option define -gaia gaia Gaia {} {}

   #  Whether to replace the existing image with new images.
   itk_option define -replace replace Replace 0 {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Name of the ARD toolbox object.
   protected variable Toolbox_ {}

   #  Name of ardstat application.
   protected variable ardstat_ {}

   #  Name of ardmask application.
   protected variable ardmask_ {}

   #  Name of autocrop application.
   protected variable autocrop_ {}

   #  Object to manage non-image temporary files.
   protected variable tempfiles_ {}

   #  Object to manage image temporary files.
   protected variable tempimages_ {}

   #  Name of image being created.
   protected variable tmpimage_ {}

   #  Name of file to save results window into.
   protected variable logfile_ GaiaArd.Log

   #  Command to perform when command completes.
   protected variable complete_cmd_ {}

   #  Name of image name control object.
   protected variable namer_ {}

   #  Whether to autocrop extracted images.
   protected variable autoautocrop_ 1

   #  An image that requires disposal when calling modified_image_
   #  (an intermediary temporary file).
   protected variable disposeimage_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  New replace option value (from checkbutton)
   common replace_

   #  End of class definition.
}
