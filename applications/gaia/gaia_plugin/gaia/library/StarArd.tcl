#+
#  Name:
#     StarArd

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

#  Invocations:
#
#        StarArd object_name [configuration options]
#
#     This creates an instance of a StarArd object. The return is
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
#        -canvasdraw canvas_draw_name
#
#     Sets the name of the StarCanvasDraw object used to control the
#     graphics content.
#
#        -canvas canvas_name
#
#     Sets the name of the canvas used to display the image and graphics.
#
#        -rtdimage rtd_image_name
#
#     Sets the name of the GaiaImage object used to display the
#     image.
#
#        -gaia
#
#     Name of Gaia widget that parents this widget (used for
#     clone creation).
#
#         -replace boolean
#
#     Controls whether new images are displayed in a new window
#     or the existing window.
#
#         -number integer
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
#         save_file
#
#     Saves the current ARD description to a file. The file
#     is chosen using a file selection dialog.
#
#         read_file
#
#     Reads an ARD description from a file. The file is chosen using
#     a file selection dialog. Note that the description must be
#     "simple" (i.e. consist of REGION(parameters,..) statements
#     only).
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

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     ALLAN: Allan Brighton (ESO)
#     {enter_new_authors_here}

#  History:
#     14-MAY-1996 (PDRAPER):
#        Original version.
#     14-JUN-1996 (PDRAPER):
#        Removed much of the functionality to StarArdTool to
#        generalise things for better reuse.
#     4-JUL-1996 (PDRAPER):
#        Converted to itcl2.0, made StarArdTool into a "has a"
#        relationship to get around inability to mix itcl/itk
#        inheritance.
#     28-NOV-1996 (PDRAPER):
#        Added ARDMASK options.
#     10-JUN-1997 (PDRAPER):
#        Finished prologue.
#     {enter_further_changes_here}
#     24-Mar-1998 (ALLAN) 
#        Changed "rect" to "rectangle", to reslove conflict with rtd bitmap name.
#     24-APR-1998 (ALLAN) 
#        Pass command line arguments to "clone" rather than use "after 500".

#-

#.

itk::usual StarArd {}

itcl::class gaia::StarArd {

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
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  Add window help.
      global env gaia_library
      add_help_button $gaia_library/StarArd.hlp "On Window..."

      #  Save measurements to a file.
      $File add command \
         -label {Save ARD description...} \
         -command [code $this save_file] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this save_file]

      #  Read measurements from a file.
      $File add command \
         -label {Read ARD description...} \
         -command [code $this read_file] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_file]

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

      #  Labels for toolbox.
      itk_component add label {frame $w_.labels}
      # allan: changed "rect" to "rectangle", conflict with rtd bitmap name
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

      #  Button to close window.
      itk_component add actionframe {frame $w_.action}
      itk_component add close {
         button $itk_component(actionframe).close -text Close \
            -command [code $this close]
      }

      #  Create buttons for creating regions interactively (this
      #  cannot be recognised as an itk component widget, so just keep
      #  it plain).
      set Buttonbox_ [$Toolbox_ make_types_frame $w_.tools]

      #  Add a button for reporting the statistics of all the regions
      #  on the image or just the selected region.
      itk_component add wholestats {
         button $itk_component(actionframe).whole \
            -text {Stats all} \
            -command [code $this stats all]
      }
      itk_component add selectedstats {
         button $itk_component(actionframe).selected \
            -text {Stats selected} \
            -command [code $this stats selected]
      }

      #  Add an entry widget to display the results.
      itk_component add statsresults {
         Scrollbox $w_.statsresults
      }

      #  Add buttons for extracting and masking out the current
      #  regions.
      itk_component add extractwhole {
         button $itk_component(actionframe).exwhole \
            -text {Extract all} \
            -command [code $this extract all]
      }
      itk_component add extractselected {
         button $itk_component(actionframe).exselect \
            -text {Extract selected} \
            -command [code $this extract selected]
      }
      itk_component add maskwhole {
         button $itk_component(actionframe).maskwhole \
            -text {Blank all} \
            -command [code $this blank all]
      }
      itk_component add maskselected {
         button $itk_component(actionframe).maskselect \
            -text {Blank selected} \
            -command [code $this blank selected]
      }
      itk_component add autocrop {
         button $itk_component(actionframe).autocrop \
            -text {Auto crop} \
            -command [code $this crop]
      }

      #  Pack everything into place.
      pack $itk_component(lrect) $itk_component(lcircle) \
	      $itk_component(ltext) $itk_component(lpoly) \
	      $itk_component(lellipse) -side left -fill x -expand 1
      pack $itk_component(label) -side top -fill x -pady 5 -padx 5
      pack $Buttonbox_ -side top -fill x -pady 5 -padx 5
      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(wholestats) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(selectedstats) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(extractwhole) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(extractselected) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(maskwhole) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(maskselected) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(autocrop) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(statsresults) -fill both -expand 1 -side top

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

      # Remove all temporary files.
      if { [info exists tempfiles_] } {
         foreach f [array names tempfiles_] {
            if { [file writable $tempfiles_($f)] } {
               file delete $tempfiles_($f)
            }
         }
      }
   }

   #  Methods:
   #  --------

   #  Close window.
   method close {} {
      destroy $w_
   }

   #  Save ARD description to a file.
   method save_file {} {
      $Toolbox_ save_file
   }

   #  Read an ARD description (in LessARD format) from a file.
   method read_file {} {
      $Toolbox_ read_file
   }

   #  Display statistics for regions.
   method stats {mode} {
      #  First save the current description to a file
      incr count_
      set tempfiles_($count_) "StarArdIn${count_}.Dat"
      set tmpfile_ $tempfiles_($count_)
      if { $mode == "all" } {
         set ok [$Toolbox_ save_description $tmpfile_]
      } else {
         set ok [$Toolbox_ save_selected_description $tmpfile_]
      }
      if { $ok } {
         #  Now startup the Ardstat application.
         if { $ardstat_ == {} } {
	     global env gaia_library
	     set path [file dirname [info nameofexecutable]]
	     set app ""
	     foreach dir [list $gaia_library $path] {
		 if {[file exists $dir/ardstat]} {
		     set app $dir/ardstat
		 }
	     }
	     if {"$app" == ""} {
		 error_dialog "Can't find the ardstat executable in $gaia_library or $path"
		 return
	     }
	     set ardstat_ [StarApp \#auto -application \
                             $app \
                             -notify [code $this measured_stats_] \
                             -show_output $itk_component(statsresults)]
         }

         #  Get the name of the current image (.sdf is special and needs
         #  to be stripped).
         set image [$itk_option(-rtdimage) cget -file]
         if { $image != "" } {
            lassign [fileName $image] image slice
            if { [file extension $image] == ".sdf" } {
               set image "[file rootname $image]${slice}"
            }

            #  And ARDSTAT on the image and file.
            blt::busy hold $w_
            update idletasks
            $ardstat_ runwith in=$image simple=f \
               oneline=t region=^${tmpfile_}
         } else {
            error_dialog "No image is displayed"
         }
      } else {
         if { $mode == "all" } {
	       error_dialog "No regions are defined"
         } else {
            error_dialog "No regions are selected"
         }
         unset tempfiles_($count_)
         incr count_ -1
      }
   }

   #  When the statistics of a region are measured do nothing but add
   #  a blank line the scrollbox is updated automatically.
   private method measured_stats_ {} {
      blt::busy release $w_
      $itk_component(statsresults) insert end " "
      $itk_component(statsresults) see end
   }

   #  Blank out regions and display in a new clone.
   method blank {mode} {

      #  First save the current description to a file
      incr count_
      set tempfiles_($count_) "StarArdIn${count_}.Dat"
      set tmpfile_ $tempfiles_($count_)
      if { $mode == "all" } {
         set ok [$Toolbox_ save_description $tmpfile_]
      } else {
         set ok [$Toolbox_ save_selected_description $tmpfile_]
      }
      if { $ok } {

         #  Now startup the Ardmask application.
         if { $ardmask_ == {} } {
            global env gaia_library
            set ardmask_ [StarApp \#auto -application \
                             $env(KAPPA_DIR)/ardmask \
                             -notify [code $this modified_image_]]
         }

         #  Get the name of the current image (.sdf is special and needs
         #  to be stripped).
         set image [$itk_option(-rtdimage) cget -file]
         if { $image != "" } {
            set ext ""
            lassign [fileName $image] image slice
            if { [file extension $image] == ".sdf" } {
               set image "[file rootname $image]${slice}"
               set ext ".sdf"
            }

            #  Create a temporary file name.
            set tmpimage_ "StarArdImg${count_}"

            #  And run ARDMASK on the image and file.
            blt::busy hold $w_
            $ardmask_ runwith in=$image ardfile=$tmpfile_ out=$tmpimage_
         } else {
            error_dialog "No image is displayed"
         }
      } else {
         if { $mode == "all" } {
            error_dialog "No regions are defined"
         } else {
            error_dialog "No regions are selected"
         }
         unset tempfiles_($count_)
         incr count_ -1
      }
   }

   #  Extract regions and display in a new clone.
   method extract {mode} {

      #  First save the current description to a file
      incr count_
      set tempfiles_($count_) "StarArdIn${count_}.Dat"
      set tmpfile_ $tempfiles_($count_)
      if { $mode == "all" } {
         set ok [$Toolbox_ save_description $tmpfile_]
      } else {
         set ok [$Toolbox_ save_selected_description $tmpfile_]
      }
      if { $ok } {

         #  Need to invert the sense of the ARD region for extraction.
	   set f [::open $tmpfile_ r]
	   set contents [::read $f]
	   ::close $f
	   set f [::open $tmpfile_ w]
	   puts -nonewline $f {.NOT.(}
           puts -nonewline $f $contents
           puts $f {)}
	   ::close $f

	   #  Now startup the Ardmask application.
	   if { $ardmask_ == {} } {
              global env gaia_library
              set ardmask_ [StarApp \#auto -application \
                               $env(KAPPA_DIR)/ardmask \
                               -notify [code $this modified_image_]]
	   }

	   #  Get the name of the current image (.sdf is special and needs
	   #  to be stripped).
	   set image [$itk_option(-rtdimage) cget -file]
	   if { $image != "" } {
              set ext ""
              lassign [fileName $image] image slice
              if { [file extension $image] == ".sdf" } {
                 set image "[file rootname $image]${slice}"
                 set ext ".sdf"
              }

              #  Create a temporary file name.
              incr count_
              set tmpimage_ "StarArdImg${count_}"

              #  And run ARDMASK on the image and file.
              blt::busy hold $w_
              $ardmask_ runwith in=$image ardfile=$tmpfile_ out=$tmpimage_
	   } else {
              error_dialog "No image is displayed"
	   }
        } else {
	   if { $mode == "all" } {
              error_dialog "No regions are defined"
	   } else {
              error_dialog "No regions are selected"
	   }
	   unset tempfiles_($count_)
	   incr count_ -1
        }
   }

   #  Crop the displayed image removing any padding pixels.
   method crop {} {

      #  Now startup the autocrop application.
      if { $autocrop_ == {} } {
	  global env gaia_library
	  set path [file dirname [info nameofexecutable]]
	  set app ""
	  foreach dir [list $gaia_library $path] {
	      if {[file exists $dir/autocrop]} {
		  set app $dir/autocrop
	      }
	  }
	  if {"$app" == ""} {
	      error_dialog "Can't find the autocrop executable in $gaia_library or $path"
	      return
	  }
	  set autocrop_ [StarApp \#auto -application \
                           $app \
                           -notify [code $this modified_image_]]
      }

      #  Get the name of the current image (.sdf is special and needs
      #  to be stripped).
      set image [$itk_option(-rtdimage) cget -file]
      if { $image != "" } {
         set ext ""
         lassign [fileName $image] image slice
         if { [file extension $image] == ".sdf" } {
            set image "[file rootname $image]${slice}"
            set ext ".sdf"
         }

         #  Create a temporary file name.
         incr count_
         set tmpimage_ "StarArdImg${count_}"

         #  And run autocrop on the image and file.
         blt::busy hold $w_
         $autocrop_ runwith in=$image out=$tmpimage_
      } else {
         error_dialog "No image is displayed"
      }
   }

   #  Display the extracted/blanked image. This is either in a clone
   #  or in the existing display.
   private method modified_image_ {} {
      set file ""
      if { ! [file readable $tmpimage_] } {
         if { ! [file readable ${tmpimage_}.sdf] } {
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
            set clone [$itk_option(-gaia) clone $file -temporary 1]
         }
      }
      blt::busy release $w_
   }

   #  Deal with replace option when changed by Option checkbutton.
   private method replace_changed_ {} {
      configure -replace $replace_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of a StarCanvasDraw widget to use to control objects.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {}

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

   #  Name of temporary files created.
   protected variable tempfiles_

   #  Name of image being created.
   protected variable tmpimage_ {}

   #  Common variables: (shared by all instances)
   #  -----------------
   #  Count of temporary files created by this class.
   common count_ 0

   #  New replace option value (from checkbutton)
   common replace_

#  End of class definition.
}
