#+
#  Name:
#     GaiaImageCtrl.tcl

#  Purpose:
#     Defines a class for controlling an RtdImage class by adding
#     a control panel, zoom and panning windows.

#  Type of Module:
#     [incr Tk] class

#  Description:
#     This module defines a class that adds to the options defined by
#     the SkyCatCtrl class that are required for use by GAIA.  It also
#     overrides any methods used within SkyCatCtrl/RtdImageCtrl that
#     make use of RtdImage so that we can use GaiaImage instead.

#  Invocation:
#     GaiaImageCtrl name [configuration options]

#  Notes:
#     This will only run with the skycat_wish installed as part
#     of the GAIA package with a Starlink extended RTD.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  Inherits:
#     Methods and configuration options of SkyCat (and Rtd).

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  History:
#     24-SEP-1997 (PDRAPER):
#        Original version
#     15-NOV-1997 (PDRAPER):
#        Commented out code relating to grid control. This is now
#        replaced by the AST grid (the code is left in place in case a
#        comparison of the two methods is helpful).
#     05-FEB-1998 (PDRAPER):
#        Removed commented out sections, added -with_warp override.
#     07-APR-1998 (PDRAPER):
#        Added code to control temporary status of image (this was
#        previously performed at the Gaia level, which proved to be
#        problematic).
#     {enter_changes_here}

#-

itk::usual GaiaImageCtrl {}

class gaia::GaiaImageCtrl {
   inherit skycat::SkyCatCtrl

   #  Constructor: create a new instance of this class.
   constructor {args} {
      #  Record toplevel window name.
      set top_ [winfo toplevel $w_]

      #  Remove options we're overriding from base classes.
      itk_option remove RtdImage::with_warp
      itk_option remove RtdImage::file

      #  Initialise all options.
      eval itk_initialize $args

   }

   #  Destructor. Remove temporary image if necessary. Withdraw
   #  toplevel containing this window to stop view of partially
   #  destroyed windows. Note when "." is destroyed we must just
   #  accept an exit without prompting (this is what all the catches
   #  achieve).
   destructor {
      if { ![catch {wm withdraw $top_}] } {
         catch {maybe_delete_}
      }
      delete_temporary_
   }

   #  Make the control panel for operating on the image (adds the
   #  ability to make the panel float).
   method make_control_panel {} {
      if { $itk_option(-float_panel) } {
         itk_component add panel {
            toplevel $w_.panel
         }
         wm withdraw $itk_component(panel)

         #  Stop this window from being destroyed.
         wm protocol $itk_component(panel) \
            WM_DELETE_WINDOW [code $this do_nothing_]
         regsub {\.rtd} $top_ {} clone
         wm title $itk_component(panel) "Control Panel ($clone)"

      } else {
         itk_component add panel {
            frame $w_.panel
         }
         pack $itk_component(panel) -side top -fill x -before $w_.imagef
      }

      #  Zoom window.
      feedback "zoom window..."
      if {$itk_option(-with_zoom_window)} {
         # set on/off by default
         global ::$itk_component(panel).zoom.dozoom
         set $itk_component(panel).zoom.dozoom $itk_option(-dozoom)

         if {$itk_option(-use_zoom_view)} {
            itk_component add zoom {
               rtd::RtdImageZoomView $itk_component(panel).zoom \
                  -target_image $this \
                  -width $itk_option(-zoom_width) \
                  -height $itk_option(-zoom_height) \
                  -factor $itk_option(-zoom_factor) \
                  -propagate $itk_option(-zoom_view_propagate) \
                  -usexshm $itk_option(-usexshm) \
                  -borderwidth 3 \
                  -relief groove
            }
         } else {
            itk_component add zoom {
               rtd::RtdImageZoom $itk_component(panel).zoom \
                  -target_image $this \
                  -width $itk_option(-zoom_width) \
                  -height $itk_option(-zoom_height) \
                  -factor $itk_option(-zoom_factor) \
                  -usexshm $itk_option(-usexshm) \
                  -borderwidth 3 \
                  -relief groove
            }
         }

         #  Tell the base class to use this zoom window when entered.
         configure -zoomwin $itk_component(zoom)
      }

      #  Add info panel.
      feedback "info panel..."
      itk_component add info {
         gaia::GaiaImagePanel $itk_component(panel).info \
            -image $this \
            -shorthelpwin $itk_option(-shorthelpwin) \
            -state disabled \
            -borderwidth 3 -relief groove
      }

      #  Panning window.
      feedback "pan window..."
      if {$itk_option(-with_pan_window)} {
         itk_component add pan {
            rtd::RtdImagePan $itk_component(panel).pan \
               -target_image $this \
               -zoomwin $itk_option(-zoomwin) \
               -width $itk_option(-pan_width) \
               -height $itk_option(-pan_height) \
               -usexshm $itk_option(-usexshm) \
               -verbose $itk_option(-verbose) \
               -borderwidth 3 \
               -relief groove
         }
      }
      if { $itk_option(-float_panel) } {
         pack $itk_component(info) -side bottom -fill both
         if {$itk_option(-with_pan_window)} {
            pack $itk_component(pan) -side left -fill y
         }
         if {$itk_option(-with_zoom_window)} {
            pack $itk_component(zoom) -side left -fill both -expand 1
         }
         after 0 [code wm deiconify $itk_component(panel)]
      } else {
         if {$itk_option(-with_pan_window)} {
            pack $itk_component(pan) -side left -fill y
         }
         pack $itk_component(info) -side left -fill both -expand 1
         if {$itk_option(-with_zoom_window)} {
            pack $itk_component(zoom) -side left -fill y
         }
      }
   }

   #  Null method.
   private method do_nothing_ {} {}

   # update the toplevel window header and icon name to include the name
   # of the file being displayed

   method update_title {} {
      set file "[file tail $itk_option(-file)]"
      set w [winfo toplevel $w_]
      regsub {\.rtd} $w {} clone
      wm title $w "Gaia/SkyCat ($clone): $file"
      wm iconname $w $file
   }

   #  Toggle the visibility of the control panel
   #  (argument is the name of the checkbutton variable to use).
   method hide_control_panel {variable} {
      global ::$variable ::$itk_component(panel).zoom.dozoom

      if {[set $variable]} {
         #  Hide the panel, turn off the zoom window, keep the canvas width.
         if { $itk_option(-with_zoom_window) } {
            set zoom_state_ [set $itk_component(panel).zoom.dozoom]
            set $itk_component(panel).zoom.dozoom 0
         }
         set w [winfo width $canvas_]
         pack forget $itk_component(panel)
         $canvas_ config -width $w
      } else {
         #  Show the panel, restore the zoom window.
         if { $itk_option(-with_zoom_window) } {
            set $itk_component(panel).zoom.dozoom $zoom_state_
         }
         pack $itk_component(panel) -side top -fill x -before $w_.imagef
      }
   }

   #  Add a generated image to display the colors in the colormap
   #  (this is packed differently from the main method so the colour
   #  map remains visible more often).
   method make_colorramp {} {
      itk_component add colorramp {
         rtd::RtdImageColorRamp $w_.colorramp \
            -height $itk_option(-colorramp_height) \
            -viewmaster $image_
      }
      pack $itk_component(colorramp) -side bottom -fill x \
         -before $itk_component(imagef)
   }

   # This method is redefined here to update the scale display.
   # resize the image and the canvas graphics by the given integer factors
   # (1 is no scale, -2 = 50%, 2 = 200% etc...)
   method scale {x y} {
      gaia::GaiaImage::scale $x $y
      if {[info exists itk_component(zoom)]} {
         $itk_component(zoom) scale
      }
      $itk_component(info).trans update_trans
   }

   #  This method is called by the image code whenever a new image is loaded.
   method new_image_cmd {} {

      rtd::RtdImage::new_image_cmd
      if {[info exists itk_component(zoom)]} {
         $itk_component(zoom) zoom
         $itk_component(zoom) scale
      }
      if {! [$image_ isclear]} {
         $itk_component(info) config -state normal
      }
      $itk_component(info) updateValues
      if {[winfo exists $w_.cut]} {
         $w_.cut update_graph
      }

      if {[winfo exists $w_.spectrum]} {
         destroy $w_.spectrum
      }

      if {[winfo exists $w_.draw]} {
         $w_.draw set_menu_state normal
      }

      if {[winfo exists $w_.wcs_info]} {
         $w_.wcs_info configure -values [$image_ wcsset]
      }

      #  Remove old temporary file, if not already done.
      maybe_delete_
      delete_temporary_
      
      #  Record this name, until another new image is set.
      set last_file_ $itk_option(-file)
   }

   #  Open and load a new image file via file name dialog. Added the
   #  ability to deal with a list of possible file extensions and a
   #  possible image slice to this method.
   method open {{dir "."} {pattern "*.*"}} {
      set file [filename_dialog $dir $pattern $w_ $itk_option(-file_types)]

      #  Deal with any slice information.
      set image $file
      set i1 [string last {(} $file]
              set i2  [string last {)} $file]
      if { $i1 > -1 && $i2 > -1 } {
         incr i1 -1
         set image [string range $image 0 $i1]
      }
      if {"$file" != ""} {
         if { $last_file_ != "" } {
            #  Already have a displayed image. Check that we do
            #  not need to delete it, before accepting the new one.
            maybe_delete_
            delete_temporary_
         }

         if {[file isfile $image]} {
            configure -file $file
         } else {
            error_dialog "There is no file named '$file'" $w_
         }
      }
   }

   #  Save the current image to a file in FITS format chosen from a
   #  file name dialog (added file patterns and .fit as default
   #  extension and update to temporary status).
   method save_as {{dir "."} {pattern "*.*"}} {
      if {[$image_ isclear]} {
         warning_dialog "No image is currently loaded" $w_
         return
      }
      #  Special case: if input file is a FITS file then we can only
      #  save it as a fits file. NDFs may be saved as other formats.
      set exten [file extension $itk_option(-file)]
      if {  $exten == ".fit" || $exten == ".fits" } {
         set file [filename_dialog $dir $pattern $w_ {{any * } {FITS *.fits} {FIT *.fit}}]
      } else {
         set file [filename_dialog $dir $pattern $w_ $itk_option(-file_types)]
      }
      if {"$file" != ""} {
         if {[file isfile $file]} {
            if {![confirm_dialog "$file exists - Do you want to overwrite it ?" $w_]} {
               return
            }
            if {[file isdir $file]} {
               error_dialog "$file is a directory" $w_
               return
            }
         }

	 #  The WCS system will be saved as well. Try to match the
	 #  type against that of the image (a projection starting with
	 #  "digit" is taken to be a DSS map).
	 set proj [$image_ astget projection]
         set msg ""
	 if { [string match {digit*} $proj] } {
            catch { $image_ dump $file DSS } msg
	 } else {
            catch { $image_ dump $file FITS-WCS } msg
	 }
         if { $msg != "" } {
            warning_dialog $msg
         }
      }
   }

   #  Set the cut levels.
   method set_cut_levels {} {
      if {[$image_ isclear]} {
         warning_dialog "No image is currently loaded" $w_
         return
      }
      utilReUseWidget gaia::GaiaImageCut $w_.cut \
         -image $this \
         -transient 1 \
         -command [code $itk_component(info) updateValues]
   }

   #  Clear the current image display and remove an windows that
   #  access it (extend parent class version to also configure
   #  temporary variable and deal with float_panel changes).
   method clear {} {
      #  If this window this previously displayed a temporary image
      #  then delete it.
      maybe_delete_
      delete_temporary_

      rtd::RtdImage::clear
      $itk_component(info) config -state disabled
      if {[winfo exists $w_.cut]} {
         destroy $w_.cut
      }
      if {[winfo exists $w_.draw]} {
         $w_.draw set_menu_state disabled
         wm withdraw $w_.draw
      }

   }

   #  Load a FITS file (internal version: use -file option/public
   #  variable), modified to deal with image slices.
   method load_fits_ {} {
      #  Deal with any slice specification.
      set image $itk_option(-file)
      set i1 [string last {(} $image]
      set i2  [string last {)} $image]
      if { $i1 > -1 && $i2 > -1 } {
         incr i1 -1
         set image [string range $image 0 $i1]
      }
      if {[file exists $image] || "$itk_option(-file)" == "-"} {
         busy {
            if {[catch {$image_ config -file $itk_option(-file) \
                           -component $itk_option(-component)} msg]} {
               error_dialog $msg $w_
               clear
            }
         }
         set w [$image_ dispwidth]
         set h [$image_ dispheight]
         set_scrollregion 0 0 $w $h
         update_title
      } else {
         error_dialog "'$itk_option(-file)' does not exist" $w_
         set file ""
         clear
      }
   }

   #  Check if any other instance of this class is displaying the
   #  current image (used when deciding to delete file, shared
   #  temporary files are retained until all instances are released).
   private method only_user_ {} {
      foreach inst [info objects "*" -isa "GaiaImageCtrl"] {
         if { $inst != $this } {
            if { $last_file_ == [$inst cget -file] } {
               return 0
            }
         }
      }
      return 1
   }

   #  See if user wants to change the temporary status of image
   #  before exit etc.
   private method maybe_delete_ {} {
      if { $itk_option(-temporary) && $last_file_ != {} } {

         #  Last displayed File is temporary, need to check that no
         #  other window has an interest in it.
         if { [only_user_] } {
            raise $w_
            regsub {\.rtd} $top_ {} clone
            set d [DialogWidget .#auto \
                      -title {Temporary image} \
                      -text "The image ($last_file_) that was displayed in \
                             window GAIA/SkyCat ($clone) is marked \
                             temporary.\n\Are you sure you want to delete it?"\
                      -buttons [list Yes No]]
            set answer [$d activate]
            if { $answer } {
               configure -temporary 0
               info_dialog "The image is stored in file $last_file_. \
                            You should rename this immediately."
            }
         }
      }
   }

   #  Delete the image if temporary. Done at exit, or when image is replaced.
   private method delete_temporary_ {} {
      if { $itk_option(-temporary) && $last_file_ != {} } {
         if { [only_user_] } {
            puts stderr "Information: deleting $last_file_"
            catch { file delete $last_file_ }
            set last_file_ {}
            configure -temporary 0
         }
      }
   }

   #  Display a popup window with information about this application,
   #  override to remove SkyCat splash logo (must merge GAIA sometime!).
   public method about {} {
      global ::about_skycat
      DialogWidget $w_.about \
         -messagewidth 6i \
         -justify center \
         -text $about_skycat
      $w_.about activate
    }

   #  Configuration options.
   #  ======================

   #  Fits image file to display. Added file change call back and
   #  initialisation of temporary status.
   itk_option define -file file File {} {
      if { $last_file_ != {} } {
         maybe_delete_
         delete_temporary_
      }
      if {"$itk_option(-file)" != ""} {

         #  This code makes it easier to center the image on startup.
         if {[winfo width $w_] <= 1} {
            after 0 [code $this load_fits_]
         } else {
            load_fits_
         }
         if { $itk_option(-file_change_cmd) != "" } {
            eval $itk_option(-file_change_cmd) $itk_option(-file)
         }
      }
      set $itk_option(-temporary) 0
   }
   itk_option define -file_change_cmd file_change_cmd File_Change_Cmd {}

   #  Debugging flag.
   itk_option define -debug debug Debug 0

   #  Floating panel option (for small displays).
   # itk_option define -float_panel float_panel Float_Panel 0

   #  Is image temporary. If set after image is displayed/configured
   #  then the associated file will be deleted when replaced or when this
   #  object is deleted.
   itk_option define -temporary teMpoRaRy TeMpoRaRy 0

   #  Names and extensions of known data types.
   itk_option define -file_types file_types File_Types \
      {{any *} {ndf *.sdf} {fits *.fit*}}

   #  Whether up, down, left and right arrows warp the cursor.
   itk_option define -with_warp with_warp With_Warp 1

   #  Protected variables:
   #  ====================

   #  State of zoom.
   protected variable zoom_state_ 0

   #  Name of last file.
   protected variable last_file_ {}

   #  Name of toplevel window (recorded for times when not otherwise available).
   protected variable top_ {}
}

