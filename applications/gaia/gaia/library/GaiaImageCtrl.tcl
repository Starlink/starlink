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
#     the SkyCatCtrl class that are required for use by GAIA. See the
#     individual method and options for details.

#  Invocation:
#     GaiaImageCtrl name [configuration options]

#  Notes:
#     This will only run with the gaia_wish installed as part of the
#     GAIA package.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  Inherits:
#     Methods and configuration options of SkyCatCtrl.

#  Copyright:
#     Copyright (C) 1998-1999 Central Laboratory of the Research Councils

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
#     26-FEB-1999 (PDRAPER):
#        Merged GaiaImage into this class. This removes the need to
#        modify RtdImageCtrl. All code relating to float_panel is
#        removed as are changes for with_warp.
#     {enter_changes_here}

#-

itk::usual GaiaImageCtrl {}

itcl::class gaia::GaiaImageCtrl {
   inherit skycat::SkyCatCtrl

   #  Constructor: create a new instance of this class.
   constructor {args} {

      #  Record toplevel window name.
      set top_ [winfo toplevel $w_]

      #  Add a bindtag to the canvas so we can add bindings that will
      #  not be changed by others.
      set tags [bindtags $canvas_]
      lappend tags mytag$this
      bindtags $canvas_ $tags

      #  Remove options we're overriding from base classes.
      itk_option remove rtd::RtdImage::show_object_menu
      itk_option remove rtd::RtdImage::drag_scroll
      itk_option remove rtd::RtdImage::file

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

      if { $after_id_ != {} } {
         catch {after cancel $after_id_}
      }
   }

   #  This method is called from the base class (TopLevelWidget) after all
   #  the options have been evaluated
   protected method init {} {
      skycat::SkyCatCtrl::init

      #  Add image band control (overrides Rtd version by resetting
      #  the image bindings).
      gaia::GaiaImageMBand $w_.newmband \
         -image $this \
         -defaultcursor $itk_option(-cursor)
   }


   #  Make the panel info subwindow. Override to use GaiaImagePanel,
   #  rather than RtdImagePanel. Also remove the make_grid_item capability.
   protected method make_panel_info {panel} {
      #  Add info panel
      feedback "info panel..."
        
      # Info panel, GaiaImagePanel object used to display image controls
      itk_component add info {
         gaia::GaiaImagePanel $panel.info \
            -image $this \
            -state disabled \
            -min_scale $itk_option(-min_scale) \
            -max_scale $itk_option(-max_scale) \
            -shorthelpwin $itk_option(-shorthelpwin) \
            -borderwidth 3 -relief groove
      }
      if { $itk_option(-float_panel) } {
         set side bottom
      } else {
         set side left
      }
      pack $itk_component(info)  \
         -side $side -fill both -expand 1

      #  Take opportunity to stop floating panel from being destroyed.
      #  Using {} as command isn't enough.
      if { $itk_option(-float_panel) } {
         wm protocol $panel WM_DELETE_WINDOW [code $this do_nothing_]
      }

      #  Make sure that the $panel.zoom.dozoom variable is always set
      #  (if switch off as a option then this isn't the case which
      #  causes problems with hide_control_panel).
      global ::$itk_component(panel).zoom.dozoom
      set $itk_component(panel).zoom.dozoom $itk_option(-dozoom)
   }
   private method do_nothing_ {} {
   }

   #  Display the toolbox window (override to use StarCanvasDraw,
   #  instead of CanvasDraw).
   protected method make_toolbox {} {
      itk_component add draw {
         gaia::StarCanvasDraw $w_.draw \
            -canvas $canvas_ \
            -transient 1 \
            -center 0 \
            -withdraw 1 \
            -clipping 0 \
            -shorthelpwin $itk_option(-shorthelpwin) \
            -withtoolbox $itk_option(-withtoolbox) \
            -defaultcursor $itk_option(-cursor) \
            -show_object_menu $itk_option(-show_object_menu) \
            -rtdimage $image_ \
            -lowestitem $imageId_ \
            -regioncommand $itk_option(-regioncommand) \
            -ignore_tag $itk_option(-ast_tag)
      }

      set_drawing_area

      # Clicking on the image or image background deselects other objects.
      $canvas_ bind $image_ <1> [code $itk_component(draw) deselect_objects]
   }

   #  This method is redefined here to also rescale pixel-width
   #  objects correctly.
   public method scale {x y} {
      rtd::RtdImageCtrl::scale $x $y
      $itk_component(draw) pixel_width_changed
   }

   #  Toggle rotation of the image and canvas items. Extended to add
   #  astrometry grid update.
   public method rotate {bool} {
      if {$bool != [$image_ rotate]} {
         rtd::RtdImage::rotate $bool

         #  Notify the astrometry grid to re-display itself if
         #  asked.
         if { $itk_option(-grid_command) != {} } {
            eval $itk_option(-grid_command)
         }
      }
   }

   #  Flip or unflip the image and canvas items about the
   #  x or y axis, as given by $xy. Extended to add astrometry grid
   #  update.
   public method flip {xy bool} {
      if {$bool != [$image_ flip $xy]} {
         rtd::RtdImage::flip $xy $bool

         #  Notify the astrometry grid to re-display itself if
         #  asked.
         if { $itk_option(-grid_command) != {} } {
            eval $itk_option(-grid_command)
         }
      }
   }

   #  Arrange to interactively create a spectrum line to display
   #  a graph of the image values along a given line. Changed to not
   #  prompt when wanted.
   public method spectrum {{showinfo 1}} {
      if {[$image_ isclear]} {
         warning_dialog "No image is currently loaded" $w_
         return
      }

      if {[winfo exists $w_.spectrum]} {
         $w_.spectrum quit
      }

      if { $showinfo} {
         set ok [action_dialog \
                    "Press OK and then drag out a line over the image with button 1" \
                    $w_]
      } else {
         set ok 1
      }
      if { $ok } {
         $itk_component(draw) set_drawing_mode line [code $this make_spectrum]
      }
   }

   #  Display a dialog for selecting objects in the image and
   #  displaying information about the selected area of the
   #  image. Override to use GaiaImagePick, which adds the ability to
   #  save the information into a disk file (GaiaPick.dat).
   public method pick_dialog {{command ""}} {
      if {[$image_ isclear]} {
         warning_dialog "No image is currently loaded" $w_
         return
      }
      utilReUseWidget gaia::GaiaImagePick $w_.pick \
         -target_image $this \
         -command $command \
         -verbose $itk_option(-verbose) \
         -orient $itk_option(-pickobjectorient) \
         -debug $itk_option(-debug) \
         -shorthelpwin $itk_option(-shorthelpwin)
      $w_.pick pick_object
   }

   #  Make a hard copy of the image display, just override use
   #  GaiaImagePrint, remove ESO references and reduce the page width
   #  slightly (not A4?).
   public method print {} {
        if {[$image_ isclear]} {
            warning_dialog "No image is currently loaded" $w_
            return
        }
        set object [$image_ object]
        set file [file tail $itk_option(-file)]
        set center [$image_ wcscenter]
        set user [id user]
        set app [lindex [winfo name .] 0]
        set date [clock format [clock seconds] -format {%b %d, %Y at %H:%M:%S}]
        utilReUseWidget gaia::GaiaImagePrint $w_.print \
            -image $this \
            -show_footer 1 \
            -whole_canvas 0 \
            -transient 1 \
            -pagewidth 8.1i \
            -top_left  "GAIA::Skycat\n$object" \
            -top_right "$file\n$center" \
            -bot_left  "$user" \
            -bot_right "$date"
   }

   #  Create a graph to display the image data values along the line
   #  just created.
   #  "line_id" is the canvas id of the line.
   #  Extended to call derived class that also saves slice as an
   #  image.
   public method make_spectrum {line_id x0 y0 x1 y1} {
      if {[winfo exists $w_.spectrum]} {
         $w_.spectrum quit
      }
      gaia::GaiaImageSpectrum $w_.spectrum \
         -x0 [expr int($x0)] \
         -y0 [expr int($y0)] \
         -x1 [expr int($x1)] \
         -y1 [expr int($y1)] \
         -image $this \
         -transient 1 \
         -shorthelpwin $itk_option(-shorthelpwin) \
         -line_id $line_id
   }

   #  Methods to deal with the autoscroll when dragging off canvas.
   protected method start_autoscan_ {x y} {
      set movex 0
      set movey 0
      if { $y >= [winfo height $canvas_]} {
         set movey 10
      } elseif {$y < 0} {
         set movey -10
      } elseif { $x >= [winfo width $canvas_]} {
         set movex 10
      } elseif {$x < 0} {
         set movex -10
      }
      autoscan_ $movex $movey
   }
   protected method autoscan_ {movex movey} {
      $canvas_ yview scroll $movey units
      $canvas_ xview scroll $movex units
      set after_id_ [after 50 [code $this autoscan_ $movex $movey]]
   }
   protected method cancelrepeat_ {} {
      after cancel $after_id_
      set after_id_ {}
   }

   #  Update the toplevel window header and icon name to include the name
   #  of the file being displayed.
   protected method update_title {} {
      set file "[file tail $itk_option(-file)]"
      set w [winfo toplevel $w_]
      wm title $w "GAIA::Skycat: $file ([$w cget -number])"
      wm iconname $w $file
   }

   #  Add a generated image to display the colors in the colormap
   #  (this is packed differently from the main method so the colour
   #  map remains visible more often).
   public method make_colorramp {} {
      itk_component add colorramp {
         rtd::RtdImageColorRamp $w_.colorramp \
            -height $itk_option(-colorramp_height) \
            -viewmaster $image_
      }
      pack $itk_component(colorramp) -side bottom -fill x \
         -before $itk_component(imagef)
   }

   #  This method is called by the image code whenever a new image is loaded.
   public method new_image_cmd {} {
      skycat::SkyCatCtrl::new_image_cmd

      #  Remove old temporary file, if not already done.
      maybe_delete_
      delete_temporary_

      #  Record this name, until another new image is set.
      set last_file_ $itk_option(-file)
   }

   #  Open and load a new image file via file name dialog. Added the
   #  ability to deal with a list of possible file extensions and a
   #  possible image slice to this method.
   public method open {{dir "."} {pattern "*.*"}} {

      set file [get_file_ $dir $pattern $itk_option(-file_types)]

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

   
   #  Reload the image file, if there is one
   #  (redefined from parent class, since we use different mmap flags here
   #  that cause the inherited version to not work).
   public method reopen {} {
       set file [$image_ cget -file]
       if {"$file" != ""} {
	   $image_ configure -file $file
       } else {
	   $image_ update
       }
   }

   #  Save the current image to a file in FITS format chosen from a
   #  file name dialog (added file patterns and .fit as default
   #  extension and update to temporary status).
   public method save_as {{dir "."} {pattern "*.*"}} {
      if {[$image_ isclear]} {
         warning_dialog "No image is currently loaded" $w_
         return
      }
      #  Special case: if input file is a FITS file then we can only
      #  save it as a fits file. NDFs may be saved as other formats.
      set exten [file extension $itk_option(-file)]
      if {  $exten == ".fit" || $exten == ".fits" } {
         set file [get_file_ $dir $pattern {{any * } {FITS *.fits} {FIT *.fit}}]
      } else {
         set file [get_file_ $dir $pattern $itk_option(-file_types)]
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

   #  Get filename using fileselection dialog. This is created once and
   #  retains the current name and filters when repeatably accessed.
   protected method get_file_ {dir pattern types} {
       if { ! [winfo exists $fileselect_] } {
	   set fileselect_ [FileSelect $w_.select -dir $dir -filter $pattern \
				-transient 1 -withdraw 1 -filter_types $types]
	   wm transient $fileselect_ [winfo toplevel $w_]
       }
       if {[$fileselect_ activate]} {
	   return [$fileselect_ get]
       }
   }

   #  Set the cut levels.
   public method set_cut_levels {} {
      if {[$image_ isclear]} {
         warning_dialog "No image is currently loaded" $w_
         return
      }
      utilReUseWidget gaia::GaiaImageCut $w_.cut \
         -image $this \
         -transient 1 \
         -shorthelpwin $itk_option(-shorthelpwin) \
         -command [code $itk_component(info) updateValues]
   }

   #  Clear the current image display and remove an windows that
   #  access it (extend parent class version to also deal with 
   #  temporary images).
   public method clear {} {

      #  If this window this previously displayed a temporary image
      #  then delete it.
      maybe_delete_
      delete_temporary_

      #  Really clear.
      skycat::SkyCatCtrl::clear
   }

   #  Load a FITS file (internal version: use -file option/public
   #  variable), modified to deal with image slices.
   public method load_fits_ {} {
      #  Deal with any slice specification.
      set image $itk_option(-file)
      set i1 [string last {(} $image]
      set i2  [string last {)} $image]
      if { $i1 > -1 && $i2 > -1 } {
         incr i1 -1
         set image [string range $image 0 $i1]
      }
      if {[file exists $image] || "$itk_option(-file)" == "-"} {
         set old_width [$image_ width]
         set old_height [$image_ height]
         busy {
            set center_ok_ 0
            if {[catch {$image_ config -file $itk_option(-file) \
                           -component $itk_option(-component)} msg]} {
               error_dialog $msg $w_
               clear
            }
            set center_ok_ 1
         }
         #  Center if image has changed size.
         if { $old_width != [$image_ width] ||
              $old_height != [$image_ height] } {
            center
         }
         set w [$image_ dispwidth]
         set h [$image_ dispheight]
         set_scrollregion 0 0 $w $h
      } else {
         error_dialog "'$itk_option(-file)' does not exist" $w_
         set file ""
         clear
      }
      update_title
   }

   #  Check if any other instance of this class is displaying the
   #  current image (used when deciding to delete file, shared
   #  temporary files are retained until all instances are released).
   private method only_user_ {} {
      global ::tcl_version
      if { $tcl_version >= 8.0 } { 
         foreach inst [itcl::find objects "*" -isa "GaiaImageCtrl"] {
            if { $inst != $this } {
               if { $last_file_ == [$inst cget -file] } {
                  return 0
               }
            }
         }
      } else {
         foreach inst [info objects "*" -isa "GaiaImageCtrl"] {
            if { $inst != $this } {
               if { $last_file_ == [$inst cget -file] } {
                  return 0
               }
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
            regsub {\.gaia} $top_ {} clone
            set d [DialogWidget .#auto \
                      -title {Temporary image} \
                      -text "The image ($last_file_) that was displayed in \
                             window GAIA::Skycat: ($clone) is marked \
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
   #  override to remove SkyCat splash logo.
   public method about {} {
      global ::about_skycat
      DialogWidget $w_.about \
         -messagewidth 6i \
         -justify center \
         -text $about_skycat
      $w_.about activate
   }

   #  Center the image in the canvas window. Override to switch off
   #  when attempting to load a new image (preserves scroll position,
   #  which is often what is really required).
   public method center {} {
      if { $center_ok_ } {
         rtd::RtdImage::center
      }
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

   #  Is image temporary. If set after image is displayed/configured
   #  then the associated file will be deleted when replaced or when this
   #  object is deleted.
   itk_option define -temporary teMpoRaRy TeMpoRaRy 0

   #  Names and extensions of known data types.
   itk_option define -file_types file_types File_Types \
      {{any *} {ndf *.sdf} {fits *.fit*}}

   #  Flag: if true, set bindings to scroll with the middle mouse
   #  button and make a depressed mouse button drag scroll the image.
   #  Note we use the mytag$this level tag for the cancel event as
   #  ButtonRelease-1 is used in other places.
   itk_option define -drag_scroll drag_scroll Drag_scroll 0 {
      if {$itk_option(-drag_scroll)} {
         bind $canvas_ <2> [code $canvas_ scan mark %x %y]
         bind $canvas_ <B2-Motion> [code $canvas_ scan dragto %x %y]
         set after_id_ {}
         bind mytag$this <ButtonRelease-1> [code $this cancelrepeat_]
         bind $canvas_ <B1-Leave> [code $this start_autoscan_ %x %y]
         bind $canvas_ <B1-Enter> [code $this cancelrepeat_]
      } else {
         bind $canvas_ <2> {}
         bind $canvas_ <B2-Motion> {}
         bind $canvas_ <B1-Leave> {}
         bind $canvas_ <B1-Enter> {}
      }
   }

   #  Flag: if true, display menus over graphic objects when selected with <3>
   itk_option define -show_object_menu show_object_menu Show_object_menu 1

   #  Canvas tag for AST graphics items. This is set to the global
   #  value and shouldn't normally be changed.
   itk_option define -ast_tag ast_tag Ast_Tag "ast_element"

   #  Command to re-draw the astrometry grid.
   itk_option define -grid_command grid_command Grid_Command {}

   #  Component of the NDF that is displayed.
   itk_option define -component component Component data

   #  Protected variables:
   #  ====================

   #  State of zoom.
   protected variable zoom_state_ 0

   #  Name of last file.
   protected variable last_file_ {}

   #  Name of toplevel window (recorded for times when not otherwise available).
   protected variable top_ {}

   #  Control use of center method by sub-classes.
   protected variable center_ok_ 1

   #  Id of after event (used to autoscroll image).
   protected variable after_id_ {}

   #  Common variables:
   #  =================

   #  Name of fileselection dialog window. Shared between all instances.
   common fileselect_ .imagectrlfs
}



