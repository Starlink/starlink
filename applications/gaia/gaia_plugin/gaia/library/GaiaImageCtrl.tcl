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
#        Removed commented out sections.
#     {enter_changes_here}

#-

itk::usual GaiaImageCtrl {}

itcl::class gaia::GaiaImageCtrl {
   inherit skycat::SkyCatCtrl

   #  Constructor: create a new instance of this class.
   constructor {args} {
      eval itk_initialize $args
   }

   #  Destructor.
   destructor {
   }

   # make the panel info subwindow
    
   method make_panel_info {panel} {
      #  Add info panel.
      feedback "info panel..."
      itk_component add info {
         gaia::GaiaImagePanel $panel.info \
            -image $this \
            -shorthelpwin $itk_option(-shorthelpwin) \
            -state disabled \
	    -min_scale $itk_option(-min_scale) \
	    -max_scale $itk_option(-max_scale) \
            -borderwidth 3 -relief groove
      }

      if { $itk_option(-float_panel) } {
	  set side bottom
      } else {
	  set side left
      }
      pack $itk_component(info)  \
	  -side $side -fill both -expand 1
   }

   #  Make the toolbox window (repeated to use StarCanvasDraw,
   #  instead of CanvasDraw).
   method make_toolbox {} {
       itk_component add draw {
	   gaia::StarCanvasDraw $w_.draw \
               -canvas $canvas_ \
               -transient 1 \
               -center 0 \
               -withdraw 1 \
	       -show_object_menu 1 \
	       -clipping 0 \
  	       -shorthelpwin $itk_option(-shorthelpwin) \
	       -withtoolbox $itk_option(-withtoolbox) \
               -defaultcursor $itk_option(-cursor) \
               -show_object_menu $itk_option(-show_object_menu) \
               -rtdimage $image_ \
               -lowestitem $imageId_ \
	       -regioncommand $itk_option(-regioncommand) \
	       -ignore_tag $itk_option(-grid_tag)
       }
   }

   #  Null method.
   private method do_nothing_ {} {}

   # This method is redefined here to update the scale display.
   # resize the image and the canvas graphics by the given integer factors
   # (1 is no scale, -2 = 50%, 2 = 200% etc...)
   method scale {x y} {
      RtdImageCtrl::scale $x $y
      $itk_component(draw) pixel_width_changed
   }

   #  Toggle rotation of the image and canvas items. Extended to add
   #  astrometry grid update.
   method rotate {bool} {
      if {$bool != [$image_ rotate]} {
         RtdImage::rotate $bool

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
   method flip {xy bool} {
      if {$bool != [$image_ flip $xy]} {
         RtdImage::flip $xy $bool

         #  Notify the astrometry grid to re-display itself if
         #  asked.
         if { $itk_option(-grid_command) != {} } {
            eval $itk_option(-grid_command)
         }
      }
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
         if {[file isfile $image]} {
            configure -file $file
            configure -temporary 0
         } else {
            error_dialog "There is no file named '$file'" $w_
         }
      }
   }

    
   # reload the image file, if there is one
   # (redefined from parent class, since we use different mmap flags here
   # that cause the inherited version to not work).

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
   #
   #  XXX allan: The base class version allows saving a region. Need to disable
   #             that here.
   method save_as {{dir "."} {pattern "*.*"} {x0 ""} {y0 ""} {x1 ""} {y1 ""}} {
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
         configure -temporary 0
      }
   }

   #  Clear the current image display and remove an windows that
   #  access it (extend parent class version to also configure 
   #  temporary variable and deal with float_panel changes).
   method clear {} {
      SkyCatCtrl::clear
      configure -temporary 0
   }

   #  Configuration options.
   #  ======================

   #  Debugging flag.
   itk_option define -debug debug Debug 0

   #  Floating panel option (for small displays).
   itk_option define -float_panel float_panel Float_Panel 0

   #  Is image temporary (do nothing about this just store the
   #  information), after saving and clear images are no longer
   #  temporary.
   itk_option define -temporary teMpoRaRy TeMpoRaRy 0 {}

   #  Names and extensions of known data types.
   itk_option define -file_types file_types File_Types \
      {{any *} {ndf *.sdf} {fits *.fit*}}

   #  Canvas tag for the astrometry grid items.
   itk_option define -grid_tag grid_tag Grid_Tag {} {
      imageconfig_ -grid_tag
   }

   # XXX allan: check this
   itk_option define -file_change_cmd file_change_cmd File_Change_Cmd {}

   #  Command to re-draw the astrometry grid.
   itk_option define -grid_command grid_command Grid_Command {}

   #  Component of the NDF that is displayed.
   itk_option define -component component Component data

   #  Protected variables:
   #  ====================

   #  State of zoom.
   protected variable zoom_state_ 0
}
