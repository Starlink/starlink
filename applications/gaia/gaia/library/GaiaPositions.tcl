#+
#  Name:
#     GaiaPositions

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Get list of positions from an image.

#  Description:
#     This class creates objects for creating a local catalogue of
#     positions from objects identified on the image. It also presents
#     image quality information determined from the list of positions
#     (this includes FWHM to estimate the global seeing).

#  Invocations:
#
#        GaiaPositions object_name [configuration options]
#
#     This creates an instance of a GaiaPositions object. The return is
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
#     See itk_option definitions.

#  Methods:
#     See individual method declarations.

#  Inheritance:
#     This object inherits GaiaSearch

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006-2007 Particle Physics & Astronomy Research Council.
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
#     23-MAR-2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPositions {}

itcl::class gaia::GaiaPositions {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Select positions ($itk_option(-number))"

      #  Create the short help window.
      make_short_help

      #  Add the menus.
      add_menus_

      #  Add the table for displaying the choosen positions.  Also
      #  adds to the edit menu and the markers menu which controls the
      #  appearance of the graphics markers.
      itk_component add table {
         gaia::GaiaPosTable $w_.table \
            -editmenu [get_menu Edit] \
            -markmenu [get_menu Markers] \
            -labelmenu [get_menu Labels] \
            -showmsize 0 \
            -rtdimage $itk_option(-rtdimage) \
            -canvas $itk_option(-canvas) \
            -image $itk_option(-image)
      }
      add_short_help $itk_component(table) \
         {Table of coordinates and associated values}
      pack $itk_component(table) -side top -fill both -expand 1

      #  Add image quality controls.
      add_image_quality_

      #  Create the actions toolbar.
      itk_component add actions {frame $w_.actions}
      pack $itk_component(actions) -fill x -side top -pady 3 -padx 3

      #  Add button to add a new position/object.
      itk_component add add {
         button $itk_component(actions).add -text Add \
            -command [code $this add]
      }
      add_short_help $itk_component(add) \
         {Add new position to table}
      pack $itk_component(add) -side left -expand 1 -pady 2 -padx 2

      #  Add button to stop adding new objects.
      itk_component add stop {
         button $itk_component(actions).stop -text "Stop" \
            -command [code $this stop]
      }
      add_short_help $itk_component(stop) \
         {Stop adding positions to table}
      pack $itk_component(stop) -side left -expand 1 -pady 2 -padx 2
      $itk_component(stop) configure -state disabled

      #  Update button for stats on all objects
      itk_component add update {
         button $itk_component(actions).update -text {Update all}\
            -command [code $this update_stats all]
      }
      add_short_help $itk_component(update) \
         {Update image quality statistics for all objects}
      pack $itk_component(update) -side left -expand 1 -pady 2 -padx 2

      #  Update button for stats on selected objects.
      itk_component add updatesel {
         button $itk_component(actions).updatesel -text {Update selected}\
            -command [code $this update_stats selected]
      }
      add_short_help $itk_component(updatesel) \
         {Update image quality statistics for selected objects}
      pack $itk_component(updatesel) -side left -expand 1 -pady 2 -padx 2

      #  Add a button to close window
      itk_component add close {
         button $itk_component(actions).close -text Close \
            -command [code $this close]
      }
      add_short_help $itk_component(close) \
         {Close window}
      pack $itk_component(close) -side left -expand 1 -pady 2 -padx 2

      #  Initialisations after widget creation.
      auto_centroid_changed_
      configure -iqesize $itk_option(-iqesize)
   }

   #  Destructor:
   #  -----------
   destructor  {
      #  Stop the adding mode interaction.
      stop

      if { $importer_ != {} && [winfo exists $importer_] } {
         catch {delete object $importer_}
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

   #  Just close the window, removing associated graphics.
   public method close {} {

      #  Stop the adding mode interaction.
      stop

      #  Add binding to restore contents and store them.
      bind [winfo toplevel $w_] <Map> [code $this reappears_]
      set oldcontents_ [$itk_component(table) get_contents]

      #  Clear table (to remove the markers)
      $itk_component(table) clear_table
      wm withdraw $w_
   }

   #  Recover table contents if deiconified.
   protected method reappears_ {} {
      if { $oldcontents_ != {} } {
         eval $itk_component(table) set_contents $oldcontents_

         #  Remove binding to restore contents (stop repeated calls).
         set oldcontents_ {}
         bind [winfo toplevel $w_] <Map> {}
      }
   }

   #  Redraw the table (external change, like image is flipped).
   public method redraw {} {
      $itk_component(table) redraw
   }

   #  Add menus to menubar.
   protected method add_menus_ {} {

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0
      add_short_help $itk_component(menubar).file {File menu: close window}

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Import positions from a text file.
      $File add command -label {Import text file...} \
         -command [code $this import_text_] \
         -accelerator {Control-m}
      bind $w_ <Control-m> [code $this import_text_]
      $short_help_win_ add_menu_short_help $File \
         {Import text file...}\
         {Import positions from a plain text file}

      #  And the options to save positions and re-read from file.
      $File add command -label {Write positions to file...} \
         -command [code $this save_positions_] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this save_positions_]
      $short_help_win_ add_menu_short_help $File \
         {Write positions to file...}\
         {Write the displayed positions out to an ordinary text file}

      $File add command -label {Read positions from a file...} \
         -command [code $this read_positions_] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_positions_]
      $short_help_win_ add_menu_short_help $File \
         {Read positions from a file...} \
      {Read a suitable set of positions from an ordinary text file}

      #  Set the exit menu item
      $File add command -label {Exit} \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add window help.
      add_help_button positions "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Edit menu (also filled by GaiaPosTable).
      set Edit [add_menubutton Edit]

      #  Add celestial coordinate precession toolbox.
      $Edit add command -label {Precess sky coordinates...} \
         -command [code $this precess_] \
         -accelerator {Control-p}
      bind $w_ <Control-p> [code $this precess_]
      $short_help_win_ add_menu_short_help $Edit {Precess sky coordinates...} \
         {Transform celestial coordinates to image system}

      #  Options menu.
      set Options [add_menubutton Options]
      add_short_help $itk_component(menubar).options {Set additional options}

      #  Add option to switch off automatic centroiding.
      set values_($this,autoc) 1
      $Options add checkbutton \
         -label {Auto centroid} \
         -variable [scope values_($this,autoc)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this auto_centroid_changed_]
      $short_help_win_ add_menu_short_help $Options {Auto centroid} \
         {Toggle automatical centroid of initial positions}

      #  Add option to define a different centroid maxshift/search box.
      set values_($this,isize) $itk_option(-isize)
      $Options add cascade -label {Centroid search box} \
         -menu [menu $Options.isize]
      $short_help_win_ add_menu_short_help $Options {Centroid search box} \
         {Change the search box used when locating centroids (pixels)}
      for {set i 3} {$i < 22} {incr i} {
         $Options.isize add radiobutton \
            -value $i \
            -label $i \
            -variable [scope values_($this,isize)] \
            -command [code $this configure -isize $i]
      }

      set values_($this,maxshift) $itk_option(-maxshift)
      $Options add cascade -label {Centroid max shift} \
         -menu [menu $Options.maxshift]
      $short_help_win_ add_menu_short_help $Options {Centroid max shift} \
         {Change the maximum shift from initial position (pixels)}

      for {set i 3.5} {$i < 22} {set i [expr $i+1.0]} {
         $Options.maxshift add radiobutton \
            -value $i \
            -label $i \
            -variable [scope values_($this,maxshift)] \
            -command [code $this configure -maxshift $i]
      }

      #  Markers menu (filled by GaiaPosTable).
      add_menubutton Markers

      #  Label control menu (filled by GaiaPosTable).
      add_menubutton Labels
   }

   #  Add controls for image quality estimates.
   protected method add_image_quality_ {} {
      itk_component add rule {
         gaia::LabelRule $w_.rule -text "Image Quality:"
      }
      pack $itk_component(rule) -side top -fill x

      #  Size of region used to fit objects.
      set lwidth 14
      itk_component add size {
         util::LabelEntryScale $w_.size \
            -text {Size of fit region:} \
            -labelwidth $lwidth \
            -increment 1 \
            -resolution 1 \
            -from 10 \
            -to 40 \
            -show_arrows 1 \
            -orient horizontal \
            -value $itk_option(-iqesize) \
            -command [code $this configure -iqesize]
      }
      pack $itk_component(size) -fill x -side top -pady 2 -padx 2
      add_short_help $itk_component(size) \
         {Size of region used around each object}

      #  Add fields for showing values.
      itk_component add fwhmx {
         util::LabelValue $w_.fwhmx -text {FwhmX:} \
            -value 0 \
            -labelwidth $lwidth
      }
      pack $itk_component(fwhmx) -fill x -side top -pady 2 -padx 2
      add_short_help $itk_component(fwhmx) \
         {Mean FWHM in X direction in pixels/arcsec (range pixels/arcsec)}

      itk_component add fwhmy {
         util::LabelValue $w_.fwhmy -text {FwhmY:} \
            -value 0 \
            -labelwidth $lwidth
      }
      pack $itk_component(fwhmy) -fill x -side top -pady 2 -padx 2
      add_short_help $itk_component(fwhmy) \
         {Mean FWHM in Y direction in pixels/arcsec (range pixels/arcsec)}

      itk_component add angle {
         util::LabelValue $w_.angle -text {Angle:} \
            -value 0 \
            -labelwidth $lwidth
      }
      pack $itk_component(angle) -fill x -side top -pady 2 -padx 2
      add_short_help $itk_component(angle) \
         {Mean position angle of major axis (degrees)}

      itk_component add peak {
         util::LabelValue $w_.peak -text {Peak:} \
            -value 0 \
            -labelwidth $lwidth
      }
      pack $itk_component(peak) -fill x -side top -pady 2 -padx 2
      add_short_help $itk_component(peak) \
         {Mean peak value of objects}

      itk_component add back {
         util::LabelValue $w_.back -text {Background:} \
            -value 0 \
            -labelwidth $lwidth
      }
      pack $itk_component(back) -fill x -side top -pady 2 -padx 2
      add_short_help $itk_component(back) \
         {Mean background level of all objects}

      itk_component add nused {
         util::LabelValue $w_.nused -text {Number used:} \
            -value 0 \
            -labelwidth $lwidth
      }
      pack $itk_component(nused) -fill x -side top -pady 2 -padx 2
      add_short_help $itk_component(nused) \
         {Number of objects used in image quality estimates}
   }

   #  Add position to table.
   public method add {} {

      #  Set up interaction to read the next <1> on the image
      #  and get the coordinates. Do this until interrupted.
      set adding_ 1
      $itk_component(stop) configure -state normal
      $itk_component(add) configure -state disabled
      while { $adding_ } {
         set info [$itk_component(table) get_coords $itk_option(-image)]
         if { $info != "" } {
            eval $itk_component(table) set_new_row $info
         }
      }
   }

   #  Stop adding positions.
   public method stop {} {
      $itk_component(stop) configure -state disabled
      $itk_component(add) configure -state normal
      set adding_ 0
      catch {$itk_component(table) stop_get_coords}
   }

   #  Read and write reference positions to a file.
   protected method read_positions_ {} {
      $itk_component(table) read_from_file
   }
   protected method save_positions_ {} {
      $itk_component(table) write_to_file
   }

   #  Toggle state of auto centroiding.
   protected method auto_centroid_changed_ {} {
      $itk_component(table) configure -init_centroid $values_($this,autoc)
   }

   #  Update image quality statistics
   public method update_stats {what} {
      set content {}
      if { "$what" == "all" } {

         #  Work with all objects.
         set content [$itk_component(table) get_contents]
      } else {
         #  Work with selected objects.
         set content [$itk_component(table) get_selected]
      }
      if { $content != {} } {
         set coords {}
         foreach line $content {
            append coords "[lindex $line 3] [lindex $line 4] "
         }
         if { [catch {$itk_option(-rtdimage) globalstats \
                         $coords $itk_option(-iqesize)} msg] == 0 } {
            lassign $msg fwhmx rfwhmx fwhmy rfwhmy angle peak back nused

            #  Get image scale.
            lassign [$itk_option(-rtdimage) wcsset] ra dec imgscale
            if { $imgscale == "" } {
               set imgscale 0.0
            }

            #  Update results controls (use format to keep precision down).
            set res [format "%.2f/%.2f (%.2f/%.2f)" \
                        $fwhmx [expr $fwhmx*$imgscale] \
                        $rfwhmx [expr $rfwhmx*$imgscale]]
            $itk_component(fwhmx) configure -value $res
            set res [format "%.2f/%.2f (%.2f/%.2f)" \
                        $fwhmy [expr $fwhmy*$imgscale] \
                        $rfwhmy [expr $rfwhmy*$imgscale]]
            $itk_component(fwhmy) configure -value $res
            $itk_component(angle) configure -value [format "%.2f" $angle]
            $itk_component(peak) configure -value [format "%.2f" $peak]
            $itk_component(back) configure -value [format "%.2f" $back]
            $itk_component(nused) configure -value $nused
         } else {
            #  No measurements so clear boxes.
            $itk_component(fwhmx) configure -value 0
            $itk_component(fwhmy) configure -value 0
            $itk_component(angle) configure -value 0
            $itk_component(peak) configure -value 0
            $itk_component(back) configure -value 0
            $itk_component(nused) configure -value 0
         }
      } else {
         warning_dialog "No positions available"
      }
   }

   #  Import displayed positions from a text file.
   protected method import_text_ {} {
      busy {

         #  Start import dialog. The output file is fixed and the user
         #  chooses the input file. The format of the output file is
         #  the correct astrometry format.
         if { $importer_ == {} } {
            set importer_ [gaia::GaiaTextImport $w_.\#auto \
                              -outfile "GaiaPositions.Dat" \
                              -title "Import positions text file" \
                              -format ast \
                              -show_infile 1 \
                              -show_outfile 0]
         } else {
            utilReUseWidget gaia::GaiaTextImport $importer_ \
               -outfile "GaiaPositions.Dat" \
               -format ast \
               -show_infile 1 \
               -show_outfile 0
         }
         lassign [$importer_ activate] outfile ra dec x y

         #  Get "id ra dec x y" positions from new file and read into
         #  table.
         if { $outfile != {} && $outfile != -1 } {
            $itk_component(table) read_positions $outfile

            #  If X and Y or RA/Dec where missing then project them
            #  from the given coordinates.
            if { $x == -1 && $y == -1 } {
               $itk_component(table) update_x_and_y
            } elseif { $ra == -1 && $dec == -1 } {
               $itk_component(table) update_ra_and_dec
            }
         }
      }
   }

   #  Transform celestial coordinates to system of image.
   protected method precess_ {} {
      busy {

         #  Start precession dialog. When started give it name of
         #  our existing table to copy. When finished we may need to
         #  update our table.
         set transform [gaia::GaiaAstTransform $w_.\#auto \
                           -image $itk_option(-image) \
                           -rtdimage $itk_option(-rtdimage) \
                           -canvas $itk_option(-canvas) \
                           -number $itk_option(-number)]
         $itk_component(table) undraw
         $transform grab $itk_component(table)
         if { [$transform activate] } {
            $itk_component(table) clear_table
            eval $itk_component(table) set_contents [$transform get_contents]
         } else {
            $itk_component(table) redraw
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Name of the canvas holding the starrtdimage widget.
   itk_option define -canvas canvas Canvas {}

   #  Name of the RtdImage widget or derived class.
   itk_option define -image image Image {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0

   #  The type of fit to be used when refining the coordinate system.
   itk_option define -fittype fittype Fittype 5 {
      set values_($this,fittype) $itk_option(-fittype)
   }

   #  The centroid search box and maximum shift.
   itk_option define -isize isize Isize 9 {
      set itk_option(-isize) [expr min(21,max(3,$itk_option(-isize)))]
      set values_($this,isize) $itk_option(-isize)
      if { [info exists itk_component(table) ] } {
         $itk_component(table) configure -isize $itk_option(-isize)
      }
   }

   #  Need to be 3.5->21.5, steps of 1.
   itk_option define -maxshift maxshift Maxshift 5.5 {
      set maxshift [expr min(21.5,max(3.5,$itk_option(-maxshift)))]
      set itk_option(-maxshift) [expr int($maxshift)+0.5]
      set values_($this,maxshift) $itk_option(-maxshift)
      if { [info exists itk_component(table) ] } {
         $itk_component(table) configure -maxshift $itk_option(-maxshift)
      }
   }

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Size of region about object, used when determining IQE.
   itk_option define -iqesize iqesize Iqesize 15 {
      set values_($this,iqesize) $itk_option(-iqesize)
      if { [info exists itk_component(table) ] } {
         $itk_component(table) configure -msize $itk_option(-iqesize)
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Last content of table when closed.
   protected variable oldcontents_ {}

   #  Window for importing text files.
   protected variable importer_ {}

   #  Indicate when we're locked in the adding mode.
   protected variable adding_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Variable to share amongst all widgets. This is indexed by the
   #  object name ($this)
   common values_

#  End of class definition.
}
