#+
#  Name:
#     GaiaHistory

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Control the display of a history list of images (a "Go" menu).

#  Description:
#     This class provides the facilities needed to manage a menu for
#     displaying images that have been opened in previous sessions and
#     moving back and forth between images opened in the current session

#  Invocations:
#
#        GaiaHistory object_name [configuration options]
#
#     This creates an instance of a GaiaHistory object. The return is
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
#     Nothing

#  Copyright:
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
#     {enter_new_authors_here}

#  History:
#     01-JUL-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaHistory {}

itcl::class gaia::GaiaHistory {

   #  Inheritances:
   #  -------------
   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Add an image to the history catalog under the given filename.
   public method add_history {filename} {

      #  Ignore non-existant image.
      if { $filename == "" || ! [file exists $filename] } {
         return
      }

      #  Access history catalogue (need to do this sometime, so do it now).
      set catalog $history_catalog_

      #  Check if the directory for the catalog exists
      set dir [file dirname $catalog]
      if { ! [file isdirectory $dir] } {
         if { [catch {exec mkdir $dir} msg] } {
            warning_dialog $msg
            return
         }
      }

      #  Make sure at least an empty catalog exists.
      if { ! [file exists $catalog] || [file size $catalog] == 0 } {

         # If it doesn't exist yet, create an empty catalog file
         if { [catch {set fd [::open $catalog w]} msg] } {
            warning_dialog "can't create image history catalog: $msg"
            return
         }
         puts $fd "Skycat History Catalog v1.0"
         puts $fd ""
         puts $fd "ra_col: -1"
         puts $fd "dec_col: -1"
         puts $fd "x_col: -1"
         puts $fd "y_col: -1"
         puts $fd "show_cols: file ra dec object NAXIS NAXIS1 NAXIS2 NAXIS3"
         puts $fd "sort_cols: timestamp"
         puts $fd "sort_order: decreasing"
         puts $fd ""
         puts $fd [join $history_cols_ "\t"]
         puts $fd "----"
         ::close $fd

         #  Get the catalog into the list of known catalogs.
         $astrocat_ open $catalog
      }

      #  Don't record Temp images they will be deleted.
      if { ! [string match {*Temp*} $filename] } {

         #  Add an entry for the given image and filename
         set id [file tail $filename]

         #  Image centre RA and Dec not easily available, so skip
         #  (need image loaded into some rtdimage).
         set ra "00:00:00"
         set dec "00:00:00"

         #  Record dimensions.
         set accessor [$gaiatoplevel get_accessor]
         set object [$accessor fitsread OBJECT]
         set naxis 3
         set dims [$accessor getdims 0]
         set naxis1 [lindex $dims 0]
         set naxis2 [lindex $dims 0]
         set naxis3 [lindex $dims 0]

         #  Also make these up.
         set lowcut 0
         set highcut 1
         set colormap "real.lasc"
         set itt "ramp.iasc"
         set colorscale "linear"
         set zoom "1"

         set timestamp [clock seconds]

         #  Get full path name of file for preview URL
         if { "[string index $filename 0]" == "/" } {
            set fullpath $filename
         } else {
            set fullpath [pwd]/$filename
         }
         set preview file:$fullpath

         set data [list [list $id $ra $dec $object \
                            $naxis $naxis1 $naxis2 $naxis3 \
                            $lowcut $highcut $colormap $itt $colorscale $zoom \
                            $timestamp $preview]]

         $astrocat_ open $catalog
         $astrocat_ save $catalog 1 $data ""

         #  Update history catalog window, if it is showing
         set w [cat::AstroCat::get_instance [file tail $catalog]]
         if { "$w" != "" && [winfo viewable $w] } {
            $w search
         }
      }
   }

   #  Populate a menu with all the image currently held in the history file.
   #  Also adds "Back" and "Forward" items to move in the list of images that
   #  have been opened in the current session (and recorded using add_history).
   public method update_history_menu {menu} {

      #  Clear existing items.
      $menu delete 0 end

      $menu add command \
         -label "Back" \
         -command [code $this previous] \
         -state disabled

      $gaiatoplevel add_menu_short_help $menu "Back" \
         {Go back again to the previous image}

      if { [info exists back_list_] && [llength $back_list_] } {
         $menu entryconfig "Back" -state normal
      }

      $menu add command \
         -label "Forward" \
         -command [code $this forward] \
         -state disabled

      $gaiatoplevel add_menu_short_help $menu "Forward" \
         {Go forward again to the next image}

      if { [info exists forward_list_] &&
           [llength $forward_list_] } {
         $menu entryconfig "Forward" -state normal
      }

      $menu add separator

      add_history_menu_items $menu 20
   }

   #  Add the first N known images to a given menu. See update_history_menu
   #  if also requiring a Back/Forward mechanism.
   public method add_history_menu_items {menu n} {

      set catalog $history_catalog_
      if { [catch {$astrocat_ open $catalog} ] } {
         #  No catalog yet
         return
      }
      set list [$astrocat_ query \
                   -nrows $n -sort timestamp -sortorder decreasing]

      foreach row $list {
         eval lassign {$row} $history_cols_
         set filename [string range $PREVIEW 5 end]
         $menu add command \
            -label $file \
            -command [code $gaiatoplevel open_image $filename]
      }
   }

   #  Go back to the previous image.
   public method previous {} {

      while { [set n [llength $back_list_]] } {

         set filename [lindex $back_list_ end]

         set currentimage [$gaiatoplevel get_image]

         if { "$filename" != "$currentimage" && [file exists $filename] } {

            #  Current file becomes forward.
            lappend forward_list_ $currentimage

            #  Remove image we're about to display from back list. This
            #  will be added again when a new image is loaded.
            set back_list_ [lrange $back_list_ 0 [expr $n-2]]

            #  Ok, now switch to previous file, making sure the current
            #  file isn't recorded (goes onto the forward list).
            set last_ {}

            #  Open image.
            $gaiatoplevel open_image $filename
            break
         }

         #  Skip non-existent files.
         set back_list_ [lrange $back_list_ 0 [expr $n-2]]
      }
   }

   #  Go forward again to the next image.
   public method forward {} {

      while { [set n [llength $forward_list_]] } {
         set filename [lindex $forward_list_ end]
         set currentimage [$gaiatoplevel get_image]

         if { "$filename" != "$currentimage" && [file exists $filename] } {

            #  Remove this from lists.
            set forward_list_ \
               [lrange $forward_list_ 0 [expr $n-2]]

            #  Load and display.
            $gaiatoplevel open_image $filename
            break
         }

         #  Skip non-existent files.
         set forward_list_ \
            [lrange $forward_list_ 0 [expr $n-2]]
      }
   }

   #  Clear the "last image" so that nothing will be recorded.
   public method clear_last {} {
      set last_ {}
   }

   #  Add the "last image" to the last_ list.
   public method record_last {} {

      #  Don't add if present as last image or no image loaded.
      if { $last_ != {} } {
         if { [info exists back_list_] } {
            set current_last [lindex $back_list_ end]
         } else {
            set current_last {}
         }

         if { $current_last != $last_ } {
            lappend back_list_ $last_
         }
      }

      set last_ [$gaiatoplevel get_image]
   }

   #  Set the location of the history file.
   public proc set_history_catalog {catalog} {
      set history_catalog_ "$catalog"
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  The GAIA toplevel instance, inheriting util::TopLevelWidget. Required,
   #  must also offer the following methods:
   #
   #     get_accessor  - return an GaiaNDAccess instance wrapping the image.
   #     open_image    - open a given filename as an image
   #     get_image     - return name of currently opened image
   #
   public variable gaiatoplevel {} {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Name of the last image opened.
   protected variable last_ {}

   #  Arrays used for the Go=>Back/Forward menu items.
   protected variable back_list_
   protected variable forward_list_

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Name of the history catalogue, shared with Skycat and images.
   protected common history_catalog_ $::env(HOME)/.skycat/history

   #  C++ astrocat object shared with AstroCat.
   protected common astrocat_ [astrocat ::cat::.astrocat]

   #  List of columns in the history catalog.
   protected common history_cols_ \
      [list file ra dec object NAXIS NAXIS1 NAXIS2 NAXIS3 \
          lowcut highcut colormap itt colorscale zoom timestamp PREVIEW]

#  End of class definition.
}
