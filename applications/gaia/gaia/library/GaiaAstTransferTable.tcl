#+
#  Name:
#     GaiaAstTransferTable

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Extends GaiaAstTable to allow the interactive definition of X-Y
#     and RA/Dec pairs on images.

#  Description:
#     This class extends the ability of the GaiaAstTable widget adding
#     controls for selecting X-Y positions on an associated image and
#     for selecting the corresponding RA-Dec positions on any other
#     image that is displayed (in another clone). The positions may be
#     "centroided" and a list of  them is displayed in a text
#     window.

#  Invocations:
#
#        GaiaAstTransferTable object_name [configuration options]
#
#     This creates an instance of a GaiaAstTransferTable object. The return is
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
#     See itk_define statements below.

#  Methods:
#     See method declarations below.

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
#     05-MAR-1999 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.
itk::usual GaiaAstTransferTable {}

itcl::class gaia::GaiaAstTransferTable {

   #  Inheritances:
   #  -------------
   inherit gaia::GaiaAstTable

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Change the internal configuration of Table widget to suit.
      set selectmode_ single
      set exportselection_ 1
      eval gaia::GaiaAstTable::constructor $args
   } {

      #  Evaluate any options [incr Tk].
      itk_option remove gaia::GaiaAstTable::coupled
      itk_option remove gaia::GaiaAstTable::bind_enters
      eval itk_initialize $args

      #  Override short help for Table window.
      add_short_help $itk_component(table) \
         {Reference positions and their associated X-Y coordinates}

      #  Disable some of the control buttons to customise the
      #  interface.
      $itk_component(new) configure -state disabled
      add_short_help $itk_component(new) {disabled}
      $itk_component(flipxy) configure -state disabled
      add_short_help $itk_component(flipxy) {disabled}
      $itk_component(flipx) configure -state disabled
      add_short_help $itk_component(flipx) {disabled}
      $itk_component(flipy) configure -state disabled
      add_short_help $itk_component(flipy) {disabled}

      #  Add separator for new "transfer" section.
      itk_component add sep1 {
         gaia::LabelRule $w_.sep1 -text "Transfer:"
      }
      pack $itk_component(sep1) -fill x -ipadx 1m

      #  Add control for selecting the image (i.e. window clone) to
      #  read the RA-Dec values from.
      add_targets_

      #  Add frame for holding table action buttons.
      itk_component add frame3 {
         frame $w_.frame3
      }

      #  Add button for adding a new row.
      itk_component add new_row {
         button $itk_component(frame3).new -text "Add row" \
            -command [code $this add_new_row] \
            -width 13
      }
      add_short_help $itk_component(new_row) \
         {Create a new blank row in the table}
      pack $itk_component(new_row) -side left -expand 1 -pady 2 -padx 2

      #  Add button for updating the X and Y coordinates of the
      #  currently selected row.
      itk_component add update_xy {
         button $itk_component(frame3).xy -text "Update X-Y" \
            -command [code $this update_xy] \
            -width 13
      }
      add_short_help $itk_component(update_xy) \
         {Update the X and Y coordinates by selecting an object}
      pack $itk_component(update_xy) -side left -expand 1 -pady 2 -padx 2

      #  Add button for updating the RA/Dec coordinates of the
      #  currently selected row.
      itk_component add update_radec {
         button $itk_component(frame3).radec -text "Update RA-Dec" \
            -command [code $this update_radec] \
            -width 13
      }
      add_short_help $itk_component(update_radec) \
         {Update the RA and Dec coordinates by selecting an object}
      pack $itk_component(update_radec) -side left -expand 1 -pady 2 -padx 2

      #  Pack button frame.
      pack $itk_component(frame3) -side top -fill x -expand 1 -pady 2 -padx 2 -anchor w

      #  First time add an empty row.
      add_new_row
   }

   #  Destructor:
   #  -----------
   destructor  {
      #  Remove RA/Dec marker if needed.
      remove_radec
   }

   #  Methods:
   #  --------

   #  Add a new row to the table. This serves as a blank entry which
   #  is updated to have the correct X-Y and RA/Dec coordinates.
   public method add_new_row {} {

      #  Create a unique identifier.
      set t $itk_component(table)
      set nselect 1
      while { $nselect > 0 } {
         $t clear_selection
         $t search "id" [incr ids_]
         set nselect [$t num_selected]
      }

      #  Create the new row.
      $itk_component(table) append_row [list $ids_ 00:00:00 00:00:00 0.0 0.0]
      $itk_component(table) new_info

      #  Make this the current selection.
      $itk_component(table) select_row end

      redraw
   }

   #  Update the X and Y coordinates of the currently selected
   #  row. This is done by selecting a position in the associated
   #  image and then refining this.
   public method update_xy {} {

      #  Get the selected row.
      set row [$itk_component(table) get_selected]
      if { $row != "" } {

         #  Raise the associated top-level window.
         raise [winfo toplevel $itk_option(-image)]

         #  Ok, get a coordinate position from the associated image.
         set result [get_coords_ $itk_option(-image)]

         #  Extract X and Y and replace. Note keep existing RA and Dec.
         lassign $result new_ra new_dec x y
         lassign $row ra dec old_x old_y
         eval lassign $row id ra dec xdummy ydummy
         eval $itk_component(table) set_row $row [list "$id $ra $dec $x $y"]
         redraw
      }
   }

   #  Update the RA and Dec coordinates of the currently selected
   #  row. This is done by selecting a position in the target image
   #  and then refining this.
   public method update_radec {} {

      #  Get the selected row.
      set row [$itk_component(table) get_selected]
      if { $row != "" } {

         #  Raise the top-level window associated.
         raise [winfo toplevel $target_]

         #  Ok, get a coordinate position from the associated image.
         set result [get_coords_ $target_]

         #  Extract RA and Dec and replace. Note keep existing X and Y.
         lassign $result ra dec new_x new_y
         lassign $row old_ra old_dec x y
         eval lassign $row id radummy decdummy x y
         eval $itk_component(table) set_row $row [list "$id $ra $dec $x $y"]
         redraw
         redraw_radec
      }
   }

   #  Add all the image views to a menu for selection as the
   #  current RA-Dec reference image (the "target").
   protected method add_targets_ {} {

      #  Menu button for selection.
      itk_component add targets {
         util::LabelMenu $w_.targets \
            -labelwidth 12 \
            -valuewidth 20 \
            -valueanchor e \
            -text "RA/Dec image:"
      }
      pack $itk_component(targets) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(targets) \
         {Image used for obtaining RA-Dec coordinates}

      #  Add a binding to update the menu item whenever it is pressed.
      #  XXX bit of a cheat to get menubutton name.
      set menu [$itk_component(targets) component mb]
      bind $menu <ButtonPress-1> "+[code $this update_targets_]"

      #  Add the menu items.
      update_targets_
   }

   #  Update (or initialise) the possible target images.
   protected method update_targets_ {} {

      #  Remove any existing menu items.
      $itk_component(targets) clear

      #  Locate and add all images. The current image is "$target_".
      set images [skycat::SkyCat::get_skycat_images]

      #  And add to the menu.
      foreach w $images {
         set name [[$w get_image] fullname]
         set clone [[winfo toplevel $w] cget -number]
         $itk_component(targets) add \
            -label "$name ($clone)" \
            -value "$w" \
            -command [code $this set_target_ "$w"]
      }
      if { [llength $images] > 1 } {
         set_target_ [lindex $images 1]
      } else {
         set_target_ [lindex $images 0]
      }

   }

   #  Set the target image for RA-Dec coordinates.
   protected method set_target_ {image} {
      if { $target_ != $image && $target_ != {} } {
         #  Remove RA/Dec marker if needed (this is needed when target
         #  image is changed).
         remove_radec
      }
      set target_ $image
   }

   #  Select a coordinate position on a given RtdImage. The return is a
   #  list of "ra dec x y".
   protected method get_coords_ {image} {

      #  Retain current canvas cursor and bindings, before overriding.
      set canvas [$image get_canvas]
      set cursor [$canvas cget -cursor]
      $canvas configure -cursor cross

      set bindtags [bindtags $canvas]
      set tag pick$w_

      #  Set bindings to get canvas to return coordinates.
      bind $tag <ButtonRelease-1> \
         [code $this picked_object_ $image $canvas %x %y]
      bindtags $canvas $tag

      #  Wait until the picked_object_ method is invoked.
      global ::$w_.picked
      set $w_.picked {}
      tkwait variable $w_.picked

      #  Restore canvas bindings and cursor.
      bindtags $canvas $bindtags
      $canvas configure -cursor $cursor

      #  Return the coordinate list.
      return [set $w_.picked]
   }

   #  This method is called when the user clicks in the image to
   #  select an object or star for the "get_coords_" method.
   protected method picked_object_ {image canvas winx winy} {

      #  Get canvas coordinates of event.
      set canvasx [$canvas canvasx $winx]
      set canvasy [$canvas canvasy $winy]

      #  Convert canvas coordinates into X-Y and RA-Dec. Image X and
      #  Y positions are centroided.
      set rtdimage [$image get_image]
      $rtdimage convert coords $canvasx $canvasy canvas imagex imagey image
      if { [catch { $rtdimage foreign centroid \
                       "-isize $itk_option(-isize) \
                        -maxshift $itk_option(-maxshift) \
                        -coords [list $imagex $imagey]" } msg] == 0 } {
         #  Succeeded so replace the x and y coordinates by the new estimates.
         lassign $msg imagex imagey
      }
      #lassign [$rtdimage astpix2wcs $imagex $imagey] ra dec equinox
      if { [catch {$rtdimage convert coords $imagex $imagey image \
                      ra dec "wcs $itk_option(-equinox)"} msg] != 0 } {
         #  No RA and Dec found.
         set ra "00:00:00"
         set dec "00:00:00"
      }

      #  Return coordinates.
      global ::$w_.picked
      set $w_.picked "$ra $dec $imagex $imagey"
   }

   #  Draw the RA/Dec marker. This is restricted to the current
   #  selection (since in principle the RA/Dec values could be
   #  obtained from a range of images and keeping track of all the
   #  markers could be a nightmare). Use same marker as X-Y
   #  image. This uses a special tag "$this_ra_dec_tag".
   public method redraw_radec {} {
      set row [$itk_component(table) get_selected]
      if { $row != {} } {
         set canvas [$target_ get_canvas]
         set image [$target_ get_image]

         #  Remove existing marker.
         remove_radec

         #  Adjust markers for image scale.
         lassign [$image scale] xs ys
         if { $xs > 1 } {
            set scale [expr $xs*$itk_option(-msize)]
         } else {
            set scale [expr $itk_option(-msize)/abs($xs)]
         }
         eval lassign $row id ra dec x y
         if { [catch \
                  { $image convert coords $ra $dec wcs x y canvas } \
                  msg] == 0 } {
            $canvas create rtd_mark $x $y \
               -type $itk_option(-mtype) \
	       -tags "${this}_ra_dec_tag" \
               -outline $itk_option(-mcolour) \
               -size $scale -fill $itk_option(-mfill) \
               -width $itk_option(-mwidth) \
               -stipple $itk_option(-mstipple)
         } else {
            error_dialog $msg
         }
      }
   }

   #  Remove existing RA/Dec marker if can (invoke this when finished
   #  with table, image is changed etc.).
   public method remove_radec {} {
      catch {
         set canvas [$target_ get_canvas]
	 $canvas delete "${this}_ra_dec_tag"
      }
   }

   #  Override clear_table to also clear the RA/Dec marker.
   public method clear_table {} {
      remove_radec
      gaia::GaiaAstTable::clear_table
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Whether position are coupled. This is always false.
   itk_option define -coupled coupled Coupled 0 {
      set itk_option(-coupled) 0
   }

   #  Whether to add the any-enter binding to graphics markers. Always
   #  false as causes occasional problems with selection moving in
   #  table (with updates of coordinates to wrong row).
   itk_option define -bind_enters bind_enters Bind_enters 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Name of current target image.
   protected variable target_ {}

   #  Row identifiers.
   protected variable ids_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
