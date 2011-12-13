#+
#  Name:
#     GaiaPosTable

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Define a class for displaying configurable object position and
#     related data.

#  Description:
#     Instances of this class display and control a series of position
#     or object related information. The columns can be configured to
#     reflect whatever information is required. If pairs of these are
#     identified as X,Y or RA/DEC coordinates then they can be coupled
#     (via an associated rtdimage) to reflect changes in each other.

#  Invocations:
#
#        GaiaPosTable object_name [configuration options]
#
#     This creates an instance of a GaiaPosTable object. The return is
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
#     See itk_option definitions below.

#  Methods:
#     See method declarations below.

#  Inheritance:
#     GaiaAstTable

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
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
#     27-MAR-2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPosTable {}

itcl::class gaia::GaiaPosTable {

   #  Inheritances:
   #  -------------
   inherit gaia::GaiaAstTable

   #  Constructor:
   #  ------------
   constructor {args} {
      #  Remove -showmsize & editcontrols from argument list.
      regsub {\-showmsize[\ ]+[^\ ]+} "$args" {} safeargs
      regsub {\-editcontrols[\ ]+[^\ ]+} "$safeargs" {} safeargs
      eval gaia::GaiaAstTable::constructor $safeargs
   } {
      #  Evaluate any options.
      itk_option remove gaia::GaiaAstTable::coupled
      eval itk_initialize $args

      #  Strings to convert screen coordinates to canvas coordinates.
      set canvasX_ "\[$itk_option(-canvas) canvasx %x\]"
      set canvasY_ "\[$itk_option(-canvas) canvasy %y\]"

      #  Reset configuration of base class to suit this case.
      add_short_help $itk_component(table) {Selected positions}

      #  Update short help for centroid, which now also updates RA/Dec.
      add_short_help $itk_component(centroid) {Centroid positions}

      #  Disable some of the control buttons to customise the
      #  interface.
      set disables "new flipxy flipx flipy"
      if { ! $itk_option(-editcontrols) } {
         append disables " modify delete grab centroid clip clear"
      }
      foreach c $disables {
         $itk_component($c) configure -state disabled
         add_short_help $itk_component($c) {disabled}
      }
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Called after all constructors are completed.
   public method init {} {
      if { ! $itk_option(-showmsize) } {
         $itk_option(-markmenu) delete Size
      }
   }

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

   #  Add a new row to the table and set its values. A new identifier
   #  is generated so only the x y, ra and dec values are required.
   public method set_new_row {ra dec x y} {

      #  Create a unique identifier.
      set t $itk_component(table)
      set nselect 1
      while { $nselect > 0 } {
         $t clear_selection
         $t search "id" [incr ids_]
         set nselect [$t num_selected]
      }

      #  Create the new row.
      $itk_component(table) append_row [list $ids_ $ra $dec $x $y]
      $itk_component(table) new_info

      #  Make this the current selection.
      $itk_component(table) select_row end
      redraw
   }

   #  Update the selected row with new X-Y and RA/Dec coordinates.
   public method update_row {} {

      #  Get the selected row.
      set row [$itk_component(table) get_selected]
      if { $row != "" } {

         #  Ok, get a coordinate position from the associated image.
         set result [get_coords $itk_option(-image)]

         #  And replace existing X, Y, RA and Dec.
         lassign $result ra dec x y
         eval lassign $row id radummy decdummy xdummy ydummy
         eval $itk_component(table) set_row $row [list "$id $ra $dec $x $y"]
         redraw
      }
   }

   #  Select a coordinate position on a given RtdImage. The return is a
   #  list of "ra dec x y".
   public method get_coords {image} {

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

      #  Wait until the "picked_object_" method is invoked.
      global ::$w_.picked
      set $w_.picked {}
      tkwait variable $w_.picked

      #  Restore canvas bindings and cursor.
      bindtags $canvas $bindtags
      $canvas configure -cursor $cursor

      #  Return the coordinate list.
      return [set $w_.picked]
   }

   #  Stop the get_coords method (if waiting) and make the return
   #  blank. Use this to interrupt get_coords.
   public method stop_get_coords {} {
      global ::$w_.picked
      set $w_.picked {}
   }

   #  This method is called when the user clicks in the image to
   #  select an object or star for the "get_coords" method.
   protected method picked_object_ {image canvas winx winy} {

      #  Get canvas coordinates of event.
      set canvasx [$canvas canvasx $winx]
      set canvasy [$canvas canvasy $winy]

      #  Convert canvas coordinates into X-Y and RA-Dec. Image X and
      #  Y positions are centroided.
      set rtdimage [$image get_image]
      $rtdimage convert coords $canvasx $canvasy canvas \
         imagex imagey image
      if { $itk_option(-init_centroid) } {
         if { [catch { $rtdimage foreign centroid \
                          "-isize $itk_option(-isize) \
                           -maxshift $itk_option(-maxshift) \
                           -coords [list $imagex $imagey]" } msg] == 0 } {
            #  Succeeded so replace the x and y coordinates by the new
            #  estimates.
            lassign $msg imagex imagey
         }
      }

      #  Convert to WCS system of image.
      set equinox [$rtdimage wcsequinox]
      if { [$rtdimage astcelestial] } {
         if { [catch { $rtdimage convert coords \
                          $imagex $imagey image ra dec "wcs $equinox" }] != 0 } {
            #  No RA and Dec found.
            set ra "00:00:00"
            set dec "00:00:00"
         }
      } else {
         set ra "00:00:00"
         set dec "00:00:00"
      }

      #  Return coordinates.
      global ::$w_.picked
      set $w_.picked "$ra $dec $imagex $imagey"
   }

   #  Replace/update the table contents with new X and Y
   #  values. Override to also update the WCS coordinates
   #  using these values.
   public method replace_x_and_y {newlist} {
      set oldcon [$itk_component(table) get_contents]
      set equinox [$itk_option(-rtdimage) wcsequinox]
      $itk_component(table) clear
      set i 0
      foreach {newx newy} $newlist {
         set id [lindex [lindex $oldcon $i] 0]
         if { [$itk_option(-rtdimage)  astcelestial] } {
            if { [catch { $itk_option(-rtdimage) convert coords \
                             $newx $newy image ra dec "wcs $equinox" }] != 0 } {
               #  No RA and Dec found.
               set ra "00:00:00"
               set dec "00:00:00"
            }
         } else {
            set ra "00:00:00"
            set dec "00:00:00"
         }
         $itk_component(table) append_row [list $id $ra $dec $newx $newy]
         incr i
      }
      $itk_component(table) new_info
      redraw
   }

   #  Update the X and Y position of a mark, unless all movements are
   #  coupled. In which case update all marks. Note effort required to
   #  deal with old_row as a list containing a list. Override from
   #  base class to also update RA and Dec coordinates.
   protected method update_mark_ {nid} {
      if { !$itk_option(-coupled) } {
         select_row $nid
         set old_row [$itk_component(table) get_selected]
         eval lassign $old_row id ra dec x y
         lassign [$itk_option(-canvas) coords $tags_($nid)] newx newy
         $itk_option(-rtdimage) convert coords $newx $newy canvas x y image
         set equinox [$itk_option(-rtdimage) wcsequinox]
         if { [$itk_option(-rtdimage) astcelestial] } {
            if { [catch {$itk_option(-rtdimage) convert coords \
                            $newx $newy canvas ra dec "wcs $equinox"}] != 0 } {
               set ra "00:00:00"
               set dec "00:00:00"
            }
         } else {
            set ra "00:00:00"
            set dec "00:00:00"
         }
         eval $itk_component(table) set_row $old_row [list "$id $ra $dec $x $y"]
      } else {

         #  Coupled isn't usual for this class so just fallback.
         update_marks_
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Whether position are coupled. This is always false.
   itk_option define -coupled coupled Coupled 0 {
      set itk_option(-coupled) 0
   }

   #  Whether to centroid initial position or not.
   itk_option define -init_centroid init_centroid Init_Centroid 1

   #  Whether to remove menu item that controls marker size.
   itk_option define -showmsize showmsize Showmsize 1

   #  Whether table positions can be editted (table control that allow
   #  this are disabled).
   itk_option define -editcontrols editcontrols Editcontrols 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  Table configuration (internal).
   protected variable selectmode_ extended
   protected variable exportselection_ 0

   #  Identifiers for new objects.
   protected variable ids_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
