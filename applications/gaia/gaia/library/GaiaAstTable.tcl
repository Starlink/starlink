#+
#  Name:
#     GaiaAstTable

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for controlling and displaying a list of sky and
#     corresponding image positions.

#  Description:
#     This class creates an object that can be used to manipulate and
#     store a list of sky-like positions (such as RA and Decs) and
#     the corresponding X,Y positions (plus an identifier). It
#     provides the ability to capture lists of such positions from
#     catalogues and the ability to add, remove, edit and centroid the
#     positions (and to write and read them from oridinary text files,
#     note these are not tab files). The table has an associated
#     equinox to which all values are converted. This may be changed
#     at any time, but the values in the table will not be changed
#     (facilities for FK4/FK5/system equinox/epoch conversion are not
#     generally available). When a catalogue is grabbed a callback
#     contain the equinox of the values is made (and this becomes the
#     equinox of the table). In general these values should be limited
#     to J2000 (FK5) and B1950 (presumably FK4, but note epoch also
#     1950).
#
#     The class can add a menu of controls for setting the marker
#     sizes, colours etc. and will globally rearrange all the
#     positions, if requested (compared with the usual markers
#     controls which allow each marker to be moved
#     independently). When globally moving all markers, <1> drag
#     offsets and <2> drag scales about a centre which is displayed as
#     a cross.
#
#     <Control-1> also allows the deletion of a marker (and its table
#     entry). Double <1> over a marker or the table line <1> pops up a
#     dialog to change the data values.

#  Invocations:
#
#        GaiaAstTable object_name [configuration options]
#
#     This creates an instance of a GaiaAstTable object. The return is
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
#     See itk_option define statements.

#  Methods:
#     See method definitions below.

#  Inheritance:
#     FrameWidget

#  Copyright:
#     Copyright (C) 1998-2005 Central Laboratory of the Research Councils.
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
#     6-JAN-1998 (PWD):
#        Original version.
#     5-MAR-1999 (PWD):
#        Now centers on selected row using <2>.
#     28-MAR-2000 (PWD):
#        Changed to GaiaAstTable (from StarAstTable).
#     10-APR-2001 (PWD):
#        Changed fitting RMS value to sqrt(sum_of_dist_squared/n)
#        rather than sqrt(sum_of_dist_squared)/n. Which was wrong.
#     29-AUG-2001 (PWD):
#        Renamed StarEnterObject to GaiaEnterObject. Added some
#        checks for the RA and Dec field formats.
#     14-JUN-2002 (PWD):
#        Changed so that ra and dec may have space separated fields.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaAstTable {}

itcl::class gaia::GaiaAstTable {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

       #  Evaluate any options.
       eval itk_initialize $args

       #  Get name of TopLevelWidget (no . allowed!).
       set top_ [winfo toplevel $w_]

       # Add options to the edit menu if given.
       if { $itk_option(-editmenu) != {} } {
          make_edit_menu_
       }

       #  Add control for markers colours etc., if given.
       if { $itk_option(-markmenu) != {} } {
          make_markers_menu_ $itk_option(-markmenu)
       }

       #  Add controls for marker labels (uses identifiers).
       if { $itk_option(-labelmenu) != {} } {
          make_labels_menu_
       }

       #  Add the table for displaying the reference positions (note
       #  fixed headings).
       itk_component add table {
          util::TableList $w_.table \
             -title $itk_option(-title) \
             -hscroll 1 \
             -selectmode $selectmode_ \
             -exportselection $exportselection_ \
             -headings {id ra dec x y} \
             -width $itk_option(-width) \
             -height 5
       }
       add_short_help $itk_component(table) \
          {Reference positions and their ideal/current X,Y places}

       #  Add frames for holding table action buttons.
       itk_component add frame1 {
          frame $w_.frame1
       }
       itk_component add frame2 {
          frame $w_.frame2
       }

       #  Miscellaneous operations.
       itk_component add new {
          button $itk_component(frame1).new -text New \
             -command [code $this enter_new_object]
       }
       add_short_help $itk_component(new) \
          {Enter a new object into the reference table}

       itk_component add modify {
          button $itk_component(frame1).modify -text Edit \
             -command [code $this edit_selected_object]
       }
       add_short_help $itk_component(modify) \
          {Edit the data of the selected object}

       itk_component add delete {
          button $itk_component(frame1).delete -text Delete \
             -command [code $this remove_selected]
       }
       add_short_help $itk_component(delete) \
          {Remove the selected rows from the table}

       #  Set table binding edit the entries.
       bind $itk_component(table).listbox <Double-1> \
          [code $this edit_selected_object]

       #  Set table binding to centre on the current object.
       bind $itk_component(table).listbox <2> \
          [code $this centre_selected_object_]

       #  Set table binding to label the current object.
       bind $itk_component(table).listbox <3> \
          [code $this label_selected_object_]

       #  Add a button to grab the catalogues entries from an
       #  AstroCatalog window (which must already exist).
       itk_component add grab {
          button $itk_component(frame1).grab -text Grab \
             -command [code $this grab]
       }
       add_short_help $itk_component(grab) \
          {Grab objects from a catalogue query window}

       #  Add a button to centroid the positions.
       itk_component add centroid {
          button $itk_component(frame1).centroid -text Centroid \
             -command [code $this centroid]
       }
       add_short_help $itk_component(centroid) \
          {Centroid the X and Y positions}

       #  Add a button to clip any positions off image.
       itk_component add clip {
          button $itk_component(frame1).clip -text Clip \
             -command [code $this clip_to_image]
       }
       add_short_help $itk_component(clip) \
          {Remove any positions lying off image}

       #  Buttons for reorienting the X,Y positions.
       itk_component add flipxy {
          button $itk_component(frame2).flipxy  \
             -bitmap rotate -command [code $this flip xy]
       }
       add_short_help $itk_component(flipxy) \
          {Exchange X and Y positions, without changing image}
       itk_component add flipx {
          button $itk_component(frame2).flipx \
             -bitmap flipx -command [code $this flip x]
       }
       add_short_help $itk_component(flipx) \
          {Flip X positions about centre, without changing image}
       itk_component add flipy {
          button $itk_component(frame2).flipy \
             -bitmap flipy -command [code $this flip y]
       }
       add_short_help $itk_component(flipy) \
          {Flip Y positions about centre, without changing image}

       #  Reset markers to their current sky positions.
       itk_component add reset {
          button $itk_component(frame2).reset \
             -text Reset -command [code $this update_x_and_y]
       }
       add_short_help $itk_component(reset) \
          {Reset X and Y to WCS projected positions}

       #  Set RA and Dec to current X and Y positions.
       itk_component add set {
          button $itk_component(frame2).set \
             -text Set -command [code $this update_ra_and_dec]
       }
       add_short_help $itk_component(set) \
          {Set RA and Dec to same positions as X and Y}

       #  Add a button to clear the table of its contents.
       itk_component add clear {
          button $itk_component(frame2).clear -text Clear \
             -command [code $this clear_table]
       }
       add_short_help $itk_component(clear) {Clear the table}

       #  Redraw markers (after clear).
       itk_component add redraw {
          button $itk_component(frame2).redraw -text Redraw \
             -command [code $this redraw]
       }
       add_short_help $itk_component(redraw) \
          {Redraw position markers}


       #  Pack all widgets into place.
       pack $itk_component(table) -side top -fill both -expand 1
       pack $itk_component(frame1) -side top -fill x -pady 3 -padx 3
       pack $itk_component(frame2) -side top -fill x -pady 3 -padx 3

       pack $itk_component(new) \
          $itk_component(modify) \
          $itk_component(delete) \
          $itk_component(grab) \
          $itk_component(centroid) \
          $itk_component(clip) -pady 2 -padx 2 -side left -fill both -expand 1

       pack $itk_component(flipxy) \
          $itk_component(flipx) \
          $itk_component(flipy) \
          $itk_component(reset) \
          $itk_component(set) \
          $itk_component(clear) \
          $itk_component(redraw) \
             -pady 2 -padx 2 -side left -fill both -expand 1

       #  Strings to convert screen coordinates to canvas coordinates.
       set canvasX_ "\[$itk_option(-canvas) canvasx %x\]"
       set canvasY_ "\[$itk_option(-canvas) canvasy %y\]"

       #  Start interface proper and draw cross if positions are coupled.
       set going_ 1
       redraw_cross_
   }

   #  Destructor:
   #  -----------
   destructor  {
      catch { clear_marks }
      if { $enter_object_ != {} } {
         catch {destroy $enter_object_}
      }
   }

   #  Methods:
   #  --------

   #  Return number of rows.
   public method total_rows {} {
       return [$itk_component(table) total_rows]
   }

   #  Return contents of table.
   public method get_contents {} {
       return [$itk_component(table) get_contents]
   }

   #  Return selected contents of table.
   public method get_selected {} {
       return [$itk_component(table) get_selected]
   }

   #  Set the contents of the table.
   public method set_contents {args} {
      $itk_component(table) append_rows $args
      redraw
   }

   #  Append a new row to the table. Call new_info when completed.
   public method append_row {row} {
      $itk_component(table) append_row $row
   }
   public method new_info {} {
      $itk_component(table) new_info
   }

   #  Grab a catalogue of objects from an AstroCat window.
   public method grab {} {
      set astrocats [cat::AstroCat::instances]
      if { $astrocats != {} } {

         #  Create a Name chooser to display the possible catalogues
         #  and allow a choice.
         gaia::NameChooser $w_.chooser \
            -title "Select a catalogue" \
            -textvariable [scope catalogue_] \
            -width 40
         foreach instance $astrocats {
            if { ![info exists have($instance)] } {
               set catalog [$instance cget -catalog]
               set table($catalog) [$instance get_table]
               set equinox($catalog) [$instance get_equinox]
               $w_.chooser insert $catalog \
         "[$instance cget -catalogtype] ([$instance get_image_name])"
               set have($instance) 1
            }
         }

         # Wait for the choice to be made.
         busy {
            tkwait window $w_.chooser
         }
         #  If catalogue_ is set then start to take over its objects.
         set catalogue [set [scope catalogue_]]
         if { $catalogue != {} } {
            set grabbed $table($catalogue)
            set size [$grabbed total_rows]
            if { $size < 1 } {
               error_dialog {The choosen catalogue contains no entries}
            } else {
               #  All ready to go.
               grab_table_ $grabbed $equinox($catalogue)
            }
         }
      } else {
         error_dialog {There are no catalogue queries being made\
                       See the "Data-Servers" menu and\
                       select "Catalogues..." or "Local Catalog..."}
      }
   }

   #  Read a TableList object that contains a catalogue. This is
   #  assumed to have the expected id, ra, dec fields of a tab table,
   #  we grab these and create our own table also has the X and Y
   #  positions that correspond to these sky coordinates shown. Note
   #  that the equinox of the table becomes that of the catalogue and
   #  this is communicated to the user of this class.
   protected method grab_table_ {oldtab {equinox 2000}} {
       set nrows [$oldtab total_rows]
       set oldcon [$oldtab get_contents]
       clear_table
       for { set i 0 } { $i < $nrows } { incr i } {
	   lassign [lindex $oldcon $i] id ra dec
	   set x -1
	   set y -1
	   catch { $itk_option(-rtdimage) convert coords \
		   $ra $dec "wcs $equinox" x y image }
	   $itk_component(table) append_row [list $id $ra $dec $x $y]
       }
       $itk_component(table) new_info
       configure -equinox $equinox

       #  If requesed notify that the equinox has been set to this new
       #  value.
       if { $itk_option(-notify_cmd) != {} } {
	   eval $itk_option(-notify_cmd) $equinox
       }
      redraw
   }

   #  If available draw the current positions on the image. This is
   #  done by either modifying the coords of exiting markers, or
   #  by creating new ones. If old markers are moved then they may
   #  be reconfigured (if configure_ is 1).
   public method redraw {} {
      set nrows [$itk_component(table) total_rows]
      set contents [$itk_component(table) get_contents]
      set canvas $itk_option(-canvas)
      #  Always unset marks_ as others can delete things on canvas.
      catch  { unset marks_ }
      lassign [$itk_option(-rtdimage) scale] xs ys
      if { $xs < 0 } {
         set scale [expr $itk_option(-msize)/abs($xs)]
      } else {
         set scale [expr $xs*$itk_option(-msize)]
      }
      for { set i 0 } { $i < $nrows } { incr i } {
         lassign [lindex $contents $i] id ra dec x y
         if { [ catch { $itk_option(-rtdimage) \
                           convert coords $x $y image x y canvas } ] == 0 } {
            if { [info exists tags_($id)] && [$canvas gettags $tags_($id)] != {} } {
               $canvas coords $tags_($id) $x $y
               if { $configure_ } {
                  $canvas itemconfigure $tags_($id) \
                     -scale 1 \
                     -type $itk_option(-mtype) \
                     -outline $itk_option(-mcolour) \
                     -size $scale -fill $itk_option(-mfill) \
                     -width $itk_option(-mwidth) \
                     -stipple $itk_option(-mstipple) \
                  }
            } else {
               set tag [$canvas create rtd_mark $x $y \
                           -scale 1 \
                           -type $itk_option(-mtype) \
                           -tags [list objects ${this}_mark] \
                           -outline $itk_option(-mcolour) \
                           -size $scale -fill $itk_option(-mfill) \
                           -width $itk_option(-mwidth) \
                           -stipple $itk_option(-mstipple)]
               set tags_($id) $tag
            }
            set marks_($tags_($id)) $id
         }
      }
      restore_bindings_
      redraw_cross_
   }

   #  Temporarily undraw (i.e. remove) markers.
   public method undraw {} {
      clear_marks
   }

   #  Add or restore bindings to the markers.
   protected method restore_bindings_ {} {
       set canvas $itk_option(-canvas)
       foreach {tag id} [array get marks_] {
	   #  Add a binding to all objects so that when they are drawn
	   #  they can be moved. Double clicking an object edits the
	   #  values associated with it. Entering an object highlights
	   #  its entry in the table. Pressing <Control-B1> over an
	   #  object removes it.
	   $canvas bind $tag <1> \
              [code eval $this record_mark_ $canvasX_ $canvasY_]
	   $canvas bind $tag <B1-Motion> \
              [code eval $this move_mark_ $tag $canvasX_ $canvasY_]
	   $canvas bind $tag <ButtonRelease-1> \
              [code eval $this update_mark_ $id]
	   $canvas bind $tag <Double-1> \
              [code eval $this edit_selected_object $tag]
           if { $itk_option(-bind_enters) } {
              $canvas bind $tag <Any-Enter> \
                 [code eval $this select_row $id]
           }
	   $canvas bind $tag <Control-Button-1> [code $this remove_object $tag]

	   #  Using <B2-Motion> to scale clashes with canvas binding
	   #  (and don't seem to be able to use break to stop it)
	   $canvas bind $tag <2> \
		 [code eval $this save_b2_ $canvasX_ $canvasY_]
	   $canvas bind $tag <B2-Motion> \
		   [code eval $this scale_mark_ $tag $canvasX_ $canvasY_]
	   $canvas bind $tag <B2-ButtonRelease> \
		   [code eval $this update_marks_ 1]
       }
   }

   #  Method to switch off/on <B2-Motion> binding to canvas and to
   #  record the initial position during a marker global movement.
   protected method save_b2_ {x y} {
       set b2bind_ [bind $itk_option(-canvas) <B2-Motion>]
       bind $itk_option(-canvas) <B2-Motion> {}
       record_mark_ $x $y
   }
   protected method restore_b2_ {} {
       if { $b2bind_ != {} } {
	   bind $itk_option(-canvas) <B2-Motion> $b2bind_
	   set b2bind_ {}
       }
   }

   #  Record the canvas position when <1> or <2> is pressed. This
   #  is the reference position for <Motion> events.
   protected method record_mark_ {x y} {
      set xref_ $x
      set yref_ $y
   }

   #  Draw or remove the cross representing the rotation/scale centre.
   protected method redraw_cross_ {} {
       set canvas $itk_option(-canvas)
      if { $canvas == {} } return
       set exists [$canvas gettags $cross_id_]
       if { $itk_option(-coupled) } {
	   lassign [$itk_option(-rtdimage) scale] xs ys
	   if { $xs > 1 } {
	       set scale [expr $xs*$itk_option(-msize)*2]
	   } else {
	       set scale [expr $itk_option(-msize)/abs($xs)*2]
	   }
           $itk_option(-rtdimage) convert coords \
		   $itk_option(-xcentre) $itk_option(-ycentre) image \
		   xcen_ ycen_ canvas
	   if {  $exists == {} } {
	       set cross_id_ [$canvas create rtd_mark $xcen_ $ycen_ \
		       -type cross \
		       -tags [list objects ${this}_cross] \
		       -outline $itk_option(-mcolour) \
		       -size $scale -fill $itk_option(-mfill) \
		       -width $itk_option(-mwidth) \
		       -stipple $itk_option(-mstipple)]
	       $canvas bind $cross_id_ <B1-Motion> \
		       [code eval $this move_centre_ $canvasX_ $canvasY_]
	       $canvas bind $cross_id_ <ButtonRelease-1> \
		       [code eval $this update_centre_ $canvasX_ $canvasY_]
	       set cmd [code eval $this move_centre_ $canvasX_ $canvasY_]
	       $canvas raise $cross_id_ all
	   } else {
	       $canvas coords $cross_id_ $xcen_ $ycen_
	       if { $configure_ } {
		   $canvas itemconfigure $cross_id_ \
			   -outline $itk_option(-mcolour) \
			   -size $scale -fill $itk_option(-mfill) \
			   -width $itk_option(-mwidth) \
			   -stipple $itk_option(-mstipple)
	       }
	   }
       } else {
	   if { $exists != {} } {
	       $canvas delete $cross_id_
	       set cross_id_ {}
	   }
	   set xcen_ 0
	   set ycen_ 0
       }
   }

   #  Select the row with the given id.
   public method select_row {id} {
       $itk_component(table) search id $id
   }

   #  Move a single mark.
   protected method move_mark_ {tag canx cany} {
       if { $itk_option(-coupled) } {
	  set dx [expr $canx-$xref_]
	  set dy [expr $cany-$yref_]
	  $itk_option(-canvas) move ${this}_mark $dx $dy
	  set xref_ $canx
	  set yref_ $cany
       } else {
	  $itk_option(-canvas) coords $tag $canx $cany
       }
   }

   #  Move the centre.
   protected method move_centre_ {canx cany} {
       $itk_option(-canvas) coords $cross_id_ $canx $cany
   }

   #  Scale/rotate a mark about the current centre, or just move it.
   protected method scale_mark_ {tag canx cany} {
       if { $itk_option(-coupled) } {
	   #  Marks move as a whole. So scale and rotate about the centre.
	   set xa [expr $xcen_-$xref_]
	   set ya [expr $ycen_-$yref_]
	   set xb [expr $xcen_-$canx]
	   set yb [expr $ycen_-$cany]
	   set ta [expr atan2($ya,$xa)]
	   set tb [expr atan2($yb,$xb)]
	   set angle [expr $r2d_*($tb-$ta)]
	   if { [catch {expr sqrt(($xb*$xb+$yb*$yb)/($xa*$xa+$ya*$ya))} scale]  } {
	       set scale 1.005
	   }
	   $itk_option(-canvas) scale ${this}_mark $xcen_ $ycen_ $scale $scale
	   can_rotate_ $angle
	   set xref_ $canx
	   set yref_ $cany
       } else {
	   #  Marks are free to move individually.
	   move_mark_ $tag $canx $cany
       }
   }

   #  Update the X and Y position of a mark, unless all movements are
   #  coupled. In which case update all marks. Note effort required to
   #  deal with old_row as a list containing a list.
   protected method update_mark_ {nid} {
      if { !$itk_option(-coupled) } {
         select_row $nid
         set old_row [$itk_component(table) get_selected]
         eval lassign $old_row id ra dec x y
         lassign [$itk_option(-canvas) coords $tags_($nid)] newx newy
         $itk_option(-rtdimage) convert coords $newx $newy canvas x y image
         eval $itk_component(table) set_row $old_row [list "$id $ra $dec $x $y"]
      } else {
         update_marks_
      }
   }

   #  Update the X and Y values for all marks (positions obtained from
   #  canvas, forces redraw of all marks as scaling changes size).
   protected method update_marks_ { {restore 0} } {
      set nrows [$itk_component(table) total_rows]
      set oldcon [$itk_component(table) get_contents]
      $itk_component(table) clear
      for { set i 0 } { $i < $nrows } { incr i } {
	 lassign [lindex $oldcon $i] id ra dec x y
	 lassign [$itk_option(-canvas) coords $tags_($id)] newx newy
	 $itk_option(-rtdimage) convert coords $newx $newy canvas x y image
	 $itk_component(table) append_row [list $id $ra $dec $x $y]
      }
      $itk_component(table) new_info
      if { $restore } {
	 restore_b2_
      }
      clear_marks
      redraw
   }

   #  Delete a marker from the canvas. Also remove associated label.
   protected method remove_mark_ {id} {
      if { $id != {} } {
         set tag $tags_($id)
         unset tags_($id)
         unset marks_($tag)
         $itk_option(-canvas) delete $tag

         #  Also remove label.
         if { [info exists ltags_($id)] } {
            $itk_option(-canvas) delete $ltags_($id)
            unset ltags_($id)
         }
      }
   }

   #  Update the X and Y values for the centre.
   protected method update_centre_ {canx cany} {
       $itk_option(-rtdimage) convert coords $canx $cany canvas x y image
       configure -xcentre $x
       configure -ycentre $y
       if { $itk_option(-centre_cmd) != {} } {
	   eval $itk_option(-centre_cmd) $x $y
       }
   }

   #  Remove the selected objects from the table.
   public method remove_selected {} {
      set selected [$itk_component(table) get_selected]
       if {[llength $selected] == 0} {
	   error_dialog "No rows are selected" $w_
	   return
       }
       set id [lindex [lindex $selected 0] 0]
       if {! [confirm_dialog "Remove object with id $id" $w_]} {
	   return
       }
       $itk_component(table) remove_selected
       foreach line $selected {
          set id [lindex [lindex $line 0] 0]
          remove_mark_ $id
       }

       #  Update the display.
       redraw
   }

   #  Edit the selected object or object with given id.
   public method edit_selected_object { {id {}}} {
      catch {destroy $w_.ef}
      if { $id != {} } {
         $itk_component(table) search id $marks_($id)
      }
      set values [lindex [$itk_component(table) get_selected] 0]

      if {[llength $values] == 0} {
         error_dialog "No rows are selected" $w_
         return
      }
      gaia::GaiaEnterObject $w_.ef \
         -title {Please edit the data for the object below:} \
         -image $itk_option(-image) \
         -labels [$itk_component(table) cget -headings] \
         -values $values \
         -command [code $this replace_object $values]
   }

   #  Remove selected object using its canvas tag.
   public method remove_object { {tag {}} } {
      if { $tag != {} } {
         $itk_component(table) search id $marks_($tag)
      }
      set selected [$itk_component(table) get_selected]
      if {[llength $selected] == 0} {
         error_dialog "No rows are selected" $w_
         return
      }
      if {! [confirm_dialog "Remove object with id [lindex [lindex $selected 0] 0]" $w_]} {
         return
      }
      $itk_component(table) remove_selected
      set id [lindex [lindex $selected 0] 0]
      remove_mark_ $id

      # update the display
      redraw
   }

   #  Check that the format of the celestial coordinates entered is
   #  correct, normally there should be just one field (hh:mm:ss or
   #  dd:mm:ss), make this so if not correct (assume spaces should be :).
   protected method check_ra_and_dec_ {data} {
      set ra [lindex $data 1]
      set lenra [llength $ra]
      if { $lenra > 1 && $lenra < 4 } {
         regsub -all {[\ ]+} [string trim $ra] ":" newra
         set data [lreplace $data 1 1 $newra]
         set lenra 1
      }

      set dec [lindex $data 2]
      set lendec [llength $dec]
      if { $lendec > 1 && $lendec < 4 } {
         regsub -all {[\ ]+} [string trim $dec] ":" newdec
         set data [lreplace $data 2 2 $newdec]
         set lendec 1
      }

      if { $lenra > 1 || $lendec > 1 } {
         error_dialog \
            "Celestial coordinates are required to have the format hh/dd:mm:ss.ss or dd.ddd"
         return ""
      }
      return $data
   }

   #  Replace an object line in the table.
   public method replace_object {old_data new_data} {
      if {"$old_data" == "$new_data"} {
         info_dialog "No changes were made" $w_
         return
      }
      set new_data [check_ra_and_dec_ $new_data]
      if { $new_data == "" } {
         return
      }
      set id [lindex $new_data 0]
      if {! [confirm_dialog "Update object with id $id ?" $w_]} {
         return
      }

      #  Replace the data. Note if the identifier is changed then
      #  (since these are used to reference the tags etc.) we need
      #  to delete the marker.
      $itk_component(table) set_row $old_data $new_data
      lassign $old_data oldid
      lassign $new_data is
      if { $id != $oldid } {
         remove_mark_ $oldid
      }

      #  And redraw all objects.
      redraw
   }

   #  Add a new object to the list.
   public method enter_new_object {} {
      if { $enter_object_ != {} } {
         catch {destroy $enter_object_}
      }
      set enter_object_ \
         [gaia::GaiaEnterObject $w_.ef \
             -title {Please enter the data for a new object below:} \
             -image $itk_option(-image) \
             -labels [$itk_component(table) cget -headings] \
             -command [code $this enter_object]]
   }

   #  Enter a new object given its data.
   public method enter_object {new_data} {
      set new_data [check_ra_and_dec_ $new_data]
      if { $new_data == "" } {
         return
      }
      set id [lindex $new_data 0]
      if {! [confirm_dialog "Enter new object with id $id ?" $w_]} {
         return
      }
      $itk_component(table) append_row $new_data
      $itk_component(table) new_info
   }

   #  Update the x and y positions using the ra and dec and the
   #  current WCS. If estqual is 1 then derive the RMS difference
   #  between the old and new X, Y pairs.
   public method update_x_and_y {{estqual 0}} {
      set nrows [$itk_component(table) total_rows]
      set oldcon [$itk_component(table) get_contents]
      $itk_component(table) clear
      set rms 0.0
      if { ! $estqual } {
         for { set i 0 } { $i < $nrows } { incr i } {
            lassign [lindex $oldcon $i] id ra dec x y
            if { [ catch { $itk_option(-rtdimage) astwcs2pix $ra $dec } msg ] == 0 } {
               lassign $msg newx newy
               $itk_component(table) append_row [list $id $ra $dec $newx $newy]
            }
         }
      } else {
         set dist 0.0
         set nout 0
         for { set i 0 } { $i < $nrows } { incr i } {
            lassign [lindex $oldcon $i] id ra dec x y
            if { [ catch { $itk_option(-rtdimage) astwcs2pix $ra $dec } msg ] == 0 } {
               lassign $msg newx newy
               $itk_component(table) append_row [list $id $ra $dec $newx $newy]
               set dist [expr $dist+($x-$newx)*($x-$newx)+($y-$newy)*($y-$newy)]
               incr nout
            }
         }
         set rms [expr sqrt($dist/$nout)]
      }
      $itk_component(table) new_info
      redraw
      return $rms
   }

   #  Update the ra and dec positions using the current x and y
   #  positions. Positions are formatted.
   public method update_ra_and_dec {} {
      set nrows [$itk_component(table) total_rows]
      set oldcon [$itk_component(table) get_contents]
      $itk_component(table) clear
      for { set i 0 } { $i < $nrows } { incr i } {
         lassign [lindex $oldcon $i] id ra dec x y
         if { [ catch { $itk_option(-rtdimage) astpix2wcs $x $y 0 1} msg ] == 0 } {
            lassign $msg newra newdec
            $itk_component(table) append_row [list $id $newra $newdec $x $y]
         }
      }
      $itk_component(table) new_info
      redraw
   }

   #  Centroid the X and Y positions, hopefully making them more accurate.
   public method centroid {} {

       #  Extract the contents of the TableList and create the
       #  current projected image coordinates.
       set nrows [$itk_component(table) total_rows]
       set contents [$itk_component(table) get_contents]
       set coords {}
       for { set i 0 } { $i < $nrows } { incr i } {
	   lassign [lindex $contents $i] id ra dec x y
	   lappend coords $x $y
       }
       busy {
	   if { [catch { $itk_option(-rtdimage) foreign centroid \
                            "-isize $itk_option(-isize) \
                             -maxshift $itk_option(-maxshift) \
                             -coords $coords" } msg] == 0 } {

	       #  Succeeded so replace the x and y coordinates
	       #  by the new estimates.
	       replace_x_and_y $msg
	   } else {
	       error_dialog "$msg"
	   }
       }
   }

   #  Update the x and y positions using a new list of paired positions
   #  (i.e. a string of values). Assumes that table just contains this
   #  many values already.
   public method replace_x_and_y {newlist} {
      set oldcon [$itk_component(table) get_contents]
      $itk_component(table) clear
      set i 0
      foreach {newx newy} $newlist {
         lassign [lindex $oldcon $i] id ra dec x y
         $itk_component(table) append_row [list $id $ra $dec $newx $newy]
         incr i
      }
      $itk_component(table) new_info
      redraw
   }

   #  Create the menu item needed to allow editing of table lines.
   protected method make_edit_menu_ {} {
      set m $itk_option(-editmenu)
      add_short_help $m {Edit/create reference positions}

      $top_ add_menuitem $m command "Remove selected" \
         {Remove selected rows} \
         -command [code $this remove_selected]

      $top_ add_menuitem $m command "Enter new object..." \
         {Enter the data for a new object} \
         -command [code $this enter_new_object]

      $top_ add_menuitem $m command "Edit selected object..." \
         {Edit the data for the selected object} \
         -command [code $this edit_selected_object]
   }

   #  Create the menu item needed to control the appearance
   #  of the markers.
   protected method make_markers_menu_ {m} {

      #  Add the menus
      foreach {label name} {Type type Size size Width width \
                               {Outline colour} outline {Fill colour} \
                               fill {Fill stipple} stipple} {
         $m add cascade -label $label -menu [menu $m.$name]
      }

      #  Add short help texts for menu items
      $top_ add_menu_short_help $m Type {Set the marker shape}
      $top_ add_menu_short_help $m Size {Set the marker size}
      $top_ add_menu_short_help $m Width {Set the marker width}
      $top_ add_menu_short_help $m Fill {Set the fill color for some markers}
      $top_ add_menu_short_help $m Outline {Set the marker colour}
      $top_ add_menu_short_help $m Stipple {Select the stipple pattern for filling objects}

      #  Add the known types.
      foreach {name bitmap} $marker_types_ {
         $m.type add radiobutton \
            -value $name \
            -bitmap $bitmap \
            -command [code $this configure -mtype $name] \
            -variable [scope values_(mtype)]
      }

      #  Width menu
      foreach i {1 2 3 4} {
         $m.width add radiobutton \
            -value width$i \
            -bitmap width$i \
            -variable [scope values_(mwidth)] \
            -command [code $this configure -mwidth $i]
      }

      #  Size menu
      foreach i {3 5 7 9 11 15 21 31} {
         $m.size add radiobutton \
            -value $i \
            -label $i \
            -variable [scope values_(msize)] \
            -command [code $this configure -msize $i]
      }

      #  Outline  menu
      foreach i $itk_option(-colors) {
         $m.outline add radiobutton \
            -value $i \
            -command [code $this configure -mcolour $i] \
            -variable [scope values_(mcolour)] \
            -background $i
      }

      #  Fill menu
      $m.fill add radiobutton \
         -value {} \
         -label None \
         -command [code $this configure -mfill {}] \
         -variable [scope values_(mfill)]
      foreach i $itk_option(-colors) {
         $m.fill add radiobutton \
            -value $i \
            -command [code $this configure -mfill $i] \
            -variable [scope values_(mfill)] \
            -background $i
      }

      # Stipple  menu
      for {set i 0} {$i < 16} {incr i} {
         set bitmap pat$i
         $m.stipple add radiobutton \
            -value pat$i \
            -bitmap $bitmap \
            -variable [scope values_(mstipple)] \
            -command [code $this configure -mstipple pat$i]
      }

      #  Redraw and clear graphics.
      $m add separator
      $m add command -label "Clear" -command [code $this clear_marks]
      $top_ add_menu_short_help $m Clear {Clear all markers}
      $m add command -label "Redraw" -command [code $this redraw]
      $top_ add_menu_short_help $m Redraw {Redraw all markers}

   }

   #  Create the menu item for controlling labels.
   protected method make_labels_menu_ {} {
      set m $itk_option(-labelmenu)
      add_short_help $m {Display/modify table id columns as labels}

      $top_ add_menuitem $m command "Label selected objects" \
         {Set label for the selected object} \
         -command [code $this label_selected_objects_]

      $top_ add_menuitem $m command "Label all objects" \
         {Add labels to all objects} \
         -command [code $this label_objects]

      $top_ add_menuitem $m command "Remove all labels" \
      {Remove all labels from objects} \
         -command [code $this clear_labels]

      #  Add the menu to control the text colour.
      $m add cascade -label Colour -menu [menu $m.textcolor]
      foreach i $itk_option(-colors) {
         $m.textcolor add radiobutton \
            -value $i \
            -command [code $this configure -textcolor $i] \
            -variable [scope itk_option(-textcolor)] \
            -background $i
      }
      $top_ add_menu_short_help $m Color {Set label colour}

      #  Menu to select font (these should be same as CanvasDraw).
      $m add cascade -label Font -menu [menu $m.font]
      foreach i $itk_option(-fonts) {
         $m.font add radiobutton \
            -value $i \
            -label {abc} \
            -command [code $this configure -textfont $i] \
            -variable [scope itk_option(-textfont)] \
            -font $i
      }
      $top_ add_menu_short_help $m Color {Set label font}

   }

   #  Clear the graphics markers from canvas.
   public method clear_marks {} {
      $itk_option(-canvas) delete ${this}_mark
      $itk_option(-canvas) delete ${this}_label
      catch {$itk_option(-canvas) delete ${this}_cross}
      catch {unset marks_}
      catch {unset tags_}
      catch {unset ltags_}
   }

   #  Clear the table.
   public method clear_table {} {
      $itk_component(table) clear
      clear_marks
   }

   #  Clear any labels.
   public method clear_labels {} {
      $itk_option(-canvas) delete ${this}_label
      catch {unset ltags_}
   }

   #  Write a copy of the table to an ordinary file.
   public method write_to_file {} {
      set w [util::FileSelect .\#auto -title "Write positions to file"]
      if {[$w activate]} {
         set filename [$w get]
         if { [file exists $filename] } {
            if { ! [ confirm_dialog \
                     "$filename exists - do you want to overwrite?"] } {
               destroy $w
               return
            }
         }
         save_positions $filename
      }
      destroy $w
   }

   #  Write table contents to a named file.
   public method save_positions {filename} {
      set fid [::open $filename w]
      set nrows [$itk_component(table) total_rows]
      set contents [$itk_component(table) get_contents]
      for { set i 0 } { $i < $nrows } { incr i } {
         puts $fid [lindex $contents $i]
      }
      ::close $fid
   }

   #  Read positions from a text file.
   public method read_from_file {} {
      set w [util::FileSelect .\#auto -title "Read positions from file"]
      if {[$w activate]} {
         read_positions [$w get]
      }
      destroy $w
   }

   #  Read positions from a named text file. This file must have
   #  either 2, 3 or 5 space separated words, if the ra and decs are
   #  colon separated, if 7 or 9 words are located then the ra and
   #  decs are assumed space separated. The file can contain comment
   #  lines starting with an #. Note for this type of table we do not
   #  invoke the equinox setting call back as we do not know the equinox.
   public method read_positions {filename} {
      if { [file readable $filename] } {
         set fid [::open $filename r]
         set ok 1
         set nlines 0
         clear_table
         while { $ok } {
            set llen [gets $fid line]
            if { $llen > 0 } {
               if { ! [string match {\#*} $line ] } {
                  set nword [llength $line]
                  if { $nword == 2 } {
                     lassign $line ra dec
                     set id [incr nlines]
                     set x -1
                     set y -1
                  } elseif { $nword == 3 } {
                     lassign $line id ra dec
                     set x -1
                     set y -1
                  } elseif { $nword == 5 } {
                     lassign $line id ra dec x y
                  } elseif { $nword == 7 } {
                     lassign $line id ra ram ras dec decm decs
                     set ra "$ra:$ram:$ras"
                     set dec "$dec:$decm:$decs"
                     set x -1
                     set y -1
                  } elseif { $nword == 9 } {
                     lassign $line id ra ram ras dec decm decs x y
                     set ra "$ra:$ram:$ras"
                     set dec "$dec:$decm:$decs"
                  } else {
                     set ok 0
                     error "Cannot interpret line: $line"
                  }
                  if { $ok } {
                     set new_data [list $id $ra $dec $x $y]
                     set new_data [check_ra_and_dec_ $new_data]
                     if { $new_data == "" } {
                        error_dialog "Cannot interpret line: $line"
                        return
                     }
                     $itk_component(table) append_row $new_data
                  }
               }
            } else {
               set ok 0
            }
         }
         ::close $fid
         $itk_component(table) new_info
         redraw
      } else {
         error "Cannot read file: $filename."
      }
   }

   #  Methods for performing globals reorientations of the displayed
   #  positions. These work from the current image X,Y position.
   public method offset {dx dy} {
      set nrows [$itk_component(table) total_rows]
      set oldcon [$itk_component(table) get_contents]
      $itk_component(table) clear
      for { set i 0 } { $i < $nrows } { incr i } {
         lassign [lindex $oldcon $i] id ra dec x y
         $itk_component(table) append_row [list $id $ra $dec [expr $x+$dx] [expr $y+$dy]]
      }
      $itk_component(table) new_info
      redraw
   }

   #  Scale image X and Y coordinates about the current centre.
   public method scale {scale} {
       set nrows [$itk_component(table) total_rows]
       set oldcon [$itk_component(table) get_contents]
       $itk_component(table) clear
       set xc $itk_option(-xcentre)
       set yc $itk_option(-ycentre)
       for { set i 0 } { $i < $nrows } { incr i } {
	   lassign [lindex $oldcon $i] id ra dec x y
	   set newx [expr $xc+($x-$xc)*$scale]
	   set newy [expr $yc+($y-$yc)*$scale]
	   $itk_component(table) append_row [list $id $ra $dec $newx $newy]
       }
       $itk_component(table) new_info
       redraw
   }

   #  Rotate image positions about a centre.
   public method rotate {angle} {
       set nrows [$itk_component(table) total_rows]
       set oldcon [$itk_component(table) get_contents]
       $itk_component(table) clear
       set xc $itk_option(-xcentre)
       set yc $itk_option(-ycentre)
       for { set i 0 } { $i < $nrows } { incr i } {
	   lassign [lindex $oldcon $i] id ra dec x y
	   set costheta [expr cos($angle*$d2r_)]
	   set sintheta [expr sin($angle*$d2r_)]
	   set dx [expr $x-$xc]
	   set dy [expr $y-$yc]
	   set newx [expr $xc+($dx*$costheta-$dy*$sintheta)]
	   set newy [expr $yc+($dx*$sintheta+$dy*$costheta)]
	   $itk_component(table) append_row [list $id $ra $dec $newx $newy]
       }
       $itk_component(table) new_info
       redraw
   }

   #  Rotate markers on canvas (without changing table).
   public method can_rotate_ {angle} {
      foreach {tag id} [array get marks_] {
	 lassign [$itk_option(-canvas) coords $tag] x y
	 set costheta [expr cos($angle*$d2r_)]
	 set sintheta [expr sin($angle*$d2r_)]
	 set dx [expr $x-$xcen_]
	 set dy [expr $y-$ycen_]
	 set newx [expr $xcen_+($dx*$costheta-$dy*$sintheta)]
	 set newy [expr $ycen_+($dx*$sintheta+$dy*$costheta)]
	 $itk_option(-canvas) coords $tag $newx $newy
      }
   }

   #  Convert the RA/Decs to another equinox and make this the
   #  current equinox.
   public method change_equinox {equinox} {
      if { $equinox != $itk_option(-equinox) } {
         set nrows [$itk_component(table) total_rows]
         set oldcon [$itk_component(table) get_contents]
         $itk_component(table) clear
         for { set i 0 } { $i < $nrows } { incr i } {
            lassign [lindex $oldcon $i] id ra dec x y
            if { [ catch { $itk_option(-rtdimage) \
                              convert coords $ra $dec "wcs $itk_option(-equinox)" \
                              ra dec "wcs $equinox" } ] == 0 } {
               $itk_component(table) append_row [list $id $ra $dec $x $y]
            }
         }
         $itk_component(table) new_info
         configure -equinox $equinox
         redraw
      }
   }

   #  Remove any markers (and their entries) that lie off the image.
   public method clip_to_image {} {
      set nrows [$itk_component(table) total_rows]
      set oldcon [$itk_component(table) get_contents]
      $itk_component(table) clear
      set width [$itk_option(-rtdimage) width]
      set height [$itk_option(-rtdimage) height]
      for { set i 0 } { $i < $nrows } { incr i } {
         lassign [lindex $oldcon $i] id ra dec x y
         if { $x <= $width && $x > 0.0 && $y <= $height && $y > 0.0 } {
            $itk_component(table) append_row [list $id $ra $dec $x $y]
         } else {
            catch {remove_mark_ $id} msg
         }
      }
      $itk_component(table) new_info
      redraw
   }

   #  Flip the X,Y position about the centre of the image.
   public method flip {way} {
      if { $way == "x" || $way == "y" || $way == "xy" } {
         set nrows [$itk_component(table) total_rows]
         set oldcon [$itk_component(table) get_contents]
         $itk_component(table) clear
         if { $way == "x" } {
            set width [$itk_option(-rtdimage) width]
            for { set i 0 } { $i < $nrows } { incr i } {
               lassign [lindex $oldcon $i] id ra dec x y
               set x [expr $width-$x]
               $itk_component(table) append_row [list $id $ra $dec $x $y]
            }
         } elseif { $way == "y" } {
            set height [$itk_option(-rtdimage) height]
            for { set i 0 } { $i < $nrows } { incr i } {
               lassign [lindex $oldcon $i] id ra dec x y
               set y [expr $height-$y]
               $itk_component(table) append_row [list $id $ra $dec $x $y]
            }
         } elseif { $way == "xy" } {
            for { set i 0 } { $i < $nrows } { incr i } {
               lassign [lindex $oldcon $i] id ra dec x y
               $itk_component(table) append_row [list $id $ra $dec $y $x]
            }
         }
         $itk_component(table) new_info
         redraw
      }
   }

   #  Centre the image display on the "selected" object.
   protected method centre_selected_object_ {} {
      set id [lindex [lindex [$itk_component(table) get_selected] 0] 0]
      set canvas $itk_option(-canvas)
      set image $itk_option(-rtdimage)
      if {"$id" == "" || $canvas == "" || $image == ""} {
         return
      }
      if { [llength [set box [$canvas bbox $tags_($id)]]] } {
         lassign $box x0 y0 x1 y1
         set x [expr ($x1+$x0)/2.0]
         set y [expr ($y1+$y0)/2.0]

         set dw [$image dispwidth]
         set dh [$image dispheight]
         set cw [winfo width $canvas]
         set ch [winfo height $canvas]
         if {$cw != 1 && $dw && $dh} {
            $canvas xview moveto [expr (($x-$cw/2.0)/$dw)]
            $canvas yview moveto [expr (($y-$ch/2.0)/$dh)]
         }
      }
   }

   #  Label all objects.
   public method label_objects {} {
      set nrows [$itk_component(table) total_rows]
      set oldcon [$itk_component(table) get_contents]
      for { set i 0 } { $i < $nrows } { incr i } {
         lassign [lindex $oldcon $i] id
         label_object_ $id
      }
   }

   #  Label the selected object using its identifier. Labels are text
   #  strings that can be moved, but not editted.
   protected method label_selected_object_ {} {
      set line [lindex [$itk_component(table) get_selected] 0]
      lassign $line id
      label_object_ $id
   }

   #  Label all selected objects using their identifiers. Labels are text
   #  strings that can be moved, but not editted.
   protected method label_selected_objects_ {} {
      foreach line [$itk_component(table) get_selected] {
         lassign $line id
         label_object_ $id
      }
   }

   #  Label the given object. The arg is the label. The position is
   #  determined from the bounding box of the marker.
   protected method label_object_ {id} {
      set canvas $itk_option(-canvas)
      set image $itk_option(-rtdimage)
      if {"$id" == "" || $canvas == "" || $image == ""} {
         return
      }

      #  Get position of text, slightly offset from centre of bounding
      #  box.
      if { [llength [set box [$canvas bbox $tags_($id)]]] } {
         lassign $box x0 y0 x1 y1
         set x [expr ($x0+$x1)*0.5]
         set y $y0
      } else {
         #  Marker not around.
         return
      }

      #  Create tag for label, remove previous copy and create it.
      set ltags_($id) label$id
      set tags [list objects ${this}_label label$id]
      $canvas delete label$id
      set labelid [$canvas create text $x $y \
                      -text "$id" \
                      -anchor c \
                      -fill $itk_option(-textcolor) \
                      -font $itk_option(-textfont) \
                      -tags $tags]

      #  Make sure user can adjust label position and properies using
      #  the canvasdraw tools.
      [$itk_option(-image) component draw] add_object_bindings $labelid
      ct_add_bindings $canvas $labelid
   }

   #  Configuration options
   #  =====================

   #  Title for table.
   itk_option define -title title Title {Reference positions}

   #  Width of the table (in characters).
   itk_option define -width width Width 40

   #  Name of a menu to add the control commands.
   itk_option define -editmenu editmenu EditMenu {}

   #  Name of a menu to add the marker control commands.
   itk_option define -markmenu markmenu MarkMenu {}

   #  Name of a menu to add the label control commands.
   itk_option define -labelmenu labelmenu LabelMenu {}

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Name of the canvas holding the starrtdimage widget.
   itk_option define -canvas canvas Canvas {}

   #  Name of the RtdImage widget or derived class.
   itk_option define -image image Image {}

   #  Define whether marks are movement coupled.
   itk_option define -coupled coupled Coupled 1 {
       if { $going_ } {
	   set configure_ 1
	   redraw_cross_
	   set configure_ 0
       }
   }

   #  Size of the markers drawn to display current positions.
   itk_option define -msize msize Msize 5 {
       set values_(msize) $itk_option(-msize)
       if { $going_ } {
	   set configure_ 1
	   redraw
	   set configure_ 0
       }
   }

   #  Type of marker drawn to display position (make sure it is lower
   #  case and one of the known types).
   itk_option define -mtype mtype Mtype circle {
      set itk_option(-mtype) [string tolower $itk_option(-mtype)]
      if { ! [regexp $marker_regexp_ $itk_option(-mtype) ] } {
         set itk_option(-mtype) cross
      }
      set values_(mtype) $itk_option(-mtype)
       if { $going_ } {
	   set configure_ 1
	   redraw
	   set configure_ 0
       }
   }

   #  Width of a marker.
   itk_option define -mwidth mwidth Mwidth 1 {
       set values_(mwidth) $itk_option(-mwidth)
       if { $going_ } {
	   set configure_ 1
	   redraw
	   set configure_ 0
       }
   }

   #  Outline colour of marker.
   itk_option define -mcolour mcolour Mcolour blue {
       set values_(mcolour) $itk_option(-mcolour)
       if { $going_ } {
	   set configure_ 1
	   redraw
	   set configure_ 0
       }
   }

   #  Fill colour of marker.
   itk_option define -mfill mfill Mfill {} {
       set values_(mfill) $itk_option(-mfill)
       if { $going_ } {
	   set configure_ 1
	   redraw
	   set configure_ 0
       }
   }

   #  Fill stipple pattern.
   itk_option define -mstipple mstipple MStipple {} {
       set values_(mstipple) $itk_option(-mstipple)
       if { $going_ } {
	   set configure_ 1
	   redraw
	   set configure_ 0
       }
   }

   #  Colour of text labels.
   itk_option define -textcolor textcolor TextColor white {
      if { $going_ } {
         $itk_option(-canvas) itemconfigure ${this}_label \
            -fill $itk_option(-textcolor)
      }
   }

   #  Possible colours.
   itk_option define -colors colors Colors {
       white
       grey90 grey80 grey70 grey60 grey50 grey40 grey30 grey20 grey10
       black
       red green blue cyan magenta yellow
   }

   #  Command to execute when a table is grabbed and an equinox may be
   #  established.
   itk_option define -notify_cmd notify_cmd Notify_Cmd {}

   #  Equinox of the table. Used in convert commands to go from RA/Dec
   #  values into X and Y and may be canvas coordinates. Necessary as
   #  convert coords with wcs use J2000 without anyother indication.
   itk_option define -equinox equinox Equinox J2000

   #  Rotation centre.
   itk_option define -xcentre xcentre Xcentre 0.0 {
       if { $going_ } {
	   set configure_ 1
	   redraw_cross_
	   set configure_ 0
       }
   }
   itk_option define -ycentre ycentre Ycentre 0.0 {
       if { $going_ } {
	   set configure_ 1
	   redraw_cross_
	   set configure_ 0
       }
   }

   #  Call back for notification of a change in the centre.
   itk_option define -centre_cmd centre_cmd Centre_Cmd {}

   #  Centroid parameters.
   itk_option define -isize isize Isize 9 {}
   itk_option define -maxshift maxshift Maxshift 5.5 {}

   #  Whether to add <Any-Enter> bindings (these can cause confusion for
   #  some applications).
   itk_option define -bind_enters bind_enters Bind_enters 0

   #  Default font used in canvas to mark objects
   itk_option define -textfont textFont TextFont TkFixedFont {
      if { $going_ } {
         $itk_option(-canvas) itemconfigure ${this}_label \
            -font $itk_option(-textfont)
      }
   }

   #  Possible fonts for drawing labels.
   itk_option define -fonts fonts Fonts {
      -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-120-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*
      -adobe-courier-medium-r-*-*-*-140-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-140-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-140-*-*-*-*-*-*
      -adobe-courier-medium-r-*-*-*-180-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-180-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-180-*-*-*-*-*-*
      -adobe-courier-medium-r-*-*-*-240-*-*-*-*-*-*
      -adobe-courier-medium-o-*-*-*-240-*-*-*-*-*-*
      -adobe-courier-bold-r-*-*-*-240-*-*-*-*-*-*
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Default values of the controls.
   protected variable default_

   #  Canvas position conversion routines.
   protected variable canvasX_ {}
   protected variable canvasY_ {}

   #  The id field associated with each mark (indexed by mark canvas tag),
   #  and the inverse (index by object id to return canvas tag).
   protected variable marks_
   protected variable tags_

   #  Tags used for any labels.
   protected variable ltags_

   #  Widths of various fields.
   protected variable vwidth_ 20
   protected variable lwidth_ 20

   #  The marker shapes and their bitmaps.
   protected variable marker_types_ \
      {dot dot plus gaiaplus cross cross square square circle
         circle triangle triangle diamond diamond}
   protected variable marker_regexp_ \
      {dot|plus|cross|square|circle|triangle|diamond}

   #  Initialisation completed.
   protected variable going_ 0

   #  Name of TopLevelWidget that this widget exists within (no . allowed).
   protected variable top_ {}

   #  Degrees to radians factor and inverse.
   protected variable d2r_ 0.017453292
   protected variable r2d_ 57.29577951

   #  Whether to reconfigure marks or not.
   protected variable configure_ 0

   #  Canvas id of cross.
   protected variable cross_id_ {}

   #  Canvas binding for <B2-Motion>.
   protected variable b2bind_ {}

   #  Reference position for canvas marker movement.
   protected variable xref_ 0
   protected variable yref_ 0

   #  Rotation/scaling centre in canvas coordinates.
   protected variable xcen_ 0
   protected variable ycen_ 0

   #  Table configuration (internal).
   protected variable selectmode_ extended
   protected variable exportselection_ 0

   #  GaiaEnterObject reference.
   protected variable enter_object_ {}

   #  Variable to share amongst all widgets.
   protected variable values_

   #  Name of catalogue selected during grab.
   protected variable catalogue_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
