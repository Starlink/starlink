#+
#  Name:
#     GaiaAstTransfer

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Aids the "transfer" of astrometric reference positions between
#     images.

#  Description:
#     This class creates a toplevel window with controls for selecting
#     X,Y positions on an associated image and for selecting
#     the corresponding RA,Dec positions on any other image that is
#     displayed (in another clone). The positions may be "centroided"
#     (using the pick object algorithm) and a list of them is
#     displayed in a text window. When completed these positions may
#     be used to update a StarAstTable object.

#  Invocations:
#
#        GaiaAstTransfer object_name [configuration options]
#
#     This creates an instance of a GaiaAstTransfer object. The return is
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

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     05-MAR-1999 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaAstTransfer {
   
   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget
   
   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      
      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Transfer coordinates ($itk_option(-number))"

      #  Add short help window.
      make_short_help

      #  Add table for displaying coordinates (note fixed headings).
      itk_component add table {
         TableList $w_.table \
            -title "Reference positions" \
            -hscroll 1 \
            -selectmode single \
            -exportselection 1 \
            -headings {id ra dec x y} \
            -width $itk_option(-width)
      }
      add_short_help $itk_component(table) \
         {Reference positions and their associated X,Y coordinates}
      pack $itk_component(table) -side top -fill both -expand 1

      #  Add control for selecting the image (i.e. window clone) to
      #  read the RA,Dec values from.
      add_targets_

      #  Add frame for holding table action buttons.
      itk_component add frame1 {
         frame $w_.frame1
      }

      #  Add button for adding a new row.
      itk_component add new {
         button $itk_component(frame1).new -text "Add row" \
            -command [code $this add_new_row]
      }
      add_short_help $itk_component(new) \
         {Create a new blank row in the table (do this first)}
      pack $itk_component(new) -side left -fill x -pady 2 -padx 2

      #  Add button for updating the X and Y coordinates of the
      #  currently selected row.
      itk_component add update_xy {
         button $itk_component(frame1).xy -text "Update X,Y" \
            -command [code $this update_xy]
      }
      add_short_help $itk_component(update_xy) \
         {Update the X and Y coordinates by selecting an object}
      pack $itk_component(update_xy) -side left -fill x -pady 2 -padx 2

      #  Add button for updating the RA/Dec coordinates of the
      #  currently selected row.
      itk_component add update_radec {
         button $itk_component(frame1).radec -text "Update RA,Dec" \
            -command [code $this update_radec]
      }
      add_short_help $itk_component(update_radec) \
         {Update the RA and Dec coordinates by selecting an object}
      pack $itk_component(update_radec) -side left -fill x  -pady 2 -padx 2

      #  Pack button frame.
      pack $itk_component(frame1) -side top -pady 2 -padx 2 -anchor w

      #  First time add a new row.
      add_new_row
   }   

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Add a new row to the table. This serves as a blank entry which
   #  is updated to have the correct X,Y and RA/Dec coordinates.
   public method add_new_row {} {

      #  Increment the identifiers.
      incr ids_

      #  Create the new row.
      $itk_component(table) append_row [list $ids_ 00:00:00 00:00:00 0.0 0.0]
      $itk_component(table) new_info

      #  Make this the current selection.
      $itk_component(table) select_row end
   }


   #  Update the X and Y coordinates of the currently selected
   #  row. This is done by selecting a position in the associated
   #  image and then refining this.
   public method update_xy {} {

      #  Get the selected row.
      set row [$itk_component(table) get_selected]
      if { $row != "" } { 

         #  Ok, get a coordinate position from the associated image.
         set result [get_coords_ $itk_option(-rtdimage)]

         #  Extract X and Y and replace.
         lassign $result ra dec x y
         eval lassign $row id ra dec xdummy ydummy
         eval $itk_component(table) set_row $row [list "$id $ra $dec $x $y"]
      }
   }

   #  Update the RA and Dec coordinates of the currently selected
   #  row. This is done by selecting a position in the target image
   #  and then refining this.
   public method update_radec {} {
      #  Get the selected row.
      set row [$itk_component(table) get_selected]
      if { $row != "" } { 

         #  Ok, get a coordinate position from the associated image.
         set result [get_coords_ $target_]

         #  Extract RA and Dec and replace.
         lassign $result ra dec x y
         eval lassign $row id radummy decdummy x y
         eval $itk_component(table) set_row $row [list "$id $ra $dec $x $y"]
      }

   }

   #  Add all the image views to a menu for selection as the
   #  current RA,Dec reference image (the "target").
   protected method add_targets_ {} {

      #  Menu button for selection.
      itk_component add targets {
         LabelMenu $w_.targets \
            -labelwidth 12 \
            -valuewidth 20 \
            -valueanchor e \
            -text "Target image:"
      }
      pack $itk_component(targets) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(targets) \
         {Image used for obtaining RA,Dec coordinates}

      #  Now locate and add all images. The current image is "$target_". 
      set images [SkyCat::get_skycat_images]
      foreach w $images {
         set name [$w cget -file]
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

   #  Set the target image for RA,Dec coordinates.
   protected method set_target_ {image} {
      set target_ $image
   }

   #  Select a coordinate position on a given image. The return is a
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

      #  Convert canvas coordinates into X,Y and RA,Dec. Image X and 
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
      $rtdimage convert coords $imagex $imagey image ra dec "wcs $itk_option(-equinox)"

      #  Return coordinates.
      global ::$w_.picked
      set $w_.picked "$ra $dec $imagex $imagey"
   }

   #  Configuration options: (public variables)
   #  ----------------------
   
   #  Name of associated RtdImage (or some derived class).
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Width of the table (in characters).
   itk_option define -width width Width 40

   #  Centroid parameters.
   itk_option define -isize isize Isize 9
   itk_option define -maxshift maxshift Maxshift 5.5

   #  Equinox of values to be entered into table.
   itk_option define -equinox equinox Equinox J2000

   #  Protected variables: (available to instance)
   #  --------------------

   #  Name of target image.
   protected variable target_ {}

   #  Row identifiers.
   protected variable ids_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
