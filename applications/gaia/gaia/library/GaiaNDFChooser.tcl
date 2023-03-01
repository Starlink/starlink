#+
#  Name:
#     GaiaNDFChooser

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Provides a toolbox for displaying NDFs and their components,
#     stored in a single container file

#  Description:
#     This class provides a selection chooser for picking NDFs
#     stored in a container file. It is different from a plain
#     file chooser in that is understands multiple NDFs stored in the
#     same container file, and also allows the selection of a
#     "displayable" component (these are the NDF data, variance and
#     quality arrays, with error as a synonym for variance as standard
#     deviations).
#
#     This class is the NDF equivalent of SkyCatHDUChooser.

#  Invocations:
#
#        GaiaNDFChooser object_name [configuration options]
#
#     This creates an instance of a GaiaNDFChooser object. The return is
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
#     See method definitions below.

#  Inheritance:
#     util::TopLevelWidget

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
#     17-JAN_2000 (PWD):
#        Original version.
#     29-NOV-2001 (PWD):
#        Refitted from 2.7 development tree to show image table.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaNDFChooser {}

itcl::class gaia::GaiaNDFChooser {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args
      wm protocol $w_ WM_DELETE_WINDOW [code $this quit]
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   # This method is called after the options have been evaluated.
   protected method init {} {
      wm title $w_ "Select NDF in container file ($itk_option(-number))"
      wm iconname $w_ "NDFs ($itk_option(-number))"

      #  Get internal image handle
      set image_ [$itk_option(-image) get_image]

      #  Create the interface.
      make_table_
      make_buttons_
      make_short_help_

      #  Pack main components, so that buttons are always shown.
      pack $itk_component(buttons) -side bottom -fill x -expand 1
      pack $itk_component(table) -side top -fill both -expand 1

      #  And start it up.
      show_hdu_list
   }

   # Quit this widget
   public method quit {} {
      destroy $w_
   }

   # Make the table component for displaying the NDF info
   protected method make_table_ {} {
      set headings [$image_ hdu listheadings]

      # TableList(n) widget for displaying the list of NDFs
      itk_component add table {
         set table_ [util::TableList $w_.table \
                        -title "NDF list for file: [$image_ cget -file]" \
                        -headings $headings \
                        -width [expr [string length $headings]*2]]
      }

      bind $table_.listbox <ButtonRelease-1> [code $this select_ndf_]
      bind $table_.listbox <Double-ButtonPress-1> [code $this set_ndf_ "data"]
      bind $table_.listbox <Return> [code $this set_ndf_ "data"]
   }

   #  Make a subwindow for displaying miniature versions of image
   #  extensions.
   protected method make_image_table_ {} {

      # Frame (BLT table) used to display images in NDF components
      itk_component add image_table {
         frame $w_.imagetab
      }
      set imagetab_ $w_.imagetab
      pack $itk_component(image_table) -side top -fill x
   }

   #  Buttons to control NDF display.
   protected method make_buttons_ {} {
      global $w_.show
      set $w_.show $itk_option(-show_images)

      # Button frame at bottom of window
      itk_component add buttons {
         frame $w_.buttons \
            -borderwidth 2 \
            -relief raised
      }
      itk_component add data {
         button $w_.buttons.data \
            -text Data \
            -command [code $this set_ndf_ data]
      }
      itk_component add var {
         button $w_.buttons.var \
            -text Variance \
            -command [code $this set_ndf_ variance]
      }
      itk_component add err {
         button $w_.buttons.err \
            -text Error \
            -command [code $this set_ndf_ error]
      }
      itk_component add qual {
         button $w_.buttons.qual \
            -text Quality \
            -command [code $this set_ndf_ quality]
      }
      itk_component add display {
         button $w_.buttons.display \
            -text "Display as one image" \
            -state disabled \
            -command [code $this display_as_one_image]
      }
      itk_component add show {
         checkbutton $w_.buttons.show \
            -text "Show NDF images" \
            -variable $w_.show \
            -onvalue 1 \
            -offvalue 0 \
            -relief raised \
            -pady 0 \
            -highlightthickness 0 \
            -command [code $this show_images]
      }
      itk_component add close {
         button $w_.buttons.close \
            -text Close \
            -command "destroy $w_"
      }

      pack $itk_component(data) -side left -expand 1
      pack $itk_component(var) -side left -expand 1
      pack $itk_component(err) -side left -expand 1
      pack $itk_component(qual) -side left -expand 1
      pack $itk_component(display) -side left -expand 1
      pack $itk_component(show) -side left -fill y -expand 1
      pack $itk_component(close) -side left -expand 1
   }

   # Set the cut levels for the image extensions to the given percent
   method auto_set_cut_levels {{percent 99}} {
      busy {
         for {set i 0} {$i < $num_images_} {incr i} {
            $ext_($i,image) autocut -percent $percent
            update idletasks
         }
      }
   }

   # Set the cut levels and colormap for the image extensions to the ones
   # used in the main image
   method use_settings_from_main_image {} {
      # return if no image
      if {! [info exists ext_(0,image)]} {
         return
      }

      lassign [$image_ cut] low high
      if { "$low" == "" || "$high" == "" || [$image_ isclear] } {
         return
      }
      set cmap [$image_ cmap file]
      set itt [$image_ itt file]
      set colorscale [$image_ colorscale]
      busy {
         for {set i 0} {$i < $num_images_} {incr i} {
            $ext_($i,image) cut $low $high
            $ext_($i,image) colorscale $colorscale
            $ext_($i,image) cmap file $cmap
            $ext_($i,image) itt file $itt
            update idletasks
         }
      }
   }

   #  Add a short help window
   protected method make_short_help_ {} {
      util::TopLevelWidget::make_short_help

      add_short_help $itk_component(table) \
         {Table: Click to select NDF, double-click to display image}

      add_short_help $itk_component(data) \
         {Open and display the selected NDF data component}

      add_short_help $itk_component(var) \
         {Open and display the selected NDF variance component}

      add_short_help $itk_component(err) \
         {Display the selected NDF variance component in standard deviations}

      add_short_help $itk_component(qual) \
         {Open and display the selected NDF quality component}

      add_short_help $itk_component(buttons).show \
         {Show/Hide the display of the miniature versions of the image extensions}

      add_short_help $itk_component(buttons).close {Close window}
   }

   #  Update the list of NDFs and the image displays, if needed
   public method show_hdu_list {} {
      set old_filename $filename_
      set filename_ [$image_ cget -file]
      set ndf_list [$image_ hdu list]
      set headings [$image_ hdu listheadings]

      #  Update the table listing
      $table_ clear
      $table_ config -height [llength $ndf_list] -info $ndf_list
      if {"$filename_" == "$old_filename"} {
         return
      }

      #  Initially we don't show the small image, until we know we
      #  have more than one.
      $w_.buttons.show config -state disabled

      #  Delete old images
      show_images
      if {"$filename_" == ""} {
         return
      }

      #  Update title.
      $table_ configure -title "NDF list for file: $filename_" \

      #  See if there is more than one image, otherwise skip it.
      set num_images_ 0
      catch {unset ext_}
      foreach ndf $ndf_list {
         eval lassign [list $ndf] $headings
         set ext_($num_images_,ndf) $number
         set ext_($num_images_,name) $name
         incr num_images_
      }
      if {$num_images_ <= 1} {
         return
      }

      #  Arrange ordering of images.
      set num_cols 4
      set num_rows [expr $num_images_/$num_cols+$num_cols]
      set max_row_ 0
      set n 0
      for {set row 0} {$row < $num_rows} {incr row} {
         for {set col 0} {$col < $num_cols} {incr col} {
            if {$n == $num_images_} {
               break
            }
            set ext_($n,row) $row
            set ext_($n,col) $col
            incr n
            set max_row_ [max $max_row_ $row]
         }
      }

      #  Enabled show image buttons, if more than one.
      if { $num_images_ > 1 } {
         $w_.buttons.show config -state normal
         #if { $use_crpix } {
         $w_.buttons.display config -state normal
         #}
      }

      # Select the NDF being displayed, if any
      select_image_ndf_ [$image_ hdu]
   }


   # Remove all image minature versions
   public method delete_images {} {
      global $w_.show
      set $w_.show 0
      delete_images_
   }

   # Really remove all image miniatures, if displayed.
   protected method delete_images_ {} {
      if {[info exists imagetab_]} {
         destroy $imagetab_
         unset imagetab_
      }
   }

   # Show all image minature versions (called by checkbutton)
   protected method show_images {} {
      global $w_.show
      delete_images_
      if { [set $w_.show] } {
         create_images
      }
   }

   # Put the images in the table
   protected method create_images {} {
      make_image_table_

      set w $imagetab_
      for {set i 0} {$i < $num_images_} {incr i} {
         set f [frame $w.f$i -borderwidth 2 -relief raised]
         set im [rtd::RtdImage $f.im \
                    -graphics 0 \
                    -displaymode 0 \
                    -canvaswidth 100 \
                    -canvasheight 100 \
                    -fitwidth 100 \
                    -fitheight 100]
         pack $im -fill both -expand 1

         # save widget names for later reference
         set ext_($i,frame) $f
         set ext_($i,RtdImage) $im
         set ext_($i,image) [$im get_image]
         set ext_($i,canvas) [$im get_canvas]

         #  Add the image to the table
         set row [expr $max_row_-$ext_($i,row)]
         set col $ext_($i,col)
         blt::blttable $w $f ${row},${col} -fill both
      }

      frame $w.buttons2 -borderwidth 2
      pack [button $w.buttons2.settings \
               -text "Use Settings from Main Image" \
               -command [code $this use_settings_from_main_image]]

      add_short_help $w.buttons2.settings \
         {Set the cut levels and colormap for the preview images to the one used in the main image}

      blt::blttable $w \
         $w.buttons2 [expr $max_row_ + 1],0 -fill x -columnspan [min 4 $num_images_]

      update  ; # wait until images are displayed (needed !)

      # configure the images
      lassign [$image_ cut] low high
      busy {
         for {set i 0} {$i < $num_images_} {incr i} {
            $ext_($i,image) config -file $filename_
            add_image_bindings_ $ext_($i,RtdImage) $ext_($i,ndf) $ext_($i,name)
            $ext_($i,image) cut $low $high
            update idletasks
         }
      }
   }

   #  This method is called when the user clicks on an image NDF icon.
   #  Display the selected image.
   protected method select_image_ndf_ {ndf {component "data"}} {

      #  Hold using blt::busy, not busy{}. See release call for why.
      blt::busy hold $w_

      if { [catch {
         for {set i 0} {$i < $num_images_} {incr i} {
            if {[info exists ext_($i,frame)]} {
               if {"$ext_($i,ndf)" == "$ndf"} {
                  $ext_($i,frame) configure -relief sunken
               } else {
                  $ext_($i,frame) configure -relief raised
               }
            }
         }
         catch "$table_ select_row [expr $ndf-1]"
         select_ndf_

         $itk_option(-image) configure -component $component
         $itk_option(-image) update_title
         $image_ hdu $ndf $component
      } msg ] } {
         warning_dialog "Failed to select NDF component: $msg"

         #  If this isn't the data component, switch back to that.
         #  Alternatively we could clear the display, but that would
         #  loose the context.
         if { $component != "data" } {
            select_image_ndf_ $ndf "data"
         }
      }

      #  Do not release if window has been destroyed. This can happen
      #  during the handle new image callback (see GaiaImageCtrl).
      if { [winfo exists $w_] } {
         blt::busy release $w_
      }
   }

   #  Add bindings to the given RtdImage itcl class object and set it to
   #  display the given NDF when clicked on. Note this always displays
   #  the "data" component by default and does the initial selection
   #  of the hdu component.
   protected method add_image_bindings_ {im ndf name {component "data"}} {
      set image [$im get_image]
      set canvas [$im get_canvas]

      #  Set the NDF for the image
      busy {
         $image hdu $ndf "data"
         $itk_option(-image) configure -component $component
         $itk_option(-image) update_title
      }

      #  Need to add 2 bindings: one for the image, one for the background
      bind $canvas <1> [code $this select_image_ndf_ $ndf $component]

      #  Set up a resize handler to change the image size
      bind $canvas <Configure> [code $this resize_ $im %w %h]

      #  Add a help message indicating which image it is
      set s $name
      if {"$s" != ""} {
         set s "($s)"
      }
      add_short_help $canvas "Click here to display NDF $ndf $s"
   }

   # This method is called when the image window is resized.
   # The rtdImage widget and the new width and height are given.
   protected method resize_ {im new_width new_height} {
      set image [$im get_image]
      $image config -fitwidth $new_width -fitheight $new_height
      $im center
   }

   #  Set the NDF to display. Makes the currently selected NDF the
   #  current NDF.
   protected method set_ndf_ {component} {
      set sel [$table_ get_selected]
      if {[llength $sel]} {
         lassign [lindex $sel 0] number
         select_image_ndf_ $number $component
      } else {
         warning_dialog "No NDF is selected (click on a row)" $w_
      }
   }

   #  This method is called when a line in the NDF list is selected.
   protected method select_ndf_ {} {
      set sel [$table_ get_selected]
      if {[llength $sel]} {
         lassign [lindex $sel 0] number name naxis1 naxis2 havvar havqual
         if { "$havvar" == "true" } {
            $itk_component(var) configure -state normal
            $itk_component(err) configure -state normal
         } else {
            $itk_component(var) configure -state disabled
            $itk_component(err) configure -state disabled
         }
         if { "$havqual" == "true" } {
            $itk_component(qual) configure -state normal
         } else {
            $itk_component(qual) configure -state disabled
         }
      }
   }

   # Display all of the image extensions as a single image (combine based
   # on CRPIX1 and CRPIX2 keywords).
   protected method display_as_one_image {} {
      busy {
         $image_ hdu display
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Target GaiaCtrlImage.
   itk_option define -image image Image {}

   #  Whether to display small images.
   itk_option define -display_images display_images Display_Images 0 {
      if { $itk_option(-display_images) } {
         show_hdu_list
      }
   }

   #  Flag: if true, images are "subsampled" when shrinking,
   #  otherwise the pixels are averaged
   itk_option define -subsample subsample Subsample 1

   #  Default: show image extensions, bool
   itk_option define -show_images show_images Show_images {0}

   #  Optionally specify TopLevelWidget to display short help messages
   itk_option define -shorthelpwin shortHelpWin ShortHelpWin {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Internal rtdimage object.
   protected variable image_

   #  Table displaying the NDFs.
   protected variable table_

   #  Table displaying image extensions.
   protected variable imagetab_

   #  Name of image file.
   protected variable filename_ {}

   #  Number of displayable NDFs in the current HDS container file.
   protected variable num_images_ 0

   #  Array(ndf,keyword) of image keyword and widget info
   protected variable ext_

   #  Max row.
   protected variable max_row_ 0

   #  Common variables: (shared by all instances)
   #  -----------------
   protected variable component_ "data"

#  End of class definition.
}
