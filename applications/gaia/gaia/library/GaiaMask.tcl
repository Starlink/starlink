#+
#  Name:
#     GaiaMask

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Control the display of an image using an integer mask to segment.

#  Description:
#     Displays an image using a given mask (usually the output from CUPID)
#     to pick out regions for display.

#  Invocations:
#
#        GaiaMask object_name [configuration options]
#
#     This creates an instance of a GaiaCube object. The return is
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
#     util::TopLevelWidget

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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     06-JUN-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaMask {}

itcl::class gaia::GaiaMask {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      set lwidth 18
      set vwidth 10

      #  Set window properties.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]
      wm title $w_ "Display masked regions ($itk_option(-number))"

      #  Add short help window
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Add the close menu item.
      add_menuitem $File command Close \
         {Close this window} \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add window help.
      add_help_button mask "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Name of input image.
      itk_component add image {
         LabelFileChooser $w_.image \
            -text "Input image:" \
            -textvariable [scope itk_option(-image)] \
            -command [code $this open] \
            -filter_types $itk_option(-filter_types) \
            -chooser_title "Select image"
      }
      pack $itk_component(image) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(image) \
         {Name of the input image to be masked}

      #  Name of input mask.
      itk_component add mask {
         LabelFileChooser $w_.mask \
            -text "Input mask:" \
            -textvariable [scope itk_option(-mask)] \
            -command [code $this open] \
            -filter_types $itk_option(-filter_types) \
            -chooser_title "Select mask"
      }
      pack $itk_component(mask) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(mask) \
         {Name of the input masked, integer data segmenting input image}

      #  Close window.
      itk_component add close {
         button $w_.close -text Close \
            -command [code $this close]
      }
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(close) {Close window}
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Release memory used for dummy image.
      if { $last_adr_ != 0 } {
         catch {$imageaccessor_ release $last_adr_}
      }

      #  Name handlers.
      if { $image_namer_ != {} } {
         catch {::delete object $image_namer_}
      }
      if { $mask_namer_ != {} } {
         catch {::delete object $mask_namer_}
      }
   }

   #  Methods:
   #  --------

   #  Close window.
   public method close {} {
      wm withdraw $w_
   }

   #  Open the image and mask and apply.
   public method open {args} {
      if { $itk_option(-image) == {} || $itk_option(-mask) == {} } {
         return
      }
      open_image_
      open_mask_
      load_image_
      load_mask_
      create_dummy_
   }

   #  Open the image.
   protected method open_image_ {} {

      if { $image_namer_ == {} } {
         set image_namer_ [GaiaImageName \#auto]
      }
      $image_namer_ configure -imagename $itk_option(-image)

      if { ! [$image_namer_ exists] } {
         if { $itk_option(-image) != {} } {
            error_dialog "No such file: $itk_option(-image)" $w_
         }
         return
      }

      $image_namer_ absolute
      set fullname [$image_namer_ fullname]

      if { $imageaccessor_ == {} } {
         set imageaccessor_ [uplevel \#0 GaiaNDAccess \#auto]
      }
      $imageaccessor_ configure -dataset "$fullname"
   }

   #  Open the mask.
   protected method open_mask_ {} {

      if { $mask_namer_ == {} } {
         set mask_namer_ [GaiaImageName \#auto]
      }
      $mask_namer_ configure -imagename $itk_option(-mask)

      if { ! [$mask_namer_ exists] } {
         if { $itk_option(-mask) != {} } {
            error_dialog "No such file: $itk_option(-mask)" $w_
         }
         return
      }

      $mask_namer_ absolute
      set fullname [$mask_namer_ fullname]

      if { $maskaccessor_ == {} } {
         set maskaccessor_ [uplevel \#0 GaiaNDAccess \#auto]
      }
      $maskaccessor_ configure -dataset "$fullname"
   }

   #  Load the image by mapping its data.
   protected method load_image_ {} {
      if { $imageaccessor_ == {} } {
         return
      }
      busy {
         $imageaccessor_ unmap
         set image_adr_ [$imageaccessor_ map "READ" "DATA"]
      }
   }

   #  Load the mask by mapping its data.
   protected method load_mask_ {} {
      if { $maskaccessor_ == {} } {
         return
      }
      busy {
         $maskaccessor_ unmap
         set mask_adr_ [$maskaccessor_ map "READ" "DATA"]
      }
   }

   #  Create the temporary image that will be used to display the masked data.
   #  XXX need to regenerate when image or mask changes.
   protected method create_dummy_ {} {

      #  Set name of the dummy image.
      set oldname $dummy_name_
      set dummy_name_ \
         [gaia::GaiaTempName::make_name "GaiaTempMask" [incr count_] ".sdf"]

      #  And create the dummy image NDF.
      set comp "DATA"
      set dummyaccessor [$imageaccessor_ createcopy $dummy_name_ $comp]

      #  Map in the data component to initialise it. Use BAD to avoid NDF bad
      #  flag from being set false.
      $dummyaccessor map "WRITE/BAD"

      #  Close before it can be displayed by the rtdimage.
      $dummyaccessor close

      #  Display this for the first time.
      display $dummy_name_

      #  Make sure everything is up-to-date (new data has been accepted by
      #  the RtdImage) so that the data replacement happens on the file
      #  we've just loaded (can get out of sync when unexpected errors
      #  autoloading Tcl scripts occur, which defer the newImage acceptance
      #  callbacks).
      #  XXX make sure this method isn't called during the update.
      ::update

      #  Image may have been waiting for clear.
      if { $imageaccessor_ == {} } {
         return
      }

      #  Now delete old dummy image (waited until released).
      if { $oldname != {} } {
         catch {::file delete $oldname} msg
         if { $msg != {} } {
            puts stderr \
               "WARNING: Failed to delete dummy image: $msg"
         }
      }

      #  Apply the mask to to the image and display, returns the masked
      #  copy as an array.
      set adr [array::maskdata $image_adr_ $mask_adr_]
      lassign [$imageaccessor_ getinfo $adr] realadr nel type
      $itk_option(-rtdimage) replaceimagedata $realadr

      #  Release memory from last time and save pointer.
      if { $last_adr_ != 0 } {
         $imageaccessor_ release $last_adr_
      }
      set last_adr_ $adr

      #  Update everything to make sure we don't get partial image refreshes.
      update idletasks
   }

   #  Display the XXX dummy image.
   public method display {name} {
      $itk_option(-gaia) open $name
   }

   #  Should be called when the data component has changed.
   protected method component_changed_ {} {

      if { $imageaccessor_ != {} } {

         #  The ERROR component is the VARIANCE component in fact, that just
         #  applies when mapping.
         set component [get_component_]

         if { [$imageaccessor_ exists $component] } {

            #  Only the DATA component is mapped by default, so make sure
            #  we access the others too.
            if { $component_ == "ERROR" } {
               $imageaccessor_ configure -maperrors 1
            } else {
               $imageaccessor_ configure -maperrors 0
            }
            if { $component != "DATA" } {
               set image_adr_ [$imageaccessor_ map "READ" $component]
            }

            #  Cause an update of the image.
            update_display_ 1
         } else {

            #  Undo changes and remain with current component.
            info_dialog "No $component_ component in image" $w_
            if { $component_ != $last_component_ } {
               set component_ $last_component_
            }
            return
         }
      }

      #  Fallback when component doesn't exist.
      set last_component_ $component_
   }

   #  Enable or disable items in NDF component menu depending on whether we
   #  have an NDF image or not.
   protected method configure_component_menu_ {} {
      if { $type_ == ".sdf" } {
         set state normal
      } else {
         set state disabled
      }
      foreach type {DATA VARIANCE ERROR QUALITY} {
         $component_menu_ entryconfigure $type -state $state
      }
   }

   #  Get the component, normally component_ but will be VARIANCE if
   #  component_ is ERROR.
   protected method get_component_ {} {
      if { $component_ == "ERROR" } {
         return "VARIANCE"
      }
      return $component_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the input image. NDF or FITS specification.
   itk_option define -image image Image {}

   #  Name of the input mask. NDF or FITS specification.
   itk_option define -mask mask Mask {}

   #  Name of the Gaia instance we're controlling.
   itk_option define -gaia gaia Gaia {}

   #  And the rtdimage instance being used.
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Filters for selecting files.
   itk_option define -filter_types filter_types Filter_types {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Data access objects for the image and mask.
   protected variable imageaccessor_ {}
   protected variable maskaccessor_ {}

   #  The name for the dummy NDF.
   protected variable dummy_name_ {}

   #  Memory used for various images.
   protected variable image_adr_ 0
   protected variable mask_adr_ 0
   protected variable dummy_adr_ 0

   #  Memory used for last mask. Free this when not needed.
   protected variable last_adr_ 0

   #  Name handling objects.
   protected variable image_namer_ {}
   protected variable mask_namer_ {}

   #  Data component to mask.
   protected variable component_ "DATA"
   protected variable last_component_ "DATA"

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The temporary image count.
   protected common count_ 0

#  End of class definition.
}
