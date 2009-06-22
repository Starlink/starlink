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

      #  Name of input mask.
      itk_component add mask {
         LabelFileChooser $w_.mask \
            -text "Input mask:" \
            -textvariable [scope itk_option(-mask)] \
            -filter_types $itk_option(-filter_types) \
            -chooser_title "Select mask"
      }
      pack $itk_component(mask) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(mask) \
         {Name of the input masked, integer data segmenting input image}

      #  Apply the mask.
      itk_component add apply {
         button $w_.apply -text Apply \
            -command [code $this apply]
      }
      pack $itk_component(apply) -side left -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(apply) {Apply the mask to displayed image}

      #  Reset the displayed image.
      itk_component add reset {
         button $w_.reset -text Reset \
            -command [code $this reset]
      }
      pack $itk_component(reset) -side left -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(reset) {Reset display unmasking the image}

      #  Close window.
      itk_component add close {
         button $w_.close -text Close \
            -command [code $this close]
      }
      pack $itk_component(close) -side left -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(close) {Close window}
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Release memory held for masked image.
      if { $last_adr_ != 0 } {
         catch {array::release $last_adr_}
      }
      if { $image_adr_ != 0 } {
         catch {array::release release $image_adr_}
      }
      catch {::delete object $maskaccessor_}

      #  Name handler.
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

   #  Apply the mask.
   public method apply {} {
      if { [open] } {
         create_mask_
      }
   }

   #  Open the mask, if changed.
   public method open {args} {
      if { [$itk_option(-rtdimage) isclear] } {
         warning_dialog "You must display an image to be masked"
         return 0
      }
      if { $itk_option(-mask) == {} } {
         warning_dialog "No mask defined"
         return 0
      }
      if { $last_mask_ != $itk_option(-mask) } {
         open_mask_
         load_mask_
         set last_mask_ $itk_option(-mask)
      }
      return 1
   }

   #  Reset displayed image.
   public method reset {} {
      if { $image_adr_ != 0 } {
         lassign [array::getinfo $image_adr_] realadr nel type
         $itk_option(-rtdimage) replaceimagedata $realadr
         $itk_option(-rtdimage) volatile 0
      }
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

   #  Create the masked data. This will replace the data in the displayed
   #  image.
   protected method create_mask_ {} {

      #  Get access to the displayed image data and its type and size.
      #  Check that address hasn't changed, that will happen when the image
      #  is renewed or changed.
      set adr [$itk_option(-rtdimage) imagedata]
      if { $image_adr_ != 0 } {
         lassign [array::getinfo $adr] realadr nel type
         if { $realadr != $adr } {
            array::release $image_adr_
            set image_adr_ 0
         }
      }
      if { $image_adr_ == 0 } {
         set bitpix [$itk_option(-rtdimage) bitpix]
         set el [expr [$itk_option(-rtdimage) width] * \
                      [$itk_option(-rtdimage) height]]

         #  And wrap as an array structure.
         set image_adr_ [array::wrap $adr $bitpix $el]
      }

      #  Apply the mask to to the image and display, returns the masked
      #  copy as an array.
      if { [catch {set adr [array::maskdata $image_adr_ $mask_adr_]} msg] } {
         error_dialog "Failed to mask data: $msg"
         return
      }

      #  Replace the displayed data with the masked data.
      lassign [array::getinfo $adr] realadr nel type
      $itk_option(-rtdimage) replaceimagedata $realadr

      #  Make sure blank pixels are shown.
      $itk_option(-rtdimage) blankvalue 1

      #  Release memory from last time mask was changed and save pointer.
      if { $last_adr_ != 0 } {
         array::release $last_adr_
      }
      set last_adr_ $adr

      #  Update everything to make sure we don't get partial image refreshes.
      update idletasks
   }

   #  Configuration options: (public variables)
   #  ----------------------

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

   #  Data access objects for the mask.
   protected variable maskaccessor_ {}

   #  Name of the last mask file opened.
   protected variable last_mask_ {}

   #  Memory used for various images.
   protected variable image_adr_ 0
   protected variable mask_adr_ 0

   #  Memory used for last mask. Free this when not needed.
   protected variable last_adr_ 0

   #  Name handling object.
   protected variable mask_namer_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
