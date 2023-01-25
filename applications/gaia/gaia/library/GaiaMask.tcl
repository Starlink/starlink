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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

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

      #  Add a "Go" menu to select previously loaded masks. Shared
      #  with general images.
      set GoMenu [add_menubutton "Go" \
                     "Go: menu with shortcuts to view previous images"]
      configure_menubutton "Go" -underline 0
      set history_ [GaiaHistory \#auto -gaiatoplevel $this]
      $GoMenu config -postcommand [code $history_ update_history_menu $GoMenu]

      #  Add window help.
      add_help_button mask "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      set lwidth 12

      #  Name of input mask.
      itk_component add mask {
         LabelFileChooser $w_.mask \
            -text "Input mask:" \
            -labelwidth $lwidth \
            -textvariable [scope itk_option(-mask)] \
            -filter_types $itk_option(-filter_types) \
            -chooser_title "Select mask"
      }
      pack $itk_component(mask) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(mask) \
         {Name of the input masked, integer data segmenting input image}

      #  Select which regions to display from the mask. If not set all.
      itk_component add values {
         util::LabelEntry $w_.values \
            -labelwidth $lwidth \
            -text "Values:" \
            -textvariable [scope values_] \
            -command [code $this apply]
      }
      pack $itk_component(values) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(values) \
         {Values of masked regions to display, empty for all}

      #  Or pick out individual values using a slider.
      if { $itk_option(-show_slider) } {
         itk_component add index {
            util::LabelEntryScale $w_.index \
               -text "Value:" \
               -value 0 \
               -labelwidth $lwidth \
               -from 0 \
               -to 2 \
               -increment 1 \
               -resolution 1 \
               -show_arrows 1 \
               -anchor w \
               -delay 25 \
               -fix_range 1 \
               -command [code $this apply_value_]
         }
         pack $itk_component(index) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(index) \
            {Value of mask region to display, if enabled}
      }

      #  Or just select all non-masked values.
      itk_component add invert {
         StarLabelCheck $w_.invert \
            -text "Invert:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope invert_] \
            -command [code $this toggle_invert_]
      }
      pack $itk_component(invert) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(invert) \
         {Invert sense and display all BAD parts of the mask}

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

      #  History.
      if { $history_ != {} } {
         catch {::delete object $history_}
      }
   }

   #  Methods:
   #  --------

   #  Close window.
   public method close {} {
      #  Put the full image back. Don't want to lose that really.
      reset
      wm withdraw $w_
   }

   #  Apply the mask.
   public method apply {args} {
      if { [open] } {
         create_mask_
      }
   }

   #  Apply the single mask value.
   protected method apply_value_ {value} {
      set values_ $value
      apply
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
         if { [open_mask_] } {
            load_mask_
            set last_mask_ $itk_option(-mask)

            #  Set the range of values for this mask.
            if { $itk_option(-show_slider) } {
               lassign [array::getminmax $mask_adr_] min max
               $itk_component(index) configure -to $max
            }
         } else {
            return 0
         }
      }
      return 1
   }

   #  Reset displayed image.
   public method reset {} {
      if { $image_adr_ != 0 } {
         lassign [array::getinfo $image_adr_] realadr nel type
         replaceimagedata $realadr
      }
   }

   #  Replace the image data pointer with a new one. Note always mark
   #  this as not volatile so that the backing file is never changed,
   #  that would happen if the other toolboxes ran and thought this
   #  data volatile (which really means never defined, not just changed).
   protected method replaceimagedata {adr} {
      $itk_option(-rtdimage) replaceimagedata $adr
      $itk_option(-rtdimage) volatile 0
   }

   #  Open the mask.
   protected method open_mask_ {} {

      #  Previous mask becomes "Back".
      $history_ record_last

      if { $mask_namer_ == {} } {
         set mask_namer_ [GaiaImageName \#auto]
      }
      $mask_namer_ configure -imagename $itk_option(-mask)

      if { ! [$mask_namer_ exists] } {
         if { $itk_option(-mask) != {} } {
            error_dialog "No such file: $itk_option(-mask)" $w_
         }
         return 0
      }

      $mask_namer_ absolute
      set fullname [$mask_namer_ fullname]

      if { $maskaccessor_ == {} } {
         set maskaccessor_ [uplevel \#0 GaiaNDAccess \#auto]
      }
      $maskaccessor_ configure -dataset "$fullname"

      #  Record this mask in Go menu.
      $history_ add_history $itk_option(-mask)
      return 1
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
      #  is renewed or changed. But make sure this isn't the masked address
      #  which it will be after a replacement.
      set adr [$itk_option(-rtdimage) imagedata]
      if { $image_adr_ != 0 } {
         lassign [array::getinfo $image_adr_] iadr nel type
         set madr 0
         if { $last_adr_ != 0 } {
            lassign [array::getinfo $last_adr_] madr nel type
         }
         if { $adr != $iadr && $adr != $madr } {
            array::release $image_adr_
            set image_adr_ 0
         }
      }
      if { $image_adr_ == 0 } {
         set bitpix [$itk_option(-rtdimage) bitpix]
         set el [expr [$itk_option(-rtdimage) width] * \
                      [$itk_option(-rtdimage) height]]
         set isfits [$itk_option(-rtdimage) isfits]

         #  And wrap as an array structure.
         set image_adr_ [array::wrap $adr $bitpix $el $isfits]
      }

      #  Apply the mask to to the image and display, returns the masked
      #  copy as an array.
      set range [get_values_]
      if { [catch {set adr [array::maskdata $image_adr_ $mask_adr_ $range]} msg] } {
         $history_ clear_last
         error "Failed to mask data: $msg"
         return
      }

      #  Replace the displayed data with the masked data.
      lassign [array::getinfo $adr] realadr nel type
      replaceimagedata $realadr

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

   #  Get the values to be displayed in the mask. Expands any implicit ranges.
   protected  method get_values_ {} {

      #  If inverting then just use -1 as the single value.
      if { $invert_ } {
         return "-1"
      }

      #  Not inverting and no values gets whole mask.
      if { $values_ == {} } {
         return {}
      }

      #  Need lists of single values or pairs of values.
      #  Clean up first, replace "," with space, remove multiple spaces
      #  and trim.
      set value [regsub -all -- {,} $values_ { }]
      set value [regsub -all -- {\s+} $value { }]
      set value [string trim $value]
      set rangelist {}
      foreach v $value {
         #  Ranges are low-high and will need expanding.
         lappend rangelist [regsub -all -- {-} $v { }]
      }
      set range {}
      foreach l $rangelist {
         if { [llength $l] > 1 } {
            set low [lindex $l 0]
            set high [lindex $l 1]
            for {set i $low} {$i <= $high} {incr i} {
               lappend range $i
            }
         } else {
            lappend range $l
         }
      }

      return $range
   }

   #  Called when inversion is toggled. Need to change UI to stop
   #  confusion over values.
   protected method toggle_invert_ {} {
      if { $invert_ } {
         set state disabled
      } else {
         set state normal
      }
      $itk_component(values) configure -state $state
      $itk_component(index) configure -state $state
   }

   #  GaiaHistory support methods.
   public method get_accessor {} {
      return $maskaccessor_
   }
   public method open_image {mask} {
      configure -mask $mask
      apply
   }
   public method get_image {} {
      return $itk_option(-mask)
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

   #  Whether to show the slider.
   itk_option define -show_slider show_slider Show_Slider 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  Whether to invert the whole mask.
   protected variable invert_ 0

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

   #  Mask values to display.
   protected variable values_ 0

   #  The GaiaHistory instance used for "Go" menu.
   protected variable history_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
