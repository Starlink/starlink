#+
#  Name:
#     Gaia3dVtkPrintTool

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Tool for printing a Gaia3dVtkWindow scene to a graphics file.

#  Description:
#     Offers to save the nominated renderer state to a file using
#     the VTK image Writers, TIFF, PNG, JPEG and Postscript. Uses
#     a vtkLargeRenderedImage so that images with a finer resolution
#     than the display can be used.

#  Copyright:
#     Copyright (C) 2007 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     01-AUG-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual Gaia3dVtkPrintTool {}

itcl::class ::gaia3d::Gaia3dVtkPrintTool {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA3D: Create hardcopy ($itk_option(-number))"

      set lwidth 15
      set vwidth 20

      #  Choice of graphics format.
      itk_component add format {
         LabelMenu $w_.format \
            -labelwidth $lwidth \
            -text "Graphic format:" \
            -variable [scope format_]
      }
      pack $itk_component(format) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(format) {Graphics format for hardcopy}
      set format_ "TIFF"
      foreach format "TIFF JPEG PNG EPS" {
         $itk_component(format) add \
            -label $format \
            -value $format \
            -command [code $this set_format_ $format]
      }

      #  Image scale, allow values 1 through 20.
      itk_component add scale {
         util::LabelEntryScale $w_.scale \
            -text "Image scale:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1 \
            -to 20 \
            -increment 1  \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -show_scale 0 \
            -validate integer \
            -value $scale_ \
            -textvariable [scope scale_]
      }
      pack $itk_component(scale) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(scale) \
         {Integer image scale, higher for better resolution}

      #  Output file name.
      itk_component add filename {
         gaia::LabelFileChooser $w_.filename \
            -labelwidth $lwidth \
            -text "Output file:" \
            -value $filename_ \
            -textvariable [scope filename_] \
            -filter_types $itk_option(-filter_types)
      }
      pack $itk_component(filename) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(filename) {Name of the output file}

      #  OK and Cancel buttons.
      itk_component add actionframe {
         frame $w_.actions
      }
      pack $itk_component(actionframe) -side bottom -fill x

      itk_component add ok {
         button $itk_component(actionframe).ok -text "OK" \
            -command [code $this accept]
      }

      #  Border around OK button and make <Return> press it.
      itk_component add default {
         frame $itk_component(actionframe).default \
            -relief sunken -borderwidth 1
      }
      raise $itk_component(ok) $itk_component(default)
      pack $itk_component(default) -side left -expand 1 -padx 3m -pady 2m
      pack $itk_component(ok) -in $itk_component(default) -padx 2m -pady 2m \
         -ipadx 2m -ipady 1m
      bind $w_ <Return> "$itk_component(ok) flash; $itk_component(ok) invoke"

      itk_component add cancel {
         button $itk_component(actionframe).cancel -text "Cancel" \
            -command [code $this close]
      }
      pack $itk_component(cancel) -side left -expand 1 -padx 3m -pady 3m \
         -ipadx 2m -ipady 1m
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods and procedures:
   #  -----------------------

   #  Close this window. Just withdraws.
   public method close {} {
      wm withdraw $w_
   }

   #  This method is called when the OK button is pressed
   public method accept {} {
      if { $filename_ != "" } {
         if { [file writable $filename_] } {
            if { ! [confirm_dialog "File $filename_ exists - Do \
you want to overwrite it ?" $w_] } {
               return
            }
         }
         #  Write the graphic. Note we have to make sure that the window
         #  isn't covered, so we close this window now..
         busy {
            close
            ::update
            hardcopy_
         }
      }
      close
   }

   #  Print a rendered scene to a graphics file.
   protected method hardcopy_ {} {
      $itk_option(-renwindow) hardcopy $filename_ $format_ $scale_
   }

   #  Set the format value.
   protected method set_format_ {format} {
      set format_ $format
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The renderer window containing the scene to be saved to a hardcopy.
   #  Must be set and will need updating for new scenes.
   itk_option define -renwindow renwindow RenWindow {}

   #  Types for picking supported file formats.
   itk_option define -filter_types filter_types Filter_Types \
      {{any *} {TIFF *.tif} {PNG *.png} {JPEG *.jpg} {EPS *.eps}}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Name of the file.
   protected variable filename_ "Gaia3D.tif"

   #  Graphic format.
   protected variable format_ "TIFF"

   #  Image scale.
   protected variable scale_ 1

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
