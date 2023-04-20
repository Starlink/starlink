#+
#  Name:
#     LabelFontChooser

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for choosing a font

#  Description:
#     This class defines a label entry field with an associated
#     button. The button, if pressed, displays a FontChooser for
#     selecting a system font.

#  Invocations:
#
#        LabelFontChooser object_name [configuration options]
#
#     This creates an instance of a LabelFontChooser object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#     See itk_option define declarations below.

#  Methods:
#     See descriptions with method declarations below

#  Inheritance:
#     LabelEntry

#  Copyright:
#     Copyright (C) 2008 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC - Durham University)
#     {enter_new_authors_here}

#  History:
#     17-MAR-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual LabelFontChooser {}

itcl::class gaia::LabelFontChooser {

   #  Inheritances:
   #  -------------
   inherit util::LabelEntry

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Add a button to the widget.
      itk_component add chooser {
         button $w_.chooser -text "Choose font..." \
            -command [code $this choose_font_]
      } {
         keep -relief -borderwidth
         rename -font -labelfont labelFont LabelFont
         rename -relief -buttonrelief buttonRelief ButtonRelief
      }
      pack $itk_component(chooser) -side right -padx 1m -ipadx 1m

      #  Now handle unprocessed configurations.
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      #  Close the font chooser...
      if { $font_chooser_ != {} && [winfo exists $font_chooser_] } {
         destroy $font_chooser_
      }
   }

   #  Methods:
   #  --------

   #  Withdraw the font chooser.
   public method withdraw {} {
      if { $font_chooser_ != {} } {
         wm withdraw $font_chooser_
      }
   }

   #  Choose a font and enter it into the entry field.
   protected method choose_font_ {} {
      if { $font_chooser_ == {} } {
         set font_chooser_ [gaia::FontChooser .\#auto \
                               -title $itk_option(-chooser_title)\
                               -fixed_width $itk_option(-chooser_fixed_width)\
                              ]
      }

      #  Set the default font to match the current selection.
      set font [get]
      if { $font != {} } {
         $font_chooser_ set_default_font $font
      }

      if { [$font_chooser_ activate] } {
         configure -value [$font_chooser_ get]
         if { "$itk_option(-command)" != "" } {
            command_proc_ $itk_option(-command)
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Title for fontselection window.
   itk_option define -chooser_title chooser_title Chooser_Title "Choose Font"

   #  Whether fontselection window should just display fixed width fonts.
   itk_option define -chooser_fixed_width chooser_fixed_width Chooser_Fixed_Width 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  The font chooser.
   protected variable font_chooser_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
