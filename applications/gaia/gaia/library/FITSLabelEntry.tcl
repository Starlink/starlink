#+
#  Name:
#     FITSLabelEntry

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     A type of LabelEntry that can obtain its value from the FITS
#     headers of an image.

#  Invocations:
#
#        FITSLabelEntry object_name [configuration options]
#
#     This creates an instance of a FITSLabelEntry object. The return is
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
#     See below

#  Inheritance:
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 2003 Central Laboratory of the Research Councils
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
#     10-SEP-2003 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual FITSLabelEntry {}

itcl::class gaia::FITSLabelEntry {

   #  Inheritances:
   #  -------------
   inherit util::LabelEntry

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create a unique name for the FITSChooser.
      set fitsChooser_ $w_.fitschooser

      #  Button to activate the FITS header value chooser.
      itk_component add chooser {
         button $w_.chooser \
            -text "FITS" \
            -command [code $this select_fits_value_]
      } {
         keep -borderwidth
         rename -font -buttonfont buttonFont ButtonFont
         rename -relief -buttonrelief buttonRelief ButtonRelief
      }
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor {
      if { $fitsChooser_ != {} && [winfo exists $fitsChooser_] } {
         delete object $fitsChooser_
      }
   }

   #  Methods:
   #  --------
   public method init {} {
      pack $itk_component(chooser) \
         -side $side_ -expand 0 -fill x -padx 1m -ipadx 1m
   }

   protected method select_fits_value_ {} {
      utilReUseWidget gaia::FITSChooser $fitsChooser_ \
         -rtdimage $itk_option(-rtdimage) \
         -command [code $this set_fits_value_]
   }

   protected method set_fits_value_ {keyword value comment} {
      configure -value "$value"
   }

   #  Configuration options: (public variables)
   #  ----------------------
   #  The image with FITS headers.
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Protected variables: (available to instance)
   #  --------------------
   #  The FITSChooser used by this instance.
   protected variable fitsChooser_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
