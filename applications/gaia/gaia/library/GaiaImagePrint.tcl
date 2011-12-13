#+
#  Name:
#     GaiaImagePrint

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Extends RtdImagePrint to add facilities required by GAIA.

#  Description:
#     This class just adds facility to set the background to the
#     colour shown on the canvas (rather than always being white).

#  Invocations:
#
#        GaiaImagePrint object_name [configuration options]
#
#     This creates an instance of a GaiaImagePrint object. The return is
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
#     rtd::RtdImagePrint

#  Copyright:
#     Copyright (C) 1999-2005 Central Laboratory of the Research Councils.
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
#     24-MAR-1999 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaImagePrint {

   #  Inheritances:
   #  -------------
   inherit rtd::RtdImagePrint

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }


   #  Methods:
   #  --------

   #  Override set_background to really set the background to the
   #  requested colour (rather than always white). Add a large
   #  surround to remove edge problems.
   method set_background {} {
      set xlow  [$canvas_ canvasx 0]
      set ylow  [$canvas_ canvasy 0]
      set xhigh [$canvas_ canvasx [winfo width $canvas_]]
      set yhigh [$canvas_ canvasy [winfo height $canvas_]]
      set xlow [expr round($xlow-100)]
      set ylow [expr round($ylow-100)]
      set xhigh [expr round($xhigh+100)]
      set yhigh [expr round($yhigh+100)]
      $canvas_ create rectangle $xlow $ylow $xhigh $yhigh \
         -fill [$canvas_ cget -background] \
         -tags ${this}_back
      $canvas_ lower ${this}_back all
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
