#+
#  Name:
#     GaiaSymbolConfig

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Set the symbols used when plotting catalogue overlays.

#  Description:
#     Overrides the cat::SymbolConfig class to add symbols that are
#     only available in GAIA.

#  Invocations:
#
#        GaiaSymbolConfig object_name [configuration options]
#
#     This creates an instance of a GaiaSymbolConfig object. The return is
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

#  Copyright:
#     Copyright (C) 2007-2009 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     13-JUL-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaSymbolConfig {}

itcl::class gaia::GaiaSymbolConfig {
   
   #  Inheritances:
   #  -------------
   inherit cat::SymbolConfig

   #  Constructor:
   #  ------------
   constructor {args} {
      
      #  Evaluate any options [incr Tk].
      eval itk_initialize $args
   }   

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Add local symbols to the Symbol menu. Only rotbox.
   protected method init {} {
      cat::SymbolConfig::init
      foreach i {rotbox rectangle} {
         $symbol_ add \
            -bitmap $i \
            -value $i \
            -command [code $this set_symbol $i]
      }
   }

   protected method set_symbol {symbol} {
      if { "$symbol" == "rotbox" } {
         $ratio_ config -state normal
         $angle_ config -state normal
      } elseif { "$symbol" == "rectangle" } {
         $ratio_ config -state normal
         $angle_ config -state disabled
      } else {
         cat::SymbolConfig::set_symbol $symbol
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------
   
   #  Protected variables: (available to instance)
   #  --------------------


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
