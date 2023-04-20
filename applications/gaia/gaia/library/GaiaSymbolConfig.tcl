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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

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
      foreach i {rotbox rectangle stcshape} {
         $symbol_ add \
            -bitmap $i \
            -value $i \
            -command [code $this set_symbol $i]
      }
      fill_table 1
   }

   #  Don't do this before we're ready, there's a timed event in fill_table
   #  proper when no symbol is defined.
   protected method fill_table {{doit 0}} {
      if { $doit == 1 } {
         cat::SymbolConfig::fill_table
      }
   }

   protected method set_symbol {symbol} {
      if { "$symbol" == "rotbox" } {
         $ratio_ config -state normal
         $angle_ config -state normal
      } elseif { "$symbol" == "rectangle" } {
         $ratio_ config -state normal
         $angle_ config -state disabled
      } elseif { "$symbol" == "stcshape" } {
         $ratio_ config -state disabled
         $angle_ config -state disabled
      } else {
         cat::SymbolConfig::set_symbol $symbol
      }
   }

   #  Map "%%" to "::" for testing expressions that may contain
   #  global variables.
   protected method map_ {var} {
      return [string map {"%%" "::"} $var]
   }


   #  Return a table row based on the current entries and menus or empty
   #  if no data was entered. Raises an error if there was something wrong.
   #  Override subclass method so that we can handle GAIA globals in
   #  expressions.
   protected method get_row {} {

      set cols [$left_ get_contents]
      set symbol [$symbol_ get]
      set color [$color_ get]

      set ratio [$ratio_ get]
      set tratio [map_ $ratio]

      set angle [$angle_ get]
      set tangle [map_ $angle]

      set label [$label_ get]

      set cond  [$cond_ get]
      set tcond [map_ $cond]

      set size [$size_ get]
      set tsize [map_ $size]

      set units [$units_ get]

      #  Test the variable usage in the expressions
      foreach var $cols {
         set $var 1
      }
      if {"$size" == ""} {
         if {[llength $cols]} {
            error "Please specify a value for size (expr or const) in $units"
         }
         return
      }

      foreach var {tsize tratio tangle tcond} {
         set v [set $var]
         if {"$v" != ""} {
            if {[catch {expr [set $var]} msg]} {
               error "in $var expr: $msg"
            }
         }
      }

      #  Label may also contain col name vars, but may not be numeric
      if {[catch {subst $label} msg]} {
         error "error in label '$label': $msg"
      }

      return [list $cols $symbol $color $ratio $angle $label $cond $size $units]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
