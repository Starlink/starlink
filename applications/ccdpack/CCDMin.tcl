proc CCDMin { args } {
#+
#  Name:
#     CCDMin

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Returns minimum value of a list

#  Arguments:
#     args = list (read)
#        List of numeric values.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     29-SEP-1995 (PDRAPER):
#     	 Original version.
#     {enter_further_changes_here}

#-

#.
   set nargs [llength $args]
   if { $nargs > 1 } { 
      set min [lindex $args 0]
      for { set i 1 } { $i < $nargs } { incr i } { 
         set v [lindex $args $i]
         if { $v < $min } { set min $v }
      }
      return $min
   } elseif { $nargs == 1 } { 
      return $args
   } else { 
      return -1
   }

#  End of procedure.
}
# $Id$
