   proc CCDPurgeEmptySets { Setbox } {
#+
#  Name:
#     CCDPurgeEmptySets

#  Purpose:
#     Remove redundant Set header lines from Set listbox.

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine goes through the listbox which is used to display
#     NDFs marked for Set membership and ensures that any SET header
#     lines which have no members are removed.

#  Arguments:
#     Setbox = string
#        Command name of the listbox which contains Sets of NDFs.
#        Lines will be parsed using the CCDItemSetIndex procedure.

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     5-JUL-2001 (MBT):
#        Original version.

#-

#  Start at the top of the listbox.
      set pos 0

#  Work down lines of listbox.  Some delicacy is required since we may
#  be changing the size of the listbox as we go.
      while { $pos < [$Setbox size] } {

#  If we encounter two adjacent SET header lines, or a SET header line
#  adjacent to the bottom of the list, delete it.
         set pos1 [expr $pos + 1]
         if { [lindex [CCDItemSetIndex $Setbox $pos] 0] == "SET" && \
              ( $pos1 == [$Setbox size] || \
                [lindex [CCDItemSetIndex $Setbox $pos1] 0] == "SET" ) } {
            $Setbox clear $pos
         } else {
            incr pos
         }
      }
   }
# $Id$
