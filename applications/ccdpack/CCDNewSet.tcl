   proc CCDNewSet { Unsetbox Setbox items } {
#+
#  Name:
#     CCDNewSet

#  Purpose:
#     Marks NDFs for Set header addition.

#  Type of Module:
#     Tcl/Tk procedure

#  Description:
#     This routine gets a list of NDFs and marks them for Set header
#     addition.  It basically does the following:
#        - enters them into the setted listbox as a complete Set
#        - marks the names in the unsetted listbox with their Set indices

#  Arguments:
#     Unsetbox = string
#        The name of the listbox containing all the available NDFs.
#     Setbox = string
#        The name of the listbox which will contain Sets of NDFs. 
#     items = list of integers
#        A list of integers (indexes) identifying items from the list in
#        Unsetbox which will consitute a new Set in the Setbox.   These
#        items should not already be in Unsetbox.

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     18-JUN-2001 (MBT):
#        Original version.
#-

#  Only proceed if this is not an empty list.
      if { [llength $items] > 0 } {
         set usize [$Unsetbox size]

#  Create a new Set item at the end of setlist.
         $Setbox insert end ""
         CCDItemSetIndex $Setbox end "SET"

#  Loop over each of the new items.
         set setindex 0
         foreach item $items {

#  Check this item is in the range of the unsetted box.
            if { $item >= 0 && $item < $usize } {

#  Annotate with the Set Index in the unsetted list.
               CCDItemSetIndex $Unsetbox $item [incr setindex]

#  Add a new item to the setted list.
               $Setbox insert end [$Unsetbox get $item]
            }
         }
      }
   }
# $Id$
