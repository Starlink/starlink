   proc CCDCopyListbox { From To mode args } {
#+
#  Name:
#     CCDCopyListbox

#  Purpose:
#     Copies a part of a listbox to another listbox.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine copies either the current selection from one listbox
#     to another, or a given range of values. The mode of operation is
#     selected by the mode argument which must take a value of select or
#     range. If mode=select then any selection in the from listbox is
#     copied to the end of the current contents of the to listbox.
#     If mode=range then the args arguments specify the lower and upper
#     indices of the entries to copy.

#  Arguments:
#     from = window (read)
#        The listbox which is to have values copied
#     to = window (read)
#        The listbox which is to have values entered. New values are
#        appended to the end of any current contents (this could be
#        cleared first by a {$to delete 0 end} before calling this
#        routine).
#      mode = string (read)
#        The mode to use when determining the range of values to copy.
#        Either "select" if the current selection is to be copied, or
#        "range" if the args arguments have the lower and upper indices
#        of the range.
#      args = list (read)
#        Up to four values. The first two are used when mode=range,
#        when they specify the lower and upper indices of the values to
#        copy. The last two are strings to prepend and append to the
#        names which are copied. If mode!=range then only two values
#        will be used in which case they are the prepend and append
#        strings.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 2000 Central Laboratory of the Research Councils.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     9-MAR-1994 (PDRAPER):
#        Original version.
#     15-MAR-1994 (PDRAPER):
#        Added prepend and append capability.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#.

#  Find out how mahy "extra" arguments we have.
      set nargs [ llength $args ]

#  Branch on the mode of operation.
      if { "$mode" == "range" } {

#  Must have supplied a range.
	 if { $nargs > 1 } {

#  Set the start index.
	    set index [lindex $args 0]

#  Set the final index. If this is "end" then use the size of the
#  listbox.
	    set finish [lindex $args 1]

#  Look for prepend and append arguments.
            if { $nargs > 2 } {
               set prepend [lindex $args 2]
               if { $nargs > 3 } {
                  set append [lindex $args 3]
	       } else {
                  set append {}
	       }
	    } else {
	       set prepend {}
	       set append {}
	    }
	    if { "$finish" == "end" } { set finish [ $From size ] }
	    for { } { $index < $finish } { incr index } {
	       $To insert end "${prepend}[ $From get $index ]${append}"
	    }
	 }
      } else {

#  Mode is assumed to be selection. Check that some values are selected.
         set indices [ $From curselection ]
	 if { $indices != {} } {

#  Look for prepend and append arguments.
            if { $nargs > 0 } {
               set prepend [lindex $args 0]
               if { $nargs > 1 } {
                  set append [lindex $args 1]
	       } else {
                  set append {}
	       }
	    } else {
	       set prepend {}
	       set append {}
	    }

#  Get the selection indices and copy the entries.
            foreach index $indices {
	       $To insert end "${prepend}[ $From get $index ]${append}"
            }
         }
      }

#  End of procedure.
   }
# $Id$
