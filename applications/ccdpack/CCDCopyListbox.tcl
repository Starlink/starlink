   proc CCDCopyListbox { from to mode args } {
#+
#  Name:
#     CCDCopyListbox

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Copies a part of a listbox to another listbox.

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
#	 appended to the end of any current contents (this could be
#	 cleared first by a {$to delete 0 end} before calling this
#	 routine).
#      mode = string (read)
#         The mode to use when determining the range of values to copy.
#	  Either "select" if the current selection is to be copied, or
#	  "range" if the args arguments have the lower and upper indices
#	  of the range.
#      args = list (read)
#         Up to four values. The first two are used when mode=range,
#	  when they specify the lower and upper indices of the values to
#	  copy. The last two are strings to prepend and append to the
#	  names which are copied. If mode!=range then only two values
#	  will be used in which case they are the prepend and append
#	  strings.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     9-MAR-1994 (PDRAPER):
#     	 Original version.
#     15-MAR-1994 (PDRAPER):
#     	 Added prepend and append capability.
#     {enter_further_changes_here}

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
	    if { "$finish" == "end" } { set finish [ $from size ] }
	    for { } { $index < $finish } { incr index } {
	       $to insert end "${prepend}[ $from get $index ]${append}"
	    }
	 }
      } else {

#  Mode is assumed to be selection. Check that some values are selected.
         set indices [ $from curselection ]
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
	       $to insert end "${prepend}[ $from get $index ]${append}"
            }
         }
      }

#  End of procedure.
   }
# $Id$
