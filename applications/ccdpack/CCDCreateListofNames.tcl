   proc CCDCreateListofNames { frombox prefix postfix } {
#+
#  Name:
#     CCDCreateListofNames

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     To create a list of the names entered in a listbox.

#  Description:
#     This routine creates a Tcl list which contains all the entries in
#     a listbox. The names extracted from the listbox are modified using
#     the values of the prefix and postfix arguments (which may be
#     empty).

#  Arguments:
#     frombox = window (read)
#        The name of the listbox whose contents are to be copied into a
#	 list.
#     prefix = string (read)
#        A string to prefix to the names extracted from the listbox
#	 before entry into the list.
#     postfix = string (read)
#        A string to be appended to the names extracted from the listbox
#	 before entry into the list

#  Return:
#     CCDCreateListofNames = list (write)
#        The list of names extracted from the listbox. Null if none are
#	 found.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     9-MAR-1994 (PDRAPER):
#     	 Original version.
#     {enter_changes_here}

#-
#.

#  Initialise the output list.
      set outlist {}

#  Check the size of the listbox.
      set size [$frombox size]
      if { $size > 0 } {

#  Have some entries. Loop until all are read.
         for { set i 0 } { $i < $size } { incr i } {
            set name "${prefix}[$frombox get $i]${postfix}"
            lappend outlist $name
	 }
      }

#  End of procedure.
      return $outlist
   }
# $Id$
