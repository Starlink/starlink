   proc CCDInsertNDFList { from tolist } {
#+
#  Name:
#     CCDInsertNDFList

#  Type of Module:
#     Tcl/Tk procedure

#  Purpose:
#     Parses and inserts a list of NDF names from an entry widget into
#     a listbox.

#  Description:
#     This routine reads the contents of an entry widget and attempts
#     to interpret this as a list of NDF names. The names may include
#     wildcards which will be expanded using globing and may be
#     separated by spaces and or commas. The resultant list of NDF
#     names are inserted at the end of the listbox (tolist).

#  Arguments:
#     from = window (read)
#        The entry widget which contains the expression to be
#	 interpreted as a list of NDF names.
#     tolist = window (read)
#        The name of the listbox which is to have the names of the NDFs
#	 entered.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     9-MAR-1994 (PDRAPER):
#     	 Original version.
#     15-MAR-1994 (PDRAPER):
#     	 Added prepend and append arguments.
#     {enter_further_changes_here}

#-

#  Get the contents of the entry widget
      set express [$from get]
      if { "$express" != {} } {

#  Remove commas from expression (note may need to protect some commas
#  in parentheses etc. in future but ok for now)
         regsub -all "," "$express" " " newexpress

#  Look at each word in turn.
         set current [pwd]
         foreach word $newexpress {
            set ndfs [glob -nocomplain $word]

#  Does word (file specification) contain full path information? Assume
#  that such names start with /, ./ or ../. If the names are not
#  complete use the [pwd] function to get the value of the current
#  directory.
            if { [ regexp (^../|^/|^./) $word ] } {
               foreach name [lsort $ndfs] {
                  $tolist insert end "${name}"
               }
            } else {

#  Add directory information.
               foreach name $ndfs {
                  $tolist insert end "${current}/${name}"
               }
            }
         }
      }

#  End of procedure.
   }
# $Id$
