proc CCDExtractFitsFromNDF { Top From To } {

#+
#  Name:
#     CCDExtractFitsFromNDF

#  Purpose:
#     Extracts FITS keywords from an NDF.

#  Type of Module:
#     Tcl/Tk

#  Arguments:
#     Top = window (read)
#        Name of top-level window that this procedure is invoked
#        from. This will be locked until the task is finished.
#     From = window (read)
#        Name of an entry widget with the name of the NDF to be read in.
#     to = window (write)
#        Name of a listbox to enter the FITS keywords into.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     2-MAR-1994 (PDRAPER):
#     	 Original version.
#     {enter_changes_here}

#-
   global TASK
#.

#  Extract the value from the entry widget.
   set NDF [$From get]

#  Check NDF exists.
   if { [ file exists $NDF.sdf ] } {

#  NDF exists try to trace the FITS information.
      CCDRunTask fitslist "$NDF accept" 1 $Top \
         " Extracting FITS items from NDF extension "

#  Task has completed. Have we any output? Task will have reported error
#  if failed.
      if { $TASK(fitslist,error) == "" } {

#  Task ran successfully. Interpret the output and enter the possible
#  FITS keywords into the listbox.
         foreach line [ split $TASK(fitslist,output) \n] {

#  Look for pattern which has at least 8 characters followed by an
#  equals sign. Extract the first eight characters as the FITS item
#  keyword name.
            if { [string match ????????*=* "$line"] } {
               set item [ string range "$line" 0 7 ]
               if { "$item" != "        " } {
                  
#  Not blank matches the above rules to enter into the listbox.
                  $To insert end "$item"
               }
            }
         }
      }
   } else {

#  NDF doesn't exist.
      if { $NDF == "" } { 
         CCDIssueInfo "No reference NDF available"
      } else {
         CCDIssueInfo "Failed to locate NDF: $NDF"
      }
   }

#  End of procedure.
}
# $Id$
