   proc CCDFITSRunPresent { topwin } {
#+
#  Name:
#     CCDFITSRunPresent
      
#  Purpose:
#     Runs the PRESENT application.

#  Language:
#     Tcl/Tk procedure

#  Description:
#     This routine runs the PRESENT application on the list of NDFs
#     whose names are entered in the CCDallndfs global variable. This
#     assumes that the NDFs already have frame type information and
#     that global parameters have been set for all the options that
#     are to be imported into the NDFs and these will NOT override any
#     values already present. It is therefore only suitable for NDFs
#     that have had FITS information already imported into them (or
#     have already been organised once by PRESENT). 

#  Arguments: 
#     topwin = window (read)
#        Name of the top-level window to parent the task output if
#        required.

#  Global variables:
#     CCDallndfs = array (read)
#        The names of all the NDFs known to the system. These will
#        have 

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     29-MAR-1995 (PDRAPER):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
      global CCDallndfs
      global CCDdir
#.

#  Create a file with all NDF names to input into PRESENT.
      if { [info exists CCDallndfs] } {
	 if { ! [ catch { open XREDUCE.NDFS w } fileid ] } {

#  Put names into list.
            foreach ndf $CCDallndfs {
               puts $fileid [ CCDFileToNDFName $ndf ]
	    }
            close $fileid

#  Run PRESENT.
            CCDRunTask ccdpack present \
"in=^XREDUCE.NDFS modify=false simple=true multientry=false adddark=false addflash=false reset accept"\
1 $topwin " Importing FITS information in CCDPACK extensions, please wait "
	 } else {

#  Failed to open file.
            CCDIssueError "Failed to open temporary file"
	 }
      } else {

#  No NDFs to process.
         CCDIssueInfo "No NDFs are available for processing, \
have you imported any into the system?"
      }

#  End of procedure.
   }
# $Id$
