   proc CCDSaveRestoreFile { filename } {
#+
#  Name:
#     CCDSaveRestoreFile.tcl

#  Type of Module:
#     Tk/Tcl

#  Purpose:
#     Saves the current global parameter setup to the named file.

#  Description:
#     This routine opens the named file and writes the contents of the
#     global parameter array CCDglobalpars into it. The form used is
#     the same as that of CCDSETUP and which can also be read by
#     CCDReadRestoreFile . A header of useful information is also
#     written to the file.

#  Arguments:
#     filename = string (read)
#        The name of the file which is to have the current global setup
#	 written to it.

#  Global parameters:
#      CCDglobalpars = array (read)
#         The array of global parameter values.

#  Return:
#      CCDrestorefile = logical (write)
#         If the write is successful then the return is set to 1
#	  otherwise it is 0.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     22-FEB-1994 (PDRAPER):
#     	 Original version.
#     {enter_changes_here}

#-

#  Global parameters:
      global CCDglobalpars
      global env                            # Environment variables

#.
      set ok 0

#  Try to open the file.
      if { ![ catch {open $filename w} fileid ] } {

#  File opened successfully write header.
         catch { exec date } date
         puts $fileid "#"
         puts $fileid "#  CCDPACK - restoration file"
         puts $fileid "#"
         puts $fileid "#  Written by $env(USER) on $date"
         puts $fileid "#"

#  Now add the global contents.
         foreach param [array names CCDglobalpars ] {
            if { $CCDglobalpars($param) != "" } { 
               puts $fileid "$param = $CCDglobalpars($param)"
            }
         }
         flush $fileid
         set ok 1
      } else {

#  Failed to open file.
         CCDIssueError "Failed to open file: $filename"
      }

#  Return
      return $ok
   }
# $Id$
