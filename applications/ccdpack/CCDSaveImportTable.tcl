  proc CCDSaveImportTable { savefile fitsbox extenbox } {
#+
#  Name:
#     CCDSaveImportTable 

#  Purpose:
#     Saves an import table described in two listboxes managed by the
#     CCDCreateImportTable interface. 

#  Type of Module:
#     Tcl/Tk procedure

#  Description:
#     This routine reads the contents of two listboxes which should
#     described a FITS import control table (as created by the
#     procedure CCDCreateImportTable). The first listbox contains the
#     FITS keywords which are to be extracted from the NDF FITS block
#     and the second the expressions of the FITS keywords which result
#     in a value for the named extension item.

#  Arguments:
#     savefile = filename (read)
#        The name of a file to contain the saved import information.
#     fitsbox = window (read)
#        The name of a listbox which contains type and keyword names
#	 of FITS items to be extracted from the FITS block.
#     extenbox = window (read)
#        The name of a listbox which contains the names of the
#	 destination extension items and the expressions of FITS
#	 keywords which result in a value.

#  Returned Value:
#     CCDSaveImportTable = boolean
#        If a file is written successfully then this returns true (1)
#	 otherwise false (0)

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     7-MAR-1994 (PDRAPER):
#     	 Original version.
#     {enter_changes_here}

#-

#  Global variables:
      global env

#.

#  Set return status.
      set status 1

#  Try to open the file.
      if { ![ catch {open $savefile w} fileid ] } {

#  File opened successfully write header.
         catch { exec date } date
         puts $fileid "#"
         puts $fileid "#  CCDPACK - import control table"
         puts $fileid "#"
         puts $fileid "#  Written by $env(USER) on $date"
         puts $fileid "#"

#  Now add the contents, first any values from the fitsbox.
         set entries [$fitsbox size]
         for { set i 0 } { $i < $entries } { incr i } {
            set newline [$fitsbox get $i]
            puts $fileid "$newline"
         }

#  Values from the extenbox.
         set entries [$extenbox size]
         for { set i 0 } { $i < $entries } { incr i } {
            set newline [$extenbox get $i]
            puts $fileid "$newline"
         }

         flush $fileid
         close $fileid
      } else {

#  Failed to open file.
         CCDIssueError "Failed to open file: $savefile"
         set status 0 
      }

#  End of procedure.
      return "$status"
   }
# $Id$
