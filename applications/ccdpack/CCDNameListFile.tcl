   proc CCDNameListFile { filename names } {
#+
#  Name:
#     CCDNameListFile

#  Purpose:
#     Write a list of names to a file.

#  Description:
#     This routine writes a list of names to a file, one per line.
#     It is intended for use when constructing indirection files for
#     presenting to ADAM tasks.

#  Arguments:
#     filename = string
#        The name of a file to which the names are written.
#     names = list of strings
#        A list of names to write, one per line.

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     18-JUN-2001 (MBT):
#        Original version.
#-

#  Attempt to open the file for writing.
      if { ! [catch { open $filename w } fileid] } {

#  Write short header.
         puts $fileid "#  Temporary file written by CCDNameListFile.tcl"

#  Write all the requested names.
         foreach name $names {
            puts $fileid $name
         }
         close $fileid

#  Open failed - log an error.
      } else {
         CCDIssueError "Failed to open temporary file ($fileid)"
      }
   }
# $Id$
