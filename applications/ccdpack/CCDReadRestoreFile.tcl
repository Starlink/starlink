   proc CCDReadRestoreFile { file } {
#+
#  Name:
#     CCDReadRestoreFile

#  Purpose:
#     Restores CCDPACK global parameters from a text file.

#  Language:
#     Tcl/Tk

#  Description:
#     This routine reads in the values from the given file and decodes
#     the contents into values for known CCDPACK global parameters. The
#     contents of the file are as those allowed by the CCDSETUP
#     application, namely:
#
#       GLOBAL_PARNAME = value1[,value2][,values3] etc.
#
#     GLOBAL_PARNAME must be one of the parameters as used by CCDSETUP.
#     These are:
#        ADC
#        BOUNDS
#        DEFERRED
#        DIRECTION
#        EXTENT
#        GENVAR
#        LOGFILE
#        LOGTO
#        MASK
#        NDFNAMES
#        PRESERVE
#        RNOISE
#        SATURATE
#        SATURATION
#        SETSAT
#        USESET
#
#     Additionally, some of these parameters may be prefixed by a
#     Set Index modifier, which is an integer followed by a comma,
#     to indicate the value specific to a given Set Index.  E.g.
#     the value "3,ADC" would be the ADC value for Set Index 3.
#
#     Lines may be continued using the character "-" at the end of the
#     line, comments may be present. These use the characters "#" and
#

#  Arguments:
#     file = string (read)
#        The name of the text file containing the commands used to set
#        the global associations.

#  Returned Value:
#     No value is returned (see Global parameters).

#  Global Parameters:
#     CCDglobalpars = array (write)
#        The resultant global parameters are set using the elements
#        CCDglobalpars(GLOBAL_PARNAME). If a value already exists this is
#        overwritten. Global parameters with no value are not set.
#     CCDsetindices = list of integers (write)
#        The NDF Set Index values that we know about.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
#     22-FEB-1994 (PDRAPER):
#        Original version.
#     4-MAR-1994 (PDRAPER):
#        Now named CCDReadRestoreFile.
#     26-JUN-2001 (MBT):
#        Upgraded for Sets.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Declare global parameters:
   global CCDglobalpars
   global CCDsetindices

#  Local constants.
   set ixpars {ADC BOUNDS DEFERRED DIRECTION EXTENT MASK RNOISE SATURATION}
   set noixpars {GENVAR LOGFILE LOGTO NDFNAMES PRESERVE SATURATE SETSAT USESET}

#.

#  Check that file is readable.
   if [file readable $file]  {

#  Prepare regular expressions.
      set ixregex "^(-?\[0-9\]+)(?:,)([join $ixpars |])"
      set noixregex "^([join [eval list $ixpars $noixpars] |])"

#  Open the file.
      set fileid [open $file r]
      while { [set line [CCDReadTextLine $fileid] ] != "" } {

#  Have a line of information to decode. Look for the keyword
#  which we expect.
         if { [string match "*=*" $line ] } {
            if { [regexp -nocase $ixregex $line whole sindex pname] } {
               set key "$sindex,[string toupper $pname]"
               set indexfound($sindex) 1
            } elseif { [regexp -nocase $noixregex $line whole pname] } {
               set key [string toupper $pname]
            } else {
               CCDIssueInfo "Unrecognised keyword ($line)"
               set keyword "default"
            }

#  Decode statement. Need the trailing value (after the equals sign).
            set equalsat [string first "=" $line]
            incr equalsat
            set trailing [string range $line $equalsat end]
	    set trailing [string trim $trailing]
            set CCDglobalpars($key) $trailing

#  Couldn't match this line.
         } else {
            CCDIssueInfo "Unrecognisable expression: $line"
         }
      }

#  Close the file.
      close $fileid

#  Update our knowledge about the Set Index values we will encounter.
      set CCDsetindices [lsort -integer [array names indexfound]]

#  Ensure that the values are in the form we want.
      if { [info exists CCDglobalpars(LOGTO)] } {
         set CCDglobalpars(LOGTO) [string toupper $CCDglobalpars(LOGTO)]
         if { $CCDglobalpars(LOGTO) != "BOTH" &&
              $CCDglobalpars(LOGTO) != "TERMINAL" } {
            unset CCDglobalpars(LOGTO)
         }
      }
      foreach name [array names CCDglobalpars *DIRECTION] {
         set CCDglobalpars($name) [string toupper $CCDglobalpars($name)]
         if { $CCDglobalpars($name) != "X" && $CCDglobalpars($name) != "Y" } {
            unset CCDglobalpars($name)
         }
      }

#  Couldn't read the file.
   } else {
      CCDIssueInfo "Failed to open file $file (not readable)"
   }

#  End of procedure.
   }
# $Id$
