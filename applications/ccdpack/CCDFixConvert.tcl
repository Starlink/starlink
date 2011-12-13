   proc CCDFixConvert { prefix } {
#+
#  Name:
#     CCDFixConvert

#  Purpose:
#     Set up temporary file names for on the fly NDF conversions.

#  Language:
#     TCL

#  Description:
#     This routine works around problems which occur when a data file
#     in a foreign file format is open from two processes at once.
#     In CCDPACK this can happen when a C/Fortran A-task spawns a
#     child process to run ccdwish.
#
#     When the first process accesses a foreign data file the NDF
#     library may create a temporary HDS container file to hold it
#     in native form.  If the second process accesses the same
#     foreign file, NDF will overwrite the same HDS container
#     file with the same converted data.  When the second process
#     exits or releases the NDF, the temporary file will be deleted,
#     so that the first process can no longer find it.
#
#     This routine tinkers with environment variables so that the
#     created HDS container file is given a different name in each
#     instance.  This means that the processes are working with
#     different native versions of the foreign data.  It is still
#     inefficient, in that it requires an extra conversion and an
#     extra copy simultaneously on disk, but it will avoid error
#     conditions.
#
#     See SSN/20 for documentation of the environment variables
#     accessed here.

#  Arguments:
#     prefix = string
#        A string to prepend to the normal name for a temporary HDS
#        container file to distinguish it from the one used prior
#        to calling this routine.

#  Copyright:
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
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     26-JUL-2001 (MBT):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
      global env

#  Prepend a string to each of the temporary environment variable names
#  so that they are different from what they were before.  The prefix
#  must go after the last-appearing directory separator ("/") if one
#  exists, otherwise at the start of the string.
      foreach envar [ array names env NDF_TEMP_* ] {
         set ival $env($envar)
         set slashpos [ string last / $ival ]
         if { $slashpos > -1 } {
            set oval [ string range $ival 0 $slashpos ]
            set oval "$oval$prefix"
            set oval "$oval[ string range $ival [ expr $slashpos + 1 ] end ]"
         } else {
            set oval $prefix$ival
         }
         set env($envar) $oval
      }
   }
# $Id$
