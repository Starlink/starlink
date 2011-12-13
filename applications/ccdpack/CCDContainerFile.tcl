   proc CCDContainerFile { ndfname } {
#+
#  Name:
#     CCDContainerFile

#  Purpose:
#     Returns the filename associated with an image.

#  Language:
#     TCL

#  Description:
#     This routine returns the filename in which an NDF structure lives.
#     Often this will just be the name of the structure with '.sdf'
#     appended, but in the case of HDS container files it may be more
#     complicated.  It works because whenever the ndgexpand command
#     is used to look inside HDS container files, the calling code
#     updates the CCDndfcontainers global array with a record of
#     what NDFs were found in what container files.  If there is no
#     corresponding entry in CCDndfcontainers, this routine returns
#     the input value, since it is presumably the name of a non-HDS file.

#  Arguments:
#     ndfname = string (read)
#        The name of an NDF structure.

#  Global Variables:
#     CCDndfcontainers = array (read)
#        An array mapping every NDF structure so far encountered to its
#        HDS container file.

#  Return Value:
#     The name of the container file in which the NDF structure lives.

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
#     15-JUN-2001 (MBT):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
      global CCDndfcontainers

#  If we've got a record of this one, return the name of the HDS
#  container file.
      if { [ array names CCDndfcontainers $ndfname ] == $ndfname } {
         return $CCDndfcontainers($ndfname)

#  Otherwise, return the input name.
      } else {
         return $ndfname
      }
   }
# $Id$
