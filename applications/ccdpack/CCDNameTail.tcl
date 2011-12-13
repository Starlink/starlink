   proc CCDNameTail { name } {
#+
#  Name:
#     CCDNameTail

#  Purpose:
#     Gets the final part of a fully qualified namespace name.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk commands

#  Description:
#     This function returns the last part (after the last ::) of a fully
#     qualified name of a Tcl variable.  It does the same as
#     the 'info namespace tail' command used to do in Itcl2.2/Tcl7.6,
#     which command does not exist in Itcl3/Tcl8.

#  Copyright:
#     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
#     1-APR-2000 (MBT):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

      regsub .*:: $name "" tail
      return $tail
}
# $Id$
