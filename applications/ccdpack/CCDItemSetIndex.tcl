   proc CCDItemSetIndex { Listbox index {value __NOVALUE__} } {
#+
#  Name:
#     CCDItemSetIndex

#  Purpose:
#     Read/write annotation of an NDF name in a listbox.

#  Language:
#     TCL

#  Description:
#     This routine operates on one entry in the setted or unsetted NDFs
#     Ccd::scrollbox widget; it reads the name and its annotation and
#     optionally writes a new annotation.
#
#     The annotation can assume one of three values: it may be the
#     empty string to indicate that no Set Index has been yet selected
#     for the NDF in question; it may be an integer giving the Set Index
#     to be assigned; or it may take the literal value "SET" to indicate
#     that the record is a header indicating the start of a Set (this
#     last case is only possible for the setted list, not the unsetted
#     one).

#  Arguments:
#     Listbox = string
#        Command name of the Ccd::scrollbox widget listing the NDFs.
#     index = string
#        The index within Listbox of the item to be modified.
#     value = string
#        Should be an integer, the empty string, or "SET" if present;
#        gives the Set Index value with which the name will be annotated.
#        If absent, the value will not be modified.

#  Return Value:
#     A two-element list is returned giving the existing annotation and
#     the name.  The existing annotation should be either an integer,
#     the empty string, or the literal "SET".

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
#     18-JUN-2001 (MBT):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get the old value of the list entry.
      set entry [$Listbox get $index]

#  Parse it to get (perhaps) a value and a name.
      regexp {^ *(-?[0-9]*|SET) *([^ ]*)} $entry line oldvalue name

#  Are we going to write a new annotation?
      if { $value != "__NOVALUE__" } {

#  Generate a new string.
         set newentry [format "%-3s %s" $value $name]

#  Replace the old entry with the new one in the list.
         $Listbox clear $index
         $Listbox insert $index $newentry
      }

#  Return the previous value.
      return [list $oldvalue $name]
   }
# $Id$
