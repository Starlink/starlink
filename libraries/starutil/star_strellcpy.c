/*
*+
*  Name:
*     star_strellcpy

*  Purpose:
*     A wrapper around strlcpy that inserts ellipsis on truncation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Description:
*     This function copies a string into a buffer but if the
*     output string has been truncated, "..." will be added to the end
*     of the string to indicate truncation. Returns true if everything
*     was okay or false if the string was truncated.

*  Invocation:
*     trunc = star_strellcpy( char * dest, const char * src, size_t sizedest);

*  Arguments:
*     dest = char * (Returned)
*        Destination buffer for "src". Must be nul-terminated.
*        Will end in "..." if the source string would not fit.
*     src = const char * (Given)
*        String to be appended onto "dest".
*     sizedest = size_t (Given)
*        The actual buffer size of "dest" including space for a nul.

*  Returned Value:
*     size_t retval
*        True if the string was copied successfully. False if it was
*        truncated.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-09-11 (TIMJ):
*        Initial version.

*  Notes:
*     - This is for use from C only.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "./util.h"

int
star_strellcpy( char * dest, const char * src, size_t size ) {
  size_t nappend = 0;
  int retval = 0;
  const char ellipsis[] = "...";
  size_t nullpos = 0;      /* position to insert ellipsis */

  nappend = star_strlcpy( dest, src, size );

  if (nappend > size) {

    /* nul a character before the end and then append ellipsis
     - if the dest string is too small we null the first and append
     what we can get away with. */
    if (sizeof(ellipsis) <= size) {
      nullpos = size - sizeof(ellipsis);
    }
    dest[nullpos] = '\0';

    star_strlcat( dest, ellipsis, size );

  } else {
    retval = 1;
  }
  return retval;
}
