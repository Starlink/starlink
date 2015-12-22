/*
*+
*  Name:
*     star_strlcat

*  Purpose:
*     Simple wrapper around the BSD strlcat function.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Description:
*     The strlcat function is similar to the strncat function except
*     that it guarantees to nul terminate the destination string
*     and returns the number of characters that will have been copied.

*  Invocation:
*     len = star_strlcat( char * dest, const char * src, size_t sizedest );


*  Arguments:
*     dest = char * (Returned)
*        Destination buffer for "src". Must be nul-terminated.
*     src = const char * (Given)
*        String to be appended onto "dest".
*     sizedest = size_t (Given)
*        The actual buffer size of "dest" including space for a nul.

*  Returned Value:
*     size_t retval
*        Length of the string that was created or the length of the
*        string that would have been created if sizedest was large enough.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-11 (TIMJ):
*        Initial version.

*  Notes:
*     - This is for use from C only.
*     - If available the system strlcat routine will be used. The entire
*       purpose of this routine is to provide a version of strlcat if
*       the operating system does not provide one (e.g. on Linux)
*     - The ONE implementation should always be used if
*       Starlink error handling is available.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include "./util.h"

#include <stdlib.h>

/* Use local or remote strlcat */
#if HAVE_STRLCAT
#  include <string.h>
#else
size_t strlcat( char * dst, const char * src, size_t size);
#include "strlcat.c"
#endif


size_t
star_strlcat( char * dest, const char * src, size_t size ) {
  size_t retval = 0;
  /* BSD function */
  retval = strlcat( dest, src, size );
  return retval;
}
