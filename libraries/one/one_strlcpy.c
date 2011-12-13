/*
*+
*  Name:
*     one_strlcpy

*  Purpose:
*     Starlink compliant wrapper around the BSD strlcpy function.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Description:
*     The strlcpy function is similar to the strncpy function except
*     that it guarantees to nul terminate the destination string
*     and returns the number of characters that will have been copied.
*     This wrapper function provides standard Starlink inherited status
*     semantics.

*  Invocation:
*     len = one_strlcpy( char * dest, const char * src,
*                        size_t sizedest, int * status );

*  Arguments:
*     dest = char * (Returned)
*        Destination buffer for "src". Will be nul-terminated.
*        If status is bad on entry, dest will be nul-terminated
*        if it is non-NULL.
*     src = const char * (Given)
*        String to be copied.
*     sizedest = size_t (Given)
*        The actual buffer size of "dest" including space for a nul.
*     status = int * (Given and Returned)
*        Inherited status. Will be set to ONE__TRUNC if the string
*        was truncated on copy.

*  Returned Value:
*     size_t retval
*        Length of the string after copying. Will either be
*        the length of the source string or one less than the
*        size of the destination buffer.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-05-29 (TIMJ):
*        Initial version.
*     2008-07-10 (TIMJ):
*        Trap null pointers.
*     2008-09-05 (TIMJ):
*        Use starutil to get strlcpy function.

*  Notes:
*     - This is for use from C only.
*     - Use starutil's star_strlcat if you can not use the Starlink error
*       system.

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

#include "ems.h"
#include "one.h"
#include "star/util.h"
#include "one_err.h"
#include "sae_par.h"

#include <stdlib.h>

size_t
one_strlcpy( char * dest, const char * src, size_t size, int * status ) {
  size_t retval = 0;

  if (dest) {
    dest[0] = '\0';
  }

  if (*status != SAI__OK) return retval;

  /* Trap null pointers - since strlcpy won't */
  if (!dest) {
    *status = SAI__ERROR;
    emsRep( " ", "one_strlcpy: Destination string is a NULL pointer "
            "(possible programming error)", status);
  }
  if (!src) {
    *status = SAI__ERROR;
    emsRep( " ", "one_strlcpy: Source string is a NULL pointer "
            "(possible programming error)", status);
  }

  /* BSD function */
  retval = star_strlcpy( dest, src, size );

  if (retval >= size) {
    *status = ONE__TRUNC;
    emsSetc("SRC", src);
    emsSeti("I", (int)size);
    emsSeti("S", (int)retval);
    emsRep( " ", "Truncated string when copying ^S characters into buffer "
           "of size ^I", status);
    /* return the actual length of the new string - we know it was truncated
       so just return the size that is actually relevant */
    retval = size - 1;
  }

  return retval;
}
