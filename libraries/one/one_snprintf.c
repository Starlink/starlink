/*
*+
*  Name:
*     one_snprintf

*  Purpose:
*     Starlink compliant wrapper around the standard snprintf function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Description:
*     The one_snprintf() function replaces all calls to sprintf() and snprintf()
*     to allow the use of inherited status and to trap for truncation.

*  Invocation:
*     int one_snprintf( char * str, size_t size, const char * format, int * status, ... );

*  Arguments:
*     str = char * (Returned)
*        Buffer to be filled from the format statement. Must be at least "size"
*        characters in length.
*     size = size_t (Given)
*        Allocated size of buffer "str".
*     format = const char * (Given)
*        Standard format string as expected by snprintf().
*     status = int * (Given and Returned)
*        Inherited status. Will be set to ONE__TRUNC if the formatted version
*        of the string does not fit into the buffer "str".
*     ... = variadic arguments required by snprintf (Given)

*  Returned Value:
*     int retval
*        Length of the string after appending. If the value exceeds the size of the
*        supplied buffer status will be set to ONE__TRUNC but the value returned
*        will indicate the size of the buffer that would be required to completely
*        expand the formatted string.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2013-03-26 (TIMJ):
*        Initial version.

*  See Also:
*     - one_strlcat

*  Notes:
*     - This is for use from C only.
*     - Use this routine in place of snprintf and sprintf.

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
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

#include <stdio.h>
#include <stdarg.h>

#include "ems.h"
#include "sae_par.h"
#include "one.h"
#include "one_err.h"

int one_snprintf( char * str, size_t size, const char * format, int * status, ... ) {
  int len;
  va_list args;

  if (*status != SAI__OK) return 0;

  va_start( args, status );
  len = vsnprintf( str, size, format, args );
  va_end( args );

  if ( len >= size ) {
    /* truncation has occurred */
    *status = ONE__TRUNC;
    emsRepf( " ", "Truncated string when formatting into a buffer of size %zu (required %d characters)",
             status, size, len );
  }

  return len;
}
