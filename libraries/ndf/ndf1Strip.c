#include <string.h>
#include "sae_par.h"
#include "ndf_ast.h"

char *ndf1Strip( char *mem, const char *text, size_t start, size_t end, size_t *nc,
                 size_t *nlspace, int *status ){
/*
*+
*  Name:
*     ndf1Strip

*  Purpose:
*     Create a copy of a substring excluding any leading or trailing spaces.

*  Synopsis:
*     char *ndf1Strip( char *mem, const char *text, size_t start, size_t end,
*                      size_t *nc, size_t *nlspace, int *status )

*  Description:
*     This function returns a pointer to a dynamically allocated buffer
*     containing a copy of a substring of the specified string. Any
*     leading or trailing spaces in the specified subatring are excluded
*     from the returned copy.

*  Parameters:
*     mem
*        Pointer to previously allocated memory that is to be used to
*        hold the returned substring. New memory will be allocated if
*        NULL is supplied for "mem". If the memory block supplied via
*        "mem" is too small to hold the returned substring, it will freed
*        and a new block allocated.
*     text
*        Pointer to the null terminated string to be copied.
*     start
*        Zero based index of the first character to be copied.
*     send
*        Zero based index of the last character to be copied.
*     *nc
*        Returned holding the number of characters in the returned
*        string, excluding the terminating null. Ignored if "nc" is NULL.
*     *nlspace
*        Returned holding the number of spaces removed following the "start"
*        position within "text". Ignored if "nlspace" is NULL.
*     *status
*        The global status.

*  Returned Value:
*     Pointer to a dynamically allocated buffer holding the required
*     substring. It should be freed using astFree when no longer needed.

*  Notes:
*     - NULL is returned if an error occurs.
*     - The whole string is copied if "start" is greater than "end".
*     - A empty string (i.e. a string containing nothing but the terminating
*       null character) is returned if the selected substring contains only
*       spaces.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version.

*-
*/

/* Local Variables: */
   char *result;           /* Returned value */
   size_t lnc;             /* No. of characters in returned string */
   size_t start0;          /* Start position before removing spaces */

/* Initialise */
   if( nc ) *nc = 0;
   if( nlspace ) *nlspace = 0;
   result = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK || !text ) return result;

/* Get the total length of the supplied string, excluding the terminating
   null. If the string has zero length, return immediately. */
   lnc = strlen( text );
   if( lnc == 0 ) {
      result = astMalloc( sizeof(char) );
      *result = 0;
      return result;
   }

/* Adjust the indices of the first and last characters to be used so
   that they are within the range of the string and so that "start" is not
   larger than "end". */
   if( start > end || start >= lnc ){
      start = 0;
      end = lnc - 1;
   } else if( end >= lnc ) {
      end = lnc - 1;
   }

/* Find the index of the first non-space character following the start
   determined above. */
   start0 = start;
   while( start <= end && text[ start ] == ' ' ) start++;

/* Return the number of leading spaces removed. */
   if( nlspace ) *nlspace = start - start0;

/* Find the index of the last non-space character before the end
   determined above. */
   while( end >= start && text[ end ] == ' ' ) end--;

/* Get the number of characters to be used (excluding the terminating
   null). */
   lnc = end - start + 1;
   if( lnc < 0 ) lnc = 0;

/* Make a null terminated copy of the above section of the supplied
   string, including room for a terminating null character. */
   result = astStore( mem, text + start, lnc + 1 );

/* Terminate it. */
   if( result ) result[ lnc ] = 0;

/* Return the length. */
   if( nc ) *nc = lnc;

/* Return the pointer. */
   return result;

}

