#include <string.h>
#include "sae_par.h"
#include "ndf_ast.h"

char *ndf1Substr( const char *text, size_t start, size_t end, int *status ){
/*
*+
*  Name:
*     ndf1Substr

*  Purpose:
*     Create a copy of a substring.

*  Synopsis:
*     char *ndf1Substr( const char *text, size_t start, size_t end,
*                       int *status )

*  Description:
*     This function returns a pointer to a dynamically allocated buffer
*     containing a copy of a substring of the specified string.

*  Parameters:
*     text
*        Pointer to the null terminated string to be copied.
*     start
*        Zero based index of the first character to be copied. The start
*        of the string is used if a negative value is supplied.
*     send
*        Zero based index of the last character to be copied. The end of
*        the string is used if the supplied value is bigger than the
*        length of the string.
*     *status
*        The global status.

*  Returned Value:
*     Pointer to a dynamically allocated buffer holding the required
*     substring. It should be freed using astFree when no longer needed.

*  Notes:
*     - NULL is returned if an error occurs, or if "text" is NULL.
*     - The whole string is copied if "start" is greater than "end".

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
*     xxx (DSB):
*        Original version.

*-
*/

/* Local Variables: */
   char *result;           /* Returned value */
   size_t lnc;             /* No. of characters in returned string */

/* Initialise */
   result = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK || !text ) return result;

/* Get the total length of the supplied string, excluding the terminating
   null. */
   lnc = strlen( text );

/* Adjust the indices of the first and last characters to be used so
   that they are within the range of the string and so that "start" is not
   larger than "end". */
   if( start > end || start >= lnc || end < 0 ){
      start = 0;
      end = lnc - 1;
   } else {
      if( start < 0 ) start = 0;
      if( end >= lnc ) end = lnc - 1;
   }

/* Adjust the number of characters to be copied. */
   lnc = end - start + 1;

/* Make a null terminated copy of the above section of the supplied
   string, including room for a terminating null character. */
   result = astStore( NULL, text + start, lnc + 1 );

/* Terminate it. */
   if( result ) result[ lnc ] = 0;

/* Return the pointer. */
   return result;

}

