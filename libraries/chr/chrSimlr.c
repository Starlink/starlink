#include "chr.h"
#include <ctype.h>

int chrSimlr( const char *str1, const char *str2 ){
/*
*+
*  Name:
*     chrSimlr

*  Purpose:
*     Return whether two strings are equal, apart from case.

*  Synopsis:
*     int chrSimlr( const char *str1, const char *str2 )

*  Description:
*     Determine whether two strings are the same, ignoring distinctions
*     between upper and lowercase letters. Their lengths must be identical
*     after removing trailing blanks.

*  Parameters:
*     str1
*        Pointer to a null terminated string holding the first string.
*     str2
*        Pointer to a null terminated string holding the second string.

*  Returned Value:
*     Returned as non-zero if the two strings are the same

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
*     15-MAY-2018 (DSB):
*        Original version, based on equivalent Fortran function.

*-
*/

/* Local Variables: */
   char c1;              /* Lowercase character from str1 */
   char c2;              /* Lowercase character from str2 */
   int result;           /* Returned value */
   size_t i;             /* Length of str1 */

/* Check supplied string is usable */
   if( !str1 || !str2 ) return 0;

/* Initialise the string length. */
   i = chrLen( str1 );
   result = ( chrLen( str2 ) == i );

/* Loop to test equality for each character. */
   while( i > 0 && result ) {
      i--;
      c1 = tolower( str1[i] );
      c2 = tolower( str2[i] );
      result = ( c1 == c2 );
   }

/* Return the result */
   return result;
}

