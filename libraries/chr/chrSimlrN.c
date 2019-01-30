#include "chr.h"
#include <ctype.h>

int chrSimlrN( const char *str1, const char *str2, size_t n ){
/*
*+
*  Name:
*     chrSimlrN

*  Purpose:
*     Return whether the starts of two strings are equal, apart from case.

*  Synopsis:
*     int chrSimlrN( const char *str1, const char *str2, size_t n )

*  Description:
*     Determine whether the first "n" characters of two strings are the
*     same, ignoring distinctions between upper and lowercase letters.

*  Parameters:
*     str1
*        Pointer to a null terminated string holding the first string.
*     str2
*        Pointer to a null terminated string holding the second string.
*     n
*        The number of characters that must match at the start of each
*        string. If the length of either string is less than "n", zero
*        will be returned.

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
*     28-MAY-2018 (DSB):
*        Original version.

*-
*/

/* Local Variables: */
   const char *p1;       /* Pointer to next str1 character */
   const char *p2;       /* Pointer to next str2 character */
   char c1;              /* Lowercase character from str1 */
   char c2;              /* Lowercase character from str2 */
   int result;           /* Returned value */
   size_t i;             /* Nummber of matching characters found so far */

/* Check supplied string is usable */
   if( !str1 || !str2 ) return 0;

/* Compare characters in lower case, until "n" characters have been
   compared, the end of either string is reached, or a mis-match is found. */
   result = 1;
   i = -1;
   p1 = str1;
   p2 = str2;
   while( ++i < n && *p1 && *p2 && result ) {
      c1 = tolower( *(p1++) );
      c2 = tolower( *(p2++) );
      result = ( c1 == c2 );
   }

/* No match if the end of either string was reached before comparing "n"
   characters. */
   if( i < n ) result = 0;

/* Return the result */
   return result;
}

