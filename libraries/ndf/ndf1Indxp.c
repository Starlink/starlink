#include <stdlib.h>
#include "ndf1.h"
#include <string.h>

int ndf1Indxp( const char *str, char ch, size_t *iat ){
/*
*+
*  Name:
*     ndf1Indxp

*  Purpose:
*     Find a character in a string, ignoring characters in parentheses.

*  Synopsis:
*     int ndf1Indxp( const char *str, char ch, size_t *iat )

*  Description:
*     This function returns the position of the first occurrence of the
*     character "ch" in the string "str", omitting any occurrences which
*     lie within parentheses "(...)". Account is taken of nested
*     parentheses.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string to be
*        searched.
*     ch
*        Character to be found.
*     *iat
*        On entry, the zero-based index of the first character to be
*        checked. On exit, the index of the matching character, if any.
*        Returned unchanged if no matching character is found.

*  Returned Value:
*     A non-zero value is returned if the character is found, and zero is
*     returned otherwise.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   const char *p;        /* Pointer to next character */
   int par;              /* Depth of nested parentheses */
   int result;           /* Returned value */

/* Initialise. */
   par = 0;
   result = 0;

/* Inspect each character in "str" before the terminating null. */
   p = str + *iat - 1;
   while( *(++p) != 0 ) {

/* If the target character is found when not inside parentheses, then
   return its position. */
      if( *p == ch && par == 0 ) {
         *iat = p - str;
         result = 1;
         break;

/* Count entries into each level of nested parenthesis. */
      } else if( *p == '(' ) {
         par++;

/* Decrement the count when leaving each level of parenthesis. Ignore
   missing left parentheses. */
      } else if( *p == ')' && par > 0 ) {
         par--;
      }
   }

/* Return the result */
   return result;
}

