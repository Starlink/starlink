#include <ctype.h>
#include <string.h>
#include "ndf1.h"

int ndf1Isalm( const char *string, size_t start, size_t end ){
/*
*+
*  Name:
*     ndf1Isalm

*  Purpose:
*     Return whether a string is alphanumeric (a-z, A-Z, 0-9, '_')

*  Synopsis:
*     int ndf1Isalm( const char *string, size_t start, size_t end )

*  Description:
*     Determine whether the given string is composed entirely of
*     alpha-numeric characters or '_'.

*  Parameters:
*     string
*        Pointer to a null terminated string holding the string to be
*        tested.
*     start
*        The zero-based index of the first character to consider in "string".
*        The whole string is used if "start" > "end".
*     end
*        The zero-based index of the last character to consider in "string".
*        The whole string is used if "start" > "end".

*  Returned Value:
*     Returns non-zero if the given string is alpha-numeric.

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
   const char *p;    /* Pointer to next character to check */
   int result;       /* Returned value */

/* Get the start and end positions to use. */
   if( start > end ) {
      start = 0;
      end = strlen( string ) - 1;
   }

/* Initialise the returned value. */
   result = 1;

/* Loop to check each character in the given string (alphanumeric or
   underscore). */
   p = string  + start - 1;
   while( result &&  *(++p) && p <= string + end ) {
      result = isalnum( *p ) || *p == '_';
   }

/* Return the result */
   return result;
}

