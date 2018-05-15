#include "chr.h"
#include <string.h>

void chrFandl( const char *string, size_t *index1, size_t *index2 ){
/*
*+
*  Name:
*     chrFandl

*  Purpose:
*     Find the first and last non-blank characters in a string.

*  Synopsis:
*     void chrFandl( const char *string, size_t *index1, size_t *index2 )

*  Description:
*     Find the indices of the first and last non-blank characters in the
*     given string. If the string contains no non-blank characters, the first
*     index is returned set to 1 and last index is returned set to 0, i.e.
*     "index1" is greater than "index2".

*  Parameters:
*     string
*        Pointer to a null terminated string holding the character string.
*     *index1
*        Returned holding the zero-based position of first non-blank character.
*     *index2
*        Returned holding the zero-based position of last non-blank character.

*  Notes:
*     - For consistency with the Fortran routine CHR_FANDL, this function
*     only checks for spaces. Other forms of whitespace characters such
*     as tabs, line-feeds, etc are considered to be non-blank.

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

/* Find the used length of the input string and check it is not zero. */
   *index2 = chrLen( string );
   if( *index2 == 0 ) {

/* The string is blank. Indicate this by returning index1 > index0. */
      *index2 = 0;
      *index1 = 1;

/* If the string contains non-space characters... */
   } else {

/* Get the zero-based index of the last non-space character. */
      (*index2)--;

/* Find the position of the first non-space character. */
      for( *index1 = 0; *index1 <= *index2; (*index1)++ ){
         if( string[*index1] != ' ' ) break;
      }
   }
}

