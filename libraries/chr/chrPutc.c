#include "chr.h"
#include "star/util.h"
#include <string.h>

void chrPutc( const char *str1, char *str2, size_t str2_length, size_t *iposn ){
/*
*+
*  Name:
*     chrPutc

*  Purpose:
*     Put a character string into another at a given position.

*  Synopsis:
*     void chrPutc( const char *str1, char *str2, size_t str2_length,
*                   size_t *iposn )

*  Description:
*     The string "str1" (or as much of it as there is room for) is copied
*     into the part of "str2" beginning at position "iposn+1". "iposn" is
*     updated to indicate the end position of the copy of "str1" within
*     "str2" after this operation. If no copying is done, "iposn" is
*     returned unchanged.

*  Parameters:
*     str1
*        Pointer to a null terminated string holding the string to be
*        copied.
*     str2
*        Pointer to a null terminated string holding the string into which
*        "str1" is to be copied.
*     str2_length
*        The declared length of the supplied 'str2' array. This should include
*        room for the terminating null.
*     *iposn
*        The zero-based position pointer within "str2".

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
   size_t size1;         /* Size of STR1 */
   size_t size2;         /* Size of STR2 */

/* Check the supplied string length is OK. */
   if( str2_length > 0 ) {

/* Get the max size of target string, excluding the trailing null. */
      size2 = str2_length - 1;

/* Check that the pointer is within string. */
      if( *iposn < size2 ) {

/* Get the length that can be copied. */
         size1 = strlen( str1 );
         size2 -= *iposn;
         if( size2 < size1 ) size1 = size2;

/* Copy the string. */
         if( size1 > 0 ) {
            star_strlcpy( str2 + *iposn, str1, size1 + 1 );

/* Update the pointer value. */
            *iposn += size1;
         }
      }
   }
}

