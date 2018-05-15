#include "chr.h"
#include "star/util.h"
#include <string.h>

void chrAppnd( const char *str1, char *str2, size_t str2_length,
               size_t *iposn ){
/*
*+
*  Name:
*     chrAppnd

*  Purpose:
*     Copy one string into another, ignoring trailing blanks.

*  Synopsis:
*     void chrAppnd( const char *str1, char *str2, size_t str2_length,
*                    size_t *iposn )

*  Description:
*     The string "str1" (or as much of it as there is room for) is copied
*     into the part of "str2" beginning at position "iposn". "iposn" is
*     updated to indicate the final length of "str2" after this operation.
*     Trailing blanks in "str1" are ignored.

*  Parameters:
*     str1
*        Pointer to a null terminated string holding the string to be
*        copied.
*     str2
*        Pointer to a null terminated string holding the string to be
*        updated.
*     str2_length
*        The maximum length of the 'str2' string. This should include
*        room for the terminating null.
*     *iposn
*        The used length of the "str2". If "str2" is empty on entry, then
*        this should be supplied as zero. On exit, the supplied value
*        is incremented by the length of "str1" (ignoring any trailing
*        spaces).

*  Notes:
*     If the output string is too small to append the entire input
*     string, truncation will occur and "iposn" will be set to the total
*     length of "str2" (excluding the trailing null).

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
   size_t str1len;       /* Used length of str1 */
   size_t str2sz;        /* Max size of str2 without terminating null */

   if( str2_length > 0 ) {
      str1len = chrLen( str1 );
      str2sz = str2_length - 1;
      if( *iposn < str2sz ) {
         star_strlcpy( str2 + *iposn, str1, str2_length - *iposn );
         *iposn += str1len;
         if( *iposn > str2sz ) *iposn = str2sz;
      }
   }
}

