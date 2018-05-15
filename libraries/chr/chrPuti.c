#include "chr.h"

void chrPuti( int ivalue, char *string, size_t string_length, size_t *iposn ){
/*
*+
*  Name:
*     chrPuti

*  Purpose:
*     Put an integer value into a string at a given position.

*  Synopsis:
*     void chrPuti( int ivalue, char *string, size_t string_length,
*                   size_t *iposn )

*  Description:
*     The integer value is encoded into a concise string which is then
*     copied into the given string beginning at position "iposn+1". "iposn"
*     is returned updated to indicate the end position of the encoded
*     number within "string". This is a combination of chrItoc and chrPutc.

*  Parameters:
*     ivalue
*        The value to be encoded into the string.
*     string
*        Pointer to a null terminated string holding the string into which
*        "ivalue" is to be copied.
*     string_length
*        The declared length of the supplied 'string' array. This should
*        include room for the terminating null.
*     *iposn
*        The zero-based position pointer within "string".

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
   char str1[ 81 ];      /* Temporary string to hold number */
   size_t size1;         /* Size of str1 */

   chrItoc( ivalue, str1, sizeof( str1 ), &size1 );
   chrPutc( str1, string, string_length, iposn );

}

