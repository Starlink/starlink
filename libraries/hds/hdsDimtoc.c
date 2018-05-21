#include <stdio.h>
#include <string.h>
#include "chr.h"
#include "star/hds.h"

void hdsDimtoc( hdsdim ivalue, char *string, size_t string_length, size_t *nchar ){
/*
*+
*  Name:
*    hdsDimtoc

*  Purpose:
*     Encode an hdsdim value as a string.

*  Synopsis:
*     void hdsDimtoc( hdsdim ivalue, char *string, size_t string_length,
*                     size_t *nchar )

*  Description:
*     Encode an hdsdim value as a (decimal) character string, using as
*     concise a format as possible, and return the number of characters
*     used. In the event of an error, "*'s will be written into to the
*     string. This is modelled on the "chrXtoc" functions in the CHR
*     library.

*  Parameters:
*     ivalue
*        The value to be encoded.
*     string
*        Pointer to an array in which to return a null terminated string
*        holding the string into which the integer value is encoded.
*     string_length
*        The maximum length of the supplied 'string' array. This should
*        include room for the terminating null.
*     *nchar
*        Returned holding the field width used in encoding the value.

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
*     21-MAY-2018 (DSB):
*        Original version.

*-
*/

/* Local Variables: */
   char buf[ 20 ];       /* Buffer for integer value */

/* Check the output buffer has non-zero length. */
   if( string_length > 0 ) {

/* Perform the internal write to the buffer, left justifying the
   resulting string. */
      *nchar = sprintf( buf, "%-zu", ivalue );

/* Copy the value to the supplied string if there is room. */
      if( *nchar < string_length ) {
         strcpy( string, buf );

/* On error, fill the returned string with "*"s. */
      } else {
        chrFill( '*', string, string_length );
         *nchar = string_length - 1;
      }
   }
}

