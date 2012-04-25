/*
 *+
 *  Name:
 *    CHR_KTOC

 *  Purpose:
 *    Encode an INTEGER*8 value as a string

 *  Language:
 *    Starlink C callable from Fortran

 *  Invocation:
 *    CALL CHR_KTOC( KVALUE, STRING, NCHAR )

 *  Description:
 *     Encode a 64-bit integer value as a (decimal) character string, using as
 *     concise a format as possible, and return the number of characters
 *     used. In the event of an error, '*'s will be written into to the
 *     string.

 *  Arguments:
 *     KVALUE = INTEGER*8 (Given)
 *        The value to be encoded.
 *     STRING = CHARACTER * ( * ) (Returned)
 *        The string into which the integer*8 value is encoded.
 *     NCHAR = INTEGER (Returned)
 *        The field width used in encoding the value.

 *  Copyright:
 *     Copyright (C) 2012 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Authors:
 *     TIMJ: Tim Jenness (JAC, Hawaii)

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 3 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful,but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  History:
 *    25-APR-2012 (TIMJ):
 *      Initial version. Done in C to simplify formatting.

 *-
 */

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "f77.h"

#ifdef PRIi64
#define FMT PRIi64
#else
#define FMT "lld"
#endif


F77_SUBROUTINE(chr_ktoc)( INTEGER8(KVALUE), CHARACTER(STR),
                          INTEGER(NCHAR) TRAIL(STR) ) {

  size_t len = 0;
  size_t buflen = STR_length + 1;
  char * str = malloc( sizeof(*str) * buflen );

  if (str == NULL) {
    *NCHAR = 1;
    cnfExprt( "*", STR, STR_length );
    return;
  }

  len = snprintf( str, buflen, "%" FMT, *KVALUE );

  /* we have truncation if len is buflen or more */
  if (len >= buflen) {
    /* fill with "*" */
    size_t i;
    for (i = 0; i < STR_length; i++ ) {
      str[i] = '*';
    }
    str[STR_length] = '\0';
    len = STR_length;
  }

  /* Export to Fortran */
  *NCHAR = len;
  cnfExprt( str, STR, STR_length );
  free(str);
}

