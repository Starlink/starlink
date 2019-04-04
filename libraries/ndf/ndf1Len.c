#include <stdlib.h>
#include "ndf1.h"

size_t ndf1Len( const char *str ){
/*
*+
*  Name:
*     ndf1Len

*  Purpose:
*     Return the declared length of a character string.

*  Synopsis:
*     size_t ndf1Len( const char *str )

*  Description:
*     This function returns the number of characters in the string
*     supplied, as determined by the Fortran intrinsic LEN function.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string.

*  Returned Value:
*     The string's length.

*  Notes:
*     This function exists purely to avoid using the intrinsic LEN function
*     in generic functions, where the compiler might otherwise object to
*     its parameter having an incorrect data type (even though such calls
*     would never actually be executed).

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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   size_t result;        /* Returned value */

/* Return the string length. */
   result = strlen( str );

/* Return the result */
   return result;
}

