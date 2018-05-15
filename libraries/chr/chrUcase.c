#include "chr.h"
#include <ctype.h>

void chrUcase( char *string ){
/*
*+
*  Name:
*     chrUcase

*  Purpose:
*     Convert a string to uppercase.

*  Synopsis:
*     void chrUcase( char *string )

*  Description:
*     The characters in the string are all converted to uppercase in situ.

*  Parameters:
*     string
*        Pointer to a null terminated string holding the string to be
*        converted to uppercase.

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
   char *pw; /* pointer to next character */

/* Convert string to uppercase. */
   pw = string - 1;
   while( *(++pw) ){
      *pw = toupper( *pw );
   }

}

