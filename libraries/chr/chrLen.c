#include "chr.h"
#include <string.h>

size_t chrLen( const char *string ){
/*
*+
*  Name:
*     chrLen

*  Purpose:
*     Return the length of a string, ignoring trailing spaces.

*  Synopsis:
*     size_t chrLen( const char *string )

*  Description:
*     Find length of string, ignoring trailing spaces.

*  Parameters:
*     string
*        Pointer to a null terminated string holding the string whose
*        length is to be determined.

*  Returned Value:
*     Returns the length of the string, not including the
*     terminating null or any trailing spaces.

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
   const char *pr;       /* Pointer to next character to test */

/* Find a pointer to the space following the last non-space character. */
   pr = string + strlen( string );
   while( pr > string ) {
      if( *(--pr) != ' ' ) {
         pr++;
         break;
      }
   }

/* Return the length of the string up to and including the last non-space
   character. */
   return pr - string;
}

