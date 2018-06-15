#include "chr.h"
#include <string.h>


void chrRmblk( char *string ){
/*
*+
*  Name:
*     chrRmblk

*  Purpose:
*     Remove all blanks from a string.

*  Synopsis:
*     void chrRmblk( char *string )

*  Description:
*     All leading and embedded blanks in the string are removed. The
*     remaining characters are moved to the left to eliminate the resulting
*     empty space, and a terminating null is appended to the end of the
*     string.

*  Parameters:
*     string
*        Pointer to a null terminated string holding the string from which
*        all leading and embedded blanks are removed.

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
   const char *pr;    /* Pointer to next character to copy */
   char *pw;          /* Pointer to next destination character */

   pr = string - 1;
   pw = string;
   while( *(++pr) ) {
      if( *pr != ' ' ) *(pw++) = *pr;
   }

   *pw = 0;
}

