#include "chr.h"
#include <string.h>

void chrLdblk( char *string ){
/*
*+
*  Name:
*     chrLdblk

*  Purpose:
*     Remove any leading blanks from a string.

*  Synopsis:
*     void chrLdblk( char *string )

*  Description:
*     Remove any leading blanks from the character string. The remaining
*     characters are moved to the left to eliminate the resulting empty
*     space, and a terminating null is appended to the end.

*  Parameters:
*     string
*        Pointer to a null terminated string holding the string from which
*        the leading blanks are to be removed.

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
   int start;         /* Are we still skipping leading blanks? */
   const char *pr;    /* Pointer to next character to copy */
   char *pw;          /* Pointer to next destination character */

   if( !string ) return;

   start = 1;
   pr = string - 1;
   pw = string;
   while( *(++pr) ) {
      if( start && *pr != ' ' ) start = 0;
      if( !start ) *(pw++) = *pr;
   }

   *pw = 0;

}

