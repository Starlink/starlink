#include <stdio.h>
#include <string.h>
#include "sae_par.h"
#include "chr.h"

void chrClean( char *string ){
/*
*+
*  Name:
*     chrClean

*  Purpose:
*     Remove all unprintable characters from a string.

*  Synopsis:
*     void chrClean( char *string )

*  Description:
*     Replace all unprintable characters in the given string with blanks.

*  Parameters:
*     string
*        Pointer to a null terminated string holding the string to be
*        cleaned.

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
*     13-MAY-2018 (DSB):
*        Original version, based on equivalent Fortran function.

*-
*/

/* Local Variables: */
   char *pr;    /* Pointer to next character */

   pr = string - 1;
   while( *(++pr) ) {
      if( *pr < ' ' || *pr > '~' ) *pr = ' ';
   }
}

