#include <ctype.h>
#include "chr.h"

int chrIsnam( const char *string ){
/*
*+
*  Name:
*     chrIsnam

*  Purpose:
*     Return whether a string is a valid name.

*  Synopsis:
*     int chrIsnam( const char *string )

*  Description:
*     Determine whether the given string is a valid name: i.e. whether it
*     starts with an alphabetic character and continues with alphanumeric
*     or underscore characters.

*  Parameters:
*     string
*        Pointer to a null terminated string holding the string to be
*        tested.

*  Returned Value:
*     Returns non-zero if the given string is a valid name, returns

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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   const char *pr;    /* Pointer to next character to check */
   int result;        /* Returned value */

/* The string is non-blank, so initialise the returned value. */
   result = isalpha( string[0] );

/* Loop to check each character in the given string. */
   pr = string;
   while( result &&  *(++pr) ) {
      result = chrIsalm( *pr );
   }

/* Return the result */
   return result;
}

