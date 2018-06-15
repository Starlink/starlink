#include "chr.h"

void chrFill( char cvalue, char *string, size_t string_length ){
/*
*+
*  Name:
*     chrFill

*  Purpose:
*     Fill a string with a given character.

*  Synopsis:
*     void chrFill( char cvalue, char *string, size_t string_length )

*  Description:
*     The given character string is filled with the specified character.

*  Parameters:
*     cvalue
*        The character specified to fill the string.
*     string
*        Pointer to an array in which to return a null terminated string
*        holding the string to be filled.
*     string_length
*        The declared length of the supplied 'string' array. This should
*        include room for the terminating null.

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
   char *pw;          /* Pointer to next destination character */
   size_t i;          /* Loop index */
   size_t size;       /* Declared size of given string */

/* Get the declared size of given string excluding terminating null. */
   if( string_length > 0 ) {
      size = string_length - 1;

/* Duplicate character into every element in the string. */
      pw = string;
      for( i = 0; i < size; i++ ){
         *(pw++) = cvalue;
      }

/* Terminate the string. */
      *pw = 0;
   }
}

