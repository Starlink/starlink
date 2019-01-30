#include "chr.h"
#include <string.h>

void chrFparx( const char *str, char oppar, char clpar, size_t *f, size_t *l ){
/*
*+
*  Name:
*     chrFparx

*  Purpose:
*     Find a parenthesised expression in a character string.

*  Synopsis:
*     void chrFparx( const char *str, char oppar, char clpar, size_t *f,
*                    size_t *l )

*  Description:
*     This function searches the string "str" to identify a sub-string
*     containing a parenthesised expression and returns the character
*     positions of the opening and closing parentheses in the "f" and "l"
*     arguments. Allowance is made for nested parentheses. If a
*     parenthesised expression was not found, then the returned value of
*     "f" will be greater than the returned value of "l".

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string to be
*        searched.
*     oppar
*        The opening parenthesis character.
*     clpar
*        The closing parenthesis character.
*     *f
*        Returned holding the zero-based character position of the opening
*        parenthesis.
*     *l
*        Returned holding the zero-based character position of the closing
*        parenthesis.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     15-MAY-2018 (DSB):
*        Original version, based on equivalent Fortran function.

*-
*/

/* Local Variables: */
   int found;            /* Whether first character found */
   int ipar;             /* Count of nested parentheses */
   size_t i;             /* Loop counter for characters */
   size_t len;           /* string length */


/* Check supplied string is usable */
   if( !str ) return;

/* Initialise. */
   *f = 1;
   *l = 0;
   found = 0;

/* Inspect each character in the string, looking for an opening
   parenthesis. */
   len = strlen( str );
   for( i = 0; i < len; i++ ){
      if( str[ i ] == oppar ) {

/* If found, then note its position. */
         found = 1;
         *f = i;
         break;
      }
   }

/* If the start of an expression has been found, then search for the
   end. */
   if( found ) {

/*Initialise the count of nested parentheses. */
      ipar = 0;

/* Loop to inspect subsequent characters in the string. */
      for( i = *f; i < len; i++ ){

/* Count opening parentheses. */
         if( str[ i ] == oppar ) {
            ipar++;

/* Count closing parentheses. */
         } else if( str[ i ] == clpar ) {
            ipar--;

/* If the number of nested parentheses falls to zero, then the final
   character of the expression has been found. Note its position. */
            if( ipar == 0 ) {
               *l = i;
               break;
            }
         }
      }
   }
}

