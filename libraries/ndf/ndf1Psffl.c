#include <stdlib.h>
#include "sae_par.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Psffl( const char *list, int mxel, size_t ibeg[], size_t iend[],
                int *el, int *status ){
/*
*+
*  Name:
*     ndf1Psffl

*  Purpose:
*     Parse a foreign format list.

*  Synopsis:
*     void ndf1Psffl( const char *list, int mxel, size_t ibeg[], size_t iend[],
*                     int *el, int *status )

*  Description:
*     This function locates elements in a list of foreign data format
*     specifications held as a character string and returns the character
*     positions at which they start and end. All blanks surrounding each
*     list element are discarded, as also are blank elements themselves.
*     Checks are performed to ensure that the space available for storing
*     element positions is not exceeded.

*  Parameters:
*     list
*        Pointer to a null terminated string holding the foreign format
*        list which is to be split up.
*     mxel
*        The maximum number of list elements expected.
*     ibeg
*        Returned holding the array of zero-based character positions
*        identifying the start of each element in "list". The supplied "ibeg"
*        array should have at least "mxel" elements.
*     iend
*        Returned holding the array of zero-based character positions
*        identifying the end of each element in "list". The supplied "iend"
*        array should have at least "mxel" elements.
*     *el
*        Number of list elements returned.
*     *status
*        The global status.

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
   const char *p;        /* Pointer to next character to check */
   size_t f;             /* First non-blank character position */
   size_t i;             /* Pointer to start of element */
   size_t l;             /* Last non-blank character position */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise the element count. */
   *el = 0;

/* Initialise the zero-based indices of the first and last non-space
   characters in the next field. Set "f" greater than "l" to indicate no
   non-space characters have yet been found. */
   f = 1;
   l = 0;

/* Initialise the zero-based index of the next character to check, and
   get a pointer to it. The "i" variable is unsigned so we cannot
   intialise it to "-1". */
   i = 0;
   p = list - 1;

/* Loop round all characters in the supplied list, exiting the loop when
   the terminating null character is reached. */
   while( *(++p) ) {

/* If the current character is a delimiter, check that the previous field
   was not blank. **/
      if( *p == ',' ) {
         if( f <= l ){

/* If we have not yet filled the returned arrays, add the previous field
   to them. Otherwise, report an error and leave the loop. */
            if( *el < mxel ){
               ibeg[ *el ] = f;
               iend[ *el ] = l;
               (*el)++;
            } else {
               *status = NDF__XSFMT;
               msgSeti( "MXEL", mxel );
               msgSetc( "LIST", list );
               errRep( " ", "Too many foreign data formats specified "
                       "(maximum permitted is ^MXEL) in the list '^LIST'.",
                       status );
               break;
            }
         }

/* Prepare for the next field by re-initialising the indices of the first
   and last non-space characters in the next field with "f" > "l". */
         f = 1;
         l = 0;

/* If the current character is not a space, update the indices of the
   first and last non-space character in the field. */
      } else if( *p != ' ' ){
         if( f > l ) f = i;
         l = i;
      }

/* Update the zero-based index of the next character to check. */
      i++;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Psffl", status );

}

