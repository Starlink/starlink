#include <string.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include "star/util.h"

void ndf1Hcpy( int nlines, size_t clen, char *hist, const char *text,
               int *status ){
/*
*+
*  Name:
*     ndf1Hcpy

*  Purpose:
*     Copy lines of history text.

*  Synopsis:
*     void ndf1Hcpy( int nlines, size_t clen, char *hist, const char *text,
*                    int *status )

*  Description:
*     This function copies lines of history text from one character array
*     to another. Each array consists of a contiguous block of fixed
*     length spaced-padded strings without terminating null characters.

*  Parameters:
*     nlines
*        Number of lines of text to copy.
*     clen
*        The length of each spaced-padded fixed-length string in text.
*     hist
*        Pointer to a character array in which to return the output history
*        lines. It has the same format as "text".
*     text
*        Pointer to a character array holding the input text lines to be copied.
*        it's length should be at least "nlines*clen". Each line of text should
*        be space-padded to occupy a length of "clen" characters. Note, there
*        should be no terminating null characters in this array.
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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local variables: */
   const char *p;
   int i;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure there are no terminating nulls in the supplied text. */
   p = text;
   for( i = 0; i < nlines; i++ ) {
      if( *(p++) == 0 ) {
         *status = NDF__FATIN;
         errRep( " ", "ndf1Hcpy: A null character was found inside a "
                 "fixed-length string (internal NDF programming error).",
                 status );
         errRepf( " ", "'%s'", status, text );
         break;
      }
   }

/* Copy the whole memory block. */
   if( *status == SAI__OK ) memcpy( hist, text, nlines*clen );

}

