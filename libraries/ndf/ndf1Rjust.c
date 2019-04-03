#include "ndf1.h"
#include <string.h>

void ndf1Rjust( char *string, int string_length ){
/*
*+
*  Name:
*     ndf1Rjust

*  Purpose:
*     Right justify a string.

*  Synopsis:
*     void ndf1Rjust( char *string, int string_length )

*  Description:
*     This function right justifies a string by filling out the spaces
*     between words with additional blank space. The right margin is
*     taken as the length of the character variable as supplied in
*     "string_length". Leading blanks are preserved.

*  Parameters:
*     string
*        Pointer to a null terminated string holding the string to be right
*        justified and returned.
*     string_length
*        The length of the supplied 'string' array. This should include
*        room for the terminating null.

*  Copyright:
*     Copyright ("c") 2018 East Asian Observatory
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
   char c;               /* Single character */
   int gap;              /* In an inter-word gap? */
   int ncomp;            /* No. characters in compressed string */
   int ngap;             /* No. inter-word gaps */
   int start;            /* At start of (non-blank) string yet? */
   size_t f;             /* Position of first non-blank character */
   size_t i;             /* Loop counter for character positions */
   size_t iout;          /* New position to move character to */
   size_t ipad;          /* INdex of next blank */
   size_t nblank;        /* No. blanks required in line */
   size_t nextra;        /* No. gaps needing an extra 1 blank */
   size_t pad;           /* Min. blanks needed between each word */
   size_t slen;          /* String length */

/* Initialise. */
   gap = 0;
   start = 0;
   iout = -1;
   ngap = 0;

/* Loop to eliminate all multiple embedded blanks from the string. */
   slen = strlen( string );
   for( i = 0; i < slen; i++ ) {

/* Get the next character from the string. */
      c = string[ i ];

/* If the character is not blank or we have not yet reached the start of
   non-blank text, then increment the output character count and move
   the character to its new position. */
      if( c != ' ' || !start ) {
         iout++;
         string[ iout ] = c;

/* Note we are not in an inter-word gap. */
         gap = 0;

/* Note where the start of non-blank text begins. */
         if( c != ' ' && !start ) {
            f = i + 1;
            start = 1;
         }

/* If the character is blank (after a previous non-blank character has
   been encountered) and we are not already in an inter-word gap, then
   increment the output character count and insert a single blank at
   that position. */
      } else if( !gap ) {
         iout++;
         string[ iout ] = ' ';

/* Note we are now in an inter-word gap and count it. */
         gap = 1;
         ngap++;
      }
   }

/* Find the length of the compressed string, eliminating any final
   blank/gap. */
   ncomp = iout;
   if( gap ) {
      ncomp--;
      ngap--;
   }

/* If the compressed string has inter-word gaps, then determine the
   minimum number of spaces to insert into each gap to right justify it. */
   if( ngap > 0 ) {
      nblank = string_length - ncomp + ngap - 1;
      pad = nblank/ngap;

/* Determine how many gaps must also receive an additional blank to make
   up the total number required. */
      nextra = nblank - pad*ngap;

/* Loop through the compressed output string in reverse to insert the
   padding. Omit any leading blanks from this process. */
      ngap = 0;
      iout = string_length;
      for( i = ncomp - 1; i > f; i-- ){

/* Extract each character. If it is blank, then count an inter-word gap
   and copy "pad" blanks into the output string in its place. */
         c = string[ i ];
         if( c == ' ' ) {
            ngap++;
            for( ipad = 0; ipad < pad; ipad++ ) {
               iout--;
               string[ iout ] = ' ';
            }

/* The last "nextra" gaps (the first ones encountered going in reverse)
   are padded with an extra blank to make up the total required. */
            if( ngap <= nextra ) {
               iout--;
               string[ iout ] = ' ';
            }

/* Retain all non-blank characters unchanged. */
         } else {
            iout--;
         }
      }
   }
}

