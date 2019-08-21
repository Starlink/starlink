#include <string.h>
#include "ndf1.h"

void ndf1Cmpbl( int lead, char *string ){
/*
*+
*  Name:
*     ndf1Cmpbl

*  Purpose:
*     Compress multiple blanks in a character string.

*  Synopsis:
*     void ndf1Cmpbl( int lead, char *string )

*  Description:
*     This function replaces occurrences of multiple blanks in a string
*     with single blanks, shifting the non-blank characters to the left
*     as a result and terminating the string after the last non-space.

*  Parameters:
*     lead
*        A zero value indicates that compression of multiple blanks
*        should be restricted to embedded blanks only (leading blanks
*        being left unchanged). A non-zero value indicates that the
*        process should be applied to leading blanks as well.
*     string
*        The null terminated string to be processed.

*  Copyright:
*      Copyright (C) 2018 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     14-MAY-2018 (DSB):
*        Original C version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   char c;         /* Single input character */
   int gap;        /* In an inter-word gap? */
   int start;      /* Start of non-blank text encountered? */
   size_t i;       /* Loop counter for input characters */
   size_t iout;    /* Output character counter */
   size_t slen;    /* Length of string */

/* Initialise. */
   iout = -1;
   gap = 0;

/* Leading blanks are only handled differently if "lead" is non-zero. */
   start = lead;

/* Loop to process all the characters in the supplied string. */
   slen = strlen( string );
   for( i = 0; i < slen; i++ ) {
      c = string[ i ];

/* If the next character is not blank or we have not yet reached the
   start of non-blank text, then increment the output character count
   and move the character to its new position. */
      if( ( c != ' ' ) || !start ) {
         iout++;
         string[ iout ] = c;

/* Note we are not in an inter-word gap. */
         gap = 0;

/*  Note when the start of non-blank text is reached. */
         if( c != ' ' ) start = 1;

/* If the character is blank (after a previous non-blank character has
   been encountered) and we are not already in an inter-word gap, then
   increment the output character count and insert a single blank at
   that position. */
      } else if ( !gap ) {
         iout++;
         string[ iout ] = ' ';

/* Note we are now in an inter-word gap. */
         gap = 1;
      }
   }

/* Terminate the string. */
   string[ iout+1 ] = 0;
}
