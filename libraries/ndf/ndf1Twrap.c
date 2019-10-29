#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include "star/util.h"
#include "ndf_ast.h"

void ndf1Twrap( const char *in, int indent, size_t *fp, size_t l,
                char *out, size_t out_length ){
/*
*+
*  Name:
*     ndf1Twrap

*  Purpose:
*     Perform line-breaking on a stream of text.

*  Synopsis:
*     void ndf1Twrap( const char *in, int indent, size_t *fp, size_t l,
*                     char *out, size_t out_length )

*  Description:
*     This function splits a stream of input text into separate output
*     lines, performing line-breaking at suitable blanks if possible.
*
*     It should be called repeatedly. On each invocation, it starts
*     inspecting the input text stream (in parameter "in") at the character
*     position identified by the "fp" parameter. It then forms the longest
*     possible output line from the characters which follow, subject to not
*     exceeding the line length (parameter "l") or overflowing the output
*     buffer (parameter "out"). Lines are broken at a blank if possible
*     (otherwise at the last character which will fit into the output line).
*     Any unfilled spaces at the end of the line are filled with spaces.
*
*     On return, "fp" is advanced to point beyond the last input character
*     transferred to the output line, ready for the next invocation. If no
*     further non-blank input characters remain to be processed, then "fp"
*     is returned set to zero.

*  Parameters:
*     in
*        Pointer to a null terminated string holding the input text stream.
*     indent
*        The number of leading spaces to include in the output buffer. It
*        can be set negative to indicate that any spaces in the input
*        should be preserved.
*     *fp
*        The formatting pointer. On entry *fp should be the zero based
*        index of the next input character to be considered. On exit, it
*        is advanced to be the index of the first character to be considered
*        on the next invocation. A value of UINT_MAX is returned if the input
*        text is exhausted (so that no further invocations are needed).
*     l
*        The length of an output line.
*     out
*        Pointer to an array in which to return a string holding the variable
*        to receive the output lines. Note, no terminating null characters
*        are added - each output line is space padded to length "l".
*     out_length
*        The length of the supplied 'out' array.

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
*     29-OCT-2019 (DSB):
*        Re-written with a simpler algorithm.

*-
*/

/* Local Variables: */
   char *outend;
   char *pout;
   char *spout;
   const char *pin;
   const char *spin;
   const char *inend;

/* Get a pointer to the first input character to be copied, skipping over
   leading spaces if "indent" is not negative. */
   pin = in + *fp;
   if( indent >= 0 ) {
      while( *pin == ' ' ) pin++;
   }

/* Get a pointer to the last input character to be copied. This excludes
   any trailing spaces and the trailing null terminator. */
   inend = pin + astChrLen( pin ) - 1;

/* Place the required number of leading spaces into the output buffer. */
   pout = out;
   while( indent-- > 0 ) *(pout++) = ' ';

/* Get a pointer to the last output character to be filled. */
   if( l < out_length ) {
      outend = out + l - 1;
   } else {
      outend = out + out_length - 1;
   }

/* Copy input characters to the output until the end of either the input
   or output is reached. Record the positions within the input and output
   of the most recent space. */
   spin = NULL;
   spout = NULL;
   while( pout <= outend && pin <= inend ) {
      if( *pin == ' ' ) {
         spin = pin;
         spout = pout;
      }
      *(pout++) = *(pin++);
   }

/* If we have copied all the input, indicate this by returning UINT_MAX
   for *fp. */
   if( pin > inend ) {
      *fp = UINT_MAX;

/* If we have not yet copied all the input and a space was encountered, return
   *fp to indicate that the first character following the most recent space
   should be copied next. Replace any output text following the final
   space with more spaces. */
   } else if( spin ) {
      *fp = spin - in + 1;
      memset( spout, ' ', pout - spout );

/* If we have not yet copied all the input but no space was encountered, return
   *fp to indicate that the next input character should be copied next. */
   } else {
      *fp = pin - in;
   }
}

