#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"
#include <ctype.h>

void ary1Ccpy( const char *cin, size_t len, char *cout, int *status ) {
/*
*+
*  Name:
*     ary1Ccpy

*  Purpose:
*     Copy a character string, checking for truncation.

*  Synopsis:
*     void ary1Ccpy( const char *cin, size_t len, char *cout, int *status )

*  Description:
*     The routine copies a character string from one variable to
*     another and checks for truncation of trailing non-blank characters.
*     If such truncation occurs, then an error is reported and a "status"
*     value set.

*  Parameters:
*     cin
*        The input character string.
*     len
*        The length of the output array.
*     cout
*        The array to receive the copied string.
*     status
*        The global status.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local Variables: */
   char *p1;
   const char *p2;
   int nc;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Copy the string, stopping when the end of the input string is reached
   or the output buffer is filled (leaving room for a trailing null). */
   p1 = cout;
   p2 = cin;
   nc = 0;
   while( nc++ < len - 1 && *p2 ) {
      *(p1++) = *(p2++);
   }

/* Terminate the output string. */
   *p1 = 0;

/* If characters remain uncopied, then test to see if they are blank.
   Report an error if they are not. */
   while( *p2 ){
      if( !isspace( *(p2++) ) ) {
         *status = ARY__TRUNC;
         msgSetc( "STRING", cout );
         errRep( "ARY1_CCPY_STR", "Character string truncated: '^STRING'.",
                 status );
         errRep( " ", "Output character variable is too short to accommodate the"
                 "returned result (possible programming error).", status );
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Ccpy", status );

}
