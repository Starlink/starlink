#include <string.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Ccpy( const char *cin, char *cout, size_t cout_length, int *status ){
/*
*+
*  Name:
*     ndf1Ccpy

*  Purpose:
*     Copy a character string, checking for truncation.

*  Synopsis:
*     void ndf1Ccpy( const char *cin, char *cout, size_t cout_length,
*                    int *status )

*  Description:
*     This function copies a character string from one variable to another
*     and checking for truncation of trailing non-blank characters. If such
*     truncation occurs, then an error is reported and a "status" value set.

*  Parameters:
*     cin
*        Pointer to a null terminated string holding the input character
*        string.
*     cout
*        Pointer to an array in which to return a null terminated copy of
*        the input string.
*     cout_length
*        The length of the supplied 'cout' array. This should include
*        room for the terminating null.
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
   size_t nc;         /* Used length of input string */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get the used length of the input string. */
   nc = astChrLen( cin );

/* If the output is too short to hold it all, just copy the section that
   will fit in the output string, and report an error. */
   if( cout_length <= nc ) {
      memcpy( cout, cin, cout_length - 1 );
      cout[ cout_length - 1 ] = 0;

      *status = NDF__TRUNC;
      msgSetc( "STRING", cout );
      errRep( " ", "Character string truncated: '^STRING'.", status );
      errRep( " ", "Output character variable is too short to "
              "accommodate the returned result (possible programming "
              "error).", status );

/* Otherwise, copy the used section of the input string. */
   } else {
      memcpy( cout, cin, nc );
      cout[ nc ] = 0;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Ccpy", status );

}

