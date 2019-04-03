#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Chxnm( const char *xname, size_t start, size_t end, int *status ){
/*
*+
*  Name:
*     ndf1Chxnm

*  Purpose:
*     Check an NDF extension name.

*  Synopsis:
*     void ndf1Chxnm( const char *xname, size_t start, size_t end, int *status )

*  Description:
*     This function checks the name of an NDF extension for standard form.
*     A standard name must be no more than NDF__SZXNM characters long, must
*     begin with an alphabetic character and continue with alphanumeric
*     characters (including underscore) only. If this test fails, then an
*     error is reported and a "status" value set. Otherwise, the function
*     returns without action.

*  Parameters:
*     xname
*        Pointer to a null terminated string holding the extension name to
*        be checked.
*     start
*        The zero-based index of the first character to consider in "xname".
*        The whole string is used if "start" > "end".
*     end
*        The zero-based index of the last character to consider in "xname".
*        The whole string is used if "start" > "end".
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
   size_t f;          /* Index of first character to use */
   size_t l;          /* Index of last character to use */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Find the indices of the first and last non-space character in the
   section of xname that is to be used. */
   astFandl( xname, start, end, &f, &l );

/* Report an error if there is nothing to check. */
   if( f > l ) {
      *status = NDF__NSXNM;
      errRep( " ", "Blank extension name specified "
              "(possible programming error).", status );

/* If the extension name is too long, or does not have the correct
   standard form, then report an error. */
   } else if( l - f + 1 > NDF__SZXNM || !ndf1Isnam( xname, f, l ) ) {
      *status = NDF__NSXNM;
      msgSetc( "XNAME", xname );
      errRep( " ", "Non-standard extension name '^XNAME' specified "
              "(possible programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Chxnm", status );

}

