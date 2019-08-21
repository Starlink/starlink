#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Chhum( const char *hmode, int *hum, int *status ){
/*
*+
*  Name:
*     ndf1Chhum

*  Purpose:
*     Validate an NDF history update mode string.

*  Synopsis:
*     void ndf1Chhum( const char *hmode, int *hum, int *status )

*  Description:
*     This function checks an NDF history update mode string for validity,
*     allowing abbreviation, and returns the equivalent integer code. An
*     error is reported and "status" set if the string supplied is not
*     valid.

*  Parameters:
*     hmode
*        Pointer to a null terminated string holding the string to be
*        validated. Valid values are: "DISABLED", "QUIET", "NORMAL" or
*        "VERBOSE" (case insensitive). Abbreviation to no less than
*        NDF__MINAB characters are allowed.
*     *hum
*        Returned holding the corresponding history update mode code: one
*        of NDF__HDISA, NDF__HQUIE, NDF__HNORM or NDF__HVERB, as defined in
*        the include file "ndf1.h".
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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check the update mode string against each valid value in turn,
   allowing abbreviation. Assign the appropriate history update mode
   code. */
   if( ndf1Simlr( hmode, 1, 0, "DISABLED", NDF__MINAB ) ) {
      *hum = NDF__HDISA;
   } else if( ndf1Simlr( hmode, 1, 0, "QUIET", NDF__MINAB ) ) {
      *hum = NDF__HQUIE;
   } else if( ndf1Simlr( hmode, 1, 0, "NORMAL", NDF__MINAB ) ) {
      *hum = NDF__HNORM;
   } else if( ndf1Simlr( hmode, 1, 0, "VERBOSE", NDF__MINAB ) ) {
      *hum = NDF__HVERB;

/* If the string is not recognised, then report an error. */
   } else {
      *status = NDF__HUMIN;
      msgSetc( "HMODE", hmode );
      errRep( " ", "Invalid history update mode string '^HMODE' specified "
              "(possible programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Chhum", status );

}

