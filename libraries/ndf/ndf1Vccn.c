#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Vccn( const char *ccomp, int *iccomp, int *status ){
/*
*+
*  Name:
*     ndf1Vccn

*  Purpose:
*     Validate NDF character component name.

*  Synopsis:
*     void ndf1Vccn( const char *ccomp, int *iccomp, int *status )

*  Description:
*     This function checks that the name of an NDF character component name
*     is valid (or is a valid abbreviation) and returns an integer
*     identifying the character component. If the name is not valid, then
*     an error is reported.

*  Parameters:
*     ccomp
*        Pointer to a null terminated string holding the component name to
*        be validated.
*     *iccomp
*        Returned holding the an identifier for the character component
*        (one of the symbolic constants NDF__LABEL, NDF__TITLE or
*        NDF__UNITS).
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

/* Compare the component name with each permitted value in turn,
   allowing abbreviations and assigning the appropriate returned value. */

/* ...LABEL component. */
   if( ndf1Simlr( ccomp, 1, 0, "LABEL", NDF__MINAB ) ) {
      *iccomp = NDF__LABEL;

/* ...TITLE component. */
   } else if( ndf1Simlr( ccomp, 1, 0, "TITLE", NDF__MINAB ) ) {
      *iccomp = NDF__TITLE;

/* ...UNITS component. */
   } else if( ndf1Simlr( ccomp, 1, 0, "UNITS", NDF__MINAB ) ) {
      *iccomp = NDF__UNITS;

/* If the component name was not recognised, then report an error. */
   } else {
      *status = NDF__CNMIN;
      msgSetc( "BADCCN", ccomp );
      errRep( " ", "Invalid character component name '^BADCCN' specified "
              "(possible programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vccn", status );

}

