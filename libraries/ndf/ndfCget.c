#include <stdlib.h>
#include "sae_par.h"
#include "star/util.h"
#include "dat_par.h"
#include "ndf1.h"
#include "dat_err.h"
#include "ndf.h"
#include "mers.h"
#include "star/util.h"

void ndfCget_( int indf, const char *comp, char *value, size_t value_length,
              int *status ){
/*
*+
*  Name:
*     ndfCget

*  Purpose:
*     Obtain the value of an NDF character component.

*  Synopsis:
*     void ndfCget( int indf, const char *comp, char *value,
*                   size_t value_length, int *status )

*  Description:
*     This function obtains the value of the specified character component
*     of an NDF (i.e. the value of the LABEL, TITLE or UNITS component).

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the
*        character component whose value is required: "LABEL", "TITLE" or
*        "UNITS".
*     value
*        Pointer to a null terminated string holding the component's value.
*     value_length
*        The length of the supplied 'value' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  If the requested component is in an undefined state, then the
*     "value" parameter will be returned unchanged. A suitable default
*     should therefore be established before calling this function.
*     -  If the length of the "value" parameter is too short to accommodate
*     the returned result without losing significant (non-blank) trailing
*     characters, then this will be indicated by an appended ellipsis, i.e.
*     "...". No error will result.

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

/* Local Variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int iccomp;           /* Identifier for character component */
   size_t n;             /* Character position to start ellipses */

/* Check inherited global status. */
   if( *status != SAI__OK || !value ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Validate the character component name. */
   ndf1Vccn( comp, &iccomp, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that information for the required character component is
   available in the DCB. */
      ndf1Dc( dcb, iccomp, status );
      if( *status == SAI__OK ) {

/* If the component is present, then mark the error stack and read its
   value. */
         if( dcb->cloc[ iccomp ] ) {
            errMark();
            datGet0C( dcb->cloc[ iccomp ], value, value_length, status );

/* If character string truncation occurred, then annul the error and
   append ellipses to the returned value. */
            if( ( *status == DAT__CONER ) || ( *status == DAT__TRUNC ) ) {
               errAnnul( status );
               n = NDF_MAX( 1, value_length - 3 );
               star_strlcpy( value + n - 1, "...", value_length - n + 1 );
            }

/* Release the error stack. */
            errRlse();
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfCget: Error obtaining the value of an NDF character "
              "component.", status );
      ndf1Trace( "ndfCget", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}
