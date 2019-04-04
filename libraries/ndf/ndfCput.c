#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfCput_( const char *value, int indf, const char *comp, int *status ){
/*
*+
*  Name:
*     ndfCput

*  Purpose:
*     Assign a value to an NDF character component.

*  Synopsis:
*     void ndfCput( const char *value, int indf, const char *comp, int *status )

*  Description:
*     This function assigns a value to the specified character component of
*     an NDF (i.e. to the LABEL, TITLE or UNITS component). Any previous
*     value is over-written.

*  Parameters:
*     value
*        Pointer to a null terminated string holding the value to be
*        assigned.
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the
*        character component whose value is to be assigned: "LABEL",
*        "TITLE" or "UNITS".
*     *status
*        The global status.

*  Notes:
*     -  The entire "value" string (including trailing blanks if present)
*     is assigned to the specified component, whose length is adjusted to
*     accommodate it.

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
   size_t l;             /* Length of character component */
   size_t lv;            /* Used length of VALUE */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Validate the component name. */
   ndf1Vccn( comp, &iccomp, status );

/* Check that WRITE access to the NDF is available. */
   ndf1Chacc( acb, "WRITE", status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that information about the required character component is
   available in the DCB. */
      ndf1Dc( dcb, iccomp, status );
      if( *status == SAI__OK ) {

/* Get the length of the "value" string. If it has zero length we will
   use a single space instead. */
         lv = strlen( value );

/* If the component is already present in the NDF, then determine its
   length. */
         if( dcb->cloc[ iccomp ] ) {
            datLen( dcb->cloc[ iccomp ], &l, status );

/* If the length does not match that of the value to be assigned, then
   annul the component's locator and erase the component. */
            if( l != NDF_MAX( lv, 1 ) ) {
               datAnnul( dcb->cloc + iccomp, status );
               datErase( dcb->loc, Ndf_DCB_ccn[ iccomp ], status );
            }
         }

/* If the component does not (now) exist, then create a new one with the
   required length. */
         if( *status == SAI__OK ) {
            if( !dcb->cloc[ iccomp ] ) {
               datNew0C( dcb->loc, Ndf_DCB_ccn[ iccomp ],
                         NDF_MAX( lv, 1 ), status );

/* Obtain a locator to the new component. */
               datFind( dcb->loc, Ndf_DCB_ccn[ iccomp ], dcb->cloc +
                        iccomp, status );
            }

/* Assign the value. */
            if( lv > 0 ) {
               datPut0C( dcb->cloc[ iccomp ], value, status );
            } else {
               datPut0C( dcb->cloc[ iccomp ], " ", status );
            }

         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      msgSetc( "C", comp );
      errRep( " ", "ndfCput: Error assigning a value to NDF character "
              "component '^C'.", status );
      ndf1Trace( "ndfCput", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}
