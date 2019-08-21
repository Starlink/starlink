#include <stdlib.h>
#include "sae_par.h"
#include "par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "par_err.h"
#include "ndf_ast.h"
#include "ndf.h"
#include "mers.h"
#include "star/util.h"

void ndfCinp_( const char *param, int indf, const char *comp, int *status ){
/*
*+
*  Name:
*     ndfCinp

*  Purpose:
*     Obtain an NDF character component value via the ADAM parameter
*     system.

*  Synopsis:
*     void ndfCinp( const char *param, int indf, const char *comp, int *status )

*  Description:
*     This function obtains a new value for a character component of an NDF
*     via the ADAM parameter system and uses it to replace any pre-existing
*     value of that component in the NDF.

*  Parameters:
*     param
*        Pointer to a null terminated string holding the name of the ADAM
*        parameter.
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the
*        character component for which a value is to be obtained: "LABEL",
*        "TITLE" or "UNITS".
*     *status
*        The global status.

*  Notes:
*     -  A "null" parameter value is interpreted as indicating that no new
*     value should be set for the character component. In this event, the
*     function will return without action (and without setting a "status"
*     value). A suitable default value for the character component should
*     therefore be established before this function is called.

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
   char value[ NDF__SZPAR + 1 ];   /* Value to be assigned */
   int iccomp;           /* Identifier for character component */
   int tstat;            /* Temporary status variable */
   size_t l;             /* Length of character component */
   size_t lval;          /* Length of value to be assigned */

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

/* Mark the error stack and obtain a character value for the NDF
   component via the ADAM parameter system. */
      errMark();
      star_strlcpy( value, " ", sizeof( value ) );
      parGet0c( param, value, sizeof( value ), status );

/* If a "null" value is obtained, then annul the error and take no
   further action. */
      if( *status == PAR__NULL ) {
         errAnnul( status );

/* If an "abort" was requested, then annul any error message and issue
   an appropriate new one. */
      } else if( *status == PAR__ABORT ) {
         tstat = *status;
         errAnnul( &tstat );
         msgSetc( "PARAM", param );
         errRep( " ", "Aborted attempt to obtain an NDF character "
                 "component value via the '%^PARAM' parameter.", status );

/* If a suitable value was obtained, then get an index to the data
   object entry in the DCB. */
      } else {
         dcb = acb->dcb;

/* Ensure that information about the required character component is
   available in the DCB. */
         ndf1Dc( dcb, iccomp, status );
         if( *status == SAI__OK ) {

/* Obtain the length in characters of the value to be assigned. */
            lval = NDF_MAX( 1, astChrLen( value ) );

/* If the component is already present in the NDF, then determine its
   length. */
            if( dcb->cloc[ iccomp ] ) {
               datLen( dcb->cloc[ iccomp ], &l, status );

/* If the length does not match that of the value to be assigned, then
   annul the component's locator and erase the component. */
               if( l != lval ) {
                  datAnnul( dcb->cloc + iccomp, status );
                  datErase( dcb->loc, Ndf_DCB_ccn[ iccomp ], status );
               }
            }

/* If the component does not (now) exist, then create a new one with the
   required length. */
            if( *status == SAI__OK ) {
               if( !dcb->cloc[ iccomp ] ) {
                  datNew0C( dcb->loc, Ndf_DCB_ccn[ iccomp ], lval, status );

/* Obtain a locator to the new component. */
                  datFind( dcb->loc, Ndf_DCB_ccn[ iccomp ],
                           dcb->cloc + iccomp, status );
               }

/* Assign the value. */
               datPut0C( dcb->cloc[ iccomp ], value, status );
            }
         }
      }

/* Release the error stack. */
      errRlse();
   }

/* If an error occurred and it was not an "abort" request, then report
   context information and call the error tracing function. */
   if( ( *status != SAI__OK ) && ( *status != PAR__ABORT ) ) {
      msgSetc( "PARAM", param );
      errRep( " ", "ndfCinp: Error obtaining an NDF character component "
              "value via the '%^PARAM' parameter.", status );
      ndf1Trace( "ndfCinp", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}
