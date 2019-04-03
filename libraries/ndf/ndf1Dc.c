#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

void ndf1Dc( NdfDCB *dcb, int iccomp, int *status ){
/*
*+
*  Name:
*     ndf1Dc

*  Purpose:
*     Ensure that character component information is available in the DCB.

*  Synopsis:
*     void ndf1Dc( NdfDCB *dcb, int iccomp, int *status )

*  Description:
*     This function ensures that information about the specified NDF
*     character component is available in the DCB. It does nothing if this
*     information is already available. Otherwise, it inspects the actual
*     data object to obtain this information and enters it into the DCB,
*     performing necessary validation checks in the process.

*  Parameters:
*     dcb
*        Pointer to the DCB entry for which information is required.
*     iccomp
*        Identifier for the character component for which information is
*        required; one of the symbolic constants NDF__LABEL, NDF__TITLE or
*        NDF__UNITS.
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
   char type[ DAT__SZTYP + 1 ];    /* Component data type */
   hdsbool_t there;                /* Whether component is present in NDF */
   hdsdim dim[ DAT__MXDIM ];       /* Component dimensions */
   int ndim;                       /* Number of component dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check whether the required information is already available. There is
   nothing to do if it is. */
   if( !dcb->kc[ iccomp ] ) {

/* If not, then initialise the DCB character component locator and see
   if the component is present in the data object. */
      dcb->cloc[ iccomp ] = NULL;
      datThere( dcb->loc, Ndf_DCB_ccn[ iccomp ], &there, status );
      if( *status == SAI__OK ) {

/* If present, then obtain a locator to it, storing this in the DCB. */
         if( there ) {
            datFind( dcb->loc, Ndf_DCB_ccn[ iccomp ], dcb->cloc + iccomp,
                     status );

/* Determine the component's data type and shape. */
            datType( dcb->cloc[ iccomp ], type, status );
            datShape( dcb->cloc[ iccomp ], DAT__MXDIM, dim, &ndim, status );
            if( *status == SAI__OK ) {

/* Report an error if it does not have a character data type. */
               if( strncmp( type, "_CHAR*", 6 ) ) {
                  *status = NDF__TYPIN;
                  msgSetc( "CCOMP", Ndf_DCB_ccn[ iccomp ] );
                  ndf1Dmsg( "NDF", dcb );
                  msgSetc( "BADTYPE", type );
                  errRep( " ", "The ^CCOMP component in the NDF structure "
                          "^NDF has an invalid HDS type of '^BADTYPE'; it "
                          "should be of type '_CHAR'.", status );

/* Report an error if it is not scalar. */
               } else if( ndim != 0 ) {
                  *status = NDF__NDMIN;
                  msgSetc( "CCOMP", Ndf_DCB_ccn[ iccomp ] );
                  ndf1Dmsg( "NDF", dcb );
                  msgSeti( "BADNDIM", ndim );
                  errRep( " ", "The ^CCOMP component in the NDF structure "
                          "^NDF is ^BADNDIM-dimensional; it should be "
                          "scalar.", status );
               }
            }

/* If the component is not suitable, then annul its DCB locator. */
            if( *status != SAI__OK ) datAnnul( dcb->cloc + iccomp, status );
         }
      }

/* Note whether DCB information is now available for this character
   component. */
      dcb->kc[ iccomp ] = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dc", status );

}

