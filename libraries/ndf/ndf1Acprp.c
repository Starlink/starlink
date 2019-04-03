#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Acprp( NdfACB *acb1, int iccomp, int accpf, NdfDCB *dcb2, int *status ){
/*
*+
*  Name:
*     ndf1Acprp

*  Purpose:
*     Propagate axis character information from one NDF to another.

*  Synopsis:
*     void ndf1Acprp( NdfACB *acb1, int iccomp, int accpf, NdfDCB *dcb2,
*                     int *status )

*  Description:
*     This function propagates axis character information from an existing
*     NDF to a new one which is being created. Propagation is controlled by
*     a logical flag.

*  Parameters:
*     acb1
*        Pointer to the input NDF entry in the ACB.
*     iccomp
*        The axis character component to be propagated (one of the symbolic
*        values NDF__ALAB or NDF__AUNI, as defined in the include file
*        "ndf1.h").
*     accpf
*        Whether the component is to be propagated. The function takes no
*        action if this value is zero.
*     dcb2
*        Pointer to the output NDF entry in the DCB.
*     *status
*        The global status.

*  Prior Requirements:
*     -  If axis character information is being propagated, then the output
*     NDF should contain an axis structure to receive this information.
*     Axis character information should not be present in this output axis
*     structure beforehand.

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
   NdfDCB *dcb1;         /* Pointer to input data object in the DCB */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower bounds (junk) */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper bounds (junk) */
   int iax;              /* Loop counter for axes */
   int ndim;             /* Number of NDF dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the axis component is being propagated, then obtain an index to
   the input data object entry in the DCB. */
   if( accpf ) {
      dcb1 = acb1->dcb;

/* Determine the number of NDF dimensions from the ARY_ system
   identifier for the main data array, held in the ACB. */
      aryBound( acb1->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );
      if( *status == SAI__OK ) {

/* Loop to process each NDF dimension. */
         for( iax = 0; iax < ndim; iax++ ){

/* Ensure that axis character component information is available in the
   input DCB entry. */
            ndf1Dac( iax, iccomp, dcb1, status );
            if( *status == SAI__OK ) {

/* If the input character component exists, then copy it to the
   appropriate output axis structure element. */
               if( dcb1->acloc[ iax ][ iccomp ] ) {
                  datCopy( dcb1->acloc[ iax ][ iccomp ], dcb2->aloc[ iax ],
                           Ndf_DCB_accn[ iccomp ], status );
               }
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Acprp", status );

}

