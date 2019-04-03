#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Aprp( NdfACB *acb1, int acpf, NdfDCB *dcb2, int *status ){
/*
*+
*  Name:
*     ndf1Aprp

*  Purpose:
*     Propagate NDF axis structure information from one NDF to another.

*  Synopsis:
*     void ndf1Aprp( NdfACB *acb1, int acpf, NdfDCB *dcb2, int *status )

*  Description:
*     This function propagates attributes and information from one NDF"s
*     axis structure to a new one which is being created.  If required,
*     only the attributes may be propagated, leaving the actual data values
*     behind.

*  Parameters:
*     acb1
*        Pointer to the input NDF entry in the ACB.
*     acpf
*        Whether to propagate the axis information (as opposed to simply
*        propagating its attributes).
*     dcb2
*        Pointer to the output NDF entry in the DCB. This NDF should not
*        contain an axis structure.
*     *status
*        The global status.

*  Implementation Deficiencies:
*     Propagation of the axis extension component is only handled in a
*     rudimentary way.

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
   HDSLoc *aloc = NULL;  /* Axis structure locator */
   NdfDCB *dcb1;         /* Pointer to input data object */
   hdsdim adim;          /* Axis structure dimension size */
   hdsdim cell;          /* Axis structure cell index */
   int iax;              /* Loop counter for axes */
   int ndim;             /* Number of NDF dimensions */
   int tstat;            /* Temporary status variable */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the input data object entry in the DCB. */
   dcb1 = acb1->dcb;

/* Determine the number of input NDF dimensions from its data array
   entry in the ACB. */
   aryNdim( acb1->did, &ndim, status );

/* Ensure that input axis structure information is available in the
   DCB. */
   ndf1Da( dcb1, status );
   if( *status == SAI__OK ) {

/* Set null initial DCB locator values for the output axis structure
   elements. */
      for( iax = 0; iax < NDF__MXDIM; iax++ ){
         dcb2->aloc[ iax ] = NULL;
      }

/* If the axis component is being propagated and an axis structure
   exists in the input NDF, then create a corresponding axis structure
   with an appropriate number of elements in the output NDF. */
      if( acpf && ( dcb1->aloc[ 0 ] ) ) {
         adim = ndim;
         datNew( dcb2->loc, "AXIS", "AXIS", 1, &adim, status );

/* Obtain a locator to the new structure. */
         datFind( dcb2->loc, "AXIS", &aloc, status );

/* Loop to obtain a locator to each structure element (i.e. cell) and
   store it in the DCB. */
         for( iax = 0; iax < ndim; iax++ ){
            cell = iax + 1;
            datCell( aloc, 1, &cell, dcb2->aloc + iax, status );
         }

/* Annul the locator to the whole axis structure. */
         datAnnul( &aloc, status );
      }

/* Propagate the axis data array information. */
      ndf1Adprp( acb1, acpf, dcb2, status );

/* If an error occurred then annul any axis structure locators which may
   have been acquired. */
      if( *status != SAI__OK ) {
         for( iax = 0; iax < ndim; iax++ ){
            datAnnul( dcb2->aloc + iax, status );
         }

/* Erase the axis structure, ignoring any errors. */
         errMark();
         tstat = SAI__OK;
         datErase( dcb2->loc, "AXIS", status );
         errAnnul( &tstat );
      }

/* Note if axis structure information in the new DCB entry is correct. */
      dcb2->ka = ( *status == SAI__OK );

/* Propagate the axis character component information. */
      ndf1Acprp( acb1, NDF__ALAB, acpf, dcb2, status );
      ndf1Acprp( acb1, NDF__AUNI, acpf, dcb2, status );

/* Propagate the variance and width array information. */
      ndf1Avprp( acb1, acpf, dcb2, status );
      ndf1Awprp( acb1, acpf, dcb2, status );

/* Loop to propagate the axis normalisation flag information for each
   input NDF axis.  Ensure that normalisation information is available
   for the input NDF. */
      if( *status == SAI__OK ) {
         for( iax = 0; iax < ndim; iax++ ){
            ndf1Dan( iax, dcb1, status );
            if( *status == SAI__OK ) {

/* Propagate the normalisation value. */
               dcb2->anrm[ iax ] = dcb1->anrm[ iax ];

/* If axis values are being propagated and an axis structure exists,
   then copy the NORMALISED component to the output structure. */
               if( acpf && ( dcb1->aloc[ iax ] ) ) {
                  ndf1Cpync( dcb1->aloc[ iax ], "NORMALISED",
                             dcb2->aloc[ iax ], status );
               }
            }

/* Note if the new information is correct. */
            dcb2->kan[ iax ] = ( *status == SAI__OK );
         }
      }

/* Propagate any MORE (extension) components present in the input axis
   structure. */
      if( *status == SAI__OK ) {
         for( iax = 0; iax < ndim; iax++ ){
            if( acpf && ( dcb1->aloc[ iax ] ) ) {
               ndf1Cpync( dcb1->aloc[ iax ], "MORE", dcb2->aloc[ iax ], status );
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Aprp", status );

}

