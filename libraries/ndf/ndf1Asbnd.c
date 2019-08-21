#include "star/hds.h"
#include "star/cmp.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Asbnd( int ndim, const hdsdim lbnd[], const hdsdim ubnd[],
                NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Asbnd

*  Purpose:
*     Set new pixel-index bounds for an NDF's axis component.

*  Synopsis:
*     void ndf1Asbnd( int ndim, const hdsdim lbnd[], const hdsdim ubnd[],
*                     NdfACB *acb, int *status )

*  Description:
*     This function sets new pixel index bounds for an NDF's axis
*     component, including possible changes in the number of NDF
*     dimensions. Existing values held in axis arrays are retained or
*     extrapolated, as appropriate.

*  Parameters:
*     ndim
*        New number of NDF dimensions.
*     lbnd
*        New lower pixel-index bounds. The supplied "lbnd" array should
*        have at least "ndim" elements.
*     ubnd
*        New upper pixel-index bounds. The supplied "ubnd" array should
*        have at least "ndim" elements.
*     acb
*        Pointer to the NDF entry in the ACB.
*     *status
*        The global status.

*  Notes:
*     -  This function can only be used to change the bounds of a base NDF.
*     It will return without action if an NDF section is supplied.
*     -  The NDF's axis structure need not exist.
*     -  This function should be invoked prior to changing the bounds of
*     the NDF's main data array, upon whose shape the process of
*     extrapolating axis arrays depends.

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
   HDSLoc *aloc = NULL;  /* Axis structure locator */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   hdsdim cell;          /* Axis structure cell index */
   hdsdim newsiz;        /* New axis structure size */
   int iax;              /* Loop counter for axes/dimensions */
   int ndimi;            /* Initial number of dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check whether the NDF is a section. There is nothing to do if it is. */
   if( !acb->cut ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Determine the initial number of dimensions of the NDF data object
   from the ARY_ system identifier for its main data array, held in the
   DCB. */
      aryNdim( dcb->did, &ndimi, status );
      if( *status == SAI__OK ) {

/* Loop through all the NDF dimensions which are being retained. */
         for( iax = 0; iax < NDF_MIN( ndimi, ndim ); iax++ ){

/* Set new pixel-index bounds for the axis width, variance and data
   arrays. */
            ndf1Awsbn( lbnd[ iax ], ubnd[ iax ], iax, acb, status );
            ndf1Avsbn( lbnd[ iax ], ubnd[ iax ], iax, acb, status );
            ndf1Adsbn( lbnd[ iax ], ubnd[ iax ], iax, acb, status );
         }

/* Loop through existing NDF dimensions which are not being retained
   (if any). */
         for( iax = ndim; iax < ndimi; iax++ ){

/* Reset the axis character components and the data, variance and width
   arrays. */
            ndf1Acrst( iax, NDF__ALAB, acb, status );
            ndf1Acrst( iax, NDF__AUNI, acb, status );
            ndf1Adrst( iax, acb, status );
            ndf1Avrst( iax, acb, status );
            ndf1Awrst( iax, acb, status );

/* If axis extension information is available and the extension locator
   is valid, then annul it. */
            if( dcb->kax[ iax ] ) {
               if( dcb->axloc[ iax ] ) datAnnul( dcb->axloc + iax, status );
            }

/* Ensure that axis normalisation information is available (to be
   retained in the DCB in case the axis is re-created). */
            ndf1Dan( iax, dcb, status );

/* If an axis structure exists, then empty the appropriate element of
   all remaining components so that the structure may be contracted to
   match the new number of NDF dimensions. Annul the DCB locator for
   the structure element. */
            if( dcb->aloc[ iax ] ) {
               ndf1Hrst( dcb->aloc[ iax ], status );
               datAnnul( dcb->aloc + iax, status );
            }
         }

/* Loop to process new NDF dimensions not initially present (if any). */
         if( *status == SAI__OK ) {
            for( iax = ndimi; iax < ndim; iax++ ){

/* Ensure that DCB information is available for all the new axis
   arrays.  These arrays do not yet exist, but this establishes their
   default attributes. */
               ndf1Dad( iax, dcb, status );
               ndf1Dav( iax, dcb, status );
               ndf1Daw( iax, dcb, status );

/* Convert the default axis array storage forms to take account of the
   new NDF bounds, if necessary. */
               ndf1Cbfrm( 1, lbnd + iax, ubnd + iax, dcb->adfrm[ iax ],
                          sizeof( dcb->adfrm[ iax ] ), status );
               ndf1Cbfrm( 1, lbnd + iax, ubnd + iax, dcb->adfrm[ iax ],
                          sizeof( dcb->adfrm[ iax ] ), status );
               ndf1Cbfrm( 1, lbnd + iax, ubnd + iax, dcb->awfrm[ iax ],
                          sizeof( dcb->awfrm[ iax ] ), status );

/* Ensure that an axis normalisation flag value is available for each
   new dimension. */
               ndf1Dan( iax, dcb, status );
            }

/* If the new number of dimensions differs from the old number and an
   axis structure exits, then obtain a locator to the axis structure
   array and alter its size appropriately. */
            if( *status == SAI__OK ) {
               if( ( ndim != ndimi ) && ( dcb->aloc[ 0 ] ) ) {
                  datFind( dcb->loc, "AXIS", &aloc, status );
                  newsiz = ndim;
                  datAlter( aloc, 1, &newsiz, status );

/* Loop to initialise any new NDF dimensions. */
                  for( iax = ndimi; iax < ndim; iax++ ){

/* Obtain a DCB locator for the axis structure element of each new
   dimension. */
                     cell = iax + 1;
                     datCell( aloc, 1, &cell, dcb->aloc + iax, status );

/* Use "hdsTune" to set the optimum number of components in the HDS
   structure and create and initialise a new axis data array. */
                     hdsTune( "NCOMP", 8, status );
                     ndf1Adcre( lbnd[ iax ], ubnd[ iax ], iax, dcb, status );

/* If the axis normalisation flag value is non-zero, then create a new
   NORMALISED component and set its value. */
                     if( dcb->anrm[ iax ] ) {
                        datNew0L( dcb->aloc[ iax ], "NORMALISED", status );
                        cmpPut0L( dcb->aloc[ iax ], "NORMALISED", 1, status );
                     }
                  }

/* Annul the locator to the axis structure. */
                  datAnnul( &aloc, status );
               }
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Asbnd", status );

}

