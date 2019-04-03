#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ary.h"
#include "mers.h"
#include "star/cmp.h"

void ndf1Acre( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Acre

*  Purpose:
*     Ensure that an NDF axis component exists, creating one if necessary.

*  Synopsis:
*     void ndf1Acre( NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that an axis component exists for an NDF data
*     object with an entry in the DCB. If the associated data object does
*     not currently have an axis component, then one is created and filled
*     with axis data values describing the default axis coordinate system.

*  Parameters:
*     dcb
*        Pointer to the data object entry in the DCB.
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
   HDSLoc *aloc = NULL;  /* Axis structure locator */
   hdsdim cell;          /* Axis structure cell subscript */
   hdsdim dima;          /* Axis structure dimension size */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper bounds */
   int iax;              /* Loop counter for axes */
   int ndim;             /* Number of NDF dimensions */
   int tstat;            /* Temporary status variable */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that axis structure information is available in the DCB. */
   ndf1Da( dcb, status );
   if( *status == SAI__OK ) {

/* See if the first axis structure element locator is valid. If so,
   then an axis structure already exists, so there is nothing to do. */
      if( !dcb->aloc[ 0 ] ) {

/* Obtain the NDF bounds and number of dimensions from the ARY_ system
   identifier for the data array held in the DCB. */
         aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );
         if( *status == SAI__OK ) {

/* Loop to ensure that DCB information is available for each NDF axis
   data array. These arrays do not exist yet, but this process
   establishes their default attributes in the DCB (which are then used
   by ndf1Adcre when creating the arrays). */
            for( iax = 0; iax < ndim; iax++ ){
               ndf1Dad( iax, dcb, status );

/* Ensure that an axis normalisation flag value is available for each
   NDF dimension. */
               ndf1Dan( iax, dcb, status );
            }

/* Create a 1-dimensional axis structure in the NDF with the necessary
   number of elements. */
            dima = ndim;
            datNew( dcb->loc, "AXIS", "AXIS", 1, &dima, status );

/* Obtain a locator to the axis structure. */
            datFind( dcb->loc, "AXIS", &aloc, status );

/* Loop to create an data array for each axis.  Obtain a locator to
   each axis structure element (cell) and store this in the DCB. */
            for( iax = 0; iax < ndim; iax++ ){
               cell = iax + 1;
               datCell( aloc, 1, &cell, dcb->aloc + iax, status );

/* Use "hdsTune" to set the optimum number of components in the HDS
   structure and create and initialise an axis data array within the
   structure element. */
               hdsTune( "NCOMP", 8, status );
               ndf1Adcre( lbnd[ iax ], ubnd[ iax ], iax, dcb, status );

/* If the axis normalisation flag value is non-zero, then create a new
   NORMALISED component and set its value. */
               if( dcb->anrm[ iax ] ) {
                  datNew0L( dcb->aloc[ iax ], "NORMALISED", status );
                  cmpPut0L( dcb->aloc[ iax ], "NORMALISED", 1, status );
               }
            }

/* Annul the locator to the entire axis structure. */
            datAnnul( &aloc, status );

/* If an error occurred, then annul any ARY_ system identifiers and
   locators which may have been acquired. */
            if( *status != SAI__OK ) {
               for( iax = 0; iax < ndim; iax++ ){
                  aryAnnul( dcb->adid + iax, status );
                  datAnnul( dcb->aloc + iax, status );
               }

/* Erase any axis structure which may have been created, ignoring any
   error which may occur. */
               errMark();
               tstat = SAI__OK;
               datErase( dcb->loc, "AXIS", &tstat );
               errAnnul( &tstat );
               errRlse();
            }

/* Note if the axis structure and axis data array information in the DCB
   is valid. */
            dcb->ka = ( *status == SAI__OK );
            for( iax = 0; iax < ndim; iax++ ){
               dcb->kad[ iax ] = ( *status == SAI__OK );
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Acre", status );

}

