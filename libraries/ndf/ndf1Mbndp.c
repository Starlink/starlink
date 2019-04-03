#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "prm_par.h"

void ndf1Mbndp( int n, int ndfs[], int *status ){
/*
*+
*  Name:
*     ndf1Mbndp

*  Purpose:
*     Match the bounds of a set of NDFs by padding.

*  Synopsis:
*     void ndf1Mbndp( int n, int ndfs[], int *status )

*  Description:
*     This function finds the pixel index bounds describing the smallest
*     region which contains all the pixels in the set of NDFs supplied and
*     "pads" each NDF so that its bounds match this region.  If necessary,
*     the function returns an identifier for an appropriate section from
*     each NDF in place of the identifier initially supplied, which is
*     annulled.

*  Parameters:
*     n
*        Number of NDF identifiers.
*     ndfs
*        Array of identifiers for the NDFs whose bounds are to be matched.
*        The supplied "ndfs" array should have at least "n" elements.
*     *status
*        The global status.

*  Notes:
*     -  The number of dimensions of each NDF section returned is set to
*     the maximum number of dimensions of all the NDFs supplied.
*     -  NDF sections are only returned if the NDF's bounds actually need
*     changing, otherwise the original NDF identifier is returned
*     unaltered.

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfACB *acbs;         /* Pointer to NDF section entry in the ACB */
   hdsdim lbnd[ NDF__MXDIM ];      /* Lower output bounds */
   hdsdim nlbnd[ NDF__MXDIM ];     /* Lower NDF bounds */
   hdsdim nubnd[ NDF__MXDIM ];     /* Upper NDF bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* Upper output bounds */
   int i;                /* Loop counter for dimensions */
   int indf;             /* Loop counter for NDFs */
   int ndim;             /* Number of output dimensions */
   int nndim;            /* Number of NDF dimensions */
   int sect;             /* Whether a section is needed */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise the output bounds and number of dimensions. */
   for( i = 0; i < NDF__MXDIM; i++ ){
      lbnd[ i ] = NUM__MAXI;
      ubnd[ i ] = NUM__MINI;
   }
   ndim = 1;

/* Loop to consider each NDF supplied. Import its identifier. */
   for( indf = 0; indf < n; indf++ ){
      ndf1Impid( ndfs[ indf ], &acb, status );
      if( *status == SAI__OK ) {

/* Obtain the NDF's bounds and number of dimensions from its data array
   component. (Note the bounds will be padded with 1's if necessary.) */
         aryBound( acb->did, NDF__MXDIM, nlbnd, nubnd, &nndim, status );
         if( *status == SAI__OK ) {

/* Accumulate the output bounds and number of dimensions. */
            for( i = 0; i < NDF__MXDIM; i++ ){
               lbnd[ i ] = NDF_MIN( lbnd[ i ], nlbnd[ i ] );
               ubnd[ i ] = NDF_MAX( ubnd[ i ], nubnd[ i ] );
            }
            ndim = NDF_MAX( ndim, nndim );
         }
      }

/* Quit considering NDFs if an error occurs. */
      if( *status != SAI__OK ) break;
   }

/* Consider each NDF in turn again. Import its identifier. */
   if( *status == SAI__OK ) {
      for( indf = 0; indf < n; indf++ ){
         ndf1Impid( ndfs[ indf ], &acb, status );
         if( *status == SAI__OK ) {

/* Obtain its bounds and number of dimensions. */
            aryBound( acb->did, NDF__MXDIM, nlbnd, nubnd, &nndim, status );
            if( *status == SAI__OK ) {

/* See if it is necessary to create a new section in order to match the
   bounds. First test if the number of dimensions needs changing. */
               if( nndim != ndim ) {
                  sect = 1;

/* Then test each pair of dimension bounds. */
               } else {
                  sect = 0;
                  for( i = 0; i < ndim; i++ ){
                     if( ( nlbnd[ i ] != lbnd[ i ] ) || ( nubnd[ i ] != ubnd[ i ] ) ) {
                        sect = 1;
                        break;
                     }
                  }
               }

/* If necessary, produce a suitable section. */
               if( sect ) {
                  ndf1Cut( acb, ndim, lbnd, ubnd, &acbs, status );

/* Annul the original ACB entry and issue an identifier for the new one
   describing the section. */
                  ndf1Anl( &acb, status );
                  ndfs[ indf ] = ndf1Expid( ( NdfObject * ) acbs, status );
               }
            }
         }

/* Quit considering NDFs if an error occurs. */
         if( *status != SAI__OK ) break;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Mbndp", status );

}

