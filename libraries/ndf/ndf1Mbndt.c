#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "prm_par.h"
#include "mers.h"

void ndf1Mbndt( int n, int ndfs[], int *status ){
/*
*+
*  Name:
*     ndf1Mbndt

*  Purpose:
*     Match the bounds of a set of NDFs by trimming.

*  Synopsis:
*     void ndf1Mbndt( int n, int ndfs[], int *status )

*  Description:
*     This function finds the pixel index bounds of the region which a set
*     of NDFs have in common (their intersection region) and "trims" them
*     so that their bounds match this region.  If necessary, the function
*     returns an identifier for an appropriate section from each NDF in
*     place of the identifier initially supplied, which is annulled.  If no
*     intersection region exists, then an error is reported.

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
*     the minimum number of dimensions of all the NDFs supplied.
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
*     3-APR-2019 (DSB):
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
      lbnd[ i ] = NUM__MINI;
      ubnd[ i ] = NUM__MAXI;
   }
   ndim = NDF__MXDIM;

/* Loop to consider each NDF supplied. Import its identifier. */
   for( indf = 0; indf < n; indf++ ){
      ndf1Impid( ndfs[ indf ], &acb, status );
      if( *status == SAI__OK ) {

/* Obtain the NDF's bounds and number of dimensions from its data array
   component.  (Note the NDF's bounds will be padded with 1's if
   necessary.) */
         aryBound( acb->did, NDF__MXDIM, nlbnd, nubnd, &nndim, status );
         if( *status == SAI__OK ) {

/* Accumulate the output bounds. */
            for( i = 0; i < NDF__MXDIM; i++ ){
               lbnd[ i ] = NDF_MAX( lbnd[ i ], nlbnd[ i ] );
               ubnd[ i ] = NDF_MIN( ubnd[ i ], nubnd[ i ] );

/* If the intersection region does not exist, then report an error. */
               if( lbnd[ i ] > ubnd[ i ] ) {
                  *status = NDF__NOTRM;
                  ndf1Amsg( "NDF", acb );
                  errRep( " ", "The NDF structure ^NDF has no pixels in "
                          "common with the other NDF(s) supplied; its "
                          "pixel-index bounds cannot be trimmed to match.",
                          status );
                  break;
               }
            }

/* Accumulate the number of output dimensions. */
            ndim = NDF_MIN( ndim, nndim );
         }
      }

/* Quit considering NDFs if an error occurs. */
      if( *status != SAI__OK ) break;
   }

/* If an intersection region has been found, then consider each NDF in
   turn again. Import its identifier. */
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
   if( *status != SAI__OK ) ndf1Trace( "ndf1Mbndt", status );

}

