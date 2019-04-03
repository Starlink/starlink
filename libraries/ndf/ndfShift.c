#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "ndf.h"
#include "mers.h"

void ndfShift_( int nshift, const hdsdim shift[], int indf, int *status ){
/*
*+
*  Name:
*     ndfShift

*  Purpose:
*     Apply pixel-index shifts to an NDF.

*  Synopsis:
*     void ndfShift( int nshift, const hdsdim shift[], int indf, int *status )

*  Description:
*     This function applies pixel-index shifts to an NDF. An integer shift
*     is applied to each dimension so that its pixel-index bounds, and the
*     indices of each pixel, change by the amount of shift applied to the
*     corresponding dimension. The NDF's pixels retain their values and
*     none are lost.

*  Parameters:
*     nshift
*        Number of dimensions to which shifts are to be applied. This must
*        not exceed the number of NDF dimensions. If fewer shifts are
*        applied than there are NDF dimensions, then the extra dimensions
*        will not be shifted.
*     shift
*        The pixel-index shifts to be applied to each dimension.
*     indf
*        NDF identifier.
*     *status
*        The global status.

*  Notes:
*     -  Pixel-index shifts applied to a base NDF will affect the
*     appearance of that NDF as seen by all base-NDF identifiers associated
*     with it. However, NDF sections derived from that base NDF will remain
*     unchanged (as regards both pixel-indices and array values).
*     -  Pixel-index shifts applied to an NDF section only affect that
*     section itself, and have no effect on other NDF identifiers.
*     -  Pixel-index shifts cannot be applied to a base NDF while any of
*     its components (or any of its axis arrays) is mapped for access, even
*     through another identifier.
*     -  Pixel-index shifts cannot be applied to an NDF section while any
*     of its components (or any of its axis arrays) is mapped for access
*     through the identifier supplied to this function.

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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper bounds */
   int i;                /* Loop counter for dimensions */
   int iax;              /* Loop counter for axes */
   int mapped;           /* Is an NDF component mapped? */
   int ndim;             /* Number of NDF dimensions */
   int there;            /* Whether component exists */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* If the number of shifts specified is not positive, then report an
   error. */
   if( nshift <= 0 ) {
      *status = NDF__SFTIN;
      msgSeti( "BADNSFT", nshift );
      errRep( " ", "Invalid number of shifts (^BADNSFT) specified "
              "(possible programming error).", status );

/* Otherwise, import the NDF identifier. */
   } else {
      ndf1Impid( indf, &acb, status );
   }

/* Determine the NDF bounds and number of dimensions from the ARY_
   system identifier for the data array, held in the ACB. */
   if( *status == SAI__OK ) {
      aryBound( acb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Check that the number of shifts does not exceed the number of NDF
   dimensions. Report an error if it does. */
      if( *status == SAI__OK ) {
         if( nshift > ndim ) {
            *status = NDF__SFTIN;
            msgSeti( "BADNSFT", nshift );
            msgSeti( "NDIM", ndim );
            errRep( " ", "Number of shifts specified (^BADNSFT) exceeds "
                    "the number of NDF dimensions (^NDIM) (possible "
                    "programming error).", status );
         }
      }

/* Obtain an index to the data object entry in the DCB. */
      if( *status == SAI__OK ) {
         dcb = acb->dcb;

/* Check that "shift" access to the NDF is available. */
         ndf1Chacc( acb, "SHIFT", status );

/* Ensure that quality and variance information is available in the DCB
   and ACB (note that this must be done at the start because once one
   component has been shifted, the bounds of subsequent components will
   no longer match it, so an error would result due to the checks which
   take place when information is obtained about these subsequent
   components). */
         ndf1Qimp( acb, status );
         ndf1Vimp( acb, status );

/* Similarly, ensure that information is available about all of the
   NDF's axis components. */
         for( iax = 0; iax < ndim; iax++ ){
            ndf1Dad( iax, dcb, status );
            ndf1Dav( iax, dcb, status );
            ndf1Daw( iax, dcb, status );
         }
      }

/* Check that no NDF component is mapped through the current ACB entry. */
      if( *status == SAI__OK ) {
         mapped = ( acb->dmap || acb->qmap || acb->vmap );

/* Check all the axis arrays for current mappings as well if necessary. */
         if( !mapped ) {
            for( iax = 0; iax < ndim; iax++ ){
               if( acb->admap[ iax ] || acb->avmap[ iax ] || acb->awmap[ iax ] ) {
                  mapped = 1;
                  break;
               }
            }
         }

/* If any component is mapped, then report an error. */
         if( mapped ) {
            *status = NDF__ISMAP;
            ndf1Amsg( "NDF", acb );
            errRep( " ", "The NDF structure ^NDF is already mapped for "
                    "access through the specified identifier (possible "
                    "programming error).", status );

/* If a base NDF has been specified, then check that none of the NDF"s
   components is mapped for access.  Report an error if any component
   is. */
         } else if( ( !acb->cut ) && ( dcb->nmap != 0 ) ) {
            *status = NDF__ISMAP;
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "The NDF structure ^NDF is already mapped for "
                    "access through another identifier (possible "
                    "programming error).", status );
         }
      }

/* Calculate the new NDF bounds by applying the shifts. */
      if( *status == SAI__OK ) {
         for( i = 0; i < nshift; i++ ){
            lbnd[ i ] += shift[ i ];
            ubnd[ i ] += shift[ i ];
         }
      }

/* DATA component:
   ==============
   Apply pixel-index shifts to the data component. */
      aryShift( nshift, shift, acb->did, status );

/* QUALITY component:
   =================
   See if the ARY_ system identifier for the quality component is valid.
   If not, then the component is not defined. */
      there = aryValid( acb->qid, status );
      if( *status == SAI__OK ) {

/* If it is defined, then apply pixel-index shifts to it. */
         if( there ) {
            aryShift( nshift, shift, acb->qid, status );

/* If it is not defined, and this is a base NDF, then convert its
   default storage form to take account of the new NDF bounds, if
   necessary. */
         } else if( !acb->cut ) {
            ndf1Cbfrm( ndim, lbnd, ubnd, dcb->qfrm, sizeof( dcb->qfrm ),
                       status );
         }
      }

/* VARIANCE:
   ========
   See if the ARY_ system identifier for the variance component is
   valid. If not, then the component is not defined. */
      there = aryValid( acb->vid, status );
      if( *status == SAI__OK ) {

/* If it is defined, then apply pixel-index shifts to it. */
         if( there ) {
            aryShift( nshift, shift, acb->vid, status );

/* If it is not defined, and this is a base NDF, then convert its
   default storage form to take account of the new NDF bounds, if
   necessary. */
         } else if( !acb->cut ) {
            ndf1Cbfrm( ndim, lbnd, ubnd, dcb->vfrm, sizeof( dcb->vfrm ),
                       status );
         }
      }

/* AXIS:
   ====
   Axis arrays will only need a shift applied to them if this is a base
   NDF. Check if this is so. */
      if( *status == SAI__OK ) {
         if( !acb->cut ) {

/* If required, loop to apply the shift to each affected NDF axis. */
            for( iax = 0; iax < nshift; iax++ ){

/* Axis data component.
   ===================
   If the array exists, then apply the appropriate shift to it. */
               if( dcb->adid[ iax ] ) {
                  aryShift( 1, shift + iax, dcb->adid[ iax ], status );

/* If it does not exist, then convert its default storage form to take
   account of the new NDF bounds, if necessary. */
               } else {
                  ndf1Cbfrm( 1, lbnd + iax, ubnd + iax, dcb->adfrm[ iax ],
                             sizeof( dcb->adfrm[ iax ] ), status );
               }

/* Axis variance component.
   =======================
   If the array exists, then apply the appropriate shift to it. */
               if( dcb->avid[ iax ] ) {
                  aryShift( 1, shift + iax, dcb->avid[ iax ], status );

/* If it does not exist, then convert its default storage form to take
   account of the new NDF bounds, if necessary. */
               } else {
                  ndf1Cbfrm( 1, lbnd + iax, ubnd + iax, dcb->avfrm[ iax ],
                             sizeof( dcb->avfrm[ iax ] ), status );
               }

/* Axis width component.
   ====================
   If the array exists, then apply the appropriate shift to it. */
               if( dcb->awid[ iax ] ) {
                  aryShift( 1, shift + iax, dcb->awid[ iax ], status );

/* If it does not exist, then convert its default storage form to take
   account of the new NDF bounds, if necessary. */
               } else {
                  ndf1Cbfrm( 1, lbnd + iax, ubnd + iax, dcb->awfrm[ iax ],
                             sizeof( dcb->awfrm[ iax ] ), status );
               }
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfShift: Error applying pixel-index shifts to an "
              "NDF.", status );
      ndf1Trace( "ndfShift", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

