#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"

void ary1Cut( AryACB *acb1, int ndim, const hdsdim *lbnd, const hdsdim *ubnd,
              AryACB **acb2, int *status ) {
/*
*+
*  Name:
*     ary1Cut

*  Purpose:
*     Obtain a cut from an existing array with an entry in the ACB.

*  Synopsis:
*     void ary1Cut( AryACB *acb1, int ndim, const hdsdim *lbnd,
*                   const hdsdim *ubnd, AryACB **acb2, int *status )

*  Description:
*     This function produces an ACB structure for a new array representing
*     a "cut" from an existing array (which may itself be a cut). The
*     bounds and dimensionality of the new array are specified in the
*     call to this function and the resulting array has access to a
*     subset (although possibly a null subset) of the data accessible
*     to the initial array from which it is derived. This accessible
*     region is determined by the new array's data transfer window,
*     which is formed from the intersection between the initial array's
*     data transfer window, the bounds of the initial array and the
*     bounds of the new array.

*  Parameters:
*     acb1
*        The ACB for an existing array.
*     ndim
*        Number of dimensions for the new cut. Also gives the length of
*        the lbnd and ubnd arrays.
*     lbnd
*        Lower dimension bounds for the cut.
*     ubnd
*        Upper dimension bounds for the cut.
*     acb2
*        Returned holding the ACB for the new cut.
*     status
*        The global status.

*  Notes:
*     -  If "status" is set on entry, then a value of NULL will be
*     returned for the "acb2" argument, although no further processing
*     will occur.
*     -  A value of NULL will also be returned for the "acb2" argument if
*     the routine fails for any reason.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   AryDCB *dcb;               /* Data object */
   hdsdim lx[ARY__MXDIM];     /* Lower intersection region bounds */
   hdsdim ux[ARY__MXDIM];     /* Upper intersection region bounds */
   int exist;                 /* Whether intersection region exists */
   int i;                     /* Loop counter for dimensions */

/* Set an initial value for the "acb2" argument. */
   *acb2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a free slot for the new array in the ACB. */
   *acb2 = ary1Ffs( ARY__ACBTYPE, status );
   if( *status == SAI__OK ){

/* Mark the new entry as a cut. */
      (*acb2)->cut = 1;

/* Transfer the access control flags. */
      (*acb2)->access = acb1->access;

/* Transfer the index to the data object entry in the DCB. */
      dcb = acb1->dcb;
      (*acb2)->dcb = dcb;

/* The new array is not mapped for access. */
      (*acb2)->mcb = 0;

/* Transfer the bad pixel flag. */
      (*acb2)->bad = acb1->bad;

/* Store the number of dimensions and the dimension bounds for the new
   array, padding the bounds information with 1's if necessary. */
      (*acb2)->ndim = ndim;
      for( i = 0; i < ndim; i++ ){
         (*acb2)->lbnd[ i ] = lbnd[ i ];
         (*acb2)->ubnd[ i ] = ubnd[ i ];
      }
      for( i = ndim; i < ARY__MXDIM; i++ ){
         (*acb2)->lbnd[ i ] = 1;
         (*acb2)->ubnd[ i ] = 1;
      }

/* Transfer the accumulated pixel shifts for the initial array to the new
   array. */
      for( i = 0; i < ARY__MXDIM; i++ ){
         (*acb2)->shift[ i ] = acb1->shift[ i ];
      }

/* If the first array did not have a data transfer window, then neither
   does the new one. */
      if( !acb1->dtwex ){
         (*acb2)->dtwex = 0;

/* Otherwise. find the intersection of the new array bounds with the old
   ones, which defines the maximum region of data to which the new array
   has access (in the current pixel index system). */
      } else {
         ary1Xsbnd( ARY__MXDIM, acb1->lbnd, acb1->ubnd,
                    ARY__MXDIM, (*acb2)->lbnd, (*acb2)->ubnd,
                    ARY__MXDIM, lx, ux, &exist, status );
         if( *status == SAI__OK ){

/* If there is no intersection region, then the new array has no data
   transfer window. */
            if( !exist ){
               (*acb2)->dtwex = 0;

/* Otherwise, convert the bounds of the intersection region into the
   reference frame pixel index system by subtracting the accumulated ACB
   pixel shifts. */
            } else {
               for( i = 0; i < ARY__MXDIM; i++ ){
                  lx[ i ] -= (*acb2)->shift[ i ];
                  ux[ i ] -= (*acb2)->shift[ i ];
               }

/* Find the intersection of this region with the data transfer window of
   the original array. This defines the new data transfer window. Note its
   bounds and whether it exists in the ACB entry for the new array. */
               ary1Xsbnd( ARY__MXDIM, acb1->ldtw, acb1->udtw, ARY__MXDIM,
                          lx, ux, ARY__MXDIM, (*acb2)->ldtw, (*acb2)->udtw,
                          &(*acb2)->dtwex, status );
            }
         }
      }
   }

/* If no error occurred, then increment the data object reference count. */
   if( *status == SAI__OK ){
      (dcb->refcount)++;

/* If there was an error, then release the ACB slot and reset it to NULL. */
   } else {
      *acb2 = ary1Rls( (AryObject *) *acb2, status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Cut", status );

}
