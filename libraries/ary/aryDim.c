#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void aryDim( Ary *ary, int ndimx, hdsdim *dim, int *ndim, int *status ) {
/*
*+
*  Name:
*     aryDim

*  Purpose:
*     Enquire the dimension sizes of an array.

*  Synopsis:
*     void aryDim( Ary *ary, int ndimx, hdsdim *dim, int *ndim, int *status )

*  Description:
*     This function returns the size in pixels of each dimension of an
*     array, together with the total number of dimensions (the size of
*     a dimension is the difference between that dimension's upper and
*     lower pixel-index bounds + 1).

*  Parameters:
*     ary
*        Array identifier.
*     ndimx
*        Maximum number of dimension sizes to return (i.e. the size of
*        the "dim" array).
*     dim
*        An array returned holding the size of each dimension in pixels.
*     ndim
*        Returned holding the total number of array dimensions.
*     status
*        The global status.

*  Notes:
*     -  If the array has fewer than "ndimx" dimensions, then any
*     remaining elements of the "dim" argument will be filled with 1's.
*     -  If the array has more than "ndimx" dimensions, then the "ndim"
*     argument will return the actual number of dimensions. In this
*     case only the first "ndimx" dimension sizes will be returned, and
*     an error will result if the size of any of the excluded
*     dimensions exceeds 1.
*     -  The symbolic constant ARY__MXDIM may be used to declare the
*     size of the "dim" argument so that it will be able to hold the
*     maximum number of array dimension sizes that this routine can
*     return. This constant is defined in the header file ary.h.

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
   int i;
   int nd;
   AryACB *acb;
   AryDCB *dcb;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );
   if( *status == SAI__OK ){

/* Return the number of array dimensions. */
      *ndim = acb->ndim;

/* Return as many dimension size values as possible. */
      nd = ( ndimx < *ndim ) ? ndimx : *ndim;
      for( i = 0; i < nd; i++ ){
         dim[ i ] = acb->ubnd[ i ] - acb->lbnd[ i ] + 1;
      }

/* Pad any remaining elements of "dim" with 1's. */
      for( ; i < ndimx; i++ ) dim[ i ] = 1;

/* If not all the dimensions were returned, then check to see whether any
   excluded dimensions have sizes greater than 1. Report an error if any
   excluded dimension does. */
      for( ; i < *ndim; i++ ){
         if( acb->ubnd[ i ] != acb->lbnd[ i ] ){
            *status = ARY__XSDIM;
            ARY__DCB_LOCK_MUTEX;
            dcb = acb->dcb;
            datMsg( "ARRAY", dcb->loc );
            ARY__DCB_UNLOCK_MUTEX;
            msgSeti( "NDIMX", ndimx );
            errRep( " ", "The array structure ^ARRAY has more than ^NDIMX "
                    "significant dimension(s).", status );
            break;
         }
      }
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryDim: Error obtaining array dimension size information.",
              status );
      ary1Trace( "aryDim", status );
   }

}
