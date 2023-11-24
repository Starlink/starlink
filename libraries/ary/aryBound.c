#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void aryBound( Ary *ary, int ndimx, hdsdim *lbnd, hdsdim *ubnd, int *ndim,
               int *status ) {
/*
*+
*  Name:
*     aryBound

*  Purpose:
*     Enquire the pixel-index bounds of an array.

*  Synopsis:
*     void aryBound( Ary *ary, int ndimx, hdsdim *lbnd, hdsdim *ubnd, int *ndim,
*                    int *status )

*  Description:
*     This function returns the lower and upper pixel-index bounds of
*     each dimension of an array, together with the total number of
*     dimensions.

*  Parameters:
*     ary
*        Array identifier.
*     ndimx
*        Maximum number of pixel-index bounds to return (i.e. the
*        declared size of the "lbnd" and "ubnd" arguments).
*     lbnd
*        Returned holding the lower pixel-index bounds for each dimension.
*     ubnd
*        Returned holding the upper pixel-index bounds for each dimension.
*     ndim
*        Returned holding the total number of array dimensions.
*     status
*        The global status.

*  Notes:
*     -  If the array has fewer than "ndimx" dimensions, then any
*     remaining elements of the "lbnd" and "ubnd" arguments will be filled
*     with 1's.
*     -  If the array has more than "ndimx" dimensions, then the "ndim"
*     argument will return the actual number of dimensions. In this
*     case only the first "ndimx" sets of bounds will be returned, and an
*     error will result if the size of any of the remaining dimensions
*     exceeds 1.
*     -  The symbolic constant ARY__MXDIM may be used to declare the
*     size of the "lbnd" and "ubnd" arguments so that they will be able to
*     hold the maximum number of array bounds that this routine can
*     return. This constant is defined in the header file "ary.h".

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
   int i;                     /* Loop counter for dimensions */
   int n;                     /* Limited no of axes */
   AryACB *acb;               /* Pointer to the Access Control Block */
   AryDCB *dcb;               /* Pointer to the Data Control Block */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );
   if( *status == SAI__OK ){

/* Return the number of array dimensions. */
      *ndim = acb->ndim;

/* Return as many array bounds values as possible. */
      n = ( *ndim < ndimx ) ? *ndim : ndimx;
      for( i = 0; i < n; i++ ){
         lbnd[ i ] = acb->lbnd[ i ];
         ubnd[ i ] = acb->ubnd[ i ];
      }

/* Pad any remaining elements in the "lbnd" and "ubnd" arguments with 1's. */
      for( ; i < ndimx; i++ ){
         lbnd[ i ] = 1;
         ubnd[ i ] = 1;
      }

/* If the array has more dimensions than there are elements in the "lbnd" and
   "ubnd" arguments, then check that the size of all remaining dimensions is
   1. */
      for( i = n; i < *ndim; i++ ){

/* Report an error if any significant dimensions have been excluded. */
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
      errRep( " ", "aryBound: Error obtaining the pixel-index bounds of an "
              "array.", status );
      ary1Trace( "aryBound", status );
   }

}
