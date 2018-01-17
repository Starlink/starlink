#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"

void ary1Vbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd, int *status ) {
/*
*+
*  Name:
*     ary1Vbnd

*  Purpose:
*     Check array bounds for validity.

*  Synopsis:
*     void ary1Vbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd,
*                    int *status )

*  Description:
*     The routine checks that the number of array dimensions and the
*     lower and upper array bounds supplied are valid and reports an
*     error if they are not. Otherwise, the routine returns without
*     action.

*  Parameters:
*     ndim
*        The number of array dimensions.
*     lbnd
*        Array of lower dimension bounds.
*     ubnd
*        Array of upper dimension bounds.
*     status
*        The global status.

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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check the number of array dimensions is valid. */
   if( ( ndim <= 0 ) || ( ndim > ARY__MXDIM ) ){
      *status = ARY__NDMIN;
      msgSeti( "NDIM", ndim );
      msgSeti( "MXDIM", ARY__MXDIM );
      errRep( " ", "Number of array dimensions (^NDIM) is invalid; this number "
              "should lie between 1 and ^MXDIM inclusive (possible "
              "programming error).", status );

/* Check the lower and upper bounds of each dimension for validity. */
   } else {
      for( i = 0; i < ndim; i++ ){
         if( lbnd[ i ] > ubnd[ i ] ){
            *status = ARY__DIMIN;
            msgSeti( "LBND", lbnd[ i ] );
            msgSeti( "DIM", i+1 );
            msgSeti( "UBND", ubnd[ i ] );
            errRep( " ", "Lower bound (^LBND) of array dimension ^DIM exceeds "
                    "the corresponding upper bound (^UBND) (possible "
                    "programming error).", status );
            break;
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Vbnd", status );

}
