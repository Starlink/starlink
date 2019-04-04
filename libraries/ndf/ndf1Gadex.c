#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"

void ndf1Gadex( hdsdim lbnd, hdsdim ubnd, Ary *ary, int upper, double *scale,
                double *zero, int *status ){
/*
*+
*  Name:
*     ndf1Gadex

*  Purpose:
*     Get extrapolation parameters for an NDF axis data array.

*  Synopsis:
*     void ndf1Gadex( hdsdim lbnd, hdsdim ubnd, Ary *ary, int upper,
*                     double *scale, double *zero, int *status )

*  Description:
*     This function obtains the parameters required to extrapolate an NDF's
*     axis data array beyond the lower or upper pixel-index limits of the
*     NDF. A linear extrapolation is used, taking the difference between
*     the nearest two existing pixels to define the gradient of the
*     extrapolated line.

*  Parameters:
*     lbnd
*        Lower pixel-index bound of the axis data array.
*     ubnd
*        Upper pixel-index bound of the axis data array.
*     ary
*        ARY_ system identifier for the (1-dimensional) axis data array.
*     upper
*        Whether extrapolation to higher pixel indices is required. If not,
*        then extrapolation to lower pixel indices is assumed.
*     *scale
*        Returned holding the extrapolation scale factor. The extrapolated
*        axis value "aval" is related to the axis array's pixel index I by
*        "aval" = I * "scale" + "zero".
*     *zero
*        Returned holding the extrapolation zero point (see formula above).
*     *status
*        The global status.

*  Notes:
*     -  If the existing axis data array contains only a single pixel, then
*     a "scale" value of 1.0D0 will be returned.

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
   Ary *arys;            /* ARY_ identifier for section */
   hdsdim l;             /* Index of first extrapolation pixel */
   hdsdim u;             /* Index of second extrapolation pixel */
   size_t el;            /* Number of mapped pixel values */
   double *aval;         /* Pointer to mapped pixel values */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Determine the indices of the two nearest pixels to be used for
   extrapolation, ensuring that they lie within the axis array. */
   if( upper ) {
      l = NDF_MAX( lbnd, ubnd - 1 );
      u = ubnd;
   } else {
      l = lbnd;
      u = NDF_MIN( ubnd, lbnd + 1 );
   }

/* Create a section containing the pixels of interest and map it for
   reading with a numeric type of _DOUBLE. */
   arySect( ary, 1, &l, &u, &arys, status );
   aryMap( arys, "_DOUBLE", "READ", (void **) &aval, &el, status );

/* Derive the extrapolation scale factor and zero point. */
   if( *status == SAI__OK ) {
      if( el <= 1 ) {
         *scale = 1.0;
      } else {
         *scale = aval[ 1 ] - aval[ 0 ];
      }
      *zero = aval[ 0 ] - (*scale)*l;
   }

/* Annul the array section (which also unmaps it). */
   aryAnnul( &arys, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Gadex", status );

}

