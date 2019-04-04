#include <stdlib.h>
#include "sae_par.h"
#include "ary.h"
#include "ndf1.h"

void ndf1Gawex( hdsdim lbnd, hdsdim ubnd, Ary *ary, int upper, double *width,
                int *status ){
/*
*+
*  Name:
*     ndf1Gawex

*  Purpose:
*     Get an extrapolation value for an NDF axis width array.

*  Synopsis:
*     void ndf1Gawex( hdsdim lbnd, hdsdim ubnd, Ary *ary, int upper,
*                     double *width, int *status )

*  Description:
*     This function obtains a value required to extrapolate an NDF's axis
*     width array beyond the lower or upper pixel-index limits of the NDF.
*     The width of the nearest available axis element is used.

*  Parameters:
*     lbnd
*        Lower pixel-index bound of the axis width array.
*     ubnd
*        Upper pixel-index bound of the axis width array.
*     ary
*        ARY_ system identifier for the (1-dimensional) axis width array.
*     upper
*        Whether extrapolation to higher pixel indices is required. If not,
*        then extrapolation to lower pixel indices is assumed.
*     *width
*        Returned holding the width value to use for extrapolation.
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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   Ary *arys;            /* ARY_ identifier for section */
   hdsdim l;             /* Lower section bound */
   hdsdim u;             /* Upper section bound */
   size_t el;            /* Number of mapped pixel values */
   double *pntr;         /* Pointer to mapped pixel value */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Determine the pixel-index bounds of a section containing the nearest
   pixel to be used for extrapolation. */
   if( upper ) {
      l = ubnd;
      u = ubnd;
   } else {
      l = lbnd;
      u = lbnd;
   }

/* Create a section containing the pixel of interest and map it for
   reading with a numeric type of _DOUBLE. */
   arySect( ary, 1, &l, &u, &arys, status );
   aryMap( arys, "_DOUBLE", "READ", (void **) &pntr, &el, status );

/* Return the extracted width value. */
   if( *status == SAI__OK ) *width = pntr[ 0 ];

/* Annul the array section (which also unmaps it). */
   aryAnnul( &arys, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Gawex", status );

}

