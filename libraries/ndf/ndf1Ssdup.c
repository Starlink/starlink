#include "sae_par.h"
#include "ndf1.h"
#include "ary.h"

void ndf1Ssdup( Ary *ary1, Ary *ary2, Ary **ary3, int *status ){
/*
*+
*  Name:
*     ndf1Ssdup

*  Purpose:
*     Duplicate an array section.

*  Synopsis:
*     void ndf1Ssdup( Ary *ary1, Ary *ary2, Ary **ary3, int *status )

*  Description:
*     This function creates a "similar section" from an array (whose ARY_
*     system identifier is supplied) using an existing array section as a
*     template.  The new array section will bear the same relationship to
*     its base array as the template does to its own base array.
*
*     Note that this function is the same as "arySsect" (q.v.), except that
*     the number of output section dimensions matches the template array,
*     rather than the original input array.

*  Parameters:
*     ary1
*        ARY_ system identifier for the array, or array section, from which
*        the new section is to be drawn.
*     ary2
*        ARY_ system identifier for the template array section (may also be
*        a base array).
*     *ary3
*        Returned holding the ARY_ system identifier for the new array
*        section.
*     *status
*        The global status.

*  Notes:
*     -  If this function is called with "status" set, then a value of NULL
*     will be returned for the "ary3" parameter, although no further
*     processing will occur. The same value will also be returned if the
*     function should fail for any reason.

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
   Ary *itmp;            /* Temporary ARY_ identifier */
   hdsdim dim[ NDF__MXDIM ];       /* Dimension sizes (junk array) */
   hdsdim lbnd1[ NDF__MXDIM ];     /* Lower bounds of array */
   hdsdim ubnd1[ NDF__MXDIM ];     /* Upper bounds of array */
   int idim;             /* Loop counter for dimensions */
   int ndim1;            /* Number of input array dimensions */
   int ndim2;            /* Number of template array dimensions */

/* Initialise the returned ARY_ identifier. */
   *ary3 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Determine the bounds and number of dimensions of the first array. */
   aryBound( ary1, NDF__MXDIM, lbnd1, ubnd1, &ndim1, status );

/* Determine the number of dimensions in the second array. */
   aryDim( ary2, NDF__MXDIM, dim, &ndim2, status );
   if( *status == SAI__OK ) {

/* If the numbers of dimensions match, we can simply select the
   required section. */
      if( ndim1 == ndim2 ) {
         arySsect( ary1, ary2, ary3, status );

/* Otherwise, pad the bounds of the first array with ones to match the
   number of dimensions in the second array. */
      } else {
         for( idim = ndim1; idim < ndim2; idim++ ){
            lbnd1[ idim ] = 1;
            ubnd1[ idim ] = 1;
         }

/* Create a temporary section from the first array with the original
   bounds but the new number of dimensions. */
         arySect( ary1, ndim2, lbnd1, ubnd1, &itmp, status );

/* From this, obtain the required section. */
         arySsect( itmp, ary2, ary3, status );

/* Annul the temporary section. */
         aryAnnul( &itmp, status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Ssdup", status );

}

