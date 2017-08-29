#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"

void ary1Gmrb( AryACB *acb, char *mtrex, char *mrfull, char *whole,
               int lmrb[ ARY__MXDIM ], int umrb[ ARY__MXDIM ],
               int lmtr[ ARY__MXDIM ], int umtr[ ARY__MXDIM ], int *status ) {
/*
*+
*  Name:
*     ary1Gmrb

*  Purpose:
*     Get mapping region bounds.

*  Synopsis:
*     void ary1Gmrb( AryACB *acb, char *mtrex, char *mrfull, char *whole,
*                    int lmrb[ ARY__MXDIM ], int umrb[ ARY__MXDIM ],
*                    int lmtr[ ARY__MXDIM ], int umtr[ ARY__MXDIM ],
*                    int *status )

*  Description:
*     The routine determines the extent of the mapping region
*     associated with access to all or part of an array. It also
*     determines whether a mapping transfer region exists, whether it
*     fills the mapping region and whether the whole of the data object
*     should be mapped. The results are returned using the pixel index
*     system of the actual data object.

*  Parameters:
*     acb
*        Pointer to the ACB for which mapping region information is required.
*     mtrex
*        Returned holding a flag indicating whether a mapping transfer
*        region exists.
*     mrfull
*        Returned holding a flag indicating whether the mapping transfer
*        region completely fills the mapping region.
*     whole
*        Returned holding a flag indicating whether the mapping transfer
*        region and mapping region both consist of the entire data object,
*        indicating that the entire object can be mapped.
*     lmrb
*        An array that is returned holding the lower mapping region bounds.
*     umrb
*        An array that is returned holding the upper mapping region bounds.
*     lmtr
*        An array that is returned holding the lower bounds of mapping transfer
*        region.
*     umtr
*        An array that is returned holding the upper bounds of mapping transfer
*        region.
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
   int i;
   AryDCB *dcb;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a pointer to the DCB. */
   dcb = acb->dcb;

/* Initialise. The mapping transfer region can only exist if a data
   transfer window exists. */
   *mtrex = acb->dtwex;
   *mrfull = *mtrex;
   *whole = *mtrex;

/* Loop to consider each dimension. */
   for( i = 0; i < ARY__MXDIM; i++ ){

/* Obtain the bounds of the mapping region, removing the ACB accumulated
   pixel index shift to convert them into the reference frame pixel index
   system. */
      lmrb[ i ] = acb->lbnd[ i ] - acb->shift[ i ];
      umrb[ i ] = acb->ubnd[ i ] - acb->shift[ i ];

/* Obtain the bounds of the mapping transfer region in the reference frame
   pixel index system. This is formed by the intersection of the mapping
   region, the data transfer window and the actual data object bounds.
   Note that the accumulated DCB pixel index shifts have to be removed
   from the latter to convert them into the reference frame system. */
      lmtr[ i ] = dcb->lbnd[ i ] - dcb->shift[ i ];
      if( lmtr[ i ] < acb->ldtw[ i ] ) lmtr[ i ] = acb->ldtw[ i ];
      if( lmtr[ i ] < lmrb[ i ] ) lmtr[ i ] = lmrb[ i ];

      umtr[ i ] = dcb->ubnd[ i ] - dcb->shift[ i ];
      if( umtr[ i ] > acb->udtw[ i ] ) umtr[ i ] = acb->udtw[ i ];
      if( umtr[ i ] > umrb[ i ] ) umtr[ i ] = umrb[ i ];

/* Convert the above results into the data object pixel index system by
   adding the accumulated DCB pixel index shifts (note the above
   calculations have to be carried out in the reference frame pixel index
   system to avoid overflow problems due to the "infinite" extent of the
   data transfer window associated with base arrays). */
      lmrb[ i ] = lmrb[ i ] + dcb->shift[ i ];
      umrb[ i ] = umrb[ i ] + dcb->shift[ i ];
      lmtr[ i ] = lmtr[ i ] + dcb->shift[ i ];
      umtr[ i ] = umtr[ i ] + dcb->shift[ i ];

/* Note whether the mapping transfer region exists. */
      *mtrex = *mtrex && ( lmtr[ i ] <= umtr[ i ] );

/* Note whether the mapping transfer region completely fills the mapping
   region. */
      *mrfull = *mtrex && *mrfull && ( lmtr[ i ] == lmrb[ i ] ) &&
                                     ( umtr[ i ] == umrb[ i ] );

/* Note whether the mapping region (and mapping transfer region) comprise
   the whole data object. */
      *whole = *mrfull && *whole && ( lmtr[ i ] == dcb->lbnd[ i ] ) &&
                                    ( umtr[ i ] == dcb->ubnd[ i ] );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Gmrb", status );

}
