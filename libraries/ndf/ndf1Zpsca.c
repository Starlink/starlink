#include <math.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "star/util.h"
#include "mers.h"

void ndf1Zpsca( NdfACB *acb, int type, const double scale[],
                const double zero[], int *status ){
/*
*+
*  Name:
*     ndf1Zpsca

*  Purpose:
*     Store the parameters defining the values in a SCALED NDF.

*  Synopsis:
*     void ndf1Zpsca( NdfACB *acb, int type, const double scale[],
*                     const double zero[], int *status )

*  Description:
*     This function stores values in an NDF that indicate that the NDF is
*     stored in SCALED form. These values are the scales and offsets that
*     related the internal values stored in the NDF array components into
*     the external values of interest to the user:
*
*        external_value = "scale"*internal_value + ZERO

*  Parameters:
*     acb
*        Pointer to the ACB entry identifying the NDF.
*     type
*        The data type in which the parameters are to be stored in the NDF.
*     scale
*        The scale factors for DATA and VARIANCE arrays. The supplied
*        "scale" array should have at least "2" elements.
*     zero
*        The zero offsets for DATA and VARIANCE arrays. The supplied "zero"
*        array should have at least "2" elements.
*     *status
*        The global status.

*  Notes:
*     - An error will be reported if the DATA or VARIANCE array is mapped
*     on entry to this function.
*     - The second element of the "scale" and "zero" arrays will be ignored
*     if the VARIANCE component of the NDF is undefined.

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
   Ary *ary;             /* The ARY identifier for the array */
   char comp[ 9 ];       /* Component name */
   int icomp;            /* Component index */
   int mapped;           /* Is the array component currently mapped? */
   int ncomp;            /* The number of array components to scale */
   int there;            /* Does the VARIANCE component exist? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the variance component does not exist, we only need to loop over
   one array component (DATA). */
   ndf1Vsta( acb, &there, status );
   if( there ) {
      ncomp = 2;
   } else {
      ncomp = 1;
   }

/* Loop over the required array components. */
   for( icomp = 0; icomp < ncomp; icomp++ ){

/* Get the identifier for the ARY array holding the current NDF
   component, get the component name, and see if it is mapped. */
      if( icomp + 1 == 1 ) {
         ary = acb->did;
         star_strlcpy( comp, "DATA", sizeof( comp ) );
         mapped = acb->dmap;
      } else {
         ary = acb->vid;
         star_strlcpy( comp, "VARIANCE", sizeof( comp ) );
         mapped = acb->vmap;
      }

/* Report an error if the array component is currently mapped. */
      if( mapped && *status == SAI__OK ) {
         *status = NDF__ACDEN;
         msgSetc( "C", comp );
         errRep( " ", "ndf1Zpsca: Scale and zero values cannot be set for "
                 "a ^C component since thearray is currently mapped for "
                 "access (internal programming error).", status );

/* Otherwise store the scale and zero values, casting to the
   requested data type. */
      } else if( type == NDF__TYPB ) {
         aryPtszB( ary, (char) NDF_NINT( scale[ icomp ] ), (char) NDF_NINT( zero[ icomp ] ),
                   status );

      } else if( type == NDF__TYPD ) {
         aryPtszD( ary, scale[ icomp ], zero[ icomp ], status );

      } else if( type == NDF__TYPI ) {
         aryPtszI( ary, (int) NDF_NINT( scale[ icomp ] ), (int) NDF_NINT( zero[ icomp ] ),
                   status );

      } else if( type == NDF__TYPK ) {
         aryPtszK( ary, (int64_t) NDF_NINT( scale[ icomp ] ), (int64_t) NDF_NINT( zero[ icomp ] ),
                   status );

      } else if( type == NDF__TYPR ) {
         aryPtszF( ary, (float) scale[ icomp ], (float) zero[ icomp ], status );

      } else if( type == NDF__TYPUB ) {
         aryPtszUB( ary, (unsigned char) NDF_NINT( scale[ icomp ] ), (unsigned char) NDF_NINT( zero[ icomp ] ),
                    status );

      } else if( type == NDF__TYPUW ) {
         aryPtszUW( ary, (unsigned short int) NDF_NINT( scale[ icomp ] ), (unsigned short int) NDF_NINT( zero[ icomp ] ),
                    status );

      } else if( type == NDF__TYPW ) {
         aryPtszW( ary, (short int) NDF_NINT( scale[ icomp ] ), (short int) NDF_NINT( zero[ icomp ] ),
                   status );

      } else if( *status == SAI__OK ) {
         *status = NDF__FATIN;
         msgSeti( "I", type );
         errRep( " ", "ndf1Zpsca: Unsupported data type (^I) supplied "
                 "(internal programming error).", status );
      }

   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Zpsca", status );

}

