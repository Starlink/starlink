#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Van( NdfACB *acb, int iaxis, int allok, int *iax1, int *iax2,
              int *status ){
/*
*+
*  Name:
*     ndf1Van

*  Purpose:
*     Validate an axis number.

*  Synopsis:
*     void ndf1Van( NdfACB *acb, int iaxis, int allok, int *iax1,
*                   int *iax2, int *status )

*  Description:
*     This function checks an axis number for validity. If the number is
*     not valid, then an error is reported. Otherwise, the function returns
*     the validated zero-based indices of the first and last axes to be
*     processed.

*  Parameters:
*     acb
*        Pointer to the ACB entry for the NDF to which the axis number
*        refers.
*     iaxis
*        Axis one-based axis index to be validated.
*     allok
*        If this parameter is set to non-zero, then an axis number of zero
*        may be used to specify that all the axes are to be processed. If
*        it is set to zero, then "iaxis" may refer only to a single axis.
*     *iax1
*        Returned holding the zero-based index of first axis to be processed.
*     *iax2
*        Returned holding the zero-based index of last axis to be processed.
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
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper bounds */
   int ndim;             /* Number of NDF dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain the bounds and number of dimensions of the NDF from the ARY_
   system identifier for its data array. */
   aryBound( acb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );
   if( *status == SAI__OK ) {

/* If all axes may be specified by giving an axis number of zero, then
   set the zero-based indices of the first and last axes to be processed. */
      if( allok && ( iaxis == 0 ) ) {
         *iax1 = 0;
         *iax2 = ndim - 1;

/* If the axis number given is invalid, then report an error. */
      } else if( iaxis < 1 || iaxis > ndim ) {
         *status = NDF__AXNIN;
         msgSeti( "IAXIS", iaxis );
         msgSeti( "NDIM", ndim );
         ndf1Amsg( "NDF", acb );

/* The error report has to be slightly different if the NDF is s
   section. */
         if( acb->cut ) {
            errRep( " ", "Invalid axis number (^IAXIS) specified; the "
                    "identifier supplied refers to the ^NDIM-dimensional "
                    "NDF section ^NDF (possible programming error).", status );
         } else {
            errRep( " ", "Invalid axis number (^IAXIS) specified; the "
                    "identifier supplied refers to the ^NDIM-dimensional "
                    "NDF ^NDF (possible programming error).", status );
         }

/* Otherwise, set the first and last axes to refer to a specific axis. */
      } else {
         *iax1 = iaxis - 1;
         *iax2 = iaxis - 1;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Van", status );

}

