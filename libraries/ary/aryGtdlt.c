#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"

void aryGtdlt( Ary *ary, int *zaxis, char ztype[DAT__SZTYP+1], float *zratio,
               int *status ) {
/*
*+
*  Name:
*     aryGtdlt

*  Purpose:
*     Get the compressed axis and data type for a DELTA array.

*  Synopsis:
*     void aryGtdlt( Ary *ary, int *zaxis, char ztype[DAT__SZTYPE+1],
*                    float *zratio, int *status )

*  Description:
*     This function returns the details of the compression used to produce
*     an array stored in DELTA form. If the array is not stored in
*     DELTA form, then null values are returned as listed below, but no
*     error is reported.
*
*     A DELTA array is compressed by storing only the differences between
*     adjacent array values along a nominated compression axis, rather than
*     the full array values. The differences are stored using a smaller data
*     type than the original absolute values. The compression is lossless
*     because any differences that will not fit into the smaller data type
*     are stored explicitly in an extra array with a larger data type.
*     Additional compression is achieved by replacing runs of equal values
*     by a single value and a repeat count.

*  Parameters:
*     ary
*        Array identifier.
*     zaxis
*        Returned holding the index of the pixel axis along which compression
*        occurred. The first axis has index 1. Zero is returned if the array
*        is not stored in DELTA form.
*     ztype
*        Returned holding the data type in which the differences between
*        adjacent array values are stored. This will be one of '_BYTE',
*        '_WORD' or '_INTEGER'. The data type of the array itself is returned
*        if the supplid array is not stored in DELTA form.
*     zratio
*        Returned holding the compression factor - the ratio of the
*        uncompressed array size to the compressed array size. This is
*        approximate as it does not include the effects of the metadata
*        needed to describe the extra components of a DELTA array (i.e. the
*        space needed to hold the component names, types, dimensions, etc).
*        A value of 1.0 is returned if the supplid array is not stored in
*        DELTA form.
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
   AryACB *acb;
   AryDCB *dcb;

/* Initialise returned values */
   *zaxis = 0;
   *ztype = 0;
   *zratio = 1.0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );

   ARY__DCB_LOCK_MUTEX;

/* Get the DCB entry associated with this ACB entry. */
   dcb = acb->dcb;

/* Get the compression information from the data object. */
   ary1Gtdlt( dcb, zaxis, ztype, zratio, status );

   ARY__DCB_UNLOCK_MUTEX;

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryGtdlt: Error getting information about a delta "
              "compressed array.", status );
      ary1Trace( "aryGtdlt", status );
   }

}
