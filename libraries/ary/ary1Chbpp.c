#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void ary1Chbpp( AryACB *acb, int *bad, int *status ) {
/*
*+
*  Name:
*     ary1Chbpp

*  Purpose:
*     Perform an explicit check that bad pixels are present in an array.

*  Synopsis:
*     void ary1Chbpp( AryACB *acb, int *bad, int *status )

*  Description:
*     This function checks explicitly that bad pixels are present in an
*     array identified by its ACB entry. It is intended to be called
*     once checks on the array's bad pixel flag have indicated that bad
*     pixels may be present, but a further check is required that bad
*     values actually appear in the data (they may not be present, for
*     instance, if the array is a section taken from a larger array and
*     it happens to avoid all the bad pixels).

*  Parameters:
*     acb
*        Pointer to the ACB structure.
*     bad
*        A non-zero value is returned only if bad values are actually
*        present in the array's data. Otherwise, zero is returned.
*     status
*        The global status.

* Prior Requirements:
*     -  The DCB mutex must be locked.

*  Notes:
*     -  If the array is mapped for access when this routine is called,
*     then checking will be performed on the actual mapped data values
*     (i.e. on the region of memory containing the data passed to the
*     application). An imaginary array component may therefore be
*     omitted from this check if mapped access to non-complex data is
*     in effect.
*     -  If the array is not mapped for access, then it will first be
*     mapped using 'READ' access and the data type of the associated
*     data object (to avoid type conversion) before performing the
*     check for bad pixels. In this case, any imaginary component
*     present will also be checked.
*     -  A non-zero value for "bad" will be returned if the array is not
*     mapped for access and is in the "undefined" state.

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
   AryDCB *dcb;               /* Data object (DCB) */
   AryMCB *mcb;               /* Mapping Control Block (MCB) */
   size_t el;                 /* Number of data elements in the array */
   void *dpntr;               /* Pointer to mapped non-imaginary data */
   void *ipntr;               /* Pointer to mapped imaginary data */

   ARY__DCB_ASSERT_MUTEX;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Calculate the number of data elements in the array. */
   ary1Nel( acb->ndim, acb->lbnd, acb->ubnd, &el, status );

/* Obtain a pointer to the array's mapping contrl block. */
   mcb = acb->mcb;

/* If this pointer is non-NULL, then the array is currently mapped for
   access. Check the non-imaginary component of the mapped data to see if
   bad pixels are present. */
   if( mcb ){
      ary1Bpp( mcb->type, el, mcb->dpntr, bad, status );
      if( *status == SAI__OK ){

/* If the array is mapped for access to complex data, and no bad pixels
   have yet been found, then the imaginary component of the mapped data
   must be checked in the same way. */
         if( mcb->complex && ( !*bad ) ){
            ary1Bpp( mcb->type, el, mcb->ipntr, bad, status );
         }
      }

/* If the array is not mapped for access, then obtain a pointer to the
   Data Control Block (data object). */
   } else {
      dcb = acb->dcb;

/* Ensure that data type and state information is available for the data
   object in the DCB. */
      ary1Dtyp( dcb, status );
      ary1Dsta( dcb, status );
      if( *status == SAI__OK ){

/* If the data object is in the "undefined" state, then BAD is
   automatically set non-zero. */
         if( !dcb->state ){
            *bad = 1;

/* Otherwise, map the array for read access. */
         } else {
            ary1Maps( acb, dcb->type, dcb->complex, "READ", NULL, &dpntr,
                      &ipntr, status );

/* If access could not be obtained, then add context information to the
   error report. */
            if( *status != SAI__OK ){
               errRep( " ", "Unable to access array values to check for bad "
                       "pixels.", status );
            }

/* Examine the non-imaginary mapped data for bad pixels. */
            ary1Bpp( dcb->type, el, dpntr, bad, status );
            if( *status == SAI__OK ){

/* If the array is complex, and no bad pixels have yet been found, then the
   imaginary component of the mapped data must be examined in the same
   way. */
               if( dcb->complex && ( !bad ) ){
                  ary1Bpp( dcb->type, el, ipntr, bad, status );
               }
            }

/* Unmap the array. */
            ARY__ACB_LOCK_MUTEX;
            ary1Umps( acb, status );
            ARY__ACB_UNLOCK_MUTEX;
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Chbpp", status );

}
