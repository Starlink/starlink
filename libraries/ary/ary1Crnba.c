#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"
#include "prm_par.h"

void ary1Crnba( AryDCB *dcb, AryACB **acb, int *status ) {
/*
*+
*  Name:
*     ary1Crnba

*  Purpose:
*     Create a new ACB base array from a DCB.

*  Synopsis:
*     void ary1Crnba( AryDCB *dcb, AryACB **acb, int *status )

*  Description:
*     The routine creates a new ACB describing a base array which
*     refers to the data object whose DCB is supplied. The new
*     array acquires its dimensionality, bounds and access control
*     flags directly from the data object and is given a data transfer
*     window of "infinite" extent. The bad pixel flag is derived from
*     the data object's bad pixel flag and state. The reference count
*     for the DCB entry is incremented by one.

*  Parameters:
*     dcb
*        Pointer to the DCB to which the ACB is to refer.
*     acb
*        Address at which to return the pointer to the new ACB.
*     status
*        The global status.

* Prior Requirements:
*     The DCB mutex must be locked.

*  Notes:
*     -  If STATUS is set on entry, then the routine will return a
*     value of NULL for the ACB pointer, although no further
*     processing will occur.
*     -  A value of NULL will also be returned if the routine fails for
*     any reason.

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
   ARY__DCB_ASSERT_MUTEX;

/* Local variables: */
   int i;                     /* Loop counter for dimensions */

/* Set an initial value of zero for the IACB argument. */
   if( acb ) *acb = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK || !acb ) return;

/* Ensure that information about the data object bounds (and number of
   dimensions), access mode, bad pixel flag and state are available in the
   DCB. */
   ary1Dfrm( dcb, status );
   ary1Dbnd( dcb, status );
   ary1Dmod( dcb, status );
   ary1Dbad( dcb, status );
   ary1Dsta( dcb, status );

   ARY__ACB_LOCK_MUTEX;

/* Allocate a new ACB. */
   *acb = ary1Ffs( ARY__ACBTYPE, status );
   if( *status == SAI__OK ){

/* Initialise the ACB entry to point to the DCB entry and indicate that
   this is a base array which is not mapped for access. */
      (*acb)->dcb = dcb;
      (*acb)->cut = 0;
      (*acb)->mcb = NULL;

/* Initialise the access control flags according to whether the data object
   can be modified (all flags set) or not (all flags cleared). */
      if( !strcmp( dcb->mode, "UPDATE" ) ) {
         (*acb)->access = (1 << ARY__MXACC) - 1;
      } else {
         (*acb)->access = 0;
      }

/* Enter the bad pixel flag value from the DCB. */
      (*acb)->bad = dcb->bad ||( !dcb->state );

/* Enter the array bounds information from the DCB, padding with 1's if
   necessary. */
      (*acb)->ndim = dcb->ndim;
      for( i = 0; i < (*acb)->ndim; i++ ){
         (*acb)->lbnd[ i ] = dcb->lbnd[ i ];
         (*acb)->ubnd[ i ] = dcb->ubnd[ i ];
      }
      for( ; i < ARY__MXDIM; i++ ){
         (*acb)->lbnd[ i ] = 1;
         (*acb)->ubnd[ i ] = 1;
      }

/* Enter the accumulated pixel shift information. */
      for( i = 0; i < ARY__MXDIM; i++ ){
         (*acb)->shift[ i ] = dcb->shift[ i ];
      }

/* Set the data transfer window for the new base array to the largest
   region possible. */
      (*acb)->dtwex = 1;
      for( i = 0; i < ARY__MXDIM; i++ ){
         (*acb)->ldtw[ i ] = NUM__MINI;
         (*acb)->udtw[ i ] = NUM__MAXI;
      }

/* Increment the DCB reference count. */
      dcb->refcount++;

/* If there was an error, then release the ACB and reset the ACB argument
   to NULL. */
   } else {
      *acb = ary1Rls( (AryObject *) *acb, status );
   }

   ARY__ACB_UNLOCK_MUTEX;

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Crnba", status );

}
