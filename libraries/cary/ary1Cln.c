#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"

void ary1Cln( AryACB *acb1, AryACB **acb2, int *status ) {
/*
*+
*  Name:
*     ary1Cln

*  Purpose:
*     Clone an ACB entry.

*  Synopsis:
*     void ary1Cln( AryACB *acb1, AryACB **acb2, int *status )

*  Description:
*     This function "clones" an ACB, producing a new ACB which is
*     a duplicate of the original (except that it is not mapped for
*     access if the original was). The data object reference count in
*     the DCB is incremented by one.

*  Parameters:
*     iacb1
*        The ACB entry to be cloned.
*     iacb2
*        The new ACB entry.
*     status
*        The global status.

*  Notes:
*     -  If "status" is set on entry, then a value of NULL will be
*     returned for the "acb2" argument, although no further processing
*     will occur.
*     -  A value of NULL will also be returned for the "acb2" argument if
*     the routine fails for any reason.

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
   AryDCB *dcb;               /* Data Control Block */

/* Set an initial value of NULL for the "acb2" argument. */
   *acb2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Find a free slot in the ACB. Reset the IACB2 argument to zero if no slot
   could be found. */
   *acb2 = ary1Ffs( ARY__ACBTYPE, status );
   if( *status != SAI__OK ){
      *acb2 = NULL;

/* Transfer the array attributes from the old ACB entry to the new one. */
   } else {

/* ...Whether the array is a cut. */
      (*acb2)->cut = acb1->cut;

/* ...Access control flags. */
      (*acb2)->access = acb1->access;

/* ...Index to data object entry in the DCB. */
      dcb = acb1->dcb;
      (*acb2)->dcb = dcb;

/* ...Number of dimensions. */
      (*acb2)->ndim = acb1->ndim;

/* ...Array bounds, data transfer window bounds and accumulated pixel
   shifts. */
      for( i = 0; i < ARY__MXDIM; i++ ){
         (*acb2)->lbnd[ i ] = acb1->lbnd[ i ];
         (*acb2)->ubnd[ i ] = acb1->ubnd[ i ];
         (*acb2)->ldtw[ i ] = acb1->ldtw[ i ];
         (*acb2)->udtw[ i ] = acb1->udtw[ i ];
         (*acb2)->shift[ i ] = acb1->shift[ i ];
      }

/* ...Whether the data transfer window exists. */
      (*acb2)->dtwex = acb1->dtwex;

/* ...Bad pixel flag. */
      (*acb2)->bad = acb1->bad;

/* Mark the new ACB entry as not being mapped for access and increment the
   data object reference count in the DCB. */
      (*acb2)->mcb = NULL;
      dcb->refcount++;
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Cln", status );

}
