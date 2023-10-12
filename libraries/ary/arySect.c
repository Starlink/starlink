#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void arySect( Ary *ary1, int ndim, const hdsdim *lbnd, const hdsdim *ubnd,
              Ary **ary2, int *status ) {
/*
*+
*  Name:
*     arySect

*  Purpose:
*     Create an array section.

*  Synopsis:
*     void arySect( Ary *ary1, int ndim, const hdsdim *lbnd,
*                   const hdsdim *ubnd, Ary **ary2, int *status )

*  Description:
*     This function creates a new array section which refers to a
*     selected region of an existing array (or array section). The
*     section may be larger or smaller in extent than the original
*     array.

*  Parameters:
*     ary
*        Identifier for the initial array.
*     ndim
*        Number of dimensions for new section. This is the length of the
*        "lbnd" and Ubnd" arrays.
*     lbnd
*        Lower pixel-index bounds for the new section.
*     ubnd
*        Upper pixel-index bounds for the new section.
*     ary2
*        Address of a variable in which to return an identifier for the
*        new section.
*     status
*        The global status.

*  Notes:
*     -  If the supplied array is locked read-only by the current thread
*     an attempt will be made to promote the lock to a read-write lock
*     and an error will be reported if this attempt fails. This promotion
*     is necessary because the meta-data associated with the cut (e.g.
*     pixel shift etc) can be modified using the returned identifier.
*     -  The number of section dimensions need not match the number of
*     dimensions in the initial array. Pixel-index bounds will be
*     padded with 1's as necessary to identify the pixels to which the
*     new section should refer.
*     -  Note that sections which extend beyond the pixel-index bounds
*     of the initial array will be padded with bad pixels.
*     -  If this routine is called with "Status" set, then a value of
*     NULL will be returned for the "ary2" argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.

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
   AryACB *acb1;                 /* Pointer to initial array in the ACB */
   AryACB *acb2 = NULL;          /* Pointer to new array in the ACB */

/* Set an initial value for the "ary2" argument. */
   *ary2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the initial array identifier. */
   acb1 = (AryACB *) ary1Impid( ary1, 1, 1, 1, status );

/* Check the bounds of the region to select for validity. */
   ary1Vbnd( ndim, lbnd, ubnd, status );

/* Create an ACB entry for the new array. */
   if( *status == SAI__OK ){
      ARY__DCB_LOCK_MUTEX;
      ARY__ACB_LOCK_MUTEX;
      ary1Cut( acb1, ndim, lbnd, ubnd, &acb2, status );
      ARY__ACB_UNLOCK_MUTEX;
      ARY__DCB_UNLOCK_MUTEX;
   }

/* Export an identifier for the new array. */
   *ary2 = ary1Expid( (AryObject *) acb2, status );

/* If an error occurred, then set a value of NULL for the "ary2"
   argument, report context information and call the error tracing
   routine. */
   if( *status != SAI__OK ){
      *ary2 = NULL;
      errRep( " ", "arySect: Error creating an array section.",
              status );
      ary1Trace( "arySect", status );
   }

}
