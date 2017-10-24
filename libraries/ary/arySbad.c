#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void arySbad( int bad, Ary *ary, int *status ) {
/*
*+
*  Name:
*     arySbad

*  Purpose:
*     Set the bad-pixel flag for an array.

*  Synopsis:
*     void arySbad( int bad, Ary *ary, int *status )

*  Description:
*     This function sets the value of the bad-pixel flag for an array
*     A call to this routine with "bad" set non-zero declares that the
*     specified array may contain bad pixel values for which checks
*     must be made by algorithms which subsequently processes its
*     values.  A call with "bad" set to zero declares that there are
*     definitely no bad values present and that subsequent checks for
*     such values may be omitted.

*  Parameters:
*     bad
*        Bad-pixel flag value to be set.
*     ary
*        Array identifier.
*     status
*        The global status.

*  Notes:
*     -  If the array is mapped for access when this routine is called,
*     then the bad-pixel flag will be associated with the mapped
*     values. This information will only be transferred to the actual
*     data object when the array is unmapped (but only if it was mapped
*     for UPDATE or WRITE access). The value transferred may be
*     modified if conversion errors occur during the unmapping process.

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
   AryMCB *mcb;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 0, 1, status );

/* Check that WRITE access to the array is permitted. */
   ary1Chacc( acb, "WRITE", status );
   if( *status == SAI__OK ){

/* Obtain a pointer to the MCB for the array. If this is non-NULL,
   then the array is currently mapped, so the bad pixel flag value
   being set refers to the mapped data. */
      mcb = acb->mcb;
      if( mcb ){

/* Enter the bad pixel flag value into the MCB. This information will then
   be transferred to the corresponding ACB entry when the data are
   unmapped. */
         mcb->bad = bad;
         mcb->pbad = bad;

/* If the array is not mapped, then set the bad pixel flag for the ACB
   entry directly. */
      } else {
         ary1Sbd( bad, acb, status );
      }
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "arySbad: Error setting the bad-pixel flag value for an"
              " array.", status );
      ary1Trace( "arySbad", status );
   }

}
