#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryBad( Ary *ary, int check, int *bad, int *status ) {
/*
*+
*  Name:
*     aryBad

*  Purpose:
*     Determine if an array may contain bad pixels.

*  Synopsis:
*     void aryBad( Ary *ary, int check, int *bad, int *status )

*  Description:
*     This function returns a boolean value indicating whether an array
*     may contain bad pixels for which checks must be made when its
*     values are processed. Only if the returned value is zero can
*     such checks be omitted.  If the "check" argument to this function
*     is set non-zero, then it will perform an explicit check (if
*     necessary) to see whether bad pixels are actually present.

*  Parameters:
*     ary
*        Array identifier.
*     check
*        If non-zero, an explicit check is performed to see if bad pixels
*        are actually present.
*     bad
*        Returned holding a flag indicating whether it is necessary to
*        check for bad pixels when processing the array's values.
*     status
*        The global status.

*  Notes:
*     -  If "check" is set to zero, then the returned value of "bad" will
*     indicate whether bad pixels might be present and should therefore
*     be checked for during subsequent processing. However, even if "bad"
*     is returned non-zero in such circumstances, it is still possible
*     that there may not actually be any bad pixels present (for
*     instance, in an array section, the region of the base array
*     accessed might happen to avoid all the bad pixels).
*     -  If "check" is set non-zero, then an explicit check will be made,
*     if necessary, to ensure that "bad" is only returned non-zero if bad
*     pixels are actually present.
*     -  If the array is mapped for access through the identifier
*     supplied, then the value of "bad" will refer to the actual mapped
*     values. It may differ from its original (unmapped) value if
*     conversion errors occurred during the mapping process, or if an
*     initialisation option of '/ZERO' was specified for an array which
*     was initially undefined, or if the mapped values have subsequently
*     been modified.
*     -  The "bad" argument will always be returned holding a non-zero value
*     if the array is in an undefined state.

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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );

/* Determine whether bad pixels are present. */
   ary1Bad( acb, check, bad, status );

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryBad: Error determining if an array may contain bad "
              "pixels.", status );
      ary1Trace( "aryBad", status );
   }

}
