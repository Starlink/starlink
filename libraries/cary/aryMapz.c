#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"
#include <string.h>

void aryMapz( Ary *ary, const char *type, const char *mmod, void **rpntr,
              void **ipntr, size_t *el, int *status ) {
/*
*+
*  Name:
*     aryMapz

*  Purpose:
*     Obtain complex mapped access to an array.

*  Synopsis:
*     void aryMapz( Ary *ary, const char *type, const char *mmod, void **rpntr,
*                   void **ipntr, size_t *el, int *status )

*  Description:
*     This function obtains complex mapped access to an array, returning
*     pointers to the real and imaginary values and a count of the
*     number of elements mapped.

*  Parameters:
*     ary
*        Array identifier.
*     type
*        The numerical data type required for accessing the array (e.g.
*        '_REAL').
*     mmod
*        The mapping mode for access to the array: 'READ', 'UPDATE' or
*        'WRITE', with an optional initialisation mode '/BAD' or
*        '/ZERO' appended.
*     rpntr
*        Returned holding a pointer to the mapped real (i.e. non-imaginary)
*        values.
*     ipntr
*        Returned holding a pointer to the mapped imaginary values.
*     el
*        Returned holding the number of elements mapped.
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
   char mode[ARY__SZMOD+1];
   char inopt[ARY__SZIOP+1];

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Validate the mapping mode string, decomposing it into an access mode and
   an initialisation option. We need to do this here so that we can
   decide whether to check for a read-only lock or a read-write lock when
   importing the Ary pointer. */
   ary1Vmmd( mmod, mode, inopt, status );

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, !strcmp( mode, "READ" ), 1, status );

/* Map the array data. */
   ary1Maps( acb, type, 1, mode, inopt, rpntr, ipntr, status );

/* Calculate the number of mapped data elements. */
   ary1Nel( acb->ndim, acb->lbnd, acb->ubnd, el, status );

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryMapz: Error obtaining complex mapped access to an array.",
              status );
      ary1Trace( "aryMapz", status );
   }

}
