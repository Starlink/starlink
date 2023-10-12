#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"
#include <string.h>

void aryMap( Ary *ary, const char *type, const char *mmod, void **pntr,
             size_t *el, int *status ) {
/*
*+
*  Name:
*     aryMap

*  Purpose:
*     Obtain mapped access to an array.

*  Synopsis:
*     void aryMap( Ary *ary, const char *type, const char *mmod, void **pntr,
*                  size_t *el, int *status )

*  Description:
*     This function obtains mapped access an array, returning a pointer
*     to the mapped values and a count of the number of elements
*     mapped.

*  Parameters:
*     ary
*        Array identifier.
*     type
*        The numerical data type required for access (e.g. "_REAL").
*     mmod
*        The mapping mode for access to the array: "READ", "UPDATE" or
*        "WRITE", with an optional initialisation mode "/BAD" or
*        "/ZERO" appended.
*     pntr
*        Returned holding a pointer to the mapped values.
*     el
*        Returned holding the number of elements mapped.
*     status
*        The global status.

*  Notes:
*     -  If the array is a scaled array, the returned mapped values will
*     be the stored array values multiplied by the scale factor and shifted
*     by the zero term.
*     -  If the array is a delta (i.e. compressed) array, the returned mapped
*     values will be the uncompressed array values.
*     -  Currently, only READ access is available for scaled and
*     compressed arrays. An error will be reported if an attempt is made to
*     get WRITE or UPDATE access to a scaled or compressed array.

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
   AryACB *acb;               /* Index to array entry in ACB */
   char mode[ARY__SZMOD+1];   /* Data access mode */
   char inopt[ARY__SZIOP+1];  /* Initialisation option */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Validate the mapping mode string, decomposing it into an access mode and
   an initialisation option. */
   ary1Vmmd( mmod, mode, inopt, status );

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, !strcmp( mode, "READ" ), 1, status );

   ARY__DCB_LOCK_MUTEX;

/* Map the array data. */
   ary1Maps( acb, type, 0, mode, inopt, pntr, NULL, status );

   ARY__DCB_UNLOCK_MUTEX;

/* Check we can de-reference "acb" safely. */
   if( *status == SAI__OK ) {

/* Calculate the number of mapped data elements. */
      ary1Nel( acb->ndim, acb->lbnd, acb->ubnd, el, status );
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryMap: Error obtaining mapped access to an array.",
              status );
      ary1Trace( "aryMap", status );
   }

}
