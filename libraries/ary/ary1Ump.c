#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"

void ary1Ump( AryACB *acb, int *status ) {
/*
*+
*  Name:
*     ary1Ump

*  Purpose:
*     Unmap an array specified by its ACB.

*  Synopsis:
*     void ary1Ump( AryACB *acb, int *status )

*  Description:
*     The routine performs an unmapping operation on an ACB through
*     which an array has previously been mapped for access.  An
*     error will be reported if the array has not been mapped through
*     the specified ACB.

*  Parameters:
*     acb
*        Pointer to the ACB on which the unmapping operation is to
*        be performed.
*     status
*        The global status.

*  Notes:
*     -  The routine attempts to execute even if 'status' is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

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
   int tstat;                 /* Temporary status variable */
   AryDCB *dcb;               /* The data object entry for the ACB */

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* Obtain a pointer to the data object referred to by the DCB. */
   *status = SAI__OK;
   dcb = acb->dcb;

/* Handle each form of array in turn, calling the appropriate routine to
   unmap it. */

/* Primitive, simple, delta and scaled arrays.
   =========================================== */
   if( ( !strcmp( dcb->form, "PRIMITIVE" ) ) ||( !strcmp( dcb->form, "SIMPLE" ) ) ||( !strcmp( dcb->form, "SCALED" ) ) ||( !strcmp( dcb->form, "DELTA" ) ) ){
      ary1Umps( acb, status );

/* If the form entry in the DCB was not recognised, then report an error. */
   } else {
      *status = ARY__FATIN;
      msgSetc( "BADFORM", dcb->form );
      errRep( " ", "Unsupported array form '^BADFORM' found in Data "
              "Control Block (internal programming error).", status );
   }

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Call error tracing routine if appropriate. */
      } else {
         ary1Trace( "ary1Ump", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}
