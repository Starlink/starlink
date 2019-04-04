#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ary.h"
#include "ndf_ast.h"

void ndf1Crnbn( NdfDCB *dcb, NdfACB **acb, int *status ){
/*
*+
*  Name:
*     ndf1Crnbn

*  Purpose:
*     Create a new ACB base NDF entry from a DCB entry.

*  Synopsis:
*     void ndf1Crnbn( NdfDCB *dcb, NdfACB **acb, int *status )

*  Description:
*     This function creates a new ACB entry describing a base NDF which
*     refers to the data object whose DCB entry is supplied. The new NDF
*     acquires its attributes directly from the data object. The reference
*     count for the DCB entry is incremented by one.

*  Parameters:
*     dcb
*        Pointer to the data object entry in the DCB to which the ACB entry
*        is to refer.
*     *acb
*        Pointer to the new ACB entry.
*     *status
*        The global status.

*  Notes:
*     -  If "status" is set on entry, then the function will return a value
*     of zero for the "acb" parameter, although no further processing will
*     occur.
*     -  A value of zero will also be returned for the "acb" parameter if
*     the function fails for any reason.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   int valid;            /* Whether array identifier is valid */

/* Set an initial value of zero for the "acb" parameter. */
   *acb = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that information about the data object's data array is
   available in the DCB. If not, annul the DCB entry. */
   ndf1Dd( dcb, status );
   if( *status != SAI__OK ) ndf1Danl( 1, &dcb, status );

/* Obtain an index to a free slot in the ACB. */
   *acb = ndf1Ffs( NDF__ACBTYPE, status );
   if( *status == SAI__OK ) {

/* Initialise the ACB entry to point to the DCB entry. */
      (*acb)->dcb = dcb;

/* Initialise the access control flags if the data object can be
   modified. */
      if( !strcmp( dcb->mod, "UPDATE" ) ) (*acb)->access = ~0;

/* DATA component:
   ==============
   Clone an ARY_ system identifier for the data array. */
      aryClone( dcb->did, &(*acb)->did, status );

/* QUALITY component:
   ==================
   Set an initial null ARY_ system identifier for the new ACB entry"s
   quality array. */
      (*acb)->qid = NULL;

/* If quality information is available in the DCB, then see if the DCB
   ARY_ system identifier for the quality array is valid. If not, then
   the quality array does not exist. */
      if( dcb->kq ) {
         valid = aryValid( dcb->qid, status );
         if( *status == SAI__OK ) {

/* If valid, then clone a quality array identifier for the ACB. */
            if( valid ) aryClone( dcb->qid, &(*acb)->qid, status );
         }
      }

/* VARIANCE component:
   ==================
   Set an initial null ARY_ system identifier for the new ACB entry"s
   variance array. */
      (*acb)->vid = NULL;

/* If variance information is available in the DCB, then see if the DCB
   ARY_ system identifier for the variance array is valid. If not, then
   the variance array does not exist. */
      if( dcb->kv ) {
         valid = aryValid( dcb->vid, status );
         if( *status == SAI__OK ) {

/* If valid, then clone a variance array identifier for the ACB. */
            if( valid ) aryClone( dcb->vid, &(*acb)->vid, status );
         }
      }

/* If there was an error, then annul any identifiers which may have
   been acquired, release the slot allocated in the ACB. */
      if( *status != SAI__OK ) {
         aryAnnul( &(*acb)->did, status );
         aryAnnul( &(*acb)->qid, status );
         aryAnnul( &(*acb)->vid, status );
         *acb = ndf1Rls( ( NdfObject * ) *acb, status );

/* Otherwise, increment the DCB reference count. */
      } else {
         dcb->refct++;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Crnbn", status );

}

