#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ary.h"

void ndf1Cln( NdfACB *acb1, NdfACB **acb2, int *status ){
/*
*+
*  Name:
*     ndf1Cln

*  Purpose:
*     Clone an ACB entry.

*  Synopsis:
*     void ndf1Cln( NdfACB *acb1, NdfACB **acb2, int *status )

*  Description:
*     This function "clones" an ACB entry, producing a new entry which is a
*     duplicate of the original (except that no components are mapped for
*     access). The data object reference count in the DCB is incremented by
*     one.

*  Parameters:
*     acb1
*        Pointer to the ACB entry to be cloned.
*     *acb2
*        Pointer to the new ACB entry.
*     *status
*        The global status.

*  Notes:
*     -  If "status" is set on entry, then a value of zero will be returned
*     for the "acb2" parameter, although no further processing will occur.
*     -  A value of zero will also be returned for the "acb2" parameter if
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int valid;            /* Whether array identifier is valid */

/* Set an initial NULL value for the "acb2" parameter. */
   *acb2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Find a free slot in the ACB. Reset the "acb2" parameter to NULL if no
   slot could be found. */
   *acb2 = ndf1Ffs( NDF__ACBTYPE, status );
   if( *status != SAI__OK ) {
      *acb2 = NULL;

/* Transfer the NDF attributes from the old ACB entry to the new one. */
   } else {

/* ...Whether the ACB entry describes an NDF section. */
      (*acb2)->cut = acb1->cut;

/* ...Access control flags. */
      (*acb2)->access = acb1->access;

/* ...Index to data object entry in the DCB. */
      (*acb2)->dcb = acb1->dcb;

/* ...Quality component control flags. */
      (*acb2)->isqbb = acb1->isqbb;
      (*acb2)->qbb = acb1->qbb;

/* DATA component:
   ==============
   Clone the ARY_ system identifier for the NDF's data array. */
      aryClone( acb1->did, &(*acb2)->did, status );

/* QUALITY component:
   ==================
   See if the ARY_ system identifier for the NDF's quality array in
   the input ACB entry is valid. */
      valid = aryValid( acb1->qid, status );
      if( *status == SAI__OK ) {

/* If so, then clone it. */
         if( valid ) {
            aryClone( acb1->qid, &(*acb2)->qid, status );

/* Otherwise, set a null identifier in the new ACB entry. */
         } else {
            (*acb2)->qid = NULL;
         }
      }

/* VARIANCE component:
   ==================
   See if the ARY_ system identifier for the NDF's variance array in
   the input ACB entry is valid. */
      valid = aryValid( acb1->vid, status );
      if( *status == SAI__OK ) {

/* If so, then clone it. */
         if( valid ) {
            aryClone( acb1->vid, &(*acb2)->vid, status );

/* Otherwise, set a null identifier in the new ACB entry. */
         } else {
            (*acb2)->vid = NULL;
         }
      }

/* If an error occurred, then annul any identifiers which might have
   been acquired and release the new ACB slot. */
      if( *status != SAI__OK ) {
         aryAnnul( &(*acb2)->did, status );
         aryAnnul( &(*acb2)->qid, status );
         aryAnnul( &(*acb2)->vid, status );
         *acb2 = ndf1Rls( ( NdfObject * ) *acb2, status );

/* Otherwise, increment the data object reference count in the DCB. */
      } else {
         dcb = acb1->dcb;
         dcb->refct++;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Cln", status );

}

