#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "mers.h"

void ndf1Cut( NdfACB *acb1, int ndim, const hdsdim lbnd[],
              const hdsdim ubnd[], NdfACB **acb2, int *status ){
/*
*+
*  Name:
*     ndf1Cut

*  Purpose:
*     Cut a section from an existing NDF with an entry in the ACB.

*  Synopsis:
*     void ndf1Cut( NdfACB *acb1, int ndim, const hdsdim lbnd[],
*                   const hdsdim ubnd[], NdfACB **acb2, int *status )

*  Description:
*     This function produces a new NDF entry in the ACB representing a
*     section from an existing NDF (which may itself be a section). The
*     bounds and dimensionality of the new NDF are specified in the call to
*     this function and the resulting NDF has access to a subset (although
*     possibly a null subset) of the data accessible to the initial NDF
*     from which it is derived.

*  Parameters:
*     acb1
*        Pointer to the ACB entry for an existing NDF.
*     ndim
*        Number of dimensions for the new section.
*     lbnd
*        Lower dimension bounds for the section. The supplied "lbnd" array
*        should have at least "ndim" elements.
*     ubnd
*        Upper dimension bounds for the section. The supplied "ubnd" array
*        should have at least "ndim" elements.
*     *acb2
*        Pointer to the ACB entry for the new section.
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
   int idim;             /* Pixel axis index */
   int valid;            /* Whether array identifier is valid */
   int64_t secmax;       /* Maximum number of pixels in section */
   int64_t secsiz;       /* Number of pixels in section */

/* Set an initial value for the "acb2" parameter. */
   *acb2 = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get the number of pixels in the section. */
   secsiz = 1;
   for( idim = 0; idim < ndim; idim++ ){
      secsiz = secsiz*( ubnd[ idim ] - lbnd[ idim ] + 1 );
   }

/* Get the maximum number of pixels in a section. "Ndf_TCB_secmax" is in units
   of maga-pixels to allow use of INTEGER*4 tuning parameters. */
   secmax = Ndf_TCB_secmax;
   secmax *= 1000000.0;

/* Report an error if the section size it too large. Also check for
   negative section sizes in case of overflow. */
   if( secsiz > secmax ) {
      *status = NDF__BGSEC;
      msgSetk( "S", secsiz );
      errRep( " ", "Requested NDF section contains too many pixels (^S).",
              status );
      msgSetk( "L", secmax );
      errRep( " ", "Current value of NDF tuning parameter SECMAX limits "
              "sections to ^L pixels.", status );

   } else if( secsiz < 0 ) {
      *status = NDF__BGSEC;
      errRep( " ", "Requested NDF section contains too many pixels.", status );
      msgSetk( "L", secmax );
      errRep( " ", "Current value of NDF tuning parameter SECMAX limits "
              "sections to ^L pixels.", status );
   }

/* Obtain an index to a free slot for the new NDF in the ACB. */
   *acb2 = ndf1Ffs( NDF__ACBTYPE, status );
   if( *status == SAI__OK ) {

/* Mark the new entry as a section. */
      (*acb2)->cut = 1;

/* Transfer the access control flags. */
      (*acb2)->access = acb1->access;

/* Transfer the index to the data object entry in the DCB. */
      (*acb2)->dcb = acb1->dcb;

/* Propagate the quality component control flags. */
      (*acb2)->qbb = acb1->qbb;
      (*acb2)->isqbb = acb1->isqbb;

/* DATA component:
   ==============
   Form a new section from the data array of the old NDF, storing the
   new ARY_ system identifier in the new ACB entry. */
      arySect( acb1->did, ndim, lbnd, ubnd, &(*acb2)->did, status );

/* QUALITY component:
   ==================
   Set an initial null value for the new ACB entry's quality array
   identifier. */
      (*acb2)->qid = NULL;

/* See if the ARY_ system quality array identifier in the old ACB entry
   is valid. */
      valid = aryValid( acb1->qid, status );
      if( *status == SAI__OK ) {

/* If so, then form a new section from it, storing the new identifier in
   the new ACB entry. */
         if( valid ) {
            arySect( acb1->qid, ndim, lbnd, ubnd, &(*acb2)->qid,
                     status );
         }
      }

/* VARIANCE component:
   ==================
   Set an initial null value for the new ACB entry's variance array
   identifier. */
      (*acb2)->vid = NULL;

/* See if the ARY_ system variance array identifier in the old ACB entry
   is valid. */
      valid = aryValid( acb1->vid, status );
      if( *status == SAI__OK ) {

/* If so, then form a new section from it, storing the new identifier in
   the new ACB entry. */
         if( valid ) {
            arySect( acb1->vid, ndim, lbnd, ubnd, &(*acb2)->vid,
                     status );
         }
      }

/* If an error occurred, then annul any identifiers which may have been
   acquired and release the new ACB slot. */
      if( *status != SAI__OK ) {
         aryAnnul( &(*acb2)->did, status );
         aryAnnul( &(*acb2)->qid, status );
         aryAnnul( &(*acb2)->vid, status );
         *acb2 = ndf1Rls( ( NdfObject * ) *acb2, status );

/* Otherwise, increment the data object reference count. */
      } else {
         dcb = (*acb2)->dcb;
         dcb->refct++;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Cut", status );

}

