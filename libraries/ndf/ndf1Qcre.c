#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include <string.h>
#include "mers.h"

void ndf1Qcre( NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Qcre

*  Purpose:
*     Create a quality component for an NDF, if necessary.

*  Synopsis:
*     void ndf1Qcre( NdfACB *acb, int *status )

*  Description:
*     This function ensures that a quality component containing a quality
*     array exists for an NDF, creating one if necessary. The array is
*     created using the default quality storage form held in the DCB. An
*     HDS locator for the quality structure is stored in the DCB by this
*     function and ARY_ system identifiers for the new array (and
*     appropriate sections thereof) are entered into the DCB and also into
*     those ACB entries which refer to the NDF data object in question. The
*     NDF is identified to this function by its ACB entry.

*  Parameters:
*     acb
*        Pointer to the ACB entry for the NDF.
*     *status
*        The global status.

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
   AryPlace *place;      /* Placeholder for quality array */
   NdfACB *acbt;         /* Pointer to ACB object to test */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   NdfDCB *dcbt;         /* Pointer to DCB object to test */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower pixel index bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper pixel index bounds */
   int islot;            /* Slot index */
   int ndim;             /* Number of NDF dimensions */
   int next;             /* Next ACB entry to test */
   int there;            /* Whether the quality array exists */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that quality information is available in the DCB and ACB. */
   ndf1Qimp( acb, status );

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* See if the quality component locator is valid. If not, then the
   component does not exist and must be created. */
   if( *status == SAI__OK ) {
      if( !dcb->qloc ) {

/* Create the quality component if necessary and obtain a locator to it
   to retain in the DCB. */
         datNew( dcb->loc, "QUALITY", "QUALITY", 0, NULL, status );
         datFind( dcb->loc, "QUALITY", &dcb->qloc, status );

/* Note that the quality array does not yet exist. */
         dcb->qid = NULL;
      }
   }

/* See if the ARY_ system identifier for the quality array is valid. If
   not, then the quality array does not exist and must be created. */
   there = aryValid( dcb->qid, status );
   if( *status == SAI__OK ) {
      if( !there ) {

/* Obtain the NDF bounds from its data array. */
         aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Tune HDS for the expected maximum number of components in the quality
   structure. */
         hdsTune( "NCOMP", 3, status );

/* Obtain a placeholder for the quality array, then handle the creation
   of each form of array in turn using the default storage form stored
   in the DCB. */
         aryPlace( dcb->qloc, "QUALITY", &place, status );

/* Primitive array.
   =============== */
         if( !strcmp( dcb->qfrm, "PRIMITIVE" ) ) {
            aryNewp( "_UBYTE", ndim, ubnd, &place, &dcb->qid, status );

/* Simple array.
   ============ */
         } else if( !strcmp( dcb->qfrm, "SIMPLE" ) ) {
            aryNew( "_UBYTE", ndim, lbnd, ubnd, &place, &dcb->qid, status );

/* If the default quality storage form entry in the DCB was not
   recognised, then report an error. */
         } else {
            *status = NDF__FATIN;
            msgSetc( "BADFORM", dcb->qfrm );
            errRep( " ", "Invalid array storage form '^BADFORM' "
                    "encountered in the NDF_ system Data Control Block "
                    "(internal programming error).", status );
         }

/* Loop to identify all the ACB entries which refer to this DCB entry. */
         next = 0;
         islot = -1;
         NDF__ACB_LOCK_MUTEX;
         acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
         while( ( *status == SAI__OK ) && ( next != -1 ) ){
            islot = next;

/* Select those entries with the correct DCB index. */
            dcbt = acbt->dcb;
            if( dcbt == dcb ) {

/* Create a section from the quality array which matches the ACB's data
   array section and store the resulting ARY_ system identifier in the
   ACB. */
               ndf1Ssdup( dcb->qid, acbt->did, &acbt->qid, status );
            }
            acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
         }
         NDF__ACB_UNLOCK_MUTEX;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Qcre", status );

}

