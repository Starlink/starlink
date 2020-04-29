#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Anl( NdfACB **acb, int *status ){
/*
*+
*  Name:
*     ndf1Anl

*  Purpose:
*     Annul an NDF entry in the ACB.

*  Synopsis:
*     void ndf1Anl( NdfACB **acb, int *status )

*  Description:
*     This function annuls an NDF entry in the ACB. If any NDF component is
*     currently mapped for access through this entry, then it is first
*     unmapped. The ACB entry is then annulled, i.e. the slot is released
*     and made available for re-use. If, as a result, the reference count
*     for the associated data object drops to zero, then the object will be
*     released from the NDF_ system and may be deleted, according to its
*     disposal mode.

*  Parameters:
*     *acb
*        Pointer to the ACB to be annulled. NULL is returned.
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

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

/* Begin a new error reporting environment. */
   errBegin( status );

/* Check that the ACB supplied is valid and report an error if it
   is not. */
   if( !ndf1IsValid( *acb ) ) {
      *status = NDF__FATIN;
      msgSetc( "ROUTINE", "ndf1Anl" );
      errRep( " ", "Function ^ROUTINE called with an invalid 'acb' "
              "pointer - internal programming error.", status );

/* Unmap all the NDF components. */
   } else {
      ndf1Ump( *acb, "*", status );

/* Release the ARY_ system identifiers held in the ACB. */

/* DATA component:
   ============== */
      aryAnnul( &(*acb)->did, status );

/* QUALITY component:
   ==================
   See if the quality array identifier is valid. If so, then annul it. */
      valid = aryValid( (*acb)->qid, status );
      if( *status == SAI__OK ) {
         if( valid ) aryAnnul( &(*acb)->qid, status );
      }

/* VARIANCE component:
   ==================
   See if the variance array identifier is valid. If so, then annul it. */
      valid = aryValid( (*acb)->vid, status );
      if( *status == SAI__OK ) {
         if( valid ) aryAnnul( &(*acb)->vid, status );
      }

/* Annul the associated data object and set the data object index in
   the ACB to zero. */
      ndf1Danl( 1, &(*acb)->dcb, status );
      (*acb)->dcb = 0;

/* Release the ACB slot. */
      *acb = ndf1Rls( ( NdfObject * ) *acb, status );
   }

/* Reset the ACB index. */
   *acb = 0;

/* Call error tracing function. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Anl", status );

/* End the error reporting environment. */
   errEnd( status );

}

