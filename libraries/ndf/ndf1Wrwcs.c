#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"

void ndf1Wrwcs( AstFrameSet *iwcs, NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Wrwcs

*  Purpose:
*     Write WCS information to an entry in the ACB.

*  Synopsis:
*     void ndf1Wrwcs( AstFrameSet *iwcs, NdfACB *acb, int *status )

*  Description:
*     This function writes new WCS (World Coordinate System) information to
*     an entry in the ACB, over-writing any information that may already be
*     present.

*  Parameters:
*     iwcs
*        A pointer to an AST_ FrameSet which contains the coordinate system
*        information to be written. This should satisfy all the
*        requirements imposed by the NDF_ library. The information is fully
*        validated by this function before use.
*     acb
*        Pointer to the NDF entry in the ACB.
*     *status
*        The global status.

*  Notes:
*     - This function can only be used to modify the WCS information in a
*     base NDF. It returns without action if the ACB entry supplied
*     identifies an NDF section.
*     - This function stores pointers to independent copies of the WCS
*     information (not cloned pointers) in the DCB for future use.

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
   AstFrameSet *iwcsv;   /* Pointer to validated WCS information */
   NdfDCB *dcb;          /* Pointer to the NDF's DCB entry */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Validate the WCS information (this results in a copy of the
   information). */
   ndf1Vwcs( acb, iwcs, &iwcsv, status );
   if( *status == SAI__OK ) {

/* If the NDF is a section, we do nothing more. Otherwise, obtain an
   index to the data object entry in the DCB and store the validated
   WCS information in the DCB entry. */
      if( *status == SAI__OK ) {
         if( !acb->cut ) {
            dcb = acb->dcb;
            ndf1Wwrt( iwcsv, dcb, status );
         }
      }
   }

/* Annul the pointer to the validated WCS information. */
   iwcsv = astAnnul( iwcsv );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Wrwcs", status );

}

