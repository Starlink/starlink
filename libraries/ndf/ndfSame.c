#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfSame_( int indf1, int indf2, int *same, int *isect, int *status ){
/*
*+
*  Name:
*     ndfSame

*  Purpose:
*     Enquire if two NDFs are part of the same base NDF.

*  Synopsis:
*     void ndfSame( int indf1, int indf2, int *same, int *isect, int *status )

*  Description:
*     This function determines whether two NDF identifiers refer to parts
*     of the same base NDF. If so, it also determines whether their
*     transfer windows intersect.

*  Parameters:
*     indf1
*        Identifier for the first NDF (or NDF section).
*     indf2
*        Identifier for the second NDF (or NDF section).
*     *same
*        Returned holding the whether the identifiers refer to parts of the
*        same base NDF.
*     *isect
*        Returned holding the whether their transfer windows intersect.
*     *status
*        The global status.

*  Notes:
*     -  If the transfer windows of the two NDFs (or NDF sections)
*     intersect, then (i) they both refer to the same base NDF, and (ii)
*     altering values in an array component of one of the NDFs can result
*     in the values in the corresponding component of the other NDF
*     changing in consequence. Thus, the array components of the two NDFs
*     are not mutually independent.

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
   NdfACB *acb1;         /* 1st NDF ACB */
   NdfACB *acb2;         /* 2nd NDF ACB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the two NDF identifiers. */
   ndf1Impid( indf1, &acb1, status );
   ndf1Impid( indf2, &acb2, status );
   if( *status == SAI__OK ) {

/* Use the ARY_ system to see if the data arrays of the two NDFs refer
   to the same base array. */
      arySame( acb1->did, acb2->did, same, isect, status );
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfSame: Error enquiring if two NDFs are part of the "
              "same base NDF.", status );
      ndf1Trace( "ndfSame", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

