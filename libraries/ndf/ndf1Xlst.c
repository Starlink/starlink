#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Xlst( NdfACB *acb, int mxextn, char extn[][ DAT__SZNAM + 1 ],
               int *nextn, int *status ){
/*
*+
*  Name:
*     ndf1Xlst

*  Purpose:
*     Obtain a list of the available extension names for an ACB entry.

*  Synopsis:
*     void ndf1Xlst( NdfACB *acb, int mxextn, char extn[][ DAT__SZNAM + 1
*                    ], int *nextn, int *status )

*  Description:
*     This function returns the names of the NDF extensions associated with
*     the specified ACB entry.

*  Parameters:
*     acb
*        Pointer to the ACB entry for which extension information is
*        required.
*     mxextn
*        The maximum number of extension names to return.
*     extn
*        Returned holding the names of the extensions in the DCB entry.
*     *nextn
*        The number of names returned in "extn".
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *loc = NULL;   /* Extension component locator */
   NdfDCB *dcb;          /* Input data object DCB */
   int iextn;            /* Loop counter for components */

/* Set an initial value for the "nextn" parameter. */
   *nextn = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the DCB entry of the input data object. */
   dcb = acb->dcb;

/* Ensure that extension (MORE) structure information is available in
   the DCB. */
   ndf1Dx( dcb, status );
   if( *status == SAI__OK ) {

/* If there is no extension (MORE) structure in the NDF, then return an
   empty list. */
      if( dcb->xloc ) {

/* Get the number of extensions in the data object. */
         datNcomp( dcb->xloc, nextn, status );

/* Limit this to the size of the supplied "extn" array. */
         *nextn = NDF_MIN( *nextn, mxextn );

/* Loop round the required extensions. */
         for( iextn = 0; iextn < *nextn; iextn++ ){

/* Obtain a locator to the IEXTN"th extension structure component. */
            datIndex( dcb->xloc, iextn + 1, &loc, status );

/* Obtain its name. */
            datName( loc, extn[ iextn ], status );

/* Annul the locator. */
            datAnnul( &loc, status );
         }

      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Xlst", status );

}

