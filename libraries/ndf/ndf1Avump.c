#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include <string.h>

void ndf1Avump( int iax, NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Avump

*  Purpose:
*     Unmap an axis variance array.

*  Synopsis:
*     void ndf1Avump( int iax, NdfACB *acb, int *status )

*  Description:
*     This function unmaps an NDF axis variance array which has previously
*     been mapped for access with the ndf1Avmap function. An error will be
*     reported if the specified axis array is not currently mapped for
*     access. The NDF is identified by its index in the ACB.

*  Parameters:
*     iax
*        Zero-based index of the axis whose variance array is to be
*        unmapped.
*     acb
*        Pointer to the NDF entry in the ACB.
*     *status
*        The global status.

*  Notes:
*     -  This function will attempt to execute even if "status" is set on
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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int dce;              /* Data conversion error? */
   int tstat;            /* Temporary status variable */
   size_t el;            /* Number of mapped array elements */

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* Obtain an index to the data object entry in the DCB. */
   *status = SAI__OK;
   dcb = acb->dcb;

/* Check that the specified axis variance array is mapped for access.
   Report an error if it is not. */
   if( !acb->avmap[ iax ] ) {
      *status = NDF__NTMAP;
      msgSeti( "AXIS", iax + 1 );
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The variance array for axis ^AXIS of the NDF structure "
              "^NDF is not mapped for access through the specified "
              "identifier (possible programming error).", status );

/* See whether conversion to standard deviations has been performed
   when the variance array's values were mapped and if values are to be
   written back to the array. */
   } else {
      if( acb->avmst[ iax ] && ( !acb->cut ) && ( strcmp( acb->avmmd[ iax ], "READ" ) ) ) {

/* If so, then back-conversion is now required. Determine the number of
   array elements to convert from the size of the mapped array. */
         arySize( acb->avmid[ iax ], &el, status );

/* Convert the mapped standard deviations back to variances. */
         ndf1S2v( 1, acb->avmtp[ iax ], el, acb->avmpt[ iax ], &dce, status );

/* If a conversion error occurred, then report context information. */
         if( *status != SAI__OK ) {
            msgSeti( "AXIS", iax + 1 );
            ndf1Amsg( "NDF", acb );
            errRep( " ", "Error converting axis standard deviations "
                    "(errors) into variance values for axis ^AXIS of the "
                    "NDF structure ^NDF", status );
         }
      }

/* Unmap the axis variance array by annulling its ARY_ system identifier
   in the ACB (if a temporary array has been mapped, then it will be
   deleted at this point). */
      aryAnnul( acb->avmid + iax, status );

/* If successful (or the only error was to encounter negative standard
   deviation values), then note the array is no longer mapped. Reset
   the mapping pointer in the ACB to zero. */
      if( ( *status == SAI__OK ) || ( *status == NDF__NGSTD ) ) {
         acb->avmap[ iax ] = 0;
         acb->avmpt[ iax ] = 0;

/* Decrement the DCB mapping counts. */
         dcb->navmp[ iax ]--;
         dcb->nmap--;
      }
   }

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Call the error tracing function if appropriate. */
      } else {
         ndf1Trace( "ndf1Avump", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}

