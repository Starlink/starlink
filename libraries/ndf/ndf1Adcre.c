#include <string.h>
#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

void ndf1Adcre( hdsdim lbnd, hdsdim ubnd, int iax, NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Adcre

*  Purpose:
*     Create an axis data array.

*  Synopsis:
*     void ndf1Adcre( hdsdim lbnd, hdsdim ubnd, int iax, NdfDCB *dcb,
*                     int *status )

*  Description:
*     This function creates an axis data array and initialises it to define
*     the default coordinate system for an NDF axis.

*  Parameters:
*     lbnd
*        Lower pixel-index bound of the axis data array.
*     ubnd
*        Upper pixel-index bound of the axis data array.
*     iax
*        Axis number.
*     dcb
*        Pointer to the data object entry in the DCB.
*     *status
*        The global status.

*  Prior Requirements:
*     -  The axis structure element which will hold the new axis data array
*     must already exist and have a locator in the DCB. The axis data array
*     should not already exist. This function does not check for these
*     conditions itself.
*     -  DCB information must already be available for the axis whose data
*     array is being created in order to establish the default array
*     attributes. (This function cannot establish this information itself
*     by calling ndf1Dad because the absence of a data array within an
*     existing axis structure will be detected as an arror by ndf1Dad).

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
   AryPlace *place;      /* ARY_ system placeholder */
   size_t el;            /* Number of mapped elements */
   void *pntr;           /* Pointer to mapped array */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an ARY_ system placeholder for the data array in the
   appropriate cell of the NDF's axis structure. */
   aryPlace( dcb->aloc[ iax ], "DATA_ARRAY", &place, status );

/* Create the data array with the required storage form, numeric type
   and shape. */

/* PRIMITIVE:
   =========
   Create a primitive array. */
   if( !strcmp( dcb->adfrm[ iax ], "PRIMITIVE" ) ) {
      aryNewp( dcb->adtyp[ iax ], 1, &ubnd, &place, dcb->adid + iax, status );

/* Map the array, initialise its values, and then unmap it. */
      aryMap( dcb->adid[ iax ], dcb->adtyp[ iax ], "WRITE", &pntr, &el, status );
      ndf1Adini( dcb->adtyp[ iax ], lbnd, ubnd, pntr, status );
      aryUnmap( dcb->adid[ iax ], status );

/* SIMPLE:
   ======
   Create a simple array. */
   } else if( !strcmp( dcb->adfrm[ iax ], "SIMPLE" ) ) {
      aryNew( dcb->adtyp[ iax ], 1, &lbnd, &ubnd, &place, dcb->adid + iax,
              status );

/* Map the array, initialise its values, and then unmap it. */
      aryMap( dcb->adid[ iax ], dcb->adtyp[ iax ], "WRITE", &pntr, &el, status );
      ndf1Adini( dcb->adtyp[ iax ], lbnd, ubnd, pntr, status );
      aryUnmap( dcb->adid[ iax ], status );

/* If the axis data array form stored in the DCB was not recognised,
   then report an error. */
   } else {
      *status = NDF__FATIN;
      msgSetc( "BADFORM", dcb->adfrm[ iax ] );
      errRep( " ", "Invalid axis array storage form '^BADFORM' encountered "
              "in the NDF_ system Data Control Block (internal programming "
              "error).", status );
   }

/* If an error occurred, then erase any axis data array which may have
   been created. */
   if( *status != SAI__OK ) aryDelet( dcb->adid + iax, status );

/* Note if the axis data array information in the DCB is valid. */
   dcb->kad[ iax ] = ( *status == SAI__OK );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Adcre", status );

}

