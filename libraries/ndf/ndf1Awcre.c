#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include <string.h>
#include "mers.h"

void ndf1Awcre( int iax, NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Awcre

*  Purpose:
*     Ensure that an NDF axis width array exists, creating one if
*     necessary.

*  Synopsis:
*     void ndf1Awcre( int iax, NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that an axis width array exists for an NDF with
*     an entry in the DCB. If the data object does not currently have an
*     axis width array, then one is created.  An NDF axis coordinate system
*     is first created by this function if necessary.

*  Parameters:
*     iax
*        Axis number.
*     dcb
*        Pointer to the data object entry in the DCB.
*     *status
*        The global status.

*  Notes:
*     -  Any width array created is left in an undefined state by this
*     function.

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
   AryPlace *place;      /* ARY_ system placeholder */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper bounds */
   int ndim;             /* Number of NDF dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that an NDF axis structure exists, creating one if necessary. */
   ndf1Acre( dcb, status );

/* Ensure that axis width array information is available. */
   ndf1Daw( iax, dcb, status );
   if( *status == SAI__OK ) {

/* See if the DCB ARY_ system identifier for the axis width array is
   valid. If so, then an axis width array exists, so there is nothing
   to do. */
      if( !dcb->awid[ iax ] ) {

/* Obtain the NDF bounds and number of dimensions from the ARY_ system
   identifier for the data array held in the DCB. */
         aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Obtain an ARY_ system placeholder for the width array in the
   appropriate cell of the NDF's axis structure. */
         aryPlace( dcb->aloc[ iax ], "WIDTH", &place, status );

/* Create the width array with the required storage form, numeric type
   and shape. */

/* ...primitive arrays. */
         if( !strcmp( dcb->awfrm[ iax ], "PRIMITIVE" ) ) {
            aryNewp( dcb->awtyp[ iax ], 1, ubnd + iax, &place,
                     dcb->awid + iax, status );

/* ...simple arrays. */
         } else if( !strcmp( dcb->awfrm[ iax ], "SIMPLE" ) ) {
            aryNew( dcb->awtyp[ iax ], 1, lbnd + iax, ubnd + iax, &place,
                    dcb->awid + iax, status );

/* If the axis width array form stored in the DCB was not recognised,
   then report an error. */
         } else {
            *status = NDF__FATIN;
            msgSetc( "BADFORM", dcb->awfrm[ iax ] );
            errRep( " ", "Invalid axis array storage form '^BADFORM' "
                    "encountered in the NDF_ system Data Control Block "
                    "(internal programming error).", status );
         }

/* If an error occurred, then erase any axis width array which may have
   been created. */
         if( *status != SAI__OK ) aryDelet( dcb->awid + iax, status );

/* Note if the axis width array information in the DCB is valid. */
         dcb->kaw[ iax ] = ( *status == SAI__OK );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Awcre", status );

}

