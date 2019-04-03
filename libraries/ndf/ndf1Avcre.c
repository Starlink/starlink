#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include <string.h>
#include "mers.h"

void ndf1Avcre( int iax, NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Avcre

*  Purpose:
*     Ensure that an NDF axis variance array exists, creating one if
*     necessary.

*  Synopsis:
*     void ndf1Avcre( int iax, NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that an axis variance array exists for an NDF
*     with an entry in the DCB. If the data object does not currently have
*     an axis variance array, then one is created.  An NDF axis coordinate
*     system is first created by this function if necessary.

*  Parameters:
*     iax
*        Axis number.
*     dcb
*        Pointer to the data object entry in the DCB.
*     *status
*        The global status.

*  Notes:
*     -  Any variance array created is left in an undefined state by this
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

/* Ensure that axis variance array information is available. */
   ndf1Dav( iax, dcb, status );
   if( *status == SAI__OK ) {

/* See if the DCB ARY_ system identifier for the axis variance array is
   valid. If so, then an axis variance array exists, so there is
   nothing to do. */
      if( !dcb->avid[ iax ] ) {

/* Obtain the NDF bounds and number of dimensions from the ARY_ system
   identifier for the data array held in the DCB. */
         aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Obtain an ARY_ system placeholder for the variance array in the
   appropriate cell of the NDF's axis structure. */
         aryPlace( dcb->aloc[ iax ], "VARIANCE", &place, status );

/* Create the variance array with the required storage form, numeric
   type and shape. */

/* ...primitive arrays. */
         if( !strcmp( dcb->avfrm[ iax ], "PRIMITIVE" ) ) {
            aryNewp( dcb->avtyp[ iax ], 1, ubnd + iax, &place,
                     dcb->avid + iax, status );

/* ...simple arrays. */
         } else if( !strcmp( dcb->avfrm[ iax ], "SIMPLE" ) ) {
            aryNew( dcb->avtyp[ iax ], 1, lbnd + iax, ubnd + iax, &place,
                    dcb->avid + iax, status );

/* If the axis variance array form stored in the DCB was not
   recognised, then report an error. */
         } else {
            *status = NDF__FATIN;
            msgSetc( "BADFORM", dcb->avfrm[ iax ] );
            errRep( " ", "Invalid axis array storage form '^BADFORM' "
                    "encountered in the NDF_ system Data Control Block "
                    "(internal programming error).", status );
         }

/* If an error occurred, then erase any axis variance array which may
   have been created. */
         if( *status != SAI__OK ) aryDelet( dcb->avid + iax, status );

/* Note if the axis variance array information in the DCB is valid. */
         dcb->kav[ iax ] = ( *status == SAI__OK );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Avcre", status );

}

