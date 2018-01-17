#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Gtdlt( AryDCB *dcb, int *zaxis, char ztype[ARY__SZTYP+1],
                float *zratio, int *status ) {
/*
*+
*  Name:
*     ary1Gtdlt

*  Purpose:
*     Get the compressed axis and data type for a DELTA array.

*  Synopsis:
*     void ary1Gtdlt( AryDCB *dcb, int *zaxis, char ztype[ARY__SZTYP+1],
*                     float *zratio, int *status )

*  Description:
*     This function returns the details of the compression used to produce
*     an array stored in DELTA form. If the array is not stored in
*     DELTA form, then null values are returned as listed below, but no
*     error is reported.

*  Parameters:
*     dcb
*        The data object (DCB).
*     zaxis
*        Returned holding the index of the pixel axis along which compression
*        occurred. The first axis has index 1. Zero is returned if the array
*        is not stored in DELTA form.
*     ztype
*        Returned holding the data type in which the differences between
*        adjacent array values are stored. This will be one of '_BYTE',
*        '_WORD' or '_INTEGER'. The data type of the array itself is returned
*        if the supplid array is not stored in DELTA form.
*     zratio
*        Returned holding the compression factor - the ratio of the
*        uncompressed array size to the compressed array size. This is
*        approximate as it does not include the effects of the metadata
*        needed to describe the extra components of a DELTA array (i.e. the
*        space needed to hold the component names, types, dimensions, etc).
*        A value of 1.0 is returned if the supplid array is not stored in
*        DELTA form.
*     status
*        The global status.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   HDSLoc *locc=NULL;        /* Locator for component */
   int there;                /* Does the component exist? */

/* Initialise returned values */
   *zaxis = 0;
   *ztype = 0;
   *zratio = 1.0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that form information is available for the array. */
   ary1Dfrm( dcb, status );

/* If the array is not stored in delta form, return the data type of the
   array itself. */
   if( strcmp( dcb->form, "DELTA" ) ){
      strcpy( ztype, dcb->type );

/* For delta arrays, get the required components from the data object. */
   } else {

      datThere( dcb->loc, "ZAXIS", &there, status );
      if( there ){
         datFind( dcb->loc, "ZAXIS", &locc, status );
         datGet0I( locc, zaxis, status );
         datAnnul( &locc, status );
      } else if( *status == SAI__OK ){
         *status = ARY__DLTIN;
         datMsg( "A", dcb->loc );
         errRep( " ", "The DELTA compressed array '^A' is invalid - the ZAXIS "
                 "component is missing.", status );
      }

      datThere( dcb->loc, "ZRATIO", &there, status );
      if( there ){
         datFind( dcb->loc, "ZRATIO", &locc, status );
         datGet0R( locc, zratio, status );
         datAnnul( &locc, status );
      } else if( *status == SAI__OK ){
         *status = ARY__DLTIN;
         datMsg( "A", dcb->loc );
         errRep( " ", "The DELTA compressed array '^A' is invalid - the ZRATIO "
                 "component is missing.", status );
      }

      datThere( dcb->loc, "DATA", &there, status );
      if( there ){
         datFind( dcb->loc, "DATA", &locc, status );
         datType( locc, ztype, status );
         datAnnul( &locc, status );
      } else if( *status == SAI__OK ){
         *status = ARY__DLTIN;
         datMsg( "A", dcb->loc );
         errRep( " ", "The DELTA compressed array '^A' is invalid - the DATA "
                 "component is missing.", status );
      }
   }
}
