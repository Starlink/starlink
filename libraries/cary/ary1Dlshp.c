#include "mers.h"
#include "sae_par.h"
#include "star/hds.h"

#include "ary1.h"
#include "ary_err.h"

void ary1Dlshp( HDSLoc *loc, int mxdim, hdsdim *dim, int *ndim, int *status ) {
/*
*+
*  Name:
*     ary1Dlshp

*  Purpose:
*     Return the dimensions of a DELTA array.

*  Synopsis:
*     void ary1Dlshp( HDSLoc *loc, int mxdim, hdsdim *dim, int *ndim,
*                     int *status )

*  Description:
*     The routine uses the components in the supplied DELTA compressed
*     data object to determine the dinmensions of the uncompressed array.

*  Parameters:
*     loc
*        Locator for a DELTA compressed data object.
*     mxdim
*        The maximum allowed number of axes.
*     dim
*        An array of length "mxdim", returned holding the number of
*        pixels along each axis of the uncompressed array.
*     ndim
*        The number of pixel axes in the uncompressed array.
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
*     23-JUN-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   HDSLoc *loc2=NULL;         /* Locator to component */
   int idim;                  /* Axis index */
   int there;                 /* Whether a component exists */
   int zaxis;                 /* Index of compression axis */
   int zdim;                  /* Length of compression axis */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the ZAXIS component exists, get its value. Otherwise report an error. */
   datThere( loc, "ZAXIS", &there, status );
   if( there ){
      datFind( loc, "ZAXIS", &loc2, status );
      datGet0I( loc2, &zaxis, status );
      datAnnul( &loc2, status );

   } else if( *status == SAI__OK ){
      *status = ARY__DLTIN;
      datMsg( "A", loc );
      errRep( " ", "The DELTA compressed array '^A' is invalid - the ZAXIS"
              "component is missing.", status );
      goto L999;
   }

/* If the ZDIM component exists, get its value. Otherwise report an error. */
   datThere( loc, "ZDIM", &there, status );
   if( there ){
      datFind( loc, "ZDIM", &loc2, status );
      datGet0I( loc2, &zdim, status );
      datAnnul( &loc2, status );

   } else if( *status == SAI__OK ){
      *status = ARY__DLTIN;
      datMsg( "A", loc );
      errRep( " ",
              "The DELTA compressed array '^A' is invalid - the ZDIM"
              "component is missing.", status );
      goto L999;
   }

/* If the FIRST_DATA component exists, get its shape. Otherwise report
   an error. The FIRST_DATA array has one less dimension than the
   uncompressed array. */
   datThere( loc, "FIRST_DATA", &there, status );
   if( there ){
      datFind( loc, "FIRST_DATA", &loc2, status );
      datShape( loc2, ARY__MXDIM - 1, dim, ndim, status );
      datAnnul( &loc2, status );

   } else if( *status == SAI__OK ){
      *status = ARY__DLTIN;
      datMsg( "A", loc );
      errRep( " ", "The DELTA compressed array '^A' is invalid - the "
              " FIRST_DATA component is missing.", status );
      goto L999;
   }

/* Get the number of axes in the uncompressed array. */
   (*ndim)++;

/* Report an error if there are too many. */
   if( *ndim > mxdim && *status == SAI__OK ){
      *status = ARY__DLTIN;
      datMsg( "A", loc );
      msgSeti( "N", *ndim );
      msgSeti( "X", mxdim );
      errRep( " ", "The DELTA compressed array '^A' is invalid - the number of"
              "axes (^N) is more than the allowed maximum (^X).", status );
      goto L999;
   }

/* Check the compression axis index is within the range of axis indices
   allowed by the value of NDIM. */
   if( ( zaxis < 1 || zaxis > *ndim ) && *status == SAI__OK ){
      *status = ARY__DLTIN;
      datMsg( "A", loc );
      msgSeti( "I", zaxis );
      errRep( " ", "The DELTA compressed array '^A' is invalid - the ZAXIS "
              "value (^I) is illegal.", status );
      goto L999;
   }

/* If all is OK, shuffle the high dimensions up to make room for the
   compression axis, and then store the dimension of the compression axis
   in the correct element of the returned array. */
   if( *status == SAI__OK ){
      for( idim = *ndim - 1; idim > zaxis - 1; idim-- ){
         dim[ idim ] = dim[ idim - 1 ];
      }
      dim[ zaxis - 1 ] = zdim;
   }

/* Call error tracing routine and exit. */
L999:
   if( *status != SAI__OK ) ary1Trace( "ary1Dlshp", status );

}
