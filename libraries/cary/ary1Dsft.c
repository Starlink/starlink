#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Dsft( int nshift, const hdsdim *shift, AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dsft

*  Purpose:
*     Apply pixel shifts to a data object entry in the DCB.

*  Synopsis:
*     void ary1Dsft( int nshift, const hdsdim *shift, AryDCB *dcb, int *status )

*  Description:
*     This function applies a set of pixel shifts to a data object
*     identified by its index in the DCB. An integer shift is applied
*     to each dimension so that the array maintains the same data
*     content, although its bounds (and the indices of each pixel)
*     change by the amount of the shift applied to the corresponding
*     dimension. The DCB entry is updated to reflect the changes.

*  Parameters:
*     nshift
*        Number of dimensions to which shifts are to be applied (i.e. the
*        length of the "shift" array). If more shifts are specified than
*        there are dimensions in the data object, then the excess shifts
*        are disregarded. If fewer shifts are specified, then the extra
*        dimensions are not shifted.
*     shift
*        Array holding the shifts to be applied to each dimension.
*     dcb
*        Pointer to the DCB describing the data object to be shifted.
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
   HDSLoc *loco = NULL;       /* Locator for ORIGIN */
   int i;                     /* Loop counter for dimensions */
   int n;                     /* Number of axes to use */
   int there;                 /* Whether there is an ORIGIN component */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that form and bounds (and dimensionality) information is
   available in the DCB. */
   ary1Dfrm( dcb, status );
   ary1Dbnd( dcb, status );

/* Handle each form of array in turn. */
   if( *status == SAI__OK ){

/* Primitive, scaled and simple arrays.
   ====================================
   All forms of array are handled here. */
      if( ( !strcmp( dcb->form, "PRIMITIVE" ) ) ||
          ( !strcmp( dcb->form, "SCALED" ) ) ||
          ( !strcmp( dcb->form, "DELTA" ) ) ||
          ( !strcmp( dcb->form, "SIMPLE" ) ) ){

/* If the array is primitive, then it must first be converted to simple
   form in order to apply pixel shifts. */
         if( !strcmp( dcb->form, "PRIMITIVE" ) ){
            ary1Dp2s( dcb, status );

/* Report context information if the conversion failed. */
            if( *status != SAI__OK ){
               errRep( " ", "Unable to perform implicit conversion from"
                       " 'PRIMITIVE' to 'SIMPLE' array storage form.",
                       status );
            }
         }

/* See if the array (which is now simple) has an ORIGIN component. */
         datThere( dcb->loc, "ORIGIN", &there, status );
         if( *status == SAI__OK ){

/* If there is no ORIGIN component, then create one. */
            if( !there ) datNew1I( dcb->loc, "ORIGIN", dcb->ndim, status );

/* Apply the pixel shifts to both sets of array bounds and to the
   accumulated pixel shifts held in the DCB. */
            n = ( nshift < dcb->ndim ) ? nshift : dcb->ndim;
            for( i = 0; i < n; i++ ){
               dcb->lbnd[ i ] += shift[ i ];
               dcb->ubnd[ i ] += shift[ i ];
               dcb->shift[ i ] += shift[ i ];
            }

/* Write new values to the ORIGIN component reflecting the array's new
   origin position. */
            datFind( dcb->loc, "ORIGIN", &loco, status );
            datPut1I( loco, dcb->ndim, dcb->lbnd, status );
            datAnnul( &loco, status );

/* Note if the DCB bounds information is correct. */
            dcb->kbnd = ( *status == SAI__OK );
         }

/* If the form information in the DCB was not recognised, then report an
   error. */
      } else {
         *status = ARY__FATIN;
         msgSetc( "BADFORM", dcb->form );
         errRep( " ", "Unsupported array form '^BADFORM' found in Data Control"
                 " Block (internal programming error).", status );
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dsft", status );

}
