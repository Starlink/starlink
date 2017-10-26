#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void arySsect( Ary *ary1, Ary *ary2, Ary **ary3, int *status ) {
/*
*+
*  Name:
*     arySsect

*  Purpose:
*     Create a similar array section to an existing one.

*  Synopsis:
*     void arySsect( Ary *ary1, Ary *ary2, Ary **ary3, int *status )

*  Description:
*     This function creates a new array section, using an existing
*     section as a template. The new section bears the same
*     relationship to its base array as the template section does to
*     its own base array. Allowance is made for pixel-index shifts
*     which may have been applied so that the pixel-indices of the new
*     section match those of the template.  The number of dimensions of
*     the input and template arrays may differ.

*  Parameters:
*     ary1
*        Identifier for the input array from which the section is to be
*        derived. This may be a base array or an array section.
*     ary2
*        Identifier for the template section (this may also be a base
*        array or an array section).
*     ary3
*        Returned holding the identifier for the new array section.
*     status
*        The global status.

*  Notes:
*     -  This routine normally generates an array section.  However, if
*     both input arrays are base arrays with identical pixel-index
*     bounds, then there is no need to create a section in order to
*     access the required part of the first array. In this case a base
*     array identifier will be returned instead.
*     -  The new section created by this routine will have the same
*     number of dimensions as the array (or array section) from which
*     it is derived. If the template (ary2) array has fewer dimensions
*     than this, then the bounds of any additional input dimensions are
*     preserved unchanged in the new array. If the template (ary2)
*     array has more dimensions, then the excess ones are ignored.
*     -  This routine takes account of the regions of each base array
*     to which the input array sections have access. It may therefore
*     restrict the region accessible to the new section (and pad with
*     "bad" pixels) so as not to grant access to regions of the base
*     array which were not previously accessible through the input
*     arrays.
*     -  If this routine is called with "status" set, then a value of
*     NULL will be returned for the "ary3" argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.

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
   AryACB *acb1;              /* ACB for the IARY1 array */
   AryACB *acb2;              /* ACB for the IARY2 array */
   AryACB *acb3;              /* ACB for the IARY3 array */
   AryDCB *dcb1;              /* DCB for the IARY1 data object */
   AryDCB *dcb2;              /* DCB for the IARY2 data object */
   hdsdim lbnd[ARY__MXDIM];   /* Lower bound of region to be accessed */
   hdsdim lx[ARY__MXDIM];     /* Lower bounds of data transfer window */
   hdsdim sft[ARY__MXDIM];    /* Differential pixel index shifts */
   hdsdim ubnd[ARY__MXDIM];   /* Upper bound of region to be accessed */
   hdsdim ux[ARY__MXDIM];     /* Upper bounds of data tranfer window */
   int i;                     /* Loop counter for dimensions */
   int match;                 /* Whether array bounds match */
   int ndim;                  /* Number of array dimensions in common */

/* Set an initial value for the IARY3 argument. */
   *ary3 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the identifiers for the input arrays. */
   acb1 = (AryACB *) ary1Impid( ary1, 1, 1, 1, status );
   acb2 = (AryACB *) ary1Impid( ary2, 1, 1, 1, status );
   if( *status == SAI__OK ){

/* Initialise the ACB for the output array. */
      acb3 = NULL;

/* Obtain the data objects for the two input arrays. */
      dcb1 = acb1->dcb;
      dcb2 = acb2->dcb;

/* Determine the number of dimensions in common between the two input
   arrays. */
      ndim = ( acb1->ndim < acb2->ndim ) ? acb1->ndim : acb2->ndim;

/* Loop through the common dimensions, deriving the pixel index shift
   necessary in each dimension to relate pixel indices in one array
   section to those in the other. This is formed by finding the shift
   needed to convert from pixel indices in the "ary2" section to pixels in
   its base array, then subtracting the equivalent quantity for the "ary1"
   section. */
      for( i = 0; i < ndim; i++ ){
         sft[ i ] = ( acb2->shift[ i ] - dcb2->shift[ i ] ) -
                    ( acb1->shift[ i ] - dcb1->shift[ i ] );

/* Subtract the shifts from the template section (IARY2) pixel index bounds
   to derive the bounds in the pixel index system of the IARY1 section
   which correspond to the same region of its base array. */
         lbnd[ i ] = acb2->lbnd[ i ] - sft[ i ];
         ubnd[ i ] = acb2->ubnd[ i ] - sft[ i ];
      }

/* If the ary1 array has more dimensions than the template, then its
   bounds in the additional dimensions are preserved. */
      for( ; i < acb1->ndim; i++ ){
         sft[ i ] = 0;
         lbnd[ i ] = acb1->lbnd[ i ];
         ubnd[ i ] = acb1->ubnd[ i ];
      }

/* Check to see if the bounds of the section to be extracted from the IARY1
   array match its existing bounds (if so, then there may be no need to
   generate a new section in order to access the required part of the base
   array). */
      match = 1;
      for( i = 0; i < ndim; i++ ){
         if( lbnd[ i ] != acb1->lbnd[ i ] ||
             ubnd[ i ] != acb1->ubnd[ i ] ){
            match = 0;
            break;
          }
      }

/* If either input array is not a base array, or if the bounds do not match
   (above), then a new section must be created. Cut the appropriate
   section from the IARY1 array, generating a new ACB entry to describe
   it. */
      if( acb1->cut || acb2->cut || !match ){
         ary1Cut( acb1, acb1->ndim, lbnd, ubnd, &acb3, status );
         if( *status == SAI__OK ){

/* At this point, the new array section's data transfer window takes
   account of the bounds of the two input arrays and the data transfer
   window of the first input array. The effect of the data transfer window
   of the second input array must now be included. This is only necessary
   if this second array is not a base array and if the output array
   currently has a data transfer window. */
            if( acb2->cut && acb3->dtwex ){

/* If the second input array has no data transfer window, then neither does
   the output array. */
               if( !acb2->dtwex ){
                  acb3->dtwex = 0;

/* Otherwise, find the intersection region between the current output array
   data transfer window and that of the second input array (both in the
   reference frame pixel index system). Note whether this intersection
   region exists in the ACB entry for the output array. */
               } else {
                  ary1Xsbnd( ARY__MXDIM, acb3->ldtw, acb3->udtw, ARY__MXDIM,
                             acb2->ldtw, acb2->udtw, ARY__MXDIM, lx, ux,
                             &acb3->dtwex, status );
                  if( *status == SAI__OK ){

/* If the intersection region exists, then transfer its bounds to the new
   ACB entry as the data transfer window bounds of the output array. */
                     if( acb3->dtwex ){
                        for( i = 0; i < ARY__MXDIM; i++ ){
                           acb3->ldtw[ i ] = lx[ i ];
                           acb3->udtw[ i ] = ux[ i ];
                        }
                     }
                  }
               }
            }
         }

/* Apply the differential pixel index shifts to the new array section to
   convert its pixel indices to match the template (ary2) array. */
         ary1Sft( acb1->ndim, sft, acb3, status );

/* If both input arrays are base arrays with the same bounds, then access
   to the entire array is required, so the ary1 base array entry in the
   ACB can simply be cloned. */
      } else {
         ary1Cln( acb1, &acb3, status );
      }

/* Export an identifier for the new array section. */
      *ary3 = ary1Expid( (AryObject *) acb3, status );

/* If an error occurred, but a new ACB entry was allocated, then clean up
   by annulling it. */
      if( ( *status != SAI__OK ) && acb3 ){
         ary1Anl( acb3, status );
      }
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "arySsect: Error obtaining an array section using an existing"
              " section as a template.", status );
      ary1Trace( "arySsect", status );
   }

}
