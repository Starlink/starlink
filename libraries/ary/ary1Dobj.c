#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Dobj( AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dobj

*  Purpose:
*     Ensure that the HDS primitive arrays holding the real and (if
*     necessary) imaginary values have been created.

*  Synopsis:
*     void ary1Dobj( AryDCB *dcb, int *status )

*  Description:
*     This function ensures that HDS arrays exist to hold the real and (if
*     necessary) imaginary parts of the array data values. This may not
*     be the case if creation of these objects was deferred when calling
*     ary1Dcre or ary1Dcrep.

*  Parameters:
*     dcb
*        The data object (DCB).
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
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

/* Local variables: */
   HDSLoc *locp = NULL;         /* Locator for parent obvject */
   char name[ DAT__SZNAM + 1 ]; /* Name of data object */
   hdsdim dim[ARY__MXDIM];      /* Dimensions of array */
   int i;                       /* Loop counter for dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Do nothing if the creation of the arrays is not deferred. */
   if( ary1Defr( dcb, status ) && *status == SAI__OK ){

/* Report an error if required information is not available. The only
   situation in which creation is deferred is if ary1Dcre(p) is called
   with its DEFER parameter set TRUE. In this case, all the required
   information should already be available because ary1Dcre(p) will have
   set it up. */
      if( !dcb->ktype ){
         *status = ARY__UNDEF;
         errRep( " ", "ary1Dobj: Deferred creation of an array is not possible "
                 "because no type information is available (ARY programming "
                 "error).", status );

      } else if( !dcb->kbnd ){
         *status = ARY__UNDEF;
         errRep( " ", "ary1Dobj: Deferred creation of an array is not possible "
                 "because no bounds information is available (ARY "
                 "programming error).", status );

      } else if( !dcb->kform ){
         *status = ARY__UNDEF;
         errRep( " ", "ary1Dobj: Deferred creation of an array is not possible "
                 "because no form information is available (ARY programming "
                 "error).", status );
      }

/* Do nothing if an error has occurred. */
      if( *status != SAI__OK ) {

/* Primitive arrays. ================ */
      } else if( !strcmp( dcb->form, "PRIMITIVE" ) ){

/* Obtain a locator to the placeholder object's parent structure. */
         datParen( dcb->loc, &locp, status );

/* Obtain the placeholder object's name. Then annul its locator and erase
   the object. */
         datName( dcb->loc, name, status );
         datAnnul( &dcb->loc, status );
         datErase( locp, name, status );

/* Create a new primitive array of the required type and shape in its place
   and obtain a new locator to it. */
         datNew( locp, name, dcb->type, dcb->ndim, dcb->ubnd, status );
         datFind( locp, name, &dcb->loc, status );

/* Link this locator into a private group to prevent external events
   annulling it. */
         hdsLink( dcb->loc, "ARY_DCB", status );

/* Obtain a non-imaginary component locator by cloning the data object
   locator. */
         datClone( dcb->loc, &dcb->dloc, status );

/* Simple and scaled arrays. ========================= */
      } else if( !strcmp( dcb->form, "SIMPLE" ) ||
                 !strcmp( dcb->form, "SCALED" ) ){

/* Calculate the axis dimensions. */
         for( i = 0; i < dcb->ndim; i++ ){
            dim[ i ] = dcb->ubnd[ i ] - dcb->lbnd[ i ] + 1;
         }

/* Create the non-imaginary data component and obtain a locator to it.
   Store the locator in the DCB. */
         datNew( dcb->loc, "DATA", dcb->type, dcb->ndim, dim, status );
         datFind( dcb->loc, "DATA", &dcb->dloc, status );

/* If a complex array is required, then create and locate the imaginary
   component similarly. */
         if( dcb->complex ){
            datNew( dcb->loc, "IMAGINARY_DATA", dcb->type, dcb->ndim, dim,
                    status );
            datFind( dcb->loc, "IMAGINARY_DATA", &dcb->iloc, status );
         }


/* Delta arrays. ============= */
      } else if( !strcmp( dcb->form, "DELTA" ) ){

/* Report an error since delta arrays can only be created by converting a
   defined simple or scaled array, and so the creation should never be
   deferred. */
         if( *status == SAI__OK ){
            *status = ARY__FATIN;
            errRep( " ", "ary1Dobj: Input array is stored in DELTA form, "
                    "but DELTA arrays should never be deferred (internal "
                    "programming error).", status );
         }

/* If the form information in the DCB was not valid, then report an error.
   ================================================================ */
      } else {
         *status = ARY__FATIN;
         msgSetc( "BADFORM", dcb->form );
         errRep( " ", "Unsupported array form '^BADFORM' found in Data Control "
                 "Block (internal programming error).", status );
      }

   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dobj", status );

}
