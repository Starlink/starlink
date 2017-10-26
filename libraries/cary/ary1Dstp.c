#include "ary_ast.h"
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Dstp( const char *type, int cmplx, AryDCB *dcb, int *dce,
               int *status ) {
/*
*+
*  Name:
*     ary1Dstp

*  Purpose:
*     Change the type of a data object identified by its DCB entry.

*  Synopsis:
*     void ary1Dstp( const char *type, int cmplx, AryDCB *dcb, int *dce,
*                    int *status )

*  Description:
*     This function changes the data type of an object identified by its
*     entry in the DCB. If the object's state is "defined", then the
*     data values which it contains undergo type conversion. If it is
*     "undefined", then no conversion is necessary. The routine can
*     convert between any numeric data type and also between complex
*     and non-complex types (and vice versa).

*  Parameters:
*     type
*        New numeric data type for the object; this should be a
*        primitive numeric HDS type string (case insensitive).
*     cmplx
*        Whether the new object data type should be complex (i.e.
*        whether it should contain an imaginary component).
*     dcb
*        Index to the data object entry in the DCB.
*     dce
*        Returned holding a flag indicating whether conversion errors
*        occurred during type conversion of data values (if this happens
*        the affected data are assigned "bad" values). This can only
*        happen if the object's state is "defined".
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
   HDSLoc *locc=NULL;         /* Component locator */
   HDSLoc *locp=NULL;         /* Parent structure locator */
   char name[DAT__SZNAM+1];   /* Object name */
   hdsdim dim[ARY__MXDIM];    /* Data component dimension sizes */
   int defer;                 /* Has creation of HDS arrays been deferred? */
   int i;                     /* Loop counter for dimensions */
   int idce;                  /* Imaginary data conversion error? */
   size_t el;                 /* Number of data elements in component */
   void *pntr;                /* Pointer to mapped component data */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that form information is available in the DCB. */
   ary1Dfrm( dcb, status );

/* Handle each form of array in turn. */
   if( *status == SAI__OK ){

/* See if the creation of the HDS arrays holding the array values is being
   deferred until the array is mapped. */
      defer = ( dcb->dloc == NULL );

/* Primitive arrays.
   ================ */
      if( !strcmp( dcb->form, "PRIMITIVE" ) ){

/* Ensure that data type, object state and bounds information is available
   in the DCB. */
         ary1Dtyp( dcb, status );
         ary1Dsta( dcb, status );
         ary1Dbnd( dcb, status );
         if( *status == SAI__OK ){

/* Obtain the dimension sizes of the array and the total number of pixels. */
            el = 1;
            for( i = 0; i < dcb->ndim; i++ ){
               dim[ i ] = dcb->ubnd[ i ];
               el *= dim[ i ];
            }

/* If the new data type is not complex, then the array form can be left as
   primitive and its numeric data type changed. This may involve erasing
   the existing object and creating a new one, so annul the non-imaginary
   component locator which will be re-acquired later. */
            if( !cmplx ){
               if( !defer ) datAnnul( &dcb->dloc, status );

/* Obtain a locator to the array's parent structure and obtain the name of
   the array. */
               datParen( dcb->loc, &locp, status );
               datName( dcb->loc, name, status );

/* Change the array's numeric data type, possibly obtaining a new data
   object locator as a result. */
               ary1Retyp( locp, name, dcb->type, dcb->state, 1,
                          dcb->ndim, dim, type, defer, &dcb->loc, dce,
                          status );

/* Re-acquire the non-imaginary component locator by cloning the data
   object locator. We leave a null locator of the creation of the HDS
   array has been deferred, but we create a placeholder to indicate that a
   primitive array should be created. */
               if( !defer ){
                  datClone( dcb->loc, &dcb->dloc, status );
               } else {
                  ary1Dfppl( locp, name, &dcb->loc, status );
               }

/* Annul the parent locator. */
               datAnnul( &locp, status );

/* If the new data type is complex, then the array must be converted to
   simple storage form. */
            } else {
               ary1Dp2s( dcb, status );

/* Report context information if the conversion failed. */
               if( *status != SAI__OK ){
                  errRep( " ", "Unable to perform implicit conversion from "
                          "'PRIMITIVE' to 'SIMPLE' array storage form.",
                          status );
               } else {

/* Otherwise, change the numeric type of the non-imaginary component in
   what is now a simple array. */
                  ary1Retyp( dcb->loc, "DATA", dcb->type, dcb->state, 1,
                             dcb->ndim, dim, type, defer, &dcb->dloc, dce,
                             status );

/* Create a new imaginary component and obtain a locator to it for storage
   in the DCB.Only do this if the non-imaginary component is available (if
   not, the creation of the HDS arrays is being deferred). */
                  dcb->iloc = NULL;
                  if( !defer ){
                     datNew( dcb->loc, "IMAGINARY_DATA", type, dcb->ndim,
                             dim, status );
                     datFind( dcb->loc, "IMAGINARY_DATA", &dcb->iloc,
                              status );

/* If the data object state is "defined", then map the new imaginary
   component and fill it with zeros. Then unmap it. */
                     if( dcb->state ){
                        datMap( dcb->iloc, type, "WRITE", dcb->ndim, dim,
                                &pntr, status );
                        ary1Vzero( type, el, pntr, status );
                        ary1Hunmp( dcb->iloc, status );
                     }
                  }
               }
            }
         }

/* Simple arrays.
   ============= */
      } else if( !strcmp( dcb->form, "SIMPLE" ) ){

/* Ensure that data type, bad pixel flag, object state and bounds
   information is available in the DCB. */
         ary1Dtyp( dcb, status );
         ary1Dbad( dcb, status );
         ary1Dsta( dcb, status );
         ary1Dbnd( dcb, status );
         if( *status == SAI__OK ){

/* Calculate the dimension sizes of the data object components and the
   total number of their elements. */
            el = 1;
            for( i = 0; i < dcb->ndim; i++ ){
               dim[ i ] = dcb->ubnd[ i ] - dcb->lbnd[ i ] + 1;
               el *= dim[ i ];
            }

/* Change the numeric type of the non-imaginary component. */
            ary1Retyp( dcb->loc, "DATA", dcb->type, dcb->state, dcb->bad,
                       dcb->ndim, dim, type, defer, &dcb->dloc, dce, status );
            idce = 0;
            if( *status == SAI__OK ){

/* If a complex type is required and the type was originally complex, then
   change the numeric type of the imaginary component. */
               if( cmplx && dcb->complex ){
                  ary1Retyp( dcb->loc, "IMAGINARY_DATA", dcb->type,
                             dcb->state, dcb->bad, dcb->ndim, dim, type,
                             defer, &dcb->iloc, &idce, status );

/* If a non-complex type is required, but the type was originally complex,
   then annul the imaginary component locator and erase the component. */
               } else if( !cmplx && dcb->complex ){
                  if( !defer ){
                     datAnnul( &dcb->iloc, status );
                     datErase( dcb->loc, "IMAGINARY_DATA", status );
                  }

/* If a complex type is required but the type was originally non-complex,
   then create and locate a new imaginary component (unless the creation
   of the HDS arrays has been deferred). */
               } else if( cmplx && !dcb->complex ){
                  if( !defer ){
                     datNew( dcb->loc, "IMAGINARY_DATA", type, dcb->ndim,
                             dim, status );
                     dcb->iloc = NULL;
                     datFind( dcb->loc, "IMAGINARY_DATA", &dcb->iloc,
                              status );

/* If the data object state is "defined", then map the new imaginary
   component and fill it with zeros. Then unmap it. */
                     if( dcb->state ){
                        datMap( dcb->iloc, type, "WRITE", dcb->ndim, dim,
                                &pntr, status );
                        ary1Vzero( type, el, pntr, status );
                        ary1Hunmp( dcb->iloc, status );
                     }
                  }
               }

/* Determine whether any data conversion errors occurred. */
               *dce = ( *dce || idce );
            }
         }

/* Scaled arrays.
   ============= */
      } else if( !strcmp( dcb->form, "SCALED" ) ){

/* Ensure that scaling and data type information is available in the DCB. */
         ary1Dscl( dcb, status );
         ary1Dtyp( dcb, status );
         if( *status == SAI__OK ){

/* Change the external data type. This is the data type of the scale and
   zero terms (the data type of the arrays of scaled values is left
   unchanged). */
            datFind( dcb->scloc, "SCALE", &locc, status );
            datRetyp( locc, type, status );
            datAnnul( &locc, status );

            datFind( dcb->scloc, "ZERO", &locc, status );
            datRetyp( locc, type, status );
            datAnnul( &locc, status );

            *dce = 0;

/* If a non-complex type is required, but the type was originally complex,
   then annul the imaginary component locator and erase the component. */
            if( !cmplx && dcb->complex ){
               if( !defer ){
                  datAnnul( &dcb->iloc, status );
                  datErase( dcb->loc, "IMAGINARY_DATA", status );
               }

/* If a complex type is required but the type was originally non-complex,
   then report an error. */
            } else if( cmplx && !dcb->complex ){
               *status = ARY__USFRM;
               errRep( " ", "Complex scaled arrays are currently unsupported by "
                       "the ARY library.", status );
            }
         }

/* delta arrays.
   ============ */
      } else if( !strcmp( dcb->form, "DELTA" ) ){

/* Report an error since delta arrays are read-only. */
         if( *status == SAI__OK ){
            *status = ARY__CMPAC;
            datMsg( "A", dcb->loc );
            errRep( " ", "The array ^A is stored using DELTA compression and "
                    "therefore its data type cannot be changed (DELTA "
                    "compressed arrays are read-only).", status );
         }

/* If the form entry in the DCB was not recognised, then report an error. */
      } else {
         *status = ARY__FATIN;
         msgSetc( "BADFORM", dcb->form );
         errRep( " ", "Unsupported array form '^BADFORM' found in Data Control "
                 "Block (internal programming error).", status );
      }

/* Store the new data type (in upper case) and complexity information in
   the DCB. */
      if( *status == SAI__OK ){
         astChrCase( type, dcb->type, 1, sizeof(dcb->type) );
         dcb->complex = cmplx;
      }

/* Note whether the information is up to date. */
      dcb->ktype = ( *status == SAI__OK );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dstp", status );

}
