#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include "ary_ast.h"

void ary1Dfrm( AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dfrm

*  Purpose:
*     Ensure that form information is available for a data object.

*  Synopsis:
*     void ary1Dfrm( AryDCB *dcb, int *status )

*  Description:
*     The routine ensures that form information is available for an
*     array. It does nothing if this information is already present in
*     the DCB. Otherwise, it determines the form by inspecting the data
*     object itself and enters the resulting information into the DCB.
*     Only those checks necessary for determining and validating the
*     form are performed on the data object.

*  Parameters:
*     dcb
*        Pointer to the DCB.
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
   HDSLoc *locvar=NULL;       /* Locator to VARIANT component */
   char *variant=NULL;        /* Pointer to VARIANT value */
   char type[DAT__SZTYP+1];   /* HDS type string */
   hdsdim dim[DAT__MXDIM];    /* HDS dimension array */
   int ndim;                  /* Number of HDS dimensions */
   int prim;                  /* Whether data object is primitive */
   int there;                 /* Whether a data component is there */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the form information is unknown, then inspect the data object. */
   if( !dcb->kform ){

/* See if the object is primitive. If so, then the form is 'PRIMITIVE'. */
      datPrim( dcb->loc, &prim, status );
      if( *status == SAI__OK ){
         if( prim ){
            strcpy( dcb->form, "PRIMITIVE" );

/* If it is not primitive, then obtain its type and shape. */
         } else {
            datType( dcb->loc, type, status );
            datShape( dcb->loc, DAT__MXDIM, dim, &ndim, status );

/* Check its type is 'ARRAY'. If not, then report an error. */
            if( *status == SAI__OK ){
               if( strcmp( type, "ARRAY" ) ){
                  *status = ARY__TYPIN;
                  datMsg( "ARRAY", dcb->loc );
                  msgSetc( "BADTYPE", type );
                  errRep( "ARY1_DFRM_TYPE",
                          "The array structure ^ARRAY has an invalid data"
                          "type of '^BADTYPE'.", status );

/* Check it is a scalar. Report an error if it is not. */
               } else if( ndim != 0 ){
                  *status = ARY__NDMIN;
                  datMsg( "ARRAY", dcb->loc );
                  msgSeti( "BADNDIM", ndim );
                  errRep( "ARY1_DFRM_NDMA",
                          "The array structure ^ARRAY is"
                          "^BADNDIM-dimensional; it should be a scalar.",
                          status );

/* If the structure is OK, then see if a VARIANT component is present,
   supplying a default form of 'SIMPLE' if not. */
               } else {
                  datThere( dcb->loc, "VARIANT", &there, status );
                  if( *status == SAI__OK ){
                     if( !there ){
                        strcpy( dcb->form, "SIMPLE" );
                     } else {

/* Obtain a locator to the VARIANT component and obtain its type and shape. */
                        datFind( dcb->loc, "VARIANT", &locvar, status );
                        datType( locvar, type, status );
                        datShape( locvar, DAT__MXDIM, dim, &ndim, status );

/* Check that the VARIANT is a character object and report an error if it
   is not. */
                        if( strncmp( type, "_CHAR*", 6 ) ){
                           *status = ARY__TYPIN;
                           datMsg( "ARRAY", dcb->loc );
                           msgSetc( "BADTYPE", type );
                           errRep( "ARY1_DFRM_VTYP",
                                   "The VARIANT component in the array"
                                   "structure ^ARRAY has an invalid HDS"
                                   "type of '^BADTYPE'; it should be of"
                                   "type '_CHAR'.", status );

/* Check that it is scalar and report an error if it is not. */
                        } else if( ndim != 0 && *status == SAI__OK ){
                           *status = ARY__NDMIN;
                           datMsg( "ARRAY", dcb->loc );
                           msgSeti( "BADNDIM", ndim );
                           errRep( "ARY1_DFRM_NDMV",
                                   "The VARIANT component in the array"
                                   "structure ^ARRAY is"
                                   "^BADNDIM-dimensional; it should be a"
                                   "scalar.", status );

/* If the VARIANT component is OK, then map it and obtain its value. */
                        } else {
                           variant = ary1Get0C( locvar, status );

/* Classify the VARIANT value to obtain the form information. */

/* ...simple array. */
                           if( *status == SAI__OK ){
                              if( astChrMatch( variant, "SIMPLE" ) ){
                                 strcpy( dcb->form, "SIMPLE" );

/* ...scaled array. */
                              } else if( astChrMatch( variant, "SCALED" ) ){
                                 strcpy( dcb->form, "SCALED" );

/* ...delta array. */
                              } else if( astChrMatch( variant, "DELTA" ) ){
                                 strcpy( dcb->form, "DELTA" );

/* ...spaced array. */
                              } else if( astChrMatch( variant, "SPACED" ) ){
                                 strcpy( dcb->form, "SPACED" );

/* ...sparse array. */
                              } else if( astChrMatch( variant, "SPARSE" ) ){
                                 strcpy( dcb->form, "SPARSE" );

/* ...polynomial array. */
                              } else if( astChrMatch( variant, "POLYNOMIAL" ) ){
                                 strcpy( dcb->form, "POLYNOMIAL" );

/* ...defered primitive array. */
                              } else if( astChrMatch( variant, "PRIMITIVE" ) ){
                                 strcpy( dcb->form, "PRIMITIVE" );

/* If the VARIANT value is not recognised, then report an error. */
                              } else {
                                 *status = ARY__VARIN;
                                 datMsg( "ARRAY", dcb->loc );
                                 msgSetc( "BADVARIANT", variant );
                                 errRep( "ARY1_DFRM_VRNT",
                                         "The VARIANT component in the array"
                                         "structure ^ARRAY has an invalid"
                                         "value of '^BADVARIANT'.", status );
                              }
                           }
                        }

/* Annul the locator to the VARIANCE component, and free the memory
   holding the null-terminated copy. */
                        datAnnul( &locvar, status );
                        variant = astFree( variant );
                     }
                  }
               }
            }
         }
      }

/* Note if form information is now available in the DCB. */
      dcb->kform = ( *status == SAI__OK );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dfrm", status );

}
