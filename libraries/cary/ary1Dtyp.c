#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void ary1Dtyp( AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dtyp

*  Purpose:
*     Ensure that type information and data component locators are
*     available for a data object.

*  Synopsis:
*     void ary1Dtyp( AryDCB *dcb, int *status )

*  Description:
*     The routine ensures that type information is available for an
*     array (including whether it is complex or not) together with
*     locators to the data components(s) present.  It does nothing if
*     this information is already available in the DCB. Otherwise, it
*     determines the type by inspecting the data object itself and
*     enters the resulting information and locators into the DCB.  Only
*     those checks necessary for determining and validating the type
*     are performed on the data object.

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
   char type[DAT__SZTYP+1];   /* HDS data type */
   HDSLoc *loc2 = NULL;       /* Component locator */
   char numer;                /* Whether the data type is numeric */
   int there;                 /* Does the named component exist? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If type information is not available, then inspect the data object. */
   if( !dcb->ktype ){

/* Report an error if the creation of the data arrays has been deferred.
   The only situation in which creation is deferred is if ary1Dcre(p) is
   called with its "defer" parameter set non-zero. In this case, all the
   required information should already be available because ary1Dcre(p)
   will have set it up. */
      if( ary1Defr( dcb, status ) ){
         *status = ARY__UNDEF;
         errRep( "ARY1_DTYP_ERR1",
                 "ARY1_DTYP: Cannot get type information because the"
                 "creation of the supplied array has been deferred (ARY"
                 "programming error).", status );
      }

/* Ensure that form information is available. */
      ary1Dfrm( dcb, status );
      if( *status == SAI__OK ){

/* Primitive arrays.
   ================ */
         if( !strcmp( dcb->form, "PRIMITIVE" ) ){

/* Obtain a non-imaginary component locator by cloning the data object
   locator. Set a null value for the imaginary component locator. */
            dcb->dloc = NULL;
            datClone( dcb->loc, &dcb->dloc, status );
            dcb->iloc = NULL;

/* Obtain the data type of the non-imaginary component for storage in the
   DCB and note that the array is not complex. */
            datType( dcb->dloc, dcb->type, status );
            dcb->complex = 0;

/* If the type is not numeric, then report an error. */
            numer = ary1Intyp( dcb->type, status );
            if( *status == SAI__OK ){
               if( !numer ){
                  *status = ARY__TYPIN;
                  datMsg( "ARRAY", dcb->loc );
                  msgSetc( "BADTYPE", dcb->type );
                  errRep( "ARY1_DTYP_PDTYP",
                          "The array ^ARRAY has an invalid data type of"
                          "'^BADTYPE'; it should have a numeric type.",
                          status );
               }
            }

/* If there was an error, then annul any locators. */
            if( *status != SAI__OK ){
               datAnnul( &dcb->dloc, status );
            }

/* Simple and scaled arrays.
   ========================= */
         } else if( !strcmp( dcb->form, "SIMPLE" ) ||
                    !strcmp( dcb->form, "SCALED" ) ){

/* Find the DATA component, storing a locator to it in the DCB, and obtain
   its type, which is also stored in the DCB. */
            dcb->dloc = NULL;
            dcb->iloc = NULL;
            datFind( dcb->loc, "DATA", &dcb->dloc, status );
            datType( dcb->dloc, dcb->type, status );

/* If the type is not numeric, then report an error. */
            numer = ary1Intyp( dcb->type, status );
            if( *status == SAI__OK ){
               if( !numer ){
                  *status = ARY__TYPIN;
                  datMsg( "ARRAY", dcb->loc );
                  msgSetc( "BADTYPE", dcb->type );
                  errRep( "ARY1_DTYP_DTYP",
                          "The DATA component in the array structure ^ARRAY"
                          "has an invalid HDS type of '^BADTYPE'; it should"
                          "have a numeric type.", status );
               } else {

/* See if there is an IMAGINARY_DATA component present. If so, then the
   array is complex, so record this fact in the DCB. */
                  datThere( dcb->loc, "IMAGINARY_DATA", &dcb->complex,
                            status );
                  if( *status == SAI__OK ){

/* Is so, report an error if we are creating a scaled array. */
                     if( dcb->complex ){
                        if( !strcmp( dcb->form, "SCALED" ) ){
                           *status = ARY__USFRM;
                           errRep( "ARY1_DSTP_SCMX",
                                   "Complex scaled arrays are currently"
                                   "unsupported by the ARY library.",
                                   status );
                        }

/* Otherwise, get a locator to it for the DCB and obtain its type. */
                        datFind( dcb->loc, "IMAGINARY_DATA", &dcb->iloc,
                                 status );
                        datType( dcb->iloc, type, status );
                        if( *status == SAI__OK ){

/* If the type does not match that of the non-imaginary component, then
   report an error. */
                           if( strcmp( type, dcb->type ) ){
                              *status = ARY__TYPIN;
                              datMsg( "ARRAY", dcb->loc );
                              msgSetc( "BADTYPE", type );
                              msgSetc( "DTYPE", dcb->type );
                              errRep( "ARY1_DTYP_IMAG",
                                      "The IMAGINARY_DATA component in the"
                                      "array structure ^ARRAY has an"
                                      "invalid HDS type of '^BADTYPE'; its"
                                      "type should match that of the DATA"
                                      "component ('^DTYPE').", status );
                           }
                        }
                     }
                  }
               }
            }

/* If there was an error, then annul any locators. */
            if( *status != SAI__OK ){
               datAnnul( &dcb->dloc, status );
               datAnnul( &dcb->iloc, status );
            }

/* Delta arrays.
   ============= */
         } else if( !strcmp( dcb->form, "DELTA" ) ){

/* Find the DATA component, storing a locator to it in the DCB. */
            dcb->dloc = NULL;
            dcb->iloc = NULL;
            datFind( dcb->loc, "DATA", &dcb->dloc, status );

/* Find the VALUE component, reporting an error if it is not there, and
   obtain its type, which is stored in the DCB. */
            datThere( dcb->loc, "VALUE", &there, status );
            if( there ){
               datFind( dcb->loc, "VALUE", &loc2, status );
               datType( loc2, dcb->type, status );
               datAnnul( &loc2, status );

            } else if( *status == SAI__OK ){
               *status = ARY__DLTIN;
               datMsg( "A", dcb->loc );
               errRep( " ",
                       "The DELTA compressed array '^A' is invalid - the"
                       "VALUE component is missing.", status );
            }

/* If the type is not numeric, then report an error. */
            numer = ary1Intyp( dcb->type, status );
            if( !numer && *status == SAI__OK ){
               *status = ARY__TYPIN;
               datMsg( "A", dcb->loc );
               msgSetc( "T", dcb->type );
               errRep( " ",
                       "The DELTA compressed array '^A' is invalid - the"
                       "VALUE component has an invalid HDS type of '^T'; it"
                       "should have a numeric type.", status );
            }

/* See if there is an IMAGINARY_DATA component present. If so, then the
   array is complex, so report an error. */
            datThere( dcb->loc, "IMAGINARY_DATA", &dcb->complex, status );
            if( dcb->complex && *status == SAI__OK ){
               *status = ARY__USFRM;
               datMsg( "A", dcb->loc );
               errRep( " ",
                       "The DELTA compressed array '^A' holds complex values"
                       "but the ARY library does not yet support complex"
                       "DELTA arrays.", status );
            }


/* If the form entry in the DCB is not supported, then report an error. */
         } else {
            *status = ARY__FATIN;
            msgSetc( "BADFORM", dcb->form );
            errRep( "ARY1_DTYP_FRM",
                    "Unsupported array form '^BADFORM' encountered in Data"
                    "Control Block (internal programming error).", status );
         }

/* Note if type information is now available in the DCB. */
         dcb->ktype = ( *status == SAI__OK );
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dtyp", status );

}
