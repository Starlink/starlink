#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void ary1Dbad( AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dbad

*  Purpose:
*     Ensure that bad pixel information is available for a data object.

*  Synopsis:
*     void ary1Dbad( AryDCB *dcb, int *status )

*  Description:
*     The routine ensures that information about whether an array may
*     contain bad pixels is available. It does nothing if this
*     information is already available in the DCB. Otherwise, it obtains
*     the information by inspecting the data object itself and enters
*     the information into the DCB. Only those checks needed to obtain
*     the bad pixel information are performed on the data object.

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
   HDSLoc *locbad=NULL;       /* Locator to BAD_PIXEL component */
   char type[DAT__SZTYP+1];   /* HDS data type */
   hdsdim dimb[DAT__MXDIM];   /* BAD_PIXEL component dimensions */
   int ndimb;                 /* Number of BAD_PIXEL dimensions */
   int there;                 /* Whether an HDS component is present */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Do nothing if bad pixel information is already available. */
   if( !dcb->kbad ){

/* Ensure that form information is available for the data object. */
      ary1Dfrm( dcb, status );
      if( *status != SAI__OK ) goto L999;

/* Primitive arrays.
   ================ */
      if( !strcmp( dcb->form, "PRIMITIVE" ) ){
         dcb->bad = 1;

/* Simple, scaled and delta arrays.
   ================================ */
      } else if( !strcmp( dcb->form, "SIMPLE" ) ||
                 !strcmp( dcb->form, "DELTA" ) ||
                 !strcmp( dcb->form, "SCALED" ) ){

/* See if the BAD_PIXEL component is present and supply a default value of
   1 if it is not. */
         datThere( dcb->loc, "BAD_PIXEL", &there, status );
         if( *status != SAI__OK ) goto L999;
         if( !there ){
            dcb->bad = 1;

/* If the BAD_PIXEL component is present, then obtain its type and shape. */
         } else {
            locbad = NULL;
            datFind( dcb->loc, "BAD_PIXEL", &locbad, status );
            datType( locbad, type, status );
            datShape( locbad, DAT__MXDIM, dimb, &ndimb, status );
            if( *status == SAI__OK ){

/* Check it is '_LOGICAL' and report an error if it is not. */
               if( strcmp( type, "_LOGICAL") ){
                  *status = ARY__TYPIN;
                  datMsg( "ARRAY", dcb->loc );
                  msgSetc( "BADTYPE", type );
                  errRep( "ARY1_DBAD_TYPE",
                          "The BAD_PIXEL component in the array structure "
                          "^ARRAY has an invalid HDS type of '^BADTYPE'; it "
                          "should be of type '_LOGICAL'.", status );

/* Check it is scalar and report an error if it is not. */
               } else if( ndimb != 0 ){
                  *status = ARY__DIMIN;
                  datMsg( "ARRAY", dcb->loc );
                  msgSeti( "BADNDIM", ndimb );
                  errRep( "ARY1_DBAD_BDIM",
                          "The BAD_PIXEL component in the array structure "
                          "^ARRAY is ^BADNDIM-dimensional; it should be a "
                          "scalar.", status );

/* Obtain the BAD_PIXEL value. */
               } else {
                  datGet0L( locbad, &dcb->bad, status );
               }
            }

/* Annul the locator to the BAD_PIXEL component. */
            datAnnul( &locbad, status );
         }

/* If the form entry in the Data Control Block was not recognised, then
   report an error. */
      } else {
         *status = ARY__FATIN;
         msgSetc( "BADFORM", dcb->form );
         errRep( "ARY1_DBAD_FRM",
                 "Unsupported array form '^BADFORM' found in Data Control "
                 "Block (internal programming error).", status );
      }

/* Note if bad pixel information is now available. */
L999:
      dcb->kbad = ( *status == SAI__OK );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dbad", status );

}
