#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "star/cmp.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Cpdlt( AryDCB *dcb1, AryDCB *dcb2, int *status ) {
/*
*+
*  Name:
*     ary1Cpdlt

*  Purpose:
*     Copy delta compression information from one DCB entry to another.

*  Synopsis:
*     void ary1Cpdlt( AryDCB *dcb1, AryDCB *dcb2, int *status )

*  Description:
*     This function copies the supplemental information describing the
*     compression of a supplied delta array (identified by its DCB entry)
*     to another existing DCB entry. Note, it only copies components of
*     the DELTA array structure that are not also conmponents of a SIMPLE
*     array (e.g. DATA, ORIGIN, etc are not copied).

*  Parameters:
*     dcb1
*        The DCB entry of the array to be copied.
*     dcb2
*        The DCB entry to recieve the copy.
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
   int there;
   HDSLoc *loc2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that form information is available for the input DCB entry. */
   ary1Dfrm( dcb1, status );
   if( *status == SAI__OK ){

/* If the first DCB entry is a delta array then we copy the compression
   information. */
      if( !strcmp( dcb1->form, "DELTA" ) ){

/* First do mandatory components. Report an error if any of these does not
   exist in the input. Otherwise, copy them to the output. */

/* ZAXIS ----- */
         datThere( dcb1->loc, "ZAXIS", &there, status );
         if( there ){
            datFind( dcb1->loc, "ZAXIS", &loc2, status );
            datCopy( loc2, dcb2->loc, "ZAXIS", status );
            datAnnul( &loc2, status );

         } else if( *status == SAI__OK ){
            *status = ARY__DLTIN;
            datMsg( "A", dcb1->loc );
            errRep( " ", "The DELTA compressed array '^A' is invalid - the ZAXIS "
                    "component is missing.", status );
         }

/* ZDIM ---- */
         datThere( dcb1->loc, "ZDIM", &there, status );
         if( there ){
            datFind( dcb1->loc, "ZDIM", &loc2, status );
            datCopy( loc2, dcb2->loc, "ZDIM", status );
            datAnnul( &loc2, status );

         } else if( *status == SAI__OK ){
            *status = ARY__DLTIN;
            datMsg( "A", dcb1->loc );
            errRep( " ", "The DELTA compressed array '^A' is invalid - the ZDIM "
                    "component is missing.", status );
         }

/* VALUE ----- */
         datThere( dcb1->loc, "VALUE", &there, status );
         if( there ){
            datFind( dcb1->loc, "VALUE", &loc2, status );
            datCopy( loc2, dcb2->loc, "VALUE", status );
            datAnnul( &loc2, status );

         } else if( *status == SAI__OK ){
            *status = ARY__DLTIN;
            datMsg( "A", dcb1->loc );
            errRep( " ", "The DELTA compressed array '^A' is invalid - the VALUE "
                    "component is missing.", status );
         }

/* FIRST_DATA ---------- */
         datThere( dcb1->loc, "FIRST_DATA", &there, status );
         if( there ){
            datFind( dcb1->loc, "FIRST_DATA", &loc2, status );
            datCopy( loc2, dcb2->loc, "FIRST_DATA", status );
            datAnnul( &loc2, status );

         } else if( *status == SAI__OK ){
            *status = ARY__DLTIN;
            datMsg( "A", dcb1->loc );
            errRep( " ", "The DELTA compressed array '^A' is invalid - the "
                    "FIRST_DATA component is missing.", status );
         }


/* FIRST_VALUE ---------- */
         datThere( dcb1->loc, "FIRST_VALUE", &there, status );
         if( there ){
            datFind( dcb1->loc, "FIRST_VALUE", &loc2, status );
            datCopy( loc2, dcb2->loc, "FIRST_VALUE", status );
            datAnnul( &loc2, status );

         } else if( *status == SAI__OK ){
            *status = ARY__DLTIN;
            datMsg( "A", dcb1->loc );
            errRep( " ", "The DELTA compressed array '^A' is invalid - the "
                    "FIRST_VALUE component is missing.", status );
         }

/* ZRATIO ------ */
         datThere( dcb1->loc, "ZRATIO", &there, status );
         if( there ){
            datFind( dcb1->loc, "ZRATIO", &loc2, status );
            datCopy( loc2, dcb2->loc, "ZRATIO", status );
            datAnnul( &loc2, status );

         } else if( *status == SAI__OK ){
            *status = ARY__DLTIN;
            datMsg( "A", dcb1->loc );
            errRep( " ", "The DELTA compressed array '^A' is invalid - the ZRATIO "
                    "component is missing.", status );
         }

/* Now do optional components. Copy these if they exist in the input, but
   do not report an error if they do not exist. */

/* SCALE ----- */
         datThere( dcb1->loc, "SCALE", &there, status );
         if( there ){
            datFind( dcb1->loc, "SCALE", &loc2, status );
            datCopy( loc2, dcb2->loc, "SCALE", status );
            datAnnul( &loc2, status );
         }


/* ZERO ---- */
         datThere( dcb1->loc, "ZERO", &there, status );
         if( there ){
            datFind( dcb1->loc, "ZERO", &loc2, status );
            datCopy( loc2, dcb2->loc, "ZERO", status );
            datAnnul( &loc2, status );
         }

/* REPEAT ------ */
         datThere( dcb1->loc, "REPEAT", &there, status );
         if( there ){
            datFind( dcb1->loc, "REPEAT", &loc2, status );
            datCopy( loc2, dcb2->loc, "REPEAT", status );
            datAnnul( &loc2, status );
         }

/* FIRST_REPEAT ------------ */
         datThere( dcb1->loc, "FIRST_REPEAT", &there, status );
         if( there ){
            datFind( dcb1->loc, "FIRST_REPEAT", &loc2, status );
            datCopy( loc2, dcb2->loc, "FIRST_REPEAT", status );
            datAnnul( &loc2, status );
         }

/* Ensure the storage form is now delta in both the data object and the
   DCB. */
         datThere( dcb2->loc, "VARIANT", &there, status );
         if( there ) datErase( dcb2->loc, "VARIANT", status );
         datNew0C( dcb2->loc, "VARIANT", 6, status );
         cmpPut0C( dcb2->loc, "VARIANT", "DELTA", status );
         strcpy( dcb2->form, "DELTA" );

/* Report an error if the input array is not a DELTA array. */
      } else if( *status == SAI__OK ){
         *status = ARY__FATIN;
         errRep( " ", "ary1Cpdlt: Input array is not a DELTA array (internal "
                 "programming error).", status );
      }

   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Cpdlt", status );

}
