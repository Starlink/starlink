#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include "prm_par.h"
#include <string.h>

void ary1Vscl( HDSLoc *loc, int *status ) {
/*
*+
*  Name:
*     ary1Vscl

*  Purpose:
*     Verify the scale and zero values associated with a scaled array.

*  Synopsis:
*     void ary1Vscl( HDSLoc *loc, int *status )

*  Description:
*     This function ensures that the supplied HDS locator contains two
*     scalar components named SCALE and ZERO, which have the same data
*     type. The SCALE value must be larger than zero. Both SCALE and ZERO
*     must not be bad. An error is reported if any problem is encoutered
*     with the supplied LOCATOR.

*  Parameters:
*     loc
*        Locator for the HDS object containing the SCALE and ZERO values.
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
   HDSLoc *loc2 = NULL;       /* Locator to SCALE or ZERO component */
   char styp[DAT__SZTYP+1];   /* Data type of the SCALE value */
   char ztyp[DAT__SZTYP+1];   /* Data type of the ZERO value */
   double scale;              /* The scale value */
   double zero;               /* The zero value */
   hdsdim dim[ARY__MXDIM];    /* Dimensions of SCALE or ZERO component */
   int ndim;                  /* No. of dimensions */
   int there;                 /* Whether a component exists */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* See if there is a SCALE component present in the data object */
   datThere( loc, "SCALE", &there, status );

/* If not, report an error. */
   if( !there ){
      if( *status == SAI__OK ){
         *status = ARY__SCLIN;
         errRep( " ", "The SCALE component in missing (internal ARY programming"
                 " error).", status );
      }

/* If there is, then obtain its shape and type. */
   } else {
      datFind( loc, "SCALE", &loc2, status );
      datShape( loc2, 1, dim, &ndim, status );
      datType( loc2, styp, status );

/* Check that it is scalar and report an error if it is not. */
      if( ndim != 0 ){
         if( *status == SAI__OK ){
            *status = ARY__NDMIN;
            msgSeti( "BADNDIM", ndim );
            errRep( " ", "The SCALE component is ^BADNDIM-dimensional; it "
                    "should be a scalar.", status );
         }
      }

/* Report an error if it is zero or negative or bad. */
      datGet0D( loc2, &scale, status );
      if( scale <= 0.0 || scale == VAL__BADD ){
         if( *status == SAI__OK ){
            *status = ARY__SCLIN;
            msgSetd( "SCALE", scale );
            errRep( " ", "The SCALE component has an invalid zero, negative or"
                    " <bad> value: ^SCALE.", status );
         }
      }

/* Annul the locator to the scale component. */
      datAnnul( &loc2, status );
   }

/* See if there is a ZERO component present in the data object */
   datThere( loc, "ZERO", &there, status );

/* If not, report an error. */
   if( !there ){
      if( *status == SAI__OK ){
         *status = ARY__SCLIN;
         errRep( " ", "The ZERO component in missing (internal ARY programming"
                 " error).", status );
      }

/* If there is, then obtain its shape and data type. */
   } else {
      datFind( loc, "ZERO", &loc2, status );
      datShape( loc2, 1, dim, &ndim, status );
      datType( loc2, ztyp, status );

/* Check that it is scalar and report an error if it is not. */
      if( ndim != 0 ){
         if( *status == SAI__OK ){
            *status = ARY__NDMIN;
            msgSeti( "BADNDIM", ndim );
            errRep( " ", "The ZERO component is ^BADNDIM-dimensional; it "
                    "should be a scalar.", status );
         }
      }

/* Report an error if it is bad. */
      datGet0D( loc2, &zero, status );
      if( zero == VAL__BADD ){
         if( *status == SAI__OK ){
            *status = ARY__SCLIN;
            errRep( " ", "The ZERO component has an invalid <bad> value.",
                    status );
         }
      }

/* Report an error if they have different data types. */
      if( strcmp( styp, ztyp ) ){
         if( *status == SAI__OK ){
            *status = ARY__SCLIN;
            errRep( " ", "The ZERO and SCALE components have different data"
                    "types.", status );
         }
      }

/* Annul the locator to the zero component. */
      datAnnul( &loc2, status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Vscl", status );

}
