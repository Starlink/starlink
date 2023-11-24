#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_err.h"

void ary1Dmod( AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dmod

*  Purpose:
*     Ensure that access mode information is available for a data
*     object.

*  Synopsis:
*     void ary1Dmod( AryDCB *dcb, int *status )

*  Description:
*     The routine ensures that information about the access mode of an
*     array is available. It does nothing if this information is already
*     present in the DCB. Otherwise, it obtains the information by
*     inspecting the data object itself and stores the result in the
*     DCB. Only those checks needed to obtain the access mode
*     information are performed on the data object.

*  Parameters:
*     dcb
*        Pointer to the DCB.
*     status
*        The global status.

* Prior Requirements:
*     - The DCB mutex must be locked.

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
   HDSLoc *loccel=NULL;       /* Locator to first pixel */
   const char *map;           /* Mode for mapping first pixel */
   hdsdim dimcel[ARY__MXDIM]; /* First pixel position */
   hdsdim dummy;              /* Dummy dimension array */
   int i;                     /* Loop counter for dimensions */
   int ncel;                  /* Number of dimensions for cell */
   int set;                   /* HDS state of component */
   void *pntr;                /* Pointer to first pixel */

   ARY__DCB_ASSERT_MUTEX;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Do nothing if access mode information is already available in the DCB. */
   if( !dcb->kmode ){

/* Ensure that form information is available for the data object. */
      ary1Dfrm( dcb, status );
      if( *status != SAI__OK ) goto L999;

/* Primitive, scaled, simple and delta arrays.
   ===========================================
   These are all handled in the same way. */
      if( ( !strcmp( dcb->form, "PRIMITIVE" ) ) ||
          ( !strcmp( dcb->form, "SCALED" ) ) ||
          ( !strcmp( dcb->form, "DELTA" ) ) ||
          ( !strcmp( dcb->form, "SIMPLE" ) ) ){

/* Ensure that type (and complexity) information, component locators and
   array bounds information are available. */
         ary1Dtyp( dcb, status );
         ary1Dbnd( dcb, status );

/* If the locator is locked for read-only access, indicate that the array
   has READ access. */
         if( datLocked( dcb->dloc, 0, status ) == 3 ) {
            strcpy( dcb->mode, "READ" );

/* Even if the locator is locked for read-write, we may still only have
   read access if the underlying HDS object was opened for read-only
   access. */
         } else if( *status == SAI__OK ) {

/* Obtain a locator to the first pixel in the non-imaginary array
   component. Note, the DATA component in a DELTA array is always
   one-dimensional. */
            if( strcmp( dcb->form, "DELTA" ) ){
               for( i = 0; i < dcb->ndim; i++ ){
                  dimcel[ i ] = 1;
               }
               ncel = dcb->ndim;
            } else {
               dimcel[ 0 ] = 1;
               ncel = 1;
            }

            datCell( dcb->dloc, ncel, dimcel, &loccel, status );

/* Obtain the component's HDS state and select an access mode for mapping
   the first pixel which will not conflict with this. */
            datState( dcb->dloc, &set, status );
            if( *status == SAI__OK ){
               if( set ){
                  map = "UPDATE";
               } else {
                  map = "WRITE";
               }

/* Mark the error stack and map the first pixel. */
               errMark();
               datMap( loccel, dcb->type, map, 0, &dummy, &pntr, status );

/* If the mapping succeeded, then UPDATE access is available. Unmap the
   first pixel and restore the component's HDS state. */
               if( *status == SAI__OK ){
                  strcpy( dcb->mode, "UPDATE" );
                  ary1Hunmp( loccel, status );
                  if( !set ) datReset( dcb->dloc, status );

/* If mapping failed with an access conflict, then the array is READ
   protected. Annul the error. */
               } else if( *status == DAT__ACCON ){
                  strcpy( dcb->mode, "READ" );
                  errAnnul( status );
               }

/* Release the error stack. */
               errRlse();
            }

/* Annul the locator to the first pixel. */
            datAnnul( &loccel, status );
         }

/* If the form information in the DCB is not valid, then report an error. */
      } else {
         *status = ARY__FATIN;
         msgSetc( "BADFORM", dcb->form );
         errRep( "ARY1_DMOD_FRM",
                 "Unsupported array form '^BADFORM' found in Data Control "
                 "Block (internal programming error).", status );
      }

/* Note if access mode information is now available in the DCB. */
L999:
      dcb->kmode = ( *status == SAI__OK );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dmod", status );

}
