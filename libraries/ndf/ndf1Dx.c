#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"
#include "ndf_ast.h"

void ndf1Dx( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Dx

*  Purpose:
*     Ensure that extension information is available in the DCB.

*  Synopsis:
*     void ndf1Dx( NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that information about a data object"s
*     extension (MORE) structure is available for a DCB entry. If this
*     information is already available, then it does nothing. Otherwise, it
*     obtains this information by inspecting the data object itself,
*     performing any necessary validation checks in the process.

*  Parameters:
*     dcb
*        Pointer to the DCB entry for which extension information is
*        required.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *locvar = NULL;/* Locator to VARIANT component */
   char type[ DAT__SZTYP + 1 ];    /* HDS data type string */
   hdsbool_t there;      /* Whether a component is present */
   hdsdim dim[ DAT__MXDIM ];       /* Array of HDS dimensions */
   int ndim;             /* Number of HDS dimensions */
   char *pntr;           /* Pointer to mapped VARIANT value */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* There is nothing to do if extension information is already available. */
   if( !dcb->kx ) {

/* Initialise the extension (MORE) structure locator and see if a MORE
   structure exists in the NDF. */
      dcb->xloc = NULL;
      datThere( dcb->loc, "MORE", &there, status );
      if( *status == SAI__OK ) {

/* If so, then obtain a locator to it, storing it in the DCB. */
         if( there ) {
            datFind( dcb->loc, "MORE", &dcb->xloc, status );

/* Obtain the MORE structure's type and shape. */
            datType( dcb->xloc, type, status );
            datShape( dcb->xloc, DAT__MXDIM, dim, &ndim, status );
            if( *status == SAI__OK ) {

/* If its type is not "EXT", but the "Ndf_TCB_warn" flag is set, then issue a
   warning message, but continue. Use a mutex to serialise access to the
   flag since it is a global variable. */
               if( strcmp( type, "EXT" ) ) {
                  NDF__TCB_LOCK_MUTEX;
                  if( Ndf_TCB_warn ) {
                     errMark();
                     *status = NDF__TYPIN;
                     msgSetc( "BADTYPE", type );
                     ndf1Dmsg( "NDF", dcb );
                     errRep( " ", "Warning: the MORE component in the NDF "
                             "structure ^NDF has an invalid data type of "
                             "'^BADTYPE'; its type should be 'EXT'.", status );
                     errFlush( status );
                     errRlse();
                  }
                  NDF__TCB_UNLOCK_MUTEX;
               }

/* If it is not scalar, then report an error. */
               if( *status == SAI__OK ) {
                  if( ndim != 0 ) {
                     *status = NDF__NDMIN;
                     msgSeti( "BADNDIM", ndim );
                     ndf1Dmsg( "NDF", dcb );
                     errRep( " ", "The MORE component in the NDF structure "
                             "^NDF is ^BADNDIM-dimensional; it should be "
                             "scalar.", status );
                  }
               }
            }

/* See if a VARIANT component is present in the MORE structure. */
            datThere( dcb->xloc, "VARIANT", &there, status );
            if( *status == SAI__OK ) {

/* If so, then obtain a locator to it and determine its type and shape. */
               if( there ) {
                  datFind( dcb->xloc, "VARIANT", &locvar, status );
                  datType( locvar, type, status );
                  datShape( locvar, DAT__MXDIM, dim, &ndim, status );
                  if( *status == SAI__OK ) {

/* Check that the VARIANT is a character object and report an error if
   it is not. */
                     if( strncmp( type, "_CHAR*", 6 ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "MORE", dcb->xloc );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The VARIANT component in the NDF "
                                "extension structure ^MORE has an invalid "
                                "data type of '^BADTYPE'; it should be of "
                                "type '_CHAR'.", status );

/* Check that it is scalar and report an error if it is not. */
                     } else if( ndim != 0 ) {
                        *status = NDF__NDMIN;
                        datMsg( "MORE", dcb->xloc );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The VARIANT component in the NDF "
                                "extension structure ^MORE is "
                                "^BADNDIM-dimensional; it should be "
                                "scalar.", status );

/* If the VARIANT component is OK so far, then map it and determine its
   length. */
                     } else {
                        pntr = ndf1Hmp0C( locvar, status );
                        if( pntr ) {

/* If its value is not "SIMPLE", then report an error. */
                           if( !astChrMatch( pntr, "SIMPLE" ) ) {
                              *status = NDF__VARIN;
                              datMsg( "MORE", dcb->xloc );
                              msgSetc( "BADVARIANT", pntr );
                              errRep( " ", "The VARIANT component in the "
                                      "NDF extension structure ^MORE has "
                                      "an invalid value of '^BADVARIANT'; "
                                      "only the value 'SIMPLE' is "
                                      "defined.", status );
                           }
                           pntr = astFree( pntr );
                        }
                     }
                  }

/* Annul the locator to the VARIANT component. */
                  datAnnul( &locvar, status );
               }
            }

/* If an error occurred, then annul the extension (MORE) structure
   locator. */
            if( *status != SAI__OK ) datAnnul( &dcb->xloc, status );
         }
      }

/* Note whether extension information is now available in the DCB. */
      dcb->kx = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dx", status );

}
