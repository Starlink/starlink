#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "star/hds.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include <string.h>
#include "ndf_ast.h"

void ndf1Dimp( HDSLoc *loc, NdfDCB **dcb, int *status ){
/*
*+
*  Name:
*     ndf1Dimp

*  Purpose:
*     Import a data object, creating a new DCB entry for it.

*  Synopsis:
*     void ndf1Dimp( HDSLoc *loc, NdfDCB **dcb, int *status )

*  Description:
*     This function imports an NDF structure into the NDF_ system, creating
*     a new DCB entry for it.

*  Parameters:
*     loc
*        HDS locator to the NDF structure to be imported.
*     *dcb
*        Pointer to the new DCB entry for the data object.
*     *status
*        The global status.

*  Notes:
*     -  The function makes a "cloned" copy of the HDS locator supplied;
*     the latter may later be annulled without affecting the operation of
*     the NDF_ system.
*     -  If "status" is set on entry, then the function will return a value
*     of zero for the "dcb" parameter, although no further processing will
*     occur.
*     -  A value of zero will also be returned for the "dcb" parameter if
*     the function fails.

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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *locvar = NULL;/* Locator to VARIANT component */
   char *pntr;           /* Pointer to mapped VARIANT value */
   char type[ DAT__SZTYP + 1 ];    /* HDS data type */
   hdsbool_t prim;       /* Whether object is a primitive */
   hdsbool_t there;      /* Whether VARIANT component is present */
   hdsdim dim[ DAT__MXDIM ];       /* Object dimensions */
   int ndim;             /* Number of object dimensions */
   int nlev;             /* Levels in HDS path name */

/* Set an initial value of zero for the "dcb" parameter. */
   *dcb = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain the data object's type, see if it is primitive, and determine
   its shape. */
   datType( loc, type, status );
   datPrim( loc, &prim, status );
   datShape( loc, DAT__MXDIM, dim, &ndim, status );
   if( *status == SAI__OK ) {

/* Report an error if the object is not scalar. */
      if( ndim != 0 ) {
         *status = NDF__NDMIN;
         datMsg( "NDF", loc );
         msgSeti( "BADNDIM", ndim );
         errRep( " ", "The NDF structure ^NDF is ^BADNDIM-dimensional; it "
                 "should be scalar.", status );

/* If it is a primitive, then it cannot contain NDF components, so
   report an error. */
      } else if( prim ) {
         *status = NDF__TYPIN;
         datMsg( "NDF", loc );
         msgSetc( "BADTYPE", type );
         errRep( " ", "The NDF structure ^NDF has an invalid data type of "
                 "'^BADTYPE'; it should be a structure.", status );

/* If its type is not NDF, then report and flush a warning message, but
   do not set "status" or alter the error stack. */
// <Temporarily removed for compatibilty with existing data structures>
//        ELSE IF ( TYPE .NE. 'NDF' ) THEN
//           CALL ERR_MARK
//           STATUS = NDF__TYPIN
//           CALL DAT_MSG( 'NDF', LOC )
//           CALL MSG_SETC( 'TYPE', TYPE )
//           CALL ERR_REP( 'NDF1_DIMP_TYPE',
//    :      'Warning: the NDF structure ^NDF has a non-standard ' //
//    :      'type of ''^TYPE''.', STATUS )
//           CALL ERR_FLUSH( STATUS )
//           CALL ERR_RLSE
      }
   }

/* See if a VARIANT component is present in the NDF structure. */
   datThere( loc, "VARIANT", &there, status );
   if( *status == SAI__OK ) {

/* If so, then obtain a locator to it and determine its type and shape. */
      if( there ) {
         datFind( loc, "VARIANT", &locvar, status );
         datType( locvar, type, status );
         datShape( locvar, DAT__MXDIM, dim, &ndim, status );
         if( *status == SAI__OK ) {

/* Check that the VARIANT is a character object and report an error if
   it is not. */
            if( strncmp( type, "_CHAR*", 6 ) ) {
               *status = NDF__TYPIN;
               datMsg( "NDF", loc );
               msgSetc( "BADTYPE", type );
               errRep( " ", "The VARIANT component in the NDF structure "
                       "^NDFhas an invalid HDS type of '^BADTYPE'; it "
                       "should be of type '_CHAR'.", status );

/* Check that it is scalar and report an error if it is not. */
            } else if( ndim != 0 ) {
               *status = NDF__NDMIN;
               datMsg( "NDF", loc );
               msgSeti( "BADNDIM", ndim );
               errRep( " ", "The VARIANT component in the NDF structure "
                       "^NDF is ^BADNDIM-dimensional; it should be "
                       "scalar.", status );

/* If the VARIANT component is OK so far, then map it. */
            } else {
               pntr = ndf1Hmp0C( locvar, status );
               if( pntr ) {

/* If its value is not "SIMPLE", and the "Ndf_TCB_warn" flag is set, then
   issue a warning message (do not leave "status" set or modify the error
   stack because a lot of people have already abused this component for
   their own use and they"d probably be annoyed if the NDF_ functions
   refused to handle their data as a result). */
                  if( !astChrMatch( pntr, "SIMPLE" ) ) {
                     if( Ndf_TCB_warn ) {
                        errMark();
                        *status = NDF__VARIN;
                        datMsg( "NDF", loc );
                        msgSetc( "BADVARIANT", pntr );
                        errRep( " ", "Warning: the VARIANT component in "
                                "the NDF structure ^NDF has an invalid "
                                "value of '^BADVARIANT'; only the value "
                                "'SIMPLE' is defined.", status );
                        errFlush( status );
                        errRlse();
                     }
                  }
                  pntr = astFree( pntr );
               }
            }
         }

/* Annul the locator to the VARIANT component. */
         datAnnul( &locvar, status );
      }
   }

/* Obtain an index to a free slot in the DCB. */
   *dcb = ndf1Ffs( NDF__DCBTYPE, status );
   if( *status == SAI__OK ) {

/* Clone the NDF locator supplied, storing the new locator in the DCB.
   Promote it to a primary locator and link it into a private group
   (to prevent any external events from annulling it without the NDF_
   system's knowledge). */
      datClone( loc, &(*dcb)->loc, status );
      prim = 1;
      datPrmry( 1, &(*dcb)->loc, &prim, status );
      hdsLink( (*dcb)->loc, "NDF_DCB", status );

/* Obtain the data object file and path names and enter them into the
   DCB. */
      hdsTrace( (*dcb)->loc, &nlev, (*dcb)->path,
                (*dcb)->file, status, sizeof( (*dcb)->path ),
                sizeof( (*dcb)->file ) );

/* If there was an error, then annul the cloned locator and release the
   slot allocated in the DCB. */
      if( *status != SAI__OK ) {
         datAnnul( &(*dcb)->loc, status );
         *dcb = ndf1Rls( ( NdfObject * ) *dcb, status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dimp", status );

}
