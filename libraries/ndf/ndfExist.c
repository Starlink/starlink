#include "star/subpar.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "par_err.h"
#include "ndf.h"
#include "mers.h"
#include <string.h>

void ndfExist_( const char *param, const char *mode, int *indf, int *status ){
/*
*+
*  Name:
*     ndfExist

*  Purpose:
*     See if an existing NDF is associated with an ADAM parameter.

*  Synopsis:
*     void ndfExist( const char *param, const char *mode, int *indf,
*                    int *status )

*  Description:
*     This function determines if an existing (and accessible) NDF is
*     associated with an ADAM parameter. If it is, then an identifier is
*     returned for it. If not, then the function returns with an identifier
*     value of NDF__NOID; this then allows the NDF structure to be created
*     (e.g. using ndfCreat) if required.

*  Parameters:
*     param
*        Pointer to a null terminated string holding the name of the ADAM
*        parameter.
*     mode
*        Pointer to a null terminated string holding the type of NDF access
*        required: "READ", "UPDATE" or "WRITE".
*     *indf
*        Returned holding the NDF identifier.
*     *status
*        The global status.

*  Notes:
*     -  If "WRITE" access is specified, then all the NDF's components will
*     be reset to an undefined state ready to receive new values. If
*     "UPDATE" access is specified, the NDF's components will retain their
*     values, which may then be modified.
*     -  The behaviour of this function is the same as ndfAssoc, except
*     that in the event of the NDF structure not existing (or being
*     inaccessible), control is returned to the application with an
*     identifier value of NDF__NOID, rather than re-prompting the user.
*     -  Note that unlike the "datExist" function, on which it is modelled,
*     this function does not set a "status" value if the data structure
*     does not exist.
*     -  If this function is called with "status" set, then a value of
*     NDF__NOID will be returned for the "indf" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason.  The NDF__NOID constant
*     is defined in the header file "ndf.h".

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
   HDSLoc *junk = NULL;  /* Junk locator value */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   char name[ NDF__SZPAR + 1 ];    /* NDF name string */
   char vmode[ NDF__SZMOD + 1 ];   /* Validated access mode string */
   int hasloc;           /* Is locator associated with parameter? */
   int tstat;            /* Temporary status variable */
   size_t ipar;          /* Parameter table index */

/* Set an initial value for the "indf" parameter. */
   *indf = NDF__NOID;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Mark the error stack, so that flushing error messages doesn't
   disturb any pre-existing error stack contents. */
   errMark();

/* Find the parameter index in the parameter tables and see whether the
   parameter already has a locator associated with it. */
   subParFindpar( param, &ipar, status );
   subParGetloc( ipar, &hasloc, &junk, status );

/* Check the access mode string for validity. */
   ndf1Vmod( mode, vmode, sizeof( vmode ), status );

/* If the access mode string is valid, then check the access mode
   against that given in the interface file. Report an error if it
   conflicts. */
// (Removed because it requires access to the SUBPAR_ common blocks at
// present.)
//     IF ( STATUS .EQ. SAI__OK ) THEN
//        IF ( ( VMODE .NE. 'READ' ) .AND.
//    :        ( .NOT. PARWRITE( IPAR ) ) ) THEN
//           STATUS = NDF__ACDEN
//           CALL MSG_SETC( 'MODE', MODE )
//           CALL MSG_SETC( 'PARAM', PARAM )
//           CALL ERR_REP( 'NDF_EXIST_ACON',
//    :                    '^MODE access to the ''%^PARAM'' ' //
//    :                    'parameter conflicts with the access ' //
//    :                    'mode specified in the ADAM interface ' //
//    :                    'file (possible programming error).',
//    :                    STATUS )
//        END IF
//     END IF
   if( *status == SAI__OK ) {

/* Obtain the NDF name via the parameter and attempt to open the data
   object. */
      subParGetname( ipar, name, sizeof(name), status );
      acb = NULL;

/* Loop until a valid NDF structure has been obtained, or a
   non-recoverable error occurs, or we determine that the NDF does not
   exist. */
      while( *status == SAI__OK ){
         ndf1Opfor( NULL, name, vmode, &acb, status );

/* If this failed for any reason except that the NDF doesn't exist,
   then the user must be re-prompted. Report contextual information and
   flush any error messages. */
         if( ( *status != SAI__OK ) && ( !ndf1Absnt( *status ) ) ) {
            msgSetc( "PARAM", param );
            errRep( " ", "ndfExist: Unable to associate an NDF "
                    "structurewith the '%^PARAM' parameter.", status );
            errFlush( status );

/* Cancel the parameter association, annulling any further error
   messages this may generate. */
            subParCancl( ipar, status );
            errAnnul( status );

/* Obtain the NDF name via the parameter and attempt to open the data
   object. */
            subParGetname( ipar, name, sizeof(name), status );
            acb = NULL;

         } else {
            break;
         }
      }
   }

/* After importing a valid data structure, check if the parameter
   system already has locators for it. If not, then save locators for
   the NDF object and the container file in the parameter system and
   link the object locator with the parameter name. */
   if( *status == SAI__OK ) {
      if( !hasloc ) ndf1Ptloc( param, ipar, vmode, acb, status );

/* If read access was requested, then disable all unwanted access modes. */
      if( *status == SAI__OK ) {
         if( !strcmp( vmode, "READ" ) ) {
            acb->access = 0;

/* If write access was requested, then reset any pre-existing NDF
   component values. */
         } else if( !strcmp( vmode, "WRITE" ) ) {
            ndf1Rst( acb, "*", status );
         }
      }

/* Export an NDF_ identifier. */
      *indf = ndf1Expid( ( NdfObject * ) acb, status );

/* If an error occurred, then annul any ACB entry which may have been
   acquired. */
      if( *status != SAI__OK ) {
         ndf1Anl( &acb, status );

/* Cancel the parameter association, without generating any further
   error messages. */
         tstat = SAI__OK;
         errMark();
         subParCancl( ipar, &tstat );
         errAnnul( &tstat );
         errRlse();
      }
   }

/* If an error occurred, then classify it... */

/* If the NDF does not exist, then this condition is simply indicated
   by a returned "indf" value of NDF__NOID, so annul the error. */
   if( ndf1Absnt( *status ) ) {
      errAnnul( status );

/* If an "abort" was requested, then annul any error messages and issue
   an appropriate new one. */
   } else if( *status == PAR__ABORT ) {
      tstat = *status;
      errAnnul( &tstat );
      msgSetc( "PARAM", param );
      errRep( " ", "Aborted attempt to associate an existing NDF structure "
              "with the '%^PARAM' parameter.", status );

/* If a "null" NDF was specified, then annul any error messages and
   issue an appropriate new one. */
   } else if( *status == PAR__NULL ) {
      tstat = *status;
      errAnnul( &tstat );
      msgSetc( "PARAM", param );
      errRep( " ", "Null NDF structure specified for the '%^PARAM' "
              "parameter.", status );

/* For other errors, add context information and call the error tracing
   function. */
   } else if( *status != SAI__OK ) {
      msgSetc( "PARAM", param );
      errRep( " ", "ndfExist: Error determining if an existing NDF "
              "structure is associated with the '%^PARAM' parameter.", status );
      ndf1Trace( "ndfExist", status );
   }

/* Release the error stack. */
   errRlse();

/* Restablish the original AST status pointer */
   NDF_FINAL

}

