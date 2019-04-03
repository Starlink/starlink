#include "star/subpar.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "par_err.h"
#include "ndf.h"
#include "mers.h"

void ndfCrepl_( const char *param, int *place, int *status ){
/*
*+
*  Name:
*     ndfCrepl

*  Purpose:
*     Create a new NDF placeholder via the ADAM parameter system.

*  Synopsis:
*     void ndfCrepl( const char *param, int *place, int *status )

*  Description:
*     This function creates a new NDF placeholder via the ADAM parameter
*     system, associates it with a parameter, and returns an identifier for
*     it.  A placeholder is used to identify a position in the underlying
*     data system (HDS) and may be passed to other functions (e.g. ndfNew)
*     to indicate where a newly created NDF should be positioned.

*  Parameters:
*     param
*        Pointer to a null terminated string holding the name of the ADAM
*        parameter.
*     *place
*        Returned holding the NDF placeholder identifying the nominated
*        position in the data system.
*     *status
*        The global status.

*  Notes:
*     -  Placeholders are intended only for local use within an application
*     and only a limited number of them are available simultaneously. They
*     are always annulled as soon as they are passed to another function to
*     create a new NDF, where they are effectively exchanged for an NDF
*     identifier.
*     -  If this function is called with "status" set, then a value of
*     NDF__NOPL will be returned for the "place" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason. The NDF__NOPL constant is
*     defined in the header file "ndf.h".

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
   NdfPCB *pcb;          /* Pointer to placeholder entry in the PCB */
   char name[ NDF__SZPAR + 1 ];    /* Placeholder name string */
   size_t ipar;             /* Parameter table index */
   int tstat;            /* Temporary status variable */

/* Set an initial value for the "place" parameter. */
   *place = NDF__NOPL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Mark the error stack, so that annulling error messages doesn't
   disturb any pre-existing error stack contents. */
   errMark();

/* Find the parameter index in the parameter tables. */
   subParFindpar( param, &ipar, status );

/* Obtain the placeholder location via the parameter. */
   subParGetname( ipar, name, sizeof(name), status );

/* Loop until a valid NDF structure has been created or a
   non-recoverable error occurs. */
   while( *status == SAI__OK ){

/* Create a placeholder entry for the object in the PCB. */
      ndf1Plfor( NULL, name, &pcb, status );

/* If this failed, then the user must be re-prompted. Report contextual
   information and flush any error messages. */
      if( *status != SAI__OK ) {
         msgSetc( "PARAM", param );
         errRep( " ", "ndfCrepl: Unable to create a new NDF placeholder "
                 "via the '%^PARAM' parameter.", status );
         errFlush( status );

/* Cancel the parameter association, annulling any further error
   messages this may generate. */
         subParCancl( ipar, status );
         errAnnul( status );

/* Obtain the placeholder location via the parameter. */
         subParGetname( ipar, name, sizeof(name), status );

      } else {
         break;
      }
   }

/* Export the required placeholder. */
   if( *status == SAI__OK ) {
      *place = ndf1Expid( ( NdfObject * ) pcb, status );

/* If an error occurred, then annul the PCB entry. */
      if( *status != SAI__OK ) ndf1Annpl( 1, &pcb, status );
   }

/* If an error occurred, then classify it... */

/* If an "abort" was requested, then annul any error messages and issue
   an appropriate new one. */
   if( *status == PAR__ABORT ) {
      tstat = *status;
      errAnnul( &tstat );
      msgSetc( "PARAM", param );
      errRep( " ", "Aborted creation of a new NDF placeholder via the "
              "'%^PARAM' parameter.", status );

/* If an "null" NDF was specified, then annul any error messages and
   issue an appropriate new one. */
   } else if( *status == PAR__NULL ) {
      tstat = *status;
      errAnnul( &tstat );
      msgSetc( "PARAM", param );
      errRep( " ", "Null NDF placeholder specified for the '%^PARAM' "
              "parameter.", status );

/* For other errors, add context information and call the error tracing
   function. */
   } else if( *status != SAI__OK ) {
      msgSetc( "PARAM", param );
      errRep( " ", "ndfCrepl: Error creating an NDF placeholder via the "
              "'%^PARAM' parameter.", status );
      ndf1Trace( "ndfCrepl", status );
   }

/* Release the error stack. */
   errRlse();

/* Restablish the original AST status pointer */
   NDF_FINAL

}

