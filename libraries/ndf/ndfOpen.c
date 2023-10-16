#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include <string.h>
#include "mers.h"

void ndfOpen_( const HDSLoc *loc, const char *name, const char *mode,
               const char *stat, int *indf, int *place, int *status ){
/*
*+
*  Name:
*     ndfOpen

*  Purpose:
*     Open an existing or new NDF.

*  Synopsis:
*     void ndfOpen( HDSLoc *loc, const char *name, const char *mode,
*                   const char *stat, int *indf, int *place, int *status )

*  Description:
*     This function finds an existing NDF data structure and returns an
*     identifier for it, or creates a placeholder for a new NDF.

*  Parameters:
*     loc
*        Locator to the enclosing HDS structure.
*     name
*        Pointer to a null terminated string holding the name of the HDS
*        structure component.
*     mode
*        Pointer to a null terminated string holding the type of NDF access
*        required: "READ", "UPDATE" or "WRITE".
*     stat
*        Pointer to a null terminated string holding the state of the NDF,
*        specifying whether it is known to exist or not: "NEW", "OLD", or
*        "UNKNOWN".
*     *indf
*        Returned holding the NDF identifier.
*     *place
*        Returned holding the NDF placeholder identifying the nominated
*        position in the data system.
*     *status
*        The global status.

*  Notes:
*     -  If the "stat" parameter is set to "NEW", then this function will
*     return a placeholder for a new NDF. If "stat" is set to "OLD", it
*     will search for an existing NDF. If "stat" is set to "UNKNOWN", it
*     will first search for an existing NDF but will return a placeholder
*     for a new NDF if an existing one cannot be found.
*     -  If this function succeeds, then a valid value will be returned for
*     "indf" if the NDF already existed, or for "place" if it did not
*     exist. The unused return parameter will be set to the appropriate
*     null value (NDF__NOID or NDF__NOPL respectively).
*     -  If "WRITE" access is specified for an existing NDF, then all the
*     NDF's components will be reset to an undefined state ready to receive
*     new values.  If "UPDATE" access is specified, the NDF"s components
*     will retain their values, which may then be modified.
*     -  An error will result if the "stat" parameter is set to "OLD" but
*     no existing NDF could be found. An error will also result if a
*     placeholder for a new NDF is to be returned but "READ" access was
*     requested.
*     -  The value given for the "name" parameter may be an HDS path name,
*     consisting of several fields separated by ".", so that an NDF can be
*     opened in a sub-component (or a sub-sub-component...) of the
*     structure identified by the locator "loc".  Array subscripts may also
*     be used in this component name.  Thus a string such as
*     "MYSTRUC.ZONE(2).IMAGE" could be used as a valid "name" value.
*     -  An NDF can be opened within an explicitly named container file by
*     supplying the symbolic value NULL for the "loc" parameter and
*     giving a full HDS object name (including a container file
*     specification) for the "name" parameter.
*     -  If a blank value is given for the "name" parameter, then the NDF
*     will be the object identified directly by the locator "loc".
*     -  If a placeholder is to be returned and the new NDF is to be a top-
*     level object, then a new container file will be created. Otherwise,
*     the container file and all structures lying above the new NDF should
*     already exist.
*     -  If the "loc" and "name" arguments identify a pre-existing object
*     which is not a top-level object, then this may be used as the basis
*     for the new NDF. An object which is to be used in this way must be an
*     empty scalar structure with an HDS type of "NDF".
*     -  The locator supplied as input to this function may later be
*     annulled without affecting the behaviour of the NDF_ system.
*     -  If this function is called with "status" set, then a value of
*     NDF__NOPL will be returned for the "place" parameter, and a value of
*     NDF__NOID will be returned for the "indf" parameter, although no
*     further processing will occur. The same values will also be returned
*     if the function should fail for any reason.
*     -  The NDF__NOPL and NDF__NOID constants are defined in the include
*     file "ndf.h". The NULL constant is defined in the include file
*     "dat_par.h" (see SUN/92).

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfPCB *pcb;          /* Pointer to new NDF entry in the PCB */
   char vmode[ NDF__SZMOD + 1 ];   /* Validated access mode string */
   char vstat[ NDF__SZSTA + 1 ];   /* Validated NDF state string */
   int canwrt;           /* NDF is writeable? */
   int iacc;             /* Loop counter for access control flags */

/* Set inital values for "indf" and "place". */
   *indf = NDF__NOID;
   *place = NDF__NOPL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check the access mode and state strings for validity. */
   ndf1Vmod( mode, vmode, sizeof( vmode ), status );
   ndf1Vstat( stat, vstat, sizeof( vstat ), status );
   if( *status == SAI__OK ) {

/* Initialise the ACB index. */
      acb = NULL;

/* OLD.
   ===
   First deal with existing NDFs. Find and import the data structure and
   export an NDF identifier for it. */
      if( !strcmp( vstat, "OLD" ) ) {
         ndf1Opfor( (HDSLoc *) loc, name, vmode, &acb, status );
         if( *status == SAI__OK ) {
            *indf = ndf1Expid( ( NdfObject * ) acb, status );

/* If an error occurred, then annul the ACB entry. */
            if( *status != SAI__OK ) ndf1Anl( &acb, status );
         }

/* NEW.
   ===
   Now deal with new NDFs. If READ access is not required, then obtain
   a PCB entry for the new NDF and export a placeholder for it. */
      } else if( !strcmp( vstat, "NEW" ) ) {
         if( strcmp( vmode, "READ" ) ) {
            NDF__DCB_LOCK_MUTEX;
            ndf1Plfor( (HDSLoc *) loc, name, &pcb, status );
            NDF__DCB_UNLOCK_MUTEX;
            if( *status == SAI__OK ) {
               *place = ndf1Expid( ( NdfObject * ) pcb, status );

/* If an error occurred, then annul the PCB entry. */
               if( *status != SAI__OK ) {
                  NDF__PCB_LOCK_MUTEX;
                  ndf1Annpl( 1, &pcb, status );
                  NDF__PCB_UNLOCK_MUTEX;
               }
            }

/* Report an error if READ access was requested. */
         } else {
            *status = NDF__MODIN;
            errRep( " ", "READ access is not permitted when accessing a "
                    "new NDF (possible programming error).", status );
         }

/* UNKNOWN.
   =======
   Now deal with NDFs of unknown state. Defer error reporting and
   attempt to access the NDF assuming it already exists. If this
   succeeds, then export an identifier for it (if an error occurs,
   annul the ACB entry). */
      } else {
         errMark();
         ndf1Opfor( (HDSLoc *) loc, name, vmode, &acb, status );
         if( *status == SAI__OK ) {
            *indf = ndf1Expid( ( NdfObject * ) acb, status );
            if( *status != SAI__OK ) ndf1Anl( &acb, status );

/* If the NDF could not be accessed because it doesn't exist, then
   annul the error. */
         } else if( ndf1Absnt( *status ) ) {
            errAnnul( status );

/* If READ access is not required, then obtain a PCB entry for a new
   NDF and export a placeholder for it. */
            if( strcmp( vmode, "READ" ) ) {
               NDF__DCB_LOCK_MUTEX;
               ndf1Plfor( (HDSLoc *) loc, name, &pcb, status );
               NDF__DCB_UNLOCK_MUTEX;
               if( *status == SAI__OK ) {
                  *place = ndf1Expid( ( NdfObject * ) pcb, status );

/* If an error occurred, then annul the PCB entry. */
                  if( *status != SAI__OK ) {
                     NDF__PCB_LOCK_MUTEX;
                     ndf1Annpl( 1, &pcb, status );
                     NDF__PCB_UNLOCK_MUTEX;
                  }
               }

/* Report an error if READ access was requested and the NDF didn't
   exist. */
            } else {
               *status = NDF__MODIN;
               errRep( " ", "READ access is not permitted when accessing a "
                       "new NDF (possible programming error).", status );
            }
         }

/* Release the error stack. */
         errRlse();
      }
   }

/* If no error has occurred and an existing NDF has been accessed, then
   check if READ access was requested. */
   if( ( *status == SAI__OK ) && ( acb ) ) {
      if( !strcmp( vmode, "READ" ) ) {

/* If so, then disable all unwanted modes of access. This must be done
   because it is possible to request READ access to an object for which
   a writeable locator has been supplied. It is also possible to access
   the same object twice - first for UPDATE and then for READ access. */
         acb->access = 0;

/* If any other access mode was requested, then examine all the ACB
   access control flags to ensure that the object is writeable. */
      } else {
         canwrt = 1;
         for( iacc = 0; iacc < NDF__ACC_MXACC; iacc++ ){
            canwrt = canwrt && ( NDF__BIT_TO_VAL(iacc) & acb->access );
         }

/* If it is not writeable, then a read-only HDS locator has been
   supplied, so report an error. */
         if( !canwrt ) {
            *status = NDF__ACDEN;
            msgSetc( "MODE", vmode );
            ndf1Amsg( "NDF", acb );
            errRep( " ", "^MODE access to the NDF structure ^NDF is not "
                    "available via the specified HDS locator (possible "
                    "programming error).", status );

/* Annul the ACB entry. */
            ndf1Anl( &acb, status );

/* If WRITE access was requested to an existing NDF, then reset any
   pre-existing NDF component values. */
         } else if( !strcmp( vmode, "WRITE" ) ) {
            ndf1Rst( acb, "*", status );
         }
      }
   }

/* If an error occurred, then reset the returned identifier and
   placeholder, report context information and call the error tracing
   function. */
   if( *status != SAI__OK ) {
      *indf = NDF__NOID;
      *place = NDF__NOPL;
      errRep( " ", "ndfOpen: Error opening an NDF data structure.", status );
      ndf1Trace( "ndfOpen", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

