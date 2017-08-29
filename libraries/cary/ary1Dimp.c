#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void ary1Dimp( HDSLoc *loc, AryDCB **dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dimp

*  Purpose:
*     Import a data object, creating a new DCB entry for it.

*  Synopsis:
*     void ary1Dimp( HDSLoc *loc, AryDCB **dcb, int *status )

*  Description:
*     The routine imports an array structure into the ARY_ system,
*     creating a new DCB entry for it.

*  Parameters:
*     loc
*        HDS locator to the array structure to be imported.
*     dcb
*        Address of a variable in which to return a pointer to the new DCB.
*     status
*        The global status.

*  Notes:
*     -  The routine makes a "cloned" copy of the HDS locator supplied;
*     the latter may later be annulled without affecting the operation
*     of the ARY_ system.
*     -  If STATUS is set on entry, then the routine will return a
*     value of NULL for the DCB pointer, although no further
*     processing will occur.
*     -  A value of NULL will also be returned if the routine fails.

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
   HDSLoc *loc1=NULL;         /* Cloned locator */
   int i;                     /* Loop counter for dimensions */
   int lock_status;           /* Type of lock on supplied HDS object */
   int nlev;                  /* Levels in HDS path name */

/* Set an initial value of NULL for the returned DCB pointer. */
   if( dcb ) *dcb = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK || !dcb ) return;

/* The supplied locator must be locked by the current thread before it
   can be imported. We will not be attempting to modify the object in
   this function, and so either a read-write or read-only lock will do.
   So report an error if the object is not locked by the current thread. */
   lock_status = datLocked( loc, status );
   if( lock_status != 1 && lock_status != 3 && *status == SAI__OK ) {
      *status = ARY__THREAD;
      datMsg( "O", loc );
      if( lock_status == 0 || lock_status == 4 ) {
         errRep( " ", "Cannot import HDS object '^O' as an ARRAY structure: "
                 "the HDS object is not locked by the current thread "
                 "(programming error).", status );
      } else if( lock_status == 2 ) {
         errRep( " ", "Cannot import HDS object '^O' as an ARRAY structure: "
                 "the HDS object is locked for read-write access by another "
                 "thread (programming error).", status );
      }
   }

/* Clone the locator supplied and link the cloned locator into a private
   group (to prevent any external events from annulling it without the
   ARY_ system's knowledge). Allocate a new DCB structure. */
   datClone( loc, &loc1, status );
   hdsLink( loc1, "ARY_DCB", status );
   *dcb = (AryDCB *) ary1Ffs( ARY__DCBTYPE, status );
   if( *status == SAI__OK ){

/* Store the data object locator in the DCB and initialise the DCB flags to
   indicate that no information about the object is yet available. */
      (*dcb)->loc = loc1;
      (*dcb)->kform = 0;
      (*dcb)->ktype = 0;
      (*dcb)->kbnd = 0;
      (*dcb)->kmode = 0;
      (*dcb)->kstate = 0;
      (*dcb)->kbad = 0;
      (*dcb)->kscl = 0;

/* Initialise the reference and mapping counts and set the disposal mode to
   'KEEP', indicating that this is not a temporary object. */
      (*dcb)->refcount = 0;
      (*dcb)->nread = 0;
      (*dcb)->nwrite = 0;
      strcpy( (*dcb)->dispose, "KEEP" );

/* Initialise the accumulated pixel shifts to zero. */
      for( i = 0; i < ARY__MXDIM; i++ ){
         (*dcb)->shift[ i ] = 0;
      }

/* Obtain form information for the array, which is written into the DCB. */
      ary1Dfrm( *dcb, status );
      if( *status == SAI__OK ){

/* If the array has a supported form, then obtain the data object file and
   path names and enter them into the DCB. */
         if( ( !strcmp( (*dcb)->form, "PRIMITIVE" ) ) ||
             ( !strcmp( (*dcb)->form, "SCALED" ) ) ||
             ( !strcmp( (*dcb)->form, "DELTA" ) ) ||
             ( !strcmp( (*dcb)->form, "SIMPLE" ) ) ){
            hdsTrace( (*dcb)->loc, &nlev, (*dcb)->path, (*dcb)->file,
                      status, sizeof((*dcb)->path), sizeof((*dcb)->file) );

/* If the array form is not one of those supported, then report an error. */
         } else {
            *status = ARY__USFRM;
            datMsg( "ARRAY", (*dcb)->loc );
            msgSetc( "USFORM", (*dcb)->form );
            errRep( "ARY1_DIMP_USF",
                    "Sorry, the array structure ^ARRAY is of '^USFORM'"
                    "form; this is not currently supported by the ARY_"
                    "routines.", status );
         }
      }
   }

/* If there was an error, then annul the cloned locator and release the
   slot allocated in the DCB. Reset the DCB pointer to NULL. */
   if( *status != SAI__OK ){
      datAnnul( &loc1, status );
      dcb = ary1Rls( (AryObject *) dcb, status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dimp", status );

}
