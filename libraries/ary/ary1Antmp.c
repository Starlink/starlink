#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"

void ary1Antmp( HDSLoc **loc, int *status ) {
/*
*+
*  Name:
*     ary1Antmp

*  Purpose:
*     Annul a locator to a temporary object, thereby erasing the object.

*  Synopsis:
*     void ary1Antmp( HDSLoc **loc, int *status )

*  Description:
*     The routine annuls a locator to a temporary object created by
*     ary1Temp thereby causing the associated object to be erased and
*     the file space associated with it to be released. If data are
*     mapped to the object via HDS, then they are first unmapped.

*  Parameters:
*     loc
*        Address of an HDS locator pointer to a temporary object to be
*        annulled. The pointer is reset to NULL by this routine.
*     status
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry. However, no additional error report is made if it
*     subsequently fails under these circumstances.

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
*     23-JUN-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.
*     25-FEB-2019 (DSB):
*        Ensure the locator for the parent is a primary locator (as per
*        NDF equivalent function).
*-
*/

/* Local variables: */
   HDSLoc *locp=NULL;         /* Locator to parent object */
   char name[DAT__SZNAM+1];   /* Name of object to be erased */
   int plocked;               /* Original lock state for parent */
   int prim;                  /* Is locator primary? */
   int tstat;                 /* Local temporary status variable */

/* Return if no pointer supplied. */
   if( !loc ) return;

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* Find the temporary object's name. */
   *status = SAI__OK;
   datName( *loc, name, status );

/* Find its parent. */
   datParen( *loc, &locp, status );

/* Attempt to lock the parent for read-write access by the current thread,
   remembering if it was already locked or not. */
   plocked = datLocked( locp, 0, status );
   datLock( locp, 0, 0, status );

/* Promote the locator for the parent to a primary locator to ensure that
   the container file will not be closed when the locator for the data
   object is annulled. */
   prim = 1;
   datPrmry( 1, &locp, &prim, status );

/* Annul the object's locator. */
   datAnnul( loc, status );

/* Erase the object. */
   datErase( locp, name, status );

/* If the parent locator was originally unlocked, unlock it now. */
   if( plocked == 0 ) {
      datUnlock( locp, 0, status );

/* If the parent locator was originally locked read-only, change it back
   to a read-only lcok. */
   } else if( plocked == 3 ) {
      datLock( locp, 0, 1, status );
   }

/* Annul the parent's locator. */
   datAnnul( &locp, status );

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;
      } else {

/* Call error tracing routine if appropriate. */
         ary1Trace( "ary1Antmp", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();

}
