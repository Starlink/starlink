/*
*+
*  Name:
*     smf_lock_data

*  Purpose:
*     Lock or unlock all AST Objects within a smfData structure.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_lock_data( smfData *data, int lock, int *status )

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the structure to be unlocked.
*     lock = int (Given)
*        If non-zero, then astLock is called. Otherwise, astUnlock is
*        called.
*     status = int * (Given and Returned)
*        Inherited status value. 

*  Description:
*     This function call astLock ot astUnlock on all AST objects within
*     the given smfData.

*  Notes:
*     - This function attempts to execute even if an error has occurred.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     27-JUN-2008 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "ems.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"
#include "sc2da/sc2ast.h"

void smf_lock_data( smfData *data, int lock, int *status ){

/* Local Variables */
   int subnum;
   smfHead *hdr = NULL;
   sc2astCache *cache = NULL;

/* Start a new error reporting context. */
   emsBegin( status );

   hdr = data->hdr;

/* If requested, lock all AST pointers for exclusive use by the current
   thread. */
   if( lock ) {
/*      if( data->history ) astLock( data->history, 0 );*/
      if( hdr ){
         if( hdr->fitshdr ) astLock( hdr->fitshdr, 0 );
         if( hdr->wcs ) astLock( hdr->wcs, 0 );
         if( hdr->tswcs ) astLock( hdr->tswcs, 0 );
         cache = hdr->cache;
         if( cache ) {
            for( subnum = 0; subnum < 8; subnum++ ) {
               if( cache->map[ subnum ] ) astLock( cache->map[ subnum ], 0 );
               if( cache->frameset[ subnum ] ) astLock( cache->frameset[ subnum ], 0 );
            }
            if( cache->azel[ 0 ] ) astLock( cache->azel[ 0 ], 0 );
            if( cache->azel[ 1 ] ) astLock( cache->azel[ 1 ], 0 );
            if( cache->skyframe ) astLock( cache->skyframe, 0 );
         }
      }

/* Otherwise, unlock all AST pointers so that they can be locked by other
   threads. */
   } else {
/*      if( data->history ) astUnlock( data->history ); */
      if( hdr ){
         if( hdr->fitshdr ) astUnlock( hdr->fitshdr );
         if( hdr->wcs ) astUnlock( hdr->wcs );
         if( hdr->tswcs ) astUnlock( hdr->tswcs );
         cache = hdr->cache;
         if( cache ) {
            for( subnum = 0; subnum < 8; subnum++ ) {
               if( cache->map[ subnum ] ) astUnlock( cache->map[ subnum ] );
               if( cache->frameset[ subnum ] ) astUnlock( cache->frameset[ subnum ] );
            }
            if( cache->azel[ 0 ] ) astUnlock( cache->azel[ 0 ] );
            if( cache->azel[ 1 ] ) astUnlock( cache->azel[ 1 ] );
            if( cache->skyframe ) astUnlock( cache->skyframe );
         }
      }
   }

/* End the error reporting context. */
   emsEnd( status );

}

