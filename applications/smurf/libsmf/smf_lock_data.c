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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27-JUN-2008 (DSB):
*        Initial version.
*     3-DEC-2008 (DSB):
*        Include cached objects used by smf_create_lutwcs and
*        smf_detpos_wcs.
*     2011-04-19 (TIMJ):
*        Catch a NULL smfData
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2011 Science & Technology Facilities Council.
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
   sc2ast_subarray_t subnum;
   smfHead *hdr = NULL;
   sc2astCache *cache1 = NULL;
   smfCreateLutwcsCache *cache2 = NULL;
   smfDetposWcsCache *cache3 = NULL;

   if (!data) return;

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

/* Cache used by sc2ast_createwcs */
         cache1 = hdr->cache1;
         if( cache1 ) {
            for( subnum = 0; subnum < SC2AST__NSUB; subnum++ ) {
               if( cache1->map[ subnum ] ) astLock( cache1->map[ subnum ], 0 );
               if( cache1->frameset[ subnum ] ) astLock( cache1->frameset[ subnum ], 0 );
            }
            if( cache1->azel[ 0 ] ) astLock( cache1->azel[ 0 ], 0 );
            if( cache1->azel[ 1 ] ) astLock( cache1->azel[ 1 ], 0 );
            if( cache1->skyframe ) astLock( cache1->skyframe, 0 );
         }

/* Cache used by smf_create_lutwcs */
         cache2 = hdr->cache2;
         if( cache2 ) {
            if( cache2->map ) astLock( cache2->map, 0 );
            if( cache2->frameset ) astLock( cache2->frameset, 0 );
            if( cache2->skyframe ) astLock( cache2->skyframe, 0 );
            if( cache2->azel[ 0 ] ) astLock( cache2->azel[ 0 ], 0 );
            if( cache2->azel[ 1 ] ) astLock( cache2->azel[ 1 ], 0 );
         }

/* Cache used by smf_detpos_wcs */
         cache3 = hdr->cache3;
         if( cache3 ) {
            if( cache3->pmap ) astLock( cache3->pmap, 0 );
            if( cache3->grid ) astLock( cache3->grid, 0 );
            if( cache3->sky ) astLock( cache3->sky, 0 );
         }
      }

/* Otherwise, unlock all AST pointers so that they can be locked by other
   threads. */
   } else {
/*      if( data->history ) astUnlock( data->history, 1 ); */
      if( hdr ){
         if( hdr->fitshdr ) astUnlock( hdr->fitshdr, 1 );
         if( hdr->wcs ) astUnlock( hdr->wcs, 1 );
         if( hdr->tswcs ) astUnlock( hdr->tswcs, 1 );

/* Cache used by sc2ast_createwcs */
         cache1 = hdr->cache1;
         if( cache1 ) {
            for( subnum = 0; subnum < SC2AST__NSUB; subnum++ ) {
               if( cache1->map[ subnum ] ) astUnlock( cache1->map[ subnum ], 1 );
               if( cache1->frameset[ subnum ] ) astUnlock( cache1->frameset[ subnum ], 1 );
            }
            if( cache1->azel[ 0 ] ) astUnlock( cache1->azel[ 0 ], 1 );
            if( cache1->azel[ 1 ] ) astUnlock( cache1->azel[ 1 ], 1 );
            if( cache1->skyframe ) astUnlock( cache1->skyframe, 1 );
         }

/* Cache used by smf_create_lutwcs */
         cache2 = hdr->cache2;
         if( cache2 ) {
            if( cache2->map ) astUnlock( cache2->map, 1 );
            if( cache2->frameset ) astUnlock( cache2->frameset, 1 );
            if( cache2->skyframe ) astUnlock( cache2->skyframe, 1 );
            if( cache2->azel[ 0 ] ) astUnlock( cache2->azel[ 0 ], 1 );
            if( cache2->azel[ 1 ] ) astUnlock( cache2->azel[ 1 ], 1 );
         }

/* Cache used by smf_detpos_wcs */
         cache3 = hdr->cache3;
         if( cache3 ) {
            if( cache3->pmap ) astUnlock( cache3->pmap, 1 );
            if( cache3->grid ) astUnlock( cache3->grid, 1 );
            if( cache3->sky ) astUnlock( cache3->sky, 1 );
         }
      }
   }

/* End the error reporting context. */
   emsEnd( status );

}

