#include "sae_par.h"
#include "cupid.h"
#include "ast.h"
#include "mers.h"
#include <float.h>

CupidPixelSet *cupidCFMakePS( int index, int *status ){
/*
*+
*  Name:
*     cupidCFMakePS

*  Purpose:
*     Get a pointer to a new CupidPixelSet structure.

*  Language:
*     Starlink C

*  Synopsis:
*     CupidPixelSet *cupidCFMakePS( int index, int *status )

*  Description:
*     This function returns a pointer to an unused PixelSet structure.
*     This will either be taken from a cache of unused structures, or
*     will be a newly created structure if there are currently no free
*     structures in the cache.
*
*     The cache is used in order to avoid the over-head of repeated
*     allocation of memory for small structures.

*  Parameters:
*     index
*        The index value to store in the PixelSet.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to the PixelSet to be used.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   CupidPixelSet *ret;  /* The returned PixelSet pointer */

/* Initialise */
   ret = NULL;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* If there are currently no free PixelSet structures in the cache,
   create a new one and return it. */
   if( cupid_ps_cache_size == 0 ) {
      ret = astMalloc( sizeof( CupidPixelSet ) );
      if( ret ) {
         ret->pop = 0;
         ret->edge = 0;
         ret->vpeak = -DBL_MAX;
         ret->index = index;
         ret->nebs = NULL;
         ret->nneb = 0;
         ret->cols = NULL;
         ret->lneb = -1;
         ret->lnebi = -1;
      }

/* Otherwise, return a pointer to the PixelSet at the end of the cache
   and reduce the size of the cache by one. */
   } else {
      ret = cupid_ps_cache[ --cupid_ps_cache_size ];
      cupid_ps_cache[ cupid_ps_cache_size ] = NULL;
      ret->index = index;
      ret->nebs = NULL;
      ret->cols = NULL;
      ret->nneb = 0;
      ret->lneb = -1;
      ret->lnebi = -1;
   }

/* Return the PixelSet pointer. */
   return ret;

}
