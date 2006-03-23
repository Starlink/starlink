#include "sae_par.h"
#include "cupid.h"
#include "ast.h"
#include "mers.h"

CupidPixelSet *cupidCFMakePS( int index ){
/*
*  Name:
*     cupidCFMakePS

*  Purpose:
*     Get a pointer to a new CupidPixelSet structure.

*  Synopsis:
*     CupidPixelSet *cupidCFMakePS( int index )

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

*  Returned Value:
*     A pointer to the PixelSet to be used.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
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
      }

/* Otherwise, return a pointer to the PixelSet at the end of the cache
   and reduce the size of the cache by one. */
   } else {
      ret = cupid_ps_cache[ --cupid_ps_cache_size ];
      cupid_ps_cache[ cupid_ps_cache_size ] = NULL;
      ret->index = index;
   }

/* Return the PixelSet pointer. */
   return ret;

}

