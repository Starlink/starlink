#include "sae_par.h"
#include "cupid.h"
#include "ast.h"
#include "mers.h"
#include <float.h>

CupidPixelSet *cupidCFFreePS( CupidPixelSet *ps, int *ipa, int nel){
/*
*  Name:
*     cupidCFFreePS

*  Purpose:
*     Free a CupidPixelSet structure.

*  Synopsis:
*     CupidPixelSet *cupidCFFreePS( CupidPixelSet *ps, int *ipa, int nel )

*  Description:
*     This function releases the resources used by a CupidPixelSet
*     structure, returning the PixelSet structure itself to a cache of
*     unused structures.

*  Parameters:
*     ps
*        Pointer to the CupidPixelSet structure to be freed.
*     ipa
*        Pointer to pixel assignment array. If this is not NULL, the
*        entire array is checked to see any pixels are still assigned to
*        the PixelSet which is being freed. An error is reported if any
*        such pixels are found. No check is performed if the pointer is
*        NULL.
*     nel
*        The length of the "ipa" array, if supplied.

*  Returned Value:
*     A NULL pointer.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     26-NOV-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   int i;              /* Index of neighbouring PixelSet */

   if( !ps ) return NULL;

/* If required, check that no pixels are still assigned to the PixelSet
   which is being freed. */
   if( ipa && *status == SAI__OK ) {
      int n = 0;
      for( i = 0; i < nel; i++ ) {
         if( ipa[ i ] == ps->index ) n++;
      }

      if( n ) {
         *status = SAI__ERROR;
         msgSeti( "I", ps->index );
         if( n > 1 ) {
            msgSeti( "N", n );
            errRep( "CUPIDCFFREEPS_ERR1", "Attempt made to free PixelSet ^I " 
                    "whilst ^N pixels are still assigned to it (internal "
                    "CUPID programming error).", status );
         } else {
            errRep( "CUPIDCFFREEPS_ERR1", "Attempt made to free PixelSet ^I " 
                    "whilst 1 pixel is still assigned to it (internal "
                    "CUPID programming error).", status );
         }
      }
   }


/* Put all scalar fields back to their initial values in prepreation for
   the PixelSet pointer being re-issued by cupidMakePS. */
   ps->nebs = astFree( ps->nebs );
   ps->nneb = 0;
   ps->pop = 0;
   ps->edge = 0;
   ps->vpeak = -DBL_MAX;
   ps->index = CUPID__CFNULL;

/* Move the supplied PixelSet structure to the end of the cache so that
   it can be re-used. */
   cupid_ps_cache = astGrow( cupid_ps_cache, cupid_ps_cache_size + 1, 
                             sizeof( CupidPixelSet * ) );
   if( astOK ) cupid_ps_cache[ cupid_ps_cache_size++ ] = ps;

/* Return a NULL pointer. */
   return NULL;

}

