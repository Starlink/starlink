#include "sae_par.h"
#include "cupid.h"
#include "ast.h"
#include "mers.h"

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
*     structure.

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

/* If required, check that no pixels are still assigned to the PixelSet
   which is being freed. */
   if( ipa && *status == SAI__OK ) {
      int n = 0;
      for( i = 0; i < nel; i++ ) {
         if( cupidMergeSet( ipa[ i ] ) == ps->index ) n++;
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

/* Free the array used to hold pointers to neighbouring PixelSets. We do
   not free the neighbouring PixelSet structures themselves. */
   ps->nb = astFree( ps->nb );

/* Free the arrays used to hold lists of neighbouring pixels. */
   if( ps->nbl ) {
      for( i = 0; i < ps->nnb; i++ ) {
         ps->nbl[ i ] = astFree( ps->nbl[ i ] );
      }

/* Free the arrays used to hold pointers to the above lists. */
      ps->nbl = astFree( ps->nbl );
   }

/* Free the array used to hold the list sizes. */
   ps->sznbl = astFree( ps->sznbl );

/* Free the supplied PixelSet structure. */
   astFree( ps );

/* Return a NULL pointer. */
   return NULL;

}

