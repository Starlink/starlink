#include "sae_par.h"
#include "cupid.h"
#include "ast.h"

CupidPixelSet *cupidCFFreePS( CupidPixelSet *ps ){
/*
*  Name:
*     cupidCFFreePS

*  Purpose:
*     Free a CupidPixelSet structure.

*  Synopsis:
*     CupidPixelSet *cupidCFFreePS( CupidPixelSet *ps )

*  Description:
*     This function releases the resources used by a CupidPixelSet
*     structure.

*  Parameters:
*     ps
*        Pointer to the CupidPixelSet structure to be freed.

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

