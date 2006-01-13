#include "sae_par.h"
#include "cupid.h"
#include "ast.h"
#include "mers.h"

CupidPixelSet *cupidCFDeletePS( CupidPixelSet *ps ){
/*
*  Name:
*     cupidCFDeletePS

*  Purpose:
*     Delete a CupidPixelSet structure.

*  Synopsis:
*     CupidPixelSet *cupidCFDeletePS( CupidPixelSet *ps )

*  Description:
*     This function releases all resources used by a CupidPixelSet
*     structure, including dynamic memory referenced by the PixelSet.

*  Parameters:
*     ps
*        Pointer to the CupidPixelSet structure to be freed.

*  Returned Value:
*     A NULL pointer.

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

/* Free the array used to hold pointers to neighbouring PixelSets. We do
   not free the neighbouring PixelSet structures themselves. */
   ps->nb = astFree( ps->nb );

/* Free the arrays used to hold pointers to the above lists. */
   ps->nbl = astFree( ps->nbl );

/* Free the array used to hold the list sizes. */
   ps->sznbl = astFree( ps->sznbl );

/* Free the meory holding the PixelSet itself, and return a NULL pointer. */
   return astFree( ps );

}

