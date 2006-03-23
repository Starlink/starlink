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

/* Free the meory holding the PixelSet itself, and return a NULL pointer. */
   return astFree( ps );

}

