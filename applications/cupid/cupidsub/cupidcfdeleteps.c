#include "sae_par.h"
#include "cupid.h"
#include "ast.h"
#include "mers.h"

CupidPixelSet *cupidCFDeletePS( CupidPixelSet *ps, int *status ){
/*
*+
*  Name:
*     cupidCFDeletePS

*  Purpose:
*     Delete a CupidPixelSet structure.

*  Language:
*     Starlink C

*  Synopsis:
*     CupidPixelSet *cupidCFDeletePS( CupidPixelSet *ps, int *status )

*  Description:
*     This function releases all resources used by a CupidPixelSet
*     structure, including dynamic memory referenced by the PixelSet.

*  Parameters:
*     ps
*        Pointer to the CupidPixelSet structure to be freed.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A NULL pointer.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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

/* Check a pointer was supplied. */
   if( !ps ) return NULL;

/* Free the memory used to hold the list of the indices of neighbouring
   clumps. */
   ps->nebs = astFree( ps->nebs );

/* Free the memory used to hold the list of the heights of the cols
   between neighbouring peaks. */
   ps->cols = astFree( ps->cols );

/* Free the memory holding the PixelSet itself, and return a NULL pointer. */
   return astFree( ps );

}
