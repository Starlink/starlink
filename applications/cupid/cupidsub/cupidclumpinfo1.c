#include "sae_par.h"
#include "mers.h"
#include "cupid.h"
#include "ast.h"
#include "ndf.h"
#include "prm_par.h"
#include "star/hds.h"
#include <math.h>

void cupidClumpInfo1( HDSLoc *cloc, CupidClumpInfo *info, int *status ){
/*
*+
*  Name:
*     cupidClumpInfo1

*  Purpose:
*     Identify clumps of emission within a 1, 2 or 3 dimensional NDF using
*     the ClumpInfo1 algorithm.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidClumpInfo1( HDSLoc *cloc, CupidClumpInfo *info, int *status );

*  Description:
*     This function examines the supplied HDS CLUMP structure, and
*     adds information about it into the supplied CupidClumpInfo structure.

*  Parameters:
*     cloc
*        A locator for a CLUMP structure such as created by cupidstoreclumps.c
*     info
*        A pointer to the structure in which to store information about
*        the supplied clump.
*     status
*        Pointer to the inherited status value.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
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
*     22-MAR-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   hdsdim lbnd[ 3 ]; /* Lower pixel index bounds of cut out */
   hdsdim ubnd[ 3 ]; /* Upper pixel index bounds of cut out */
   int i;            /* Loop index */
   int indf;         /* NDF identifier for clump cut-out */
   int ndim;         /* No. of pixel axes in cut out */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* If the "info" structure has not yet been initialised do so now. */
   if( !info->init ) {
      info->lbnd[ 0 ] = VAL__MAXI;
      info->lbnd[ 1 ] = VAL__MAXI;
      info->lbnd[ 2 ] = VAL__MAXI;
      info->ubnd[ 0 ] = VAL__MINI;
      info->ubnd[ 1 ] = VAL__MINI;
      info->ubnd[ 2 ] = VAL__MINI;
      info->npix = astGetI( info->iwcs, "Nin" );
      info->nwcs = astGetI( info->iwcs, "Nout" );
      info->init = 1;
   }

/* Get an NDF identifier for the NDF holding the clump cut-out. */
   ndfFind( cloc, "MODEL", &indf, status );

/* Get the pixel index bounds of the cut-out. */
   ndfBound( indf, 3, lbnd, ubnd, &ndim, status );

/* Update the pixel index bounds of the bounding box to include the
   supplied clump. */
   for( i = 0; i < ndim; i++ ) {
      if( lbnd[ i ] < info->lbnd[ i ] ) info->lbnd[ i ] = lbnd[ i ];
      if( ubnd[ i ] > info->ubnd[ i ] ) info->ubnd[ i ] = ubnd[ i ];
   }

/* Clear up. */
   ndfAnnul( &indf, status );
}
