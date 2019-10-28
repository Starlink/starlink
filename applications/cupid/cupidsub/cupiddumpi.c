#include "sae_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "ast.h"
#include "dat_par.h"
#include "cupid.h"
#include <stdio.h>

void cupidDumpI( int *array, int ndim, hdsdim *dims, hdsdim *slbnd,
                 const char *text, int *status ){
/*
*+
*  Name:
*     cupidDumpI

*  Purpose:
*     Dump a integer array.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidDumpI( int *array, int ndim, hdsdim *dims, hdsdim *slbnd,
*                      const char *text, int *status )

*  Description:
*     This function is a diagnostic function which dumps the supplied
*     array.

*  Parameters:
*     array
*        Pointer to the array to be dumped.
*     ndim
*        The number of pixel axes.
*     dims
*        Pointer to the size of each pixel axis.
*     slbnd
*        Pointer to the lower pixel bounds of each pixel axis.
*     text
*        Description of the values to be dumped.
*     status
*        Pointer to the inherited status value.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     7-DEC-2005 (DSB):
*        Original version.
*     1-SEP-2009 (DSB):
*        Added "text" argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   int *adata;
   int indf, place;
   hdsdim lbnd[3], ubnd[3];
   size_t i, el;
   static int jj = 0;
   char name[ 100 ];

   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = slbnd[ i ];
      ubnd[ i ] = dims[ i ] + slbnd[ i ] - 1;
   }

   jj++;
   sprintf( name, "idata%d", jj );
   printf("   Dumping %s (%s)\n\n", name, text );
   ndfOpen( NULL, name, "WRITE", "NEW", &indf, &place, status );
   ndfNew( "_INTEGER", ndim, lbnd, ubnd, &place, &indf, status );
   ndfMap( indf, "DATA", "_INTEGER", "WRITE", (void *) &adata, &el, status );
   if( adata ) {
      for( i = 0; i < el; i++ ) adata[ i ] = array[ i ];
   }
   ndfCput( text, indf, "Title", status );
   ndfAnnul( &indf, status );
}
