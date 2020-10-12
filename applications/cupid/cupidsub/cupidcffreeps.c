#include "sae_par.h"
#include "cupid.h"
#include "ast.h"
#include "mers.h"
#include <float.h>

CupidPixelSet *cupidCFFreePS( CupidPixelSet *ps, int *ipa, size_t nel,
         int *status ){
/*
*+
*  Name:
*     cupidCFFreePS

*  Purpose:
*     Free a CupidPixelSet structure.

*  Language:
*     Starlink C

*  Synopsis:
*     CupidPixelSet *cupidCFFreePS( CupidPixelSet *ps, int *ipa, size_t nel,
*        int *status )

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
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A NULL pointer.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     26-NOV-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   size_t i;              /* Index of neighbouring PixelSet */

/* Check a PixelSet was supplied. */
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
   ps->cols = astFree( ps->cols );
   ps->nneb = 0;
   ps->pop = 0;
   ps->edge = 0;
   ps->vpeak = -DBL_MAX;
   ps->index = CUPID__CFNULL;
   ps->lneb = -1;
   ps->lnebi = -1;

/* Move the supplied PixelSet structure to the end of the cache so that
   it can be re-used. */
   cupid_ps_cache = astGrow( cupid_ps_cache, cupid_ps_cache_size + 1,
                             sizeof( CupidPixelSet * ) );
   if( astOK ) cupid_ps_cache[ cupid_ps_cache_size++ ] = ps;

/* Return a NULL pointer. */
   return NULL;

}
