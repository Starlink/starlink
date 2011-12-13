#include "sae_par.h"
#include "star/hds.h"
#include "mers.h"
#include "cupid.h"

void cupidDatCopy( HDSLoc *loc1, HDSLoc *loc2, int *status ){
/*
*+
*  Name:
*     cupidDatCopy

*  Purpose:
*     Copy an HDS structure into a given structure.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidDatCopy( HDSLoc *loc1, HDSLoc *loc2, int *status )

*  Description:
*     This function copies the HDS structure located by "loc1" into the HDS
*     structure located by "loc2".

*  Parameters:
*     loc1
*        HDS locator for the structure to be copied. An error is reported
*        if this is a primitive.
*     loc2
*        HDS locator for the structure to receive the copy of "loc1". An
*        error is reported if this is a primitive.
*     status
*        Pointer to the inherited status value.

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
*     10-NOV-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   HDSLoc *cloc;                /* Locator for structure component */
   char name[ DAT__SZNAM + 1 ]; /* Component name */
   int i;                       /* Index of next component */
   int ncomp;                   /* Number of components in structure */
   int prim;                    /* Is "loc1" primitive? */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Initialise all HDS locatorpointers to NULL since HDS now objects if it
   receives an uninitialised pointer. */
   cloc = NULL;

/* Report an error is either locator is for a primitive value. */
   datPrim( loc1, &prim, status );
   if( prim ) {
      *status = SAI__ERROR;
      errRep( "CUPIDDATCOPY_ERR1", "cupidDatCopy: Primitive value "
              "supplied for loc1 (internal CUPID programming error).",
              status );
   } else {
      datPrim( loc2, &prim, status );
      if( prim ) {
         *status = SAI__ERROR;
         errRep( "CUPIDDATCOPY_ERR1", "cupidDatCopy: Primitive value "
                 "supplied for loc2 (internal CUPID programming error).",
                 status );

/* Loop round all components of "loc1". */
      } else {
         datNcomp( loc1, &ncomp, status );
         for( i = 0; i < ncomp; i++ ) {

/* Get a locator for this component. */
            datIndex( loc1, i + 1, &cloc, status );

/* Get the name of the component. */
            datName( cloc, name, status );

/* Copy the component into loc2 with the same name. */
            datCopy( cloc, loc2, name, status );

/* Annul locators. */
            datAnnul( &cloc, status );
         }
      }
   }
}
