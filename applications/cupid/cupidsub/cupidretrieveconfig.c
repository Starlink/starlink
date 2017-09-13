#include "sae_par.h"
#include "ast.h"
#include "star/hds.h"
#include "star/grp.h"
#include "star/kaplibs.h"
#include "cupid.h"
#include <string.h>

AstKeyMap *cupidRetrieveConfig( HDSLoc *xloc, int *status ){
/*
*+
*  Name:
*     cupidRetrieveConfig

*  Purpose:
*     Retrieve the CUPID configuraton from the given CUPID extension.

*  Language:
*     Starlink C

*  Synopsis:
*     AstKeyMap *cupidRetrieveConfig( HDSLoc *xloc, int *status )

*  Description:
*     This function reads the CONFIG component of the supplied CUPID
*     extension and forms an AST KeyMap from it. It is the reverse of
*     cupidStoreConfig.

*  Parameters:
*     xloc
*        HDS locator for the CUPID extension.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     Pointer to a new AST KeyMap holding the configuration parameters.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     7-APR-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   AstKeyMap *ret;       /* Pointer to returned KeyMap */
   Grp *grp;             /* Pointer to group */
   HDSLoc *aloc;         /* HDS locator for entire CONFIG array */
   HDSLoc *cloc;         /* HDS locator for single cell of ONFIG array */
   char text[ GRP__SZNAM + 1 ];/* Value extracted from GRP group */
   hdsdim i;             /* Index of next entry in group */
   int size;             /* Number of lines of text */
   int there;            /* Does component exist?*/

   ret = NULL;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Initialise all HDS locator pointers to NULL since HDS now objects if it
   receives an uninitialised pointer. */
   aloc = NULL;
   cloc = NULL;

/* Do nothing if the CUPID extension does not have a CONFIG component. */
   datThere( xloc, "CONFIG", &there, status );
   if( there ) {

/* Create an empty GRP group. */
      grp = grpNew( "", status );

/* Get a locator to the whole array of character strings. */
      datFind( xloc, "CONFIG", &aloc, status );

/* Find how many there are and loop round them all. */
      datSize( aloc, (size_t *) &size, status );
      for( i = 1; i <= size; i++ ) {

/* Get a locator to the current cell of the array and read its value into
   character string "text". */
         datCell( aloc, 1, &i, &cloc, status );
         datGet0C( cloc, text, GRP__SZNAM, status );
         datAnnul( &cloc, status );

/* Store this string in the GRP group. */
         grpPut1( grp, text, 0, status );
      }

/* Create an AST KeyMap holding the configuration setting in the group. */
      kpg1Kymap( grp, &ret, status );

/* Delete the group */
      grpDelet( &grp, status );

/* Return an empty KeyMap if the CUPID extension does not contain a CONFIG
   component. */
   } else {
      ret = astKeyMap( " " );
   }

/* Return the pointer */
   return ret;
}
