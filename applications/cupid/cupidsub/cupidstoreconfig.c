#include "sae_par.h"
#include "ast.h"
#include "star/hds.h"
#include "star/grp.h"
#include "star/kaplibs.h"
#include "cupid.h"
#include <string.h>

void cupidStoreConfig( HDSLoc *loc, AstKeyMap *config, int *status ){
/*
*+
*  Name:
*     cupidStoreConfig

*  Purpose:
*     Store the configuraton used by CLUMPS in the given CUPID extension.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidStoreConfig( HDSLoc *loc, AstKeyMap *config, int *status )

*  Description:
*     This function extracts each keyword/value pair from the given
*     configuration keymap, and stores them in the CUPID extension.

*  Parameters:
*     loc
*        HDS locator for the CUPID extension.
*     config
*        An AST KeyMap holding the configuration parameters.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-NOV-2005 (DSB):
*        Original version.
*     15-JUL-2008 (TIMJ):
*        Tweak to GRP C API.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   Grp *grp;             /* Pointer to group */
   HDSLoc *aloc;         /* HDS locator for entire CONFIG array */
   HDSLoc *cloc;         /* HDS locator for single cell of ONFIG array */
   char name[ GRP__SZNAM + 1 ];/* Value extracted from GRP group */
   char *pname;          /* Pointer to pass to grpGet */
   hdsdim el;            /* Index of element to store */
   int i;                /* Index of next entry in group */
   hdsdim n;             /* Number of entries in group */
   int nc;               /* Number of characters in group entry */
   int ncmax;            /* Max number of characters in any group entry */
   hdsdim subs[ 1 ];     /* Array containing required cell index */
   int there;            /* Does component exist?*/

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Initialise all HDS locator pointers to NULL since HDS now objects if it
   receives an uninitialised pointer. */
   aloc = NULL;
   cloc = NULL;

/* Create a GRP group containing the required text. */
   grp = NULL;
   kpg1Kygrp( config, &grp, status );

/* Get the number of values in the group. Pass on if it is zero. */
   n = grpGrpsz( grp, status );
   if( n ) {

/* We need to pass a pointer to the "name" variable to grpGet */
      pname = name;

/* Scan the group to find the length of the longest value. */
      ncmax = 0;
      for( i = 1; i <= n; i++ ) {
         grpGet( grp, i, 1, &pname, GRP__SZNAM + 1, status );
         nc = astChrLen( name );
         if( nc > ncmax ) ncmax = nc;
      }

/* Create a suitable array of character strings in the CUPID extension,
   and get a locator for the whole array. */
      datThere( loc, "CONFIG", &there, status );
      if( there ) datErase( loc, "CONFIG", status );
      datNewC( loc, "CONFIG", ncmax + 1, 1, &n, status );
      datFind( loc, "CONFIG", &aloc, status );

/* Store each list item in the new array. */
      el = 1;
      for( i = 0; i < n; i++ ) {
         subs[ 0 ]= i + 1;
         cloc = NULL;
         datCell( aloc, 1, subs, &cloc, status );
         grpGet( grp, subs[ 0 ], 1, &pname, GRP__SZNAM + 1, status );
         nc = astChrLen( pname );
         datPutC( cloc, 0, &el, pname, astChrLen( name ), status );
         datAnnul( &cloc, status );
      }

/* Free resources. */
      datAnnul( &aloc, status );
   }

/* Delete the group */
   grpDelet( &grp, status );

}
