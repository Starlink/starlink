#include "sae_par.h"
#include "ast.h"
#include "star/hds.h"
#include "star/grp.h"
#include "star/kaplibs.h"
#include "cupid.h"
#include <string.h>

void cupidStoreConfig( HDSLoc *loc, AstKeyMap *config ){
/*
*  Name:
*     cupidStoreConfig

*  Purpose:
*     Store the configuraton used by CLUMPS in the given CUPID extension.

*  Synopsis:
*     void cupidStoreConfig( HDSLoc *loc, AstKeyMap *config )

*  Description:
*     This function extracts each keyword/value pair from the given 
*     configuration keymap, and stores them in the CUPID extension.

*  Parameters:
*     loc
*        HDS locator for the CUPID extension.
*     config
*        An AST KeyMap holding the configuration parameters.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     9-NOV-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   Grp *grp;             /* Pointer to group */
   HDSLoc *aloc;         /* HDS locator for entire CONFIG array */
   HDSLoc *cloc;         /* HDS locator for single cell of ONFIG array */
   char name[ GRP__SZNAM + 1 ];/* Value extracted from GRP group */
   char *pname;          /* Pointer to pass to grpGet */
   int el;               /* Index of element to store */
   int i;                /* Index of next entry in group */
   int n;                /* Number of entries in group */
   int nc;               /* Number of characters in group entry */
   int ncmax;            /* Max number of characters in any group entry */
   int subs[ 1 ];        /* Array containing required cell index */

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
   grpGrpsz( grp, &n, status );
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
      datNewC( loc, "CONFIG", ncmax + 1, 1, &n, status );
      datFind( loc, "CONFIG", &aloc, status );

/* Store each list item in the new array. */
      el = 1;
      for( i = 0; i < n; i++ ) {
         subs[ 0 ]= i + 1;
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
