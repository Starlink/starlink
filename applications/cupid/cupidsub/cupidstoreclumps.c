#include "sae_par.h"
#include "star/hds.h"
#include "cupid.h"

void cupidStoreClumps( char *xloc, char *clist, int nclump ){
/*
*  Name:
*     cupidStoreClumps

*  Purpose:
*     Get the value of a configuration parameter.

*  Synopsis:
*     void cupidStoreClumps( char *xloc, char *clist, int nclump )

*  Description:
*     This function copies the HDS structures describing the found clumps
*     into the supplied CUPID extension, and then annuls the locators
*     in "clist".

*  Parameters:
*     xloc
*        HDS locator for the CUPID extension.
*     clist
*        A pointer to a character string containing a list of "nclump"
*        HDS locators, each occupying ( DAT__SZLOC + 1 ) elements of the
*        array. These locators will be annulled before returning.
*     nclump
*        The number of locators in "clist".

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     10-NOV-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   char aloc[ DAT__SZLOC + 1 ]; /* Locator for array of Clump structures */
   char cloc[ DAT__SZLOC + 1 ]; /* Locator for array cell */
   char *loc;                /* Pointer to start of next locator */
   int i;                    /* Index of next locator */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Create an array of "nclump" Clump structures in the extension, and get
   a locator to it. */
   datNew( xloc, "CLUMPS", "CLUMP", 1, &nclump, status );
   datFind( xloc, "CLUMPS", aloc, status );

/* Loop round all the supplied locators. */
   loc = clist;
   for( i = 0; i < nclump; i++ ) {

/* Get a locator for the cell of the array. */
      datCell( aloc, 1, &i, cloc, status );

/* Copy the Clump object located by the next element in the "clist" array
   into the current cell. */
      cupidDatCopy( loc, cloc );

/* Annul both locators. */
      datAnnul( loc, status );
      datAnnul( cloc, status );

/* Get a pointer to the next locator. */
      loc += DAT__SZLOC + 1;

   }

/* Annul the locator to the array within the extension */   
   datAnnul( aloc, status );
}
