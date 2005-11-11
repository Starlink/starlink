#include "sae_par.h"
#include "star/hds.h"
#include "par.h"
#include "mers.h"
#include "cupid.h"

/* Local Constants: */
#define MAXCAT   50   /* Max length of catalogue name */

void cupidStoreClumps( const char *param, char *xloc, char *clist, 
                       int nclump, int ndim, const char *ttl ){
/*
*  Name:
*     cupidStoreClumps

*  Purpose:
*     Store properties of all clumps found by the CLUMPS command.

*  Synopsis:
*     void cupidStoreClumps( const char *param, char *xloc, char *clist, 
*                            int nclump, int ndim, const char *ttl )

*  Description:
*     This function optionally saves the clump properties in an output
*     catalogue, and then copies the HDS structures describing the found 
*     clumps into the supplied CUPID extension, and then annuls the locators
*     in "clist".

*  Parameters:
*     param
*        The ADAM parameter to associate with the output catalogue.
*     xloc
*        HDS locator for the CUPID extension.
*     clist
*        A pointer to a character string containing a list of "nclump"
*        HDS locators, each occupying ( DAT__SZLOC + 1 ) elements of the
*        array. These locators will be annulled before returning.
*     nclump
*        The number of locators in "clist".
*     ndim
*        The number of pixel axes in the data.
*     ttl
*        The title for the output catalogue (if any).

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
   char cat[ MAXCAT + 1 ];      /* Catalogue name */
   char cloc[ DAT__SZLOC + 1 ]; /* Locator for array cell */
   char *loc;                   /* Pointer to start of next locator */
   double *tab;                 /* Pointer to catalogue table */
   int i;                       /* Index of next locator */
   int ncol;                    /* number of catalogue columns */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* See if an output catalogue is to be created. If not, annul the null
   parameter error. */
   parGet0c( param, cat, MAXCAT, status );
   if( *status == PAR__NULL ) {
      errAnnul( status );

/* Otherwise create the catalogue. */
   } else {

/* Get memory to hold a table of clump parameters. */
      ncol = ( ( ndim == 1 ) ? CUPID__GCNP1 : ( 
                 (ndim ==2 ) ? CUPID__GCNP2 : CUPID__GCNP3 ) ) + 1;
      tab = astMalloc( sizeof(double)*nclump*ncol );

/* Loop round all the supplied locators. */
      loc = clist;
      for( i = 1; i <= nclump && *status == SAI__OK; i++ ) {

/* Get a locator for the cell of the array. */
         datCell( aloc, 1, &i, cloc, status );

/* Copy the clump parameters into the table. */
         cupidClumpCat( NULL, cloc, tab, nclump, i, ndim, ttl );

/* Annul the cell locators. */
         datAnnul( cloc, status );

/* Get a pointer to the next locator. */
         loc += DAT__SZLOC + 1;
      }

/* Create the catalogue. */
      cupidClumpCat( param, NULL, tab, nclump, nclump, ndim, ttl );

/* Free resources */
      tab = astFree( tab );
   }
 
/* Create an array of "nclump" Clump structures in the extension, and get
   a locator to it. */
   datNew( xloc, "CLUMPS", "CLUMP", 1, &nclump, status );
   datFind( xloc, "CLUMPS", aloc, status );

/* Loop round all the supplied locators. */
   loc = clist;
   for( i = 1; i <= nclump; i++ ) {

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
