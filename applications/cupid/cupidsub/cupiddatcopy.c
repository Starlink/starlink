#include "sae_par.h"
#include "star/hds.h"
#include "mers.h"
#include "cupid.h"

void cupidDatCopy( HDSLoc *loc1, HDSLoc *loc2 ){
/*
*  Name:
*     cupidDatCopy

*  Purpose:
*     Copy an HDS structure into a given structure.

*  Synopsis:
*     void cupidDatCopy( HDSLoc *loc1, HDSLoc *loc2 )

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
   HDSLoc *cloc;                /* Locator for structure component */
   char name[ DAT__SZNAM + 1 ]; /* Component name */
   int i;                       /* Index of next component */
   int ncomp;                   /* Number of components in structure */
   int prim;                    /* Is "loc1" primitive? */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

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
