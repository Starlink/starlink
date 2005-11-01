#include "sae_par.h"
#include "cupid.h"
#include <stdio.h>

void cupidGCListClump( int iclump, int ndim, double *par, double chisq ){
/*
*  Name:
*     cupidGCListClump

*  Purpose:
*     Add a clump to the output list. 

*  Synopsis:
*     void cupidGCListClump( int iclump, int ndim, double *par, double chisq )

*  Description:
*     This function adds a clump to the output list. 

*  Parameters:
*     iclump
*        The (1-based) index of the clump to be added to the list.
*     ndim
*        The number of pixel axes in the NDF.
*     par
*        The parameters describing the Gaussian fit to the clump (see
*        cupidGCFit.cgen for a description)..
*     chisq
*        The chi-squared associated with the fit.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     5-OCT-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   int i;               /* Parameter index */
   int np;              /* Number of parameters */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Determine the number of significant parameters for the Gaussian model. */
   if( ndim == 1 ) {
      np = CUPID__GCNP1;
   } else if( ndim == 2 ) {
      np = CUPID__GCNP2;
   } else {
      np = CUPID__GCNP3;
   }

/* List them */
   printf( "\nClump %d:  chi_sq: %g  pars: ", iclump, chisq );
   for( i = 0; i < np; i++ ) printf( "%g ", par[ i ] );
   printf("\n");

}

