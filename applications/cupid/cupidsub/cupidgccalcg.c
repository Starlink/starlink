#include "sae_par.h"
#include "prm_par.h"
#include "cupid.h"

/* Global Variables: */
/* ================= */
/* A structure holding the global parameters of the GaussClump algorithm 
   needed by this function. These are set by function cupidGaussClumps. */
extern CupidGC cupidGC;


void cupidGCcalcg( int n, double *x, int *nf, double *g ){
/*
*  Name:
*     cupidGCcalcg

*  Purpose:
*     The gradient of the merit function to be minimised by the GaussClumps 
*     algorithm.

*  Synopsis:
*     void cupidGCcalcg( int n, double *x, int *nf, double *g )

*  Description:
*     This function evaluates the gradient of the merit function describing 
*     the fit between a given Gaussian model and a given data array. It is
*     designed to be called by the pdaSumsl minimisation function.

*  Parameters:
*     n
*        The number of parameters describing the model.
*     x
*        Pointer to an array holding the "n" parameters which define the
*        model.
*     nf 
*        On entry, "*nf" is the number of times "calcf" has been called.
*        Returned equal to zero if the merit function cannot be evaluated
*        at "x". Intermediate values are calculated and cached when a new
*        value of "*nf" is supplied, and these values are used on
*        subsequent invocations until a new value of "*nf" is supplied.
*     g
*        Pointer to an array in which to return the rate of change of the
*        merit function with respect to each of the "n" parameters.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     18-OCT-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   int ipar;              /* Parameter index */
   int newx;              /* Are we dealing with a new parameter set? */

/* If the invocation count has not changed we must be dealing with the
   same point as last time. If it has changed, record its value so that
   "calcf" can access it. */
   if( *nf == cupidGC.nf ) {
      newx = 0;
   } else {
      newx = 1;
      cupidGC.nf = *nf;
   }

/* Loop round each parameter */
   for( ipar = 0; ipar < n; ipar++ ) {
      
/* Calculate the rate of change of the chi-squared with respect to this
   parameter. */
      g[ ipar ] = cupidGCChiSq( cupidGC.ndim, x, ipar, newx );
             
/* The second and subsequent passes through this loop use the same "x"
   position as the first. */
      newx = 0;
   }

}

