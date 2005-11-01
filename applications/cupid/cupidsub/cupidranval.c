#include "sae_par.h"
#include "cupid.h"
#include "star/pda.h"

float cupidRanVal( int normal, float p[2] ){
/*
*  Name:
*     cupidRanVal

*  Purpose:
*     Return a random sample from a uniform or normal distribution.

*  Synopsis:
*     float cupidRanVal( int normal, float p[2] )

*  Description:
*     This function returns a random sample from a uniform or normal
*     distribution.

*  Parameters:
*     normal
*        If non-zero, then the returned sample is drawn from a normal
*        (Gaussian) distribution. Otherwise, it is drawn from a uniform
*        distribution.
*     p
*        Element zero contains the mean of the distribution, element one
*        contains the width (i.e. the standard deviation for a normal
*        distribution, or the half-width for a uniform distribution).

*  Returned Value:
*     The model value or gradient.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     31-OCT-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   float ret;             /* Returned value */

/* Initialise */
   ret = p[ 0 ];

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Get the required sample. */
   if( normal ) {
      ret = pdaRnnor( p[ 0 ], p[ 1 ] );
   } else {
      ret = p[ 0 ] + 2*( pdaRand() - 0.5 )*p[ 1 ];
   }

/* Return the required value */
   return ret;   

}

