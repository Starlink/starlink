#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "mers.h"
#include "sae_par.h"


void kpg1Rnorm( int el, double *array, int seed, int *status ){
/*
*  Name:
*     kpg1Rnorm

*  Purpose:
*     Returns a set of random samples from a normal distribution.

*  Language:
*     C.

*  Invocation:
*     void kpg1Rnorm( int el, double *array, int seed, int *status )

*  Description:
*     This routine returns a  set of random samples from a normal
*     distribution with mean=0.0 and standard deviation=1.0. It uses
*     the default GSL random number generator type.

*  Arguments:
*     el
*         The number of samples to return.
*     array
*        The array in which to return the values.
*     seed
*        The seed to use. If zero or negative, the value of
*        environment variable STAR_SEED is used if set, and a
*        non-repeatable value is used if STAR_SEED is not set.
*     status
*        The inherited status.

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     22-JUL-2015 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/

/* Local Variables: */
   const char *starseed;
   double *p;
   gsl_rng *r;
   int i;
   unsigned long int iseed;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Create a GSL random number generator of the default type. */
   r = gsl_rng_alloc( gsl_rng_default );

/* Choose the seed. If zero or negative, use the value of STAR_SEED (if set)
   which is any arbitrary string. If STAR_SEED is not set, use the sum
   of the current time and the current process ID. */
   if( seed <= 0 ) {
      starseed = getenv( "STAR_SEED" );
      if( starseed ) {
         iseed = 0;
         while( *starseed ) iseed += *(starseed++);

      } else {
         iseed = (unsigned long int) time( NULL ) +
                 (unsigned long int) getpid();
      }
   } else {
      iseed = (unsigned long int) seed;
   }

/* Set the seed. */
   gsl_rng_set( r, iseed );

/* Fill the array with random samples. */
   p = array;
   for( i = 0; i < el; i++ ) {
      *(p++) = gsl_ran_gaussian( r, 1.0 );
   }

/* Free the GSL resources. */
   gsl_rng_free( r );

}

