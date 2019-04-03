#include <math.h>
#include <stdlib.h>
#include "sae_par.h"
#include "prm_par.h"
#include <math.h>
#include "ndf1.h"

void ndf1Stats( size_t dim, const double data[], double *maxv, double *minv,
                double *mean, double *sigma, double *rms, size_t *ngood,
                int *status ){
/*
*+
*  Name:
*     ndf1Stats

*  Purpose:
*     Get simple statistics for a vectorised array.

*  Synopsis:
*     void ndf1Stats( size_t dim, const double data[], double *maxv,
*                     double *minv, double *mean, double *sigma,
*                     double *rms, size_t *ngood, int *status )

*  Description:
*     This function returns simple statistics for the supplied
*     1-dimensional array.

*  Parameters:
*     dim
*        Length of the array.
*     data
*        The array. The supplied "data" array should have at least "dim"
*        elements.
*     *maxv
*        Returned holding the maximum value in the array.
*     *minv
*        Returned holding the minimum value in the array.
*     *mean
*        Returned holding the mean value in the array.
*     *sigma
*        Returned holding the standard deviation of the values in the
*        array.
*     *rms
*        Returned holding the root-mean-squared value in the array.
*     *ngood
*        Returned holding the number of good values in the array.
*     *status
*        The global status.

*  Notes:
*     - VAL__BADD is returned for all values if there are no good values in
*     the array, or if an error has already occurred.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 3 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   size_t i;                /* Loop counter for array elements */
   const double *pd;        /* Pointer to next value */

/* Initialise error return values. */
   *maxv = VAL__BADD;
   *minv = VAL__BADD;
   *mean = VAL__BADD;
   *sigma = VAL__BADD;
   *rms = VAL__BADD;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise running sums, etc. */
   *maxv = VAL__MIND;
   *minv = VAL__MIND;
   *mean = 0.0;
   *sigma = 0.0;
   *rms = 0.0;
   *ngood = 0;

/*Check each value. */
   pd = data;
   for( i = 0; i < dim; i++,pd++ ){
      if( *pd != VAL__BADD ) {
         if( *pd > *maxv ) *maxv = *pd;
         if( *pd < *minv ) *minv = *pd;
         (*ngood)++;
         *rms += (*pd)*(*pd);
         *mean += *pd;
      }
   }

/* Calculate the returned values */
   if( *ngood > 0 ) {
      *mean /= *ngood;
      *rms /= *ngood;
      *sigma = sqrt( *rms - (*mean)*(*mean) );
      *rms = sqrt( *rms );
   } else {
      *maxv = VAL__BADD;
      *minv = VAL__BADD;
      *mean = VAL__BADD;
      *sigma = VAL__BADD;
      *rms = VAL__BADD;
   }

}

