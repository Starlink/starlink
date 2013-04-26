
/*
*+
*  Name:
*     smf_rebincube_norm3d

*  Purpose:
*     Normalise the data values and variances using a 3D weights array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_rebincube_norm3d( dim_t nout, int genvar,
*                                size_t nused, float *data_array,
*                                float *var_array, double *wgt_array,
*                                int *status );

*  Arguments:
*     nout = dim_t (Given)
*        Number of elements in output cube.
*     genvar = int (Given)
*        Indicates how the output variances should be calculated:
*           0 = do not calculate any output variances
*           1 = use spread of input data values
*           2 = use system noise temperatures
*     nused = size_t (Given)
*        Number of input elements pasted into the output cube.
*     data_array = float * (Given and Returned)
*        The 3D data array for the output cube.
*     var_array = float * (Given and Returned)
*        The 3D array in which to store the variances for the output cube if
*        "genvar" is not zero (the supplied pointer is ignored if "genvar" is
*        zero).
*     wgt_array = double * (Given and Returned)
*        The 3D array in which to store the relative weighting for each pixel
*        in the output cube. If "genvar" is 2, this array should be the
*        same size as "var_array". If "genvar" is 2, this array should be
*        twice the size of "var_array".
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     Normalise the data values and variances using a 2D weights array.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     23-APR-2006 (DSB):
*        Initial version.
*     7-JAN-2009 (DSB):
*        Remove unused parameter nxy.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_rebincube_norm3d"

void smf_rebincube_norm3d( dim_t nout, int genvar, size_t nused, float *data_array,
                           float *var_array, double *wgt_array, int *status ){

/* Local Variables */
   dim_t iv;                   /* Vector index into output 3D array */
   dim_t nonzero;              /* No. of o/p pixels with nonzero weight */
   float d;                    /* Data value */
   double fac;                 /* Factor to correct for loss of degree of freedom */
   double n;                   /* Mean no. of input pixels per output pixel */
   double sw;                  /* Sum of weights */
   double swdd;                /* Sum of squared input data value times weight */
   double sww;                 /* Sum of squared weights */
   double swwv;                /* Sum of squared weight times input variance */
   double totwgt;              /* Total weight of data in output cube */
   double mwpip;               /* Mean weight per input pixel used */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* First normalise the data array, counting the number of output pixels
   that have non-zero weights. Also find the total weight. */
   totwgt = 0.0;
   nonzero = 0;
   for( iv = 0; iv < nout; iv++ ) {
      if( wgt_array[ iv ] > 0.0 ) {
         data_array[ iv ] /= wgt_array[ iv ];
         totwgt += wgt_array[ iv ];
         nonzero++;
      } else {
         data_array[ iv ] = VAL__BADR;
      }
   }

/* Now normalise the variance array. If there are no good output pixels,
   set all variances bad. */
   if( nonzero == 0 ) {
      if( genvar ) {
         for( iv = 0; iv < nout; iv++ ) var_array[ iv ] = VAL__BADR;
      }

/* Now handle cases where the output variance is calculated on the basis
   of the spread of input values. */
   } else if( genvar == 1 ) {

/* Find the mean weight per input pixel. */
      mwpip = totwgt/nused;

/* We use the weighted mean squared residual between each input data value
   and the weighted mean data value as an estimate of the variance of the
   input data. We need to correct this by a factor of n/(n-1) (where "n"
   is the number of input values) to take account of the reduced number
   of degrees of freedom when calculating this weighted mean squared
   residual (this is important when not many input values are being
   combined). We estimate "n" for each output pixel as the weight for the
   output pixel divided by the mean weight per input pixel. */

      for( iv = 0; iv < nout; iv++ ) {
         sw = wgt_array[ iv ];
         n = sw/mwpip;

         if( n > 2.0 ) {
            fac = n /( n - 1.0 );
            d = data_array[ iv ];
            sww = var_array[ iv ];
            swdd = wgt_array[ iv + nout ];
            var_array[ iv ] = ( swdd/sw - d*d )*fac*sww/( sw*sw );
            if( var_array[ iv ] <= 0.0 ) var_array[ iv ] = VAL__BADR;
         } else {
            var_array[ iv ] = VAL__BADR;
         }
      }

/* Now handle cases where the output variance is calculated on the basis
   of the input Tsys values. */
   } else if( genvar == 2 ) {

      for( iv = 0; iv < nout; iv++ ) {
         sw = wgt_array[ iv ];
         if( sw > 0.0 ) {
            swwv = var_array[ iv ];
            var_array[ iv ] = swwv/( sw*sw );
         } else {
            var_array[ iv ] = VAL__BADR;
         }
      }
   }
}
