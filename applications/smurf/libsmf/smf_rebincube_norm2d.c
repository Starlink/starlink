/*
*+
*  Name:
*     smf_rebincube_norm2d

*  Purpose:
*     Normalise the data values and variances using a 2D weights array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_rebincube_norm2d( dim_t nout, dim_t nxy, int genvar,
*                                float *data_array,
*                                float *var_array, double *wgt_array,
*                                dim_t *pop_array, int *status );

*  Arguments:
*     nout = dim_t (Given)
*        Number of elements in output cube.
*     nxy = dim_t (Given)
*        Number of elements in one spatial plane of the output cube.
*     genvar = int (Given)
*        Indicates how the output variances should be calculated:
*           0 = do not calculate any output variances
*           1 = use spread of input data values
*           2 = use system noise temperatures
*     data_array = float * (Given and Returned)
*        The 3D data array for the output cube.
*     var_array = float * (Given and Returned)
*        A 2D array in which to store the variances for the output cube if
*        "genvar" is not zero (the supplied pointer is ignored if "genvar" is
*        zero). This array should be big enough to hold a single spatial
*        plane from the output cube (all planes in the output cube will have
*        the same variance).
*     wgt_array = double * (Given and Returned)
*        An array in which to store the relative weighting for each pixel in
*        the output cube. If "genvar" is 2, this array should be big enough
*        to hold a single spatial plane from the output cube (all planes in
*        the output cube will have the same weight). If "genvar" is 2, this
*        array should be big enough to hold two spatial planes from the
*        output cube.
*     pop_array = dim_t * (Given)
*        A 2D array holding the number of input spectra that contribute to
*        each output spectrum. This array should be the same size as
*        "var_array".
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

#define FUNC_NAME "smf_rebincube_norm2d"

void smf_rebincube_norm2d( dim_t nout, dim_t nxy, int genvar,
                           float *data_array, float *var_array,
                           double *wgt_array, dim_t *pop_array, int *status ){

/* Local Variables */
   dim_t ichan;                /* Output channel index */
   dim_t iv0;                  /* Offset for pixel in 1st o/p spectral channel */
   dim_t iv;                   /* Vector index into output 3D array */
   dim_t nchanout;             /* No. of spectral channels in output */
   dim_t ndd;                  /* Number of values summed in sdd */
   dim_t nonzero;              /* No. of o/p pixels with nonzero weight */
   double fac;                 /* Factor to correct for loss of degree of freedom */
   double n;                   /* Mean no. of input pixels per output pixel */
   double sdd;                 /* Sum of squared data values */
   double sw;                  /* Sum of weights */
   double swdd;                /* Sum of squared input data value times weight */
   double sww;                 /* Sum of squared weights */
   double swwv;                /* Sum of squared weight times input variance */
   float d;                    /* Data value */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* First normalise the data array, counting the number of output pixels
   that have non-zero weights. */
   iv0 = 0;
   nonzero = 0;
   for( iv = 0; iv < nout; iv++,iv0++ ) {
      if( iv0 == nxy ) iv0 = 0;
      if( wgt_array[ iv0 ] > 0.0 && data_array[ iv ] != VAL__BADR ) {
         data_array[ iv ] /= wgt_array[ iv0 ];
         nonzero++;
      } else {
         data_array[ iv ] = VAL__BADR;
      }
   }

/* Now normalise the variance array. If there are no good output pixels,
   set all variances bad. */
   if( nonzero == 0 ) {
      if( genvar ) {
         for( iv0 = 0; iv0 < nxy; iv0++ ) var_array[ iv0 ] = VAL__BADR;
      }

/* Now handle cases where the output variance is calculated on the basis
   of the spread of input values. */
   } else if( genvar == 1 ) {

/* We use the weighted mean squared residual between each input data value
   and the weighted mean data value as an estimate of the variance of the
   input data. We need to correct this by a factor of n/(n-1) (where "n"
   is the number of input spectra contributing to the output spectrum) to
   take account of the reduced number of degrees of freedom when calculating
   this weighted mean squared residual (this is important when not many
   input values are being combined). The ratio is estimated as the
   number of input values that have been pasted into the output cube,
   divided by the number of output pixels that have non-zero weights. */
      nchanout = nout/nxy;
      for( iv0 = 0; iv0 < nxy; iv0++ ) {
         sw = wgt_array[ iv0 ];
         n = pop_array[ iv0 ];
         if( n > 2.0 && sw > 0.0 ) {
            fac = n /( n - 1.0 );

            sww = var_array[ iv0 ];
            swdd = wgt_array[ iv0 + nxy ];

            ndd = 0;
            sdd = 0.0;
            for( ichan = 0; ichan < nchanout; ichan++ ) {
               d = data_array[ iv0 + ichan*nxy ];
               if( d != VAL__BADR ) {
                  sdd += ( (double)d * (double)d );
                  ndd++;
               }
            }
            var_array[ iv0 ] = ( (swdd/sw) - (sdd/ndd) )*fac*sww/( sw*sw );
            if( var_array[ iv0 ] <= 0.0 ) var_array[ iv0 ] = VAL__BADR;

         } else {
            var_array[ iv0 ] = VAL__BADR;
         }
      }

/* Now handle cases where the output variance is calculated on the basis
   of the input Tsys values. */
   } else if( genvar == 2 ) {
      for( iv0 = 0; iv0 < nxy; iv0++ ) {
         sw = wgt_array[ iv0 ];
         if( sw > 0.0 ) {
            swwv = var_array[ iv0 ];
            var_array[ iv0 ] = swwv/( sw*sw );
         } else {
            var_array[ iv0 ] = VAL__BADR;
         }
      }
   }
}
