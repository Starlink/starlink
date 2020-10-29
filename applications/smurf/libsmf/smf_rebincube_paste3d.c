
/*
*+
*  Name:
*     smf_rebincube_paste3d

*  Purpose:
*     Paste a single input spectrum into the output cube using nearest
*     neighbour rebinning and a 3D weights array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_rebincube_paste3d( dim_t nchan, dim_t nout, int *spectab,
*                                dim_t iv0, dim_t nxy, double wgt,
*                                int genvar, double invar, float *ddata,
*                                float *data_array, float *var_array,
*                                double *wgt_array, int64_t *nused,
*                                int *status );

*  Arguments:
*     nchan = dim_t (Given)
*        Number of spectral channels in input cube.
*     nout = dim_t (Given)
*        Number of elements in output cube.
*     spectab = int * (Given)
*        This array should have "nchan" elements, and each element should
*        hold the integer index (zero-based) of the nearest neighbouring
*        output channel. A value of -1 should flag input channels that do
*        not have any corresponding output channel.
*     iv0 = dim_t (Given)
*        The index within the output cube of the pixel corresponding to
*        channel zero of the output spectrum into which the input spectrum
*        is to be pasted.
*     nxy = dim_t (Given)
*        Number of elements in one spatial plane of the output cube.
*     wgt = double (Given)
*        The weight for the input spectrum.
*     genvar = int (Given)
*        Indicates how the output variances should be calculated:
*           0 = do not calculate any output variances
*           1 = use spread of input data values
*           2 = use system noise temperatures
*     invar = double (Given)
*        The Variance for the input spectrum.
*     ddata = float * (Given)
*        A pointer to the first data value in the input spectrum.
*     data_array = float * (Given and Returned)
*        The 3D data array for the output cube. This is updated on exit to
*        include the data from the supplied input spectrum.
*     var_array = float * (Given and Returned)
*        A 2D array in which to store the variances for the output cube if
*        "genvar" is not zero (the supplied pointer is ignored if "genvar" is
*        zero). The supplied array is updated on exit to include the data from
*        the supplied input spectrum. This array should be big enough to hold
*        a single spatial plane from the output cube (all planes in the
*        output cube will have the same variance).
*     wgt_array = double * (Given and Returned)
*        An array in which to store the relative weighting for each pixel in
*        the output cube. The supplied array is update on exit to include the
*        data from the supplied input spectrum. If "genvar" is 2, this array
*        should be big enough to hold a single spatial plane from the output
*        cube (all planes in the output cube will have the same weight). If
*        "genvar" is 2, this array should be big enough to hold two spatial
*        planes from the output cube.
*     nused = int64_t * (Given and Returned)
*        Use to accumulate the total number of input data samples that
*        have been pasted into the output cube.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Returned Function Value:
*     A flag that is non-zero if and only if at least one good value from
*     the input spectrum was pasted into the output spectrum.

*  Description:
*     Paste a single input spectrum into the output cube using nearest
*     neighbour rebinning and a 2D weights array.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     23-APR-2006 (DSB):
*        Initial version.
*     29-OCT-2020 (DSB):
*        Return a flag indicating if the spectrum was used.
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
#include <stdint.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_rebincube_paste3d"

int smf_rebincube_paste3d( dim_t nchan, dim_t nout, int *spectab, dim_t iv0,
                           dim_t nxy, double wgt, int genvar, double invar,
                           float *ddata, float *data_array,
                           float *var_array, double *wgt_array, int64_t *nused,
                           int *status ){

/* Local Variables */
   dim_t iv;                   /* Vector index into output 3D array */
   dim_t ichan;                /* Index of current channel */
   int result = 0;             /* Returned flag */

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Loop round all spectral channels. */
   for( ichan = 0; ichan < nchan; ichan++, ddata++ ) {

/* If this input data value is good, and has a valid spectral position in
   the output cube, add it into the output arrays. */
      if( *ddata != VAL__BADR && spectab[ ichan ] >= 0 ) {
         result = 1;

/* Get the offset within the output arrays to the value to be incremented. */
         iv = iv0 + spectab[ ichan ]*nxy;

/* Increment the output data array by the weighted input data value. */
         data_array[ iv ] += ( *ddata )*wgt;

/* Increment the output weights array by the weight. */
         wgt_array[ iv ] += wgt;

/* Increment the total number of good input pixels pasted into the output
   cube. */
         (*nused)++;

/* Now store info needed to calculate the output variances. What this
   info is depends on whether output variances are being calculated on the
   basis of input Tsys values or on the basis of the spread of input data
   values. First deal with Tsys variances. */
         if( genvar == 2 ) {
            var_array[ iv ] += wgt*wgt*invar;

/* Now deal with "spread" variances. */
         } else if( genvar == 1 ) {
            var_array[ iv ] += wgt*wgt;
            wgt_array[ iv + nout ] += ( *ddata )*( *ddata )*wgt;
         }
      }
   }

   return result;
}

