
/*
*+
*  Name:
*     smf_rebincube_init

*  Purpose:
*     Initilise things prior to pasting the first input cube into the
*     outout cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebincube_init( int is2d, dim_t nxy, dim_t nout, int genvar,
*                         float *data_array, float *var_array,
*                         double *wgt_array, float *texp_array,
*                         float *teff_array, int *nused, int *status );

*  Arguments:
*     is2d = (Given)
*        Non-zero if the weights array and variance array are 2D.
*     nxy = dim_t (Given)
*        Number of elements in a single plane of the output cube.
*     nout = dim_t (Given)
*        Total number of elements in the output cube.
*     genvar = int (Given)
*        Indicates how the output variances should be calculated:
*           0 = do not calculate any output variances
*           1 = use spread of input data values
*           2 = use system noise temperatures
*     data_array = float * (Returned)
*        The data array for the output cube.
*     var_array = float * (Returned)
*        An array in which to store the variances for the output cube if
*        "genvar" is not zero (the supplied pointer is ignored if "genvar"
*        is zero). If "is2d" is non-zero, this array should have "nxy"
*        elements. Otherwise it should have "nout" elements.
*     wgt_array = double * (Returned)
*        An array in which to store the relative weighting for each pixel in
*        the output cube. This array should be the length of "var_array",
*        unless "genvar" is 1, in which case it should be twice the length
*        of "var_array".
*     texp_array = float * (Returned)
*        A work array, which holds the total exposure time for each output
*        spectrum. It should have "nxy" elements.
*     teff_array = float * (Given and Returned)
*        A work array, which holds the effective integration time for each
*        output spectrum, scaled by a factor of 4. It should have "nxy"
*        elements.
*     nused = int * (Returned)
*        Use to accumulate the total number of input data samples that
*        have been pasted into the output cube.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     All the supplied arrays are filled with zero, and "*nused" is set
*     to zero.

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

/* Starlink includes */
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_rebincube_init"

void smf_rebincube_init( int is2d, dim_t nxy, dim_t nout, int genvar,
                         float *data_array, float *var_array,
                         double *wgt_array, float *texp_array,
                         float *teff_array, int *nused, int *status ) {

/* Local Variables */
   dim_t iv;                   /* Vector index into output 3D array */
   dim_t nelwgt;               /* No of elements in weights array */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Note the number of elements in the weight array. */
   nelwgt = is2d ? nxy : nout;

/* Initialise the total number of input values pasted into the output so far. */
   *nused = 0;

/* Initialise the output data array. */
   for( iv = 0; iv < nout; iv++ ) data_array[ iv ] = 0.0;

/* Initialise the weights array (note, its length depends on how the
   output variances are being created). */
   if( genvar == 1 ) {
      for( iv = 0; iv < 2*nelwgt; iv++ ) wgt_array[ iv ] = 0.0;
   } else {
      for( iv = 0; iv < nelwgt; iv++ ) wgt_array[ iv ] = 0.0;
   }

/* Initialise the output variance array. */
   if( genvar ) {
      for( iv = 0; iv < nelwgt; iv++ ) var_array[ iv ] = 0.0;
   }

/* Initialise the total and effective exposure times for each output
   spectrum. */
   for( iv = 0; iv < nxy; iv++ ) texp_array[ iv ] = 0.0;
   for( iv = 0; iv < nxy; iv++ ) teff_array[ iv ] = 0.0;

}
