/*
*+
*  Name:
*     smf_resampcube_copy

*  Purpose:
*     Copy a single sky cube spectrum into the output time series cube
*     using nearest neighbour resampling.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_resampcube_copy( dim_t nchan, dim_t *spectab,
*                               dim_t iv0, dim_t nxy, float *ddata,
*                               float *in_data, int *status );

*  Arguments:
*     nchan = dim_t (Given)
*        Number of spectral channels in the template time series cube.
*     spectab = dim_t * (Given)
*        This array should have "nchan" elements, and each element should
*        hold the integer index (zero-based) of the nearest neighbouring
*        sky cube channel. A value of -1 should flag time series channels
*        that do not have any corresponding sky cube channel.
*     iv0 = dim_t (Given)
*        The index within the sky cube of the pixel corresponding to
*        channel zero of the spectrum being copied.
*     nxy = dim_t (Given)
*        Number of elements in one spatial plane of the sky cube.
*     ddata = float * (Given)
*        A pointer to the first data value in the output time series spectrum.
*     in_data = float * (Given)
*        The 3D data array for the input sky cube.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     Copy a single spectrum from the sky cube to the output time series
*     cube using nearest neighbour resampling.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     1-FEB-2008 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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

#define FUNC_NAME "smf_resampcube_copy"

void smf_resampcube_copy( dim_t nchan, dim_t *spectab,
                          dim_t iv0, dim_t nxy, float *ddata,
                          float *in_data, int *status ){

/* Local Variables */
   dim_t iv;                   /* Vector index into sky cube 3D array */
   dim_t ichan;                /* Index of current channel */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Loop round all spectral channels. */
   for( ichan = 0; ichan < nchan; ichan++, ddata++ ) {

/* If this spectral channel has a valid spectral position in the sky cube,
   add it into the output arrays. */
      if( spectab[ ichan ] >= 0 ) {

/* Get the offset within the sky cube array to the value to be copied. */
         iv = iv0 + spectab[ ichan ]*nxy;

/* Store the sky cube value in the output time series array. */
         *ddata = in_data[ iv ];

      }
   }
}

