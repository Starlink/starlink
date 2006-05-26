/*
*+
*  Name:
*     smf_boxcar1

*  Purpose:
*     Low-level routine to smooth a 1-D array with a boxcar filter

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_boxcar1 ( double *series, const int ninpts, int window, int *status )

*  Arguments:
*     series = double* (Given)
*        Pointer to data array
*     ninpts = int (Given)
*        Number of points in input series
*     window = int (Given)
*        Size of boxcar filter window (in array elements)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine performs a boxcar average of an input array over a
*     given window, replacing values in the array with the appropriate
*     average value. If the window size exceeds the size of the input
*     array then the routine will replace the array values with the
*     mean of the entire array.

*  Notes: 
*     Does not deal with bad values

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-26 (AGG):
*        Initial test version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <stdio.h>
#include <string.h>

/* GSL includes */
#include <gsl/gsl_statistics_double.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_boxcar1"

void smf_boxcar1 ( double *series, const int ninpts, int window, int *status) {

  /* Local variables */
  size_t i;                   /* Loop counter */
  size_t j;                   /* Loop counter */
  double smooth;              /* Smoothed value */
  size_t next;                /* Next starting element */

  /* Check status */
  if (*status != SAI__OK) return;

  /* Return if window is unity or the array only has 1 point */
  if ( window == 1 || ninpts == 1 ) return;

  /* Sanity check: is window smaller than size of input array? */
  if ( window > ninpts ) {
    msgSeti("B",window);
    msgSeti("N",ninpts);
    msgOut(FUNC_NAME, "Size of window exceeds extent of data array: will return average for whole array", status);
    window = ninpts;
  }

  /* Initialize the sum and next index */
  smooth = 0;
  next = window - 1;
  /* Loop over the points in the input array */
  for ( i=0; i<ninpts; i++ ) {
    if ( i == next ) {
      /* Calculate mean */
      smooth /= (double)window;
      /* Back fill series with smoothed values */
      for ( j=0; j<window; j++) {
	series[i-j] = smooth;
      }
      /* Set next starting index */
      next += window;
      /* If bigger tha array size, set next starting index to the
	 array size */
      if ( next > ninpts) {
	next = ninpts - 1;
      }
      /* Reset the smoothed value to zero */
      smooth = 0;
    } else {
      /* Accumulate the sum */
      smooth += series[i];
    }
  }  

}
