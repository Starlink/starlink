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
*     smf_boxcar1 ( double *series, const size_t ninpts, size_t window, 
*                   int *status )

*  Arguments:
*     series = double* (Given)
*        Pointer to data array
*     ninpts = const size_t (Given)
*        Number of points in input series
*     window = size_t (Given)
*        Size of boxcar filter window (in array elements)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine performs an in-place boxcar average of an input
*     array over a given window, replacing values in the array with
*     the appropriate average value. If the window size exceeds the
*     size of the input array then the routine will replace the array
*     values with the mean of the entire array. The half-windows at
*     the start and end are filled with the nearest calculated smooth
*     values.

*  Notes: 
*     Does not deal with bad values

*  Authors:
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-26 (AGG):
*        Initial test version
*     2006-10-11 (AGG):
*        Change int arguments to size_t
*     2007-06-27 (EC):
*        Changed algorithm to calculate a "smooth" boxcar (old algorithm
*        assigned same smooth value to all samples within disjoint windows)
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <stdio.h>
#include <string.h>

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

void smf_boxcar1 ( double *series, const size_t ninpts, size_t window, int *status) {

  /* Local variables */
  size_t i;                   /* Loop counter */
  size_t j;                   /* Loop counter */
  double pad=0;               /* Pad value */
  double *seriescopy;         /* Copy of the time series */
  double sum;                 /* Sum of values in the window */
  
  /* Check status */
  if (*status != SAI__OK) return;

  /* Return if window is unity or the array only has 1 point */
  if ( window == 1 || ninpts == 1 ) return;

  /* Sanity check: is window smaller than size of input array? */
  if ( window > ninpts ) {
    msgSeti("B",(int)window);
    msgSeti("N",(int)ninpts);
    msgOut(" ", "Size of window exceeds extent of data array: will return average for whole array", status);
    window = ninpts;
  }

  /* Make a copy of the time series that won't get altered as we go */
  seriescopy = smf_malloc( ninpts, sizeof(*series), 0, status );

  if( *status == SAI__OK ) {
    memcpy( seriescopy, series, ninpts*sizeof(*series) );

    sum = 0;

    for( i=0; i<ninpts; i++ ) {
      /* sum another point from the unaltered array */
      sum += seriescopy[i];

      /* If we have a full window start calculating smooth values */
      if( i >= (window-1) ) {
	series[i-window/2] = sum / (double) window;

	/* Subtract off the first sample in the window here before adding in
           a new point next time around the loop */
	sum -= seriescopy[i-(window-1)];
      }
    }

    /* Pad the start and end half-windows with nearest smoothed value */

    if( (ninpts == window) && ((ninpts % 2) == 0 ) ) {
      /* In this special case modify the algorithm to correctly pad the
	 entire array with the single mean value */
      pad = series[ninpts-1-window/2];
      for( i=0; i<ninpts; i++ ) {
    	series[i] = pad;
      }
    } else { 
      /* Otherwise pad value comes one sample after/before the ends of the
         half-windows */ 
      for( i=0; i<window/2; i++ ) {
    	series[i] = series[window/2+1];
    	series[ninpts-1-i] = series[ninpts-2-window/2];
      }
    }
  }

  /* Clean Up */
  if ( seriescopy ) 
    seriescopy = smf_free( seriescopy, status );

}
