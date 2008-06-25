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
*                   unsigned char *qual, unsigned char mask, 
*                   int *status )

*  Arguments:
*     series = double* (Given)
*        Pointer to data array
*     ninpts = const size_t (Given)
*        Number of points in input series
*     window = size_t (Given)
*        Size of boxcar filter window (in array elements)
*     qual = usigned char* (Given)
*        If specified, use this QUALITY array to decide which samples
*        to use (provided mask). Otherwise data are only ignored if set
*        to VAL__BADD.
*     mask = unsigned char (Given)
*        Use with qual to define which bits in quality are relevant to
*        ignore data in the calculation.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine performs an in-place boxcar average of an input
*     array over a given window, replacing values in the array with
*     the appropriate average value. If the window size exceeds the
*     size of the input array then the routine will replace the array
*     values with the mean of the entire array. The half-windows at
*     the start and end are filled with smoothed values over shortened
*     intervals (smoothly changes from WINDOW to 1 sample in length). 

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
*     2008-04-14 (EC):
*        -added QUALITY masking
*        -algorithm now smooths with shortened intervals at array ends
*     2008-06-25 (EC):
*        -Only evaluate smooth values with good QUALITY / not VAL__BADD
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

void smf_boxcar1( double *series, const size_t ninpts, size_t window, 
		  unsigned char *qual, unsigned char mask, 
		  int *status) {

  /* Local variables */
  size_t off;                 /* offset from loop counter to modified sample */
  size_t count;               /* Number of samples in window */
  size_t i;                   /* Loop counter */
  double *seriescopy;         /* Copy of the time series */
  double sum;                 /* Sum of values in the window */
  
  /* Check status */
  if (*status != SAI__OK) return;

  /* Return if window is unity or the array only has 1 point */
  if ( window == 1 || ninpts <= 1 ) return;

  /* Sanity check: is window smaller than size of input array? */
  if ( window > ninpts ) {
    msgSeti("B",(int)window);
    msgSeti("N",(int)ninpts);
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Size of window (^B) exceeds extent of data array (^N)", 
	   status);
    return;
  }

  /* Make a copy of the time series that won't get altered as we go */
  seriescopy = smf_malloc( ninpts, sizeof(*series), 0, status );

  if( *status == SAI__OK ) {
    memcpy( seriescopy, series, ninpts*sizeof(*series) );

    sum = 0;
    count = 0;

    if( qual ) {    /* QUALITY checking version */
      
      for( i=0; i<ninpts; i++ ) {
	/* sum another point from the unaltered array */
	if( !(qual[i]&mask) ) {
	  sum += seriescopy[i];
	  count++;
	}
	
	if( i < (window-1) ) off = -i/2;
	else off = -window/2;
	
	/* As soon as we have at least 2 samples start applying smooth val */
	if( (count > 1) && !(qual[i+off]&mask) ) {
	  series[i+off] = sum / (double) count;      
	}
	
	/* Subtract off the first sample in the window if we are at
	   least window samples from the start here before adding in a
	   new point next time around the loop */
	if( (i >= (window-1)) && !(qual[i-(window-1)]&mask) ) {
	  sum -= seriescopy[i-(window-1)];
	  count--;
	}
	
      }
      
      /* at the end of the array smooth using the partial window */
      for( i=ninpts-window; i<ninpts; i++ ) {
	off = (ninpts-i-1)/2;
	
	if( (count > 1) && !(qual[i+off]&mask) ) {
	  series[i+off] = sum / (double) count;
	}
	
	/* Remove the sample at i from the window */
	if( seriescopy[i] != VAL__BADD ) {
	  sum -= seriescopy[i];
	  count--;
	}
      }

    } else {        /* VAL__BADD checking version */

      for( i=0; i<ninpts; i++ ) {
	/* sum another point from the unaltered array */
	if( seriescopy[i] != VAL__BADD ) {
	  sum += seriescopy[i];
	  count++;
	}
	
	if( i < (window-1) ) off = -i/2;
	else off = -window/2;
	
	/* As soon as we have at least 2 samples start applying smooth val */
	if( (count > 1) && (series[i+off] != VAL__BADD) ) {
	  series[i+off] = sum / (double) count;      
	}
	
	/* Subtract off the first sample in the window if we are at
	   least window samples from the start here before adding in a
	   new point next time around the loop */
	if( (i >= (window-1)) && (seriescopy[i-(window-1)] != VAL__BADD) ) {
	  sum -= seriescopy[i-(window-1)];
	  count--;
	}
	
      }
      
      /* at the end of the array smooth using the partial window */
      for( i=ninpts-window; i<ninpts; i++ ) {
	off = (ninpts-i-1)/2;
	
	if( (count > 1) && (series[i+off] != VAL__BADD) ) {
	  series[i+off] = sum / (double) count;
	}
	
	/* Remove the sample at i from the window */
	if( seriescopy[i] != VAL__BADD ) {
	  sum -= seriescopy[i];
	  count--;
	}
      }
    }
  }

  /* Clean Up */
  if ( seriescopy ) seriescopy = smf_free( seriescopy, status );

}
