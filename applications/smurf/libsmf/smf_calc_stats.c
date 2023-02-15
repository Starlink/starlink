/*
*+
*  Name:
*     smf_calc_stats

*  Purpose:
*     Low-level routine to compute statistics of a range of values

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_calc_stats ( const smfData *data, const char *mode, const dim_t index,
*                      dim_t lo, dim_t hi, int nclip, const float clip[],
*                      double *mean, double *stdev,
*                      int *status )

*  Arguments:
*     data = const smfData* (Given)
*        Pointer to input data struct
*     mode = const char* (Given)
*        Mode to determine whether data are for fixed bolometer or timeslice.
*        - "b" to calculate stats for a single bolometer at coordinate "index".
*        - "t" to calculate stats for a 2d slice at a particular time "index"
*     index = const dim_t (Given)
*        Index into array of fixed datapoint. For "b" this will be the bolometer
*        number (assuming flattened). For "t" this will be the time slice.
*     lo = dim_t (Given)
*        Lower index bound into array. For "b" this is the index of the first
*        time slice. For "t" this is the start bolometer. "0" for min value.
*     hi = dim_t (Given)
*        Upper index bound into array. For "b" this is the end time slice.
*        For "t" this is the end bolometer.
*        "0" for max value.
*     nclip = int (Given)
*        Number of K-sigma clipping iterations to apply (number of elements
*        in "clip").
*     clip = const float[] (Given)
*        N-sigma clip levels to use. Expressed as standard deviations.
*     mean = double* (Returned)
*        Mean over specified interval
*     stdev = double* (Returned)
*        Standard deviation of sample
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculates the mean and standard deviation of a
*     sample of points specified by three indices. The KAPPA routine
*     kpgStatx is used. The first index is which bolometer or
*     timeslice we are interest in, the second and third mark the
*     range of values to include in the sample. If both lo and hi are
*     zero then the entire range is used. On error a mean and std
*     deviation of VAL__BADD are returned.

*  Notes:
*     - The range lo to hi is INCLUSIVE.
*     - Further API updates are likely in order to take fuller advantage
*       of kpgStatx.
*     - This routine is not thread-safe since it uses Fortran kaplibs routines.

*  Authors:
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-05-17 (AGG):
*        Initial test version
*     2006-05-18 (EC):
*        -Change mean/sigma to pointers so that they may be returned
*        -Pointer math bugs / range checking
*     2006-05-26 (AGG):
*        - Replace GSL calls with weighted versions to cope with
*          bad values
*     2006-07-10 (AGG):
*        - Fix indexing bug
*        - Eliminate GSL calls, now call kpgStatd
*     2006-07-11 (AGG):
*        Delete weight-setting code
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-07-03 (EC):
*        Use dim_t for index, lo and hi
*     2008-08-21 (TIMJ):
*        Works for multiple data types.
*     2008-09-02 (TIMJ):
*        Add clipping (see kpg1Statx).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilties Council.
*     Copyright (C) 2006-2008 University of British Columbia. All Rights
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
#include "star/kaplibs.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_calc_stats"

/* Maximum number of sigma-clipping values */
#define MXCLIP 5

void smf_calc_stats ( const smfData *data, const char *mode, const dim_t index,
                      dim_t lo, dim_t hi, int nclip, const float clip[],
                      double *mean, double *stdev,
                      int *status) {

  /* Local variables */
  void *indata = NULL;        /* Pointer to input data array */
  dim_t k;                    /* Loop counter */
  dim_t mult;                 /* stride through data array */
  dim_t npts;                 /* Number of data points in range */
  dim_t nbperel;              /* number of bytes in data type */
  dim_t nbol;                 /* Number of bolometers */
  dim_t nmax;                 /* Max value for index */
  dim_t nsamp;                /* Number of samples */
  dim_t offset;               /* offset into data array */
  void *statsdata = NULL;     /* Pointer to array for computing stats */
  dim_t temp;                 /* Temporary variable */

  /* Per data type pointers */
  double *in_d = NULL;        /* pointer to double input data */
  double *stats_d = NULL;     /* pointer to double stats data */
  int *in_i = NULL;           /* pointer to int input data */
  int *stats_i = NULL;        /* pointer to int stats data */

  /* Current list of variables for kpgStatd - move into API as appropriate */
  int bad = 1;                /* Do we check for bad pixels? Default to yes */
  dim_t ngood;                /* Number of valid pixels before clipping */
  dim_t imin;                 /* Index where the pixel with the lowest value was
				 (first) found before clipping */
  double dmin;                /* Minimum pixel value in the array before clipping */
  dim_t imax;                 /* Index where the pixel with the highest value was
				 (first) found before clipping*/
  double dmax;                /* Maximum pixel value in the array before clipping */
  double sum;                 /* Sum of valid pixels before clipping */
  dim_t ngoodc;               /* Number of valid pixels in the array after clipping */
  dim_t iminc;                /* Index where the pixel with the lowest value was
				 (first) found after clipping */
  double dminc;               /* Minimum pixel value in the array after clipping */
  dim_t imaxc;                /* Index where the pixel with the highest value was
				 (first) found after clipping */
  double dmaxc;               /* Maximum pixel value in the array after clipping */
  double sumc;                /* Sum of valid pixels after clipping */
  double meanc;               /* Mean of valid pixels after clipping */
  double stdevc;              /* Standard deviation of the above*/

  /* Check status */
  if (*status != SAI__OK) return;

  /* Initialize mean and std deviation to bad values */
  *mean = VAL__BADD;
  *stdev = VAL__BADD;

  /* Do we have 2-D image or 3-D timeseries data? */
  if ( data->ndims != 3 ) {
    /* Abort with an error if the number of dimensions is not  3 */
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti("ND", data->ndims);
      errRep(FUNC_NAME,
             "Number of dimensions of input file is ^ND: should be 3. Meaningless to compute statistics for 2-D data.",
             status);
      return;
    }
  }

  /* Check mode */
  if ( strncmp( mode, "b", 1 ) == 0 ) {
    /*msgOutif(MSG__VERB," ", "Calculating stats for a fixed bolometer", status);*/

    nmax = (data->dims)[0] * (data->dims)[1];
    nsamp = (data->dims)[2];
    nbol = nmax;
  } else if ( strncmp( mode, "t", 1 ) == 0 ) {
    /*msgOutif(MSG__VERB," ", "Calculating stats for a fixed timeslice", status);*/

    nmax = (data->dims)[2];
    nsamp = (data->dims)[0] * (data->dims)[1];
    nbol = nsamp;
  } else {
    if ( *status == SAI__OK ) {
      msgSetc("M", mode);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unsupported mode, ^M. Must be b or t.", status);
    }
    return;
  }

  /* Check index is in range */
  if ( index >= nmax ) {
    if ( *status == SAI__OK) {
      msgSetk("I", index);
      msgSetk("N", nmax);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested index, ^I, is out of range (max is ^N).",
             status);
    }
    return;
  }

  /* Check requested range is valid */
  if ( lo >= nsamp ) {
    if ( *status == SAI__OK) {
      msgSetk("J", lo);
      msgSetk("N", nsamp);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested sample, ^J, is out of range (0 < lo < ^N).",
             status);
    }
    return;
  }
  if ( hi >= nsamp ) {
    if ( *status == SAI__OK) {
      msgSetk("J", hi);
      msgSetk("N", nsamp);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested sample, ^J, is out of range (0 < hi < ^N).",
             status);
    }
    return;
  }

  /* Check hi is larger than lo; swap if not */
  if ( lo > hi ) {
    temp = lo;
    lo = hi;
    hi = temp;
    msgOutif(MSG__VERB," ", "Oops - lo > hi. Swapping them round.",
             status);
  }

  /* If lo and hi are both zero then the whole range is assumed */
  if ( lo == 0 && hi == 0 ) {
    hi = nsamp - 1;
  }

  /* Check if they're equal */
  if ( lo == hi ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME,
             "Requested index range is zero (lo = hi). Unable to compute statistics.",
             status);
    }
    return;
  }

  /* How many bytes per element */
  nbperel = smf_dtype_size(data, status );

  /* Allocate memory for data */
  npts = hi - lo + 1;

  statsdata = astMalloc( npts*nbperel );
  if( statsdata == NULL ) {
    errRep( FUNC_NAME, "Unable to allocate memory for statistics array",
            status );
    return;
  }

  /* Set range of data. Use <= because the range is inclusive. */

  /* Calculate offset and multiplier. This lets us to work in either mode
     without repeating code */
  if (strncmp( mode, "b", 1 ) == 0 ) {
    offset = index;
    mult = nbol;
  } else {
    offset = nbol * index;
    mult = 1;
  }

  indata = (data->pntr)[0];
  if( indata != NULL ) {
    switch ( data->dtype ) {
      /* duplicate the for loop rather than using unsigned char and nbperel.
         This is slightly clearer and also more efficient. */
    case SMF__DOUBLE:
      in_d = indata;
      stats_d = statsdata;
      for (k = lo; k <= hi; k++ ) {
        stats_d[k-lo] = in_d[offset+(k*mult)];
      }
      break;
    case SMF__INTEGER:
      in_i = indata;
      stats_i = statsdata;
      for (k = lo; k <= hi; k++ ) {
        stats_i[k-lo] = in_i[offset+(k*mult)];
      }
      break;
    default:
      msgSetc( "TYP", smf_dtype_string( data, status ));
      *status = SAI__ERROR;
      errRep(" ", FUNC_NAME " Unsupported data type ^TYP",
             status);
    }
  } else {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Empty input data array.", status);
  }

  /* Calculate stats */
  if ( *status == SAI__OK) {
    switch ( data->dtype ) {
    case SMF__DOUBLE:
      kpgStat8d( bad, npts, stats_d, nclip, clip,
                &ngood, &imin, &dmin, &imax, &dmax, &sum, mean, stdev,
                &ngoodc, &iminc, &dminc, &imaxc, &dmaxc, &sumc, &meanc, &stdevc,
                status);
      break;
    case SMF__INTEGER:
      kpgStat8i( bad, npts, stats_i, nclip, clip,
                &ngood, &imin, &dmin, &imax, &dmax, &sum, mean, stdev,
                &ngoodc, &iminc, &dminc, &imaxc, &dmaxc, &sumc, &meanc, &stdevc,
                status);
      break;
    default:
      msgSetc( "TYP", smf_dtype_string( data, status ));
      *status = SAI__ERROR;
      errRep(" ", FUNC_NAME " Unsupported data type ^TYP",
             status);
    }

    /* if clipping was enabled, switch to the clipped result */
    if (nclip > 0) {
      *mean = meanc;
      *stdev = stdevc;
    }

  }

  /* Free resources */
  if ( statsdata != NULL)
    statsdata = astFree( statsdata );

}
