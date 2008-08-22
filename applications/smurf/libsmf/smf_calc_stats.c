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
*                      dim_t lo, dim_t hi, double *mean, double *stdev,
*                      int *status ) 

*  Arguments:
*     data = const smfData* (Given)
*        Pointer to input data struct
*     mode = const char* (Given)
*        Mode to determine whether data are for fixed bolometer or timeslice
*     index = const int (Given)
*        Index into array of fixed datapoint
*     lo = int (Given)
*        Lower index bound into array
*     hi = int (Given)
*        Upper index bound into array
*     mean = double* (Returned)
*        Mean over specified interval
*     stdev = double* (Returned)
*        Standard deviation of sample
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculates the mean and standard deviation of a
*     sample of points specified by three indices. The Kappa routine
*     kpgStatd is used. The first index is which bolometer or
*     timeslice we are interest in, the second and third mark the
*     range of values to include in the sample. If both lo and hi are
*     zero then the entire range is used. On error a mean and std
*     deviation of VAL__BADD are returned.

*  Notes: 
*     - The range lo to hi is INCLUSIVE.
*     - Further API updates are likely in order to take fuller advantage
*       of kpsStatd.

*  Authors:
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
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
*     {enter_further_changes_here}

*  Copyright:
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
                      dim_t lo, dim_t hi, double *mean, double *stdev, 
		      int *status) {

  /* Local variables */
  double *indata = NULL;      /* Pointer to input data array */
  size_t k;                   /* Loop counter */
  size_t npts;                /* Number of data points in range */
  dim_t nbol;                 /* Number of bolometers */
  dim_t nmax;                 /* Max value for index */
  dim_t nsamp;                  /* Number of samples */
  double *statsdata = NULL;   /* Pointer to array for computing stats */
  int temp;                   /* Temporary variable */

  /* Current list of variables for kpgStatd - move into API as appropriate */
  int nclip = 0;              /* Number of K-sigma clipping iterations to apply: 
				 none at present  */
  float clip[ MXCLIP ];       /* Array of clipping limits for successive iterations,
				 expressed as standard deviations. */
  int bad = 1;                /* Do we check for bad pixels? Default to yes */
  int ngood;                  /* Number of valid pixels before clipping */
  int imin;                   /* Index where the pixel with the lowest value was 
				 (first) found before clipping */
  double dmin;                /* Minimum pixel value in the array before clipping */
  int imax;                   /* Index where the pixel with the highest value was 
				 (first) found before clipping*/
  double dmax;                /* Maximum pixel value in the array before clipping */
  double sum;                 /* Sum of valid pixels before clipping */
  int ngoodc;                 /* Number of valid pixels in the array after clipping */
  int iminc;                  /* Index where the pixel with the lowest value was 
				 (first) found after clipping */
  double dminc;               /* Minimum pixel value in the array after clipping */
  int imaxc;                  /* Index where the pixel with the highest value was 
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

  /* Should check data type for double */
  smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status);
  if ( *status != SAI__OK) return;

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
      return;
    }
  }

  /* Check index is in range */
  if ( index >= nmax ) {
    if ( *status == SAI__OK) {
      msgSeti("I", index);
      msgSeti("N", nmax);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested index, ^I, is out of range (max is ^N).", 
             status);
      return;
    }
  }

  /* Check requested range is valid */
  if ( lo >= nsamp ) {
    if ( *status == SAI__OK) {
      msgSeti("J", lo);
      msgSeti("N", nsamp);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested sample, ^J, is out of range (0 < lo < ^N).",
             status);
      return;
    }
  }
  if ( hi >= nsamp ) {
    if ( *status == SAI__OK) {
      msgSeti("J", hi);
      msgSeti("N", nsamp);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested sample, ^J, is out of range (0 < hi < ^N).",
             status);
      return;
    }
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
      return;
    }
  }

  /* Allocate memory for data */
  npts = hi - lo + 1;

  statsdata = smf_malloc( npts, sizeof(double), 0, status );
  if( statsdata == NULL ) {
    errRep( FUNC_NAME, "Unable to allocate memory for statistics array", 
            status );
    return;
  }

  /* Set range of data. Use <= because the range is inclusive. */
  indata = (data->pntr)[0];
  if( indata != NULL ) {
    if ( strncmp( mode, "b", 1 ) == 0 ) {
      /* Pick out a bolometer time series */
      for ( k=lo; k<=hi; k++) {
        statsdata[k-lo] = indata[index + k*nbol];
      }
    } else {
      /* Pick out a range of bolometers from a timeslice */
      for ( k=lo; k<=hi; k++) {
        statsdata[k-lo] = indata[nbol*index + k];
      }
    }
  } else {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Empty input data array.", status);
  }

  /* Calculate stats */
  if ( *status == SAI__OK) {
    kpgStatd( bad, npts, statsdata, nclip, clip, 
              &ngood, &imin, &dmin, &imax, &dmax, &sum, mean, stdev,
              &ngoodc, &iminc, &dminc, &imaxc, &dmaxc, &sumc, &meanc, &stdevc,
              status);
  }    

  /* Free resources */
  if ( statsdata != NULL)
    statsdata = smf_free( statsdata, status );

}
