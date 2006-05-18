/*
*+
*  Name:
*     smf_calc_stats

*  Purpose:
*     Low-level routine to compute statistics using GSL

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_calc_stats ( const smfData *data, const char *mode, const int index,
*                      int lo, int hi, double mean, double sigma,
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
*     mean = double (Returned)
*        Mean over specified interval
*     sigma = double (Returned)
*        Standard deviation of sample
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculates the mean and standard deviation of a
*     sample of points specified by three indices. The GSL routines
*     gsl_stats_mean and gsl_stats_sd are used. The first index is
*     which bolometer or timeslice we are interest in, the second and
*     third mark the range of values to include in the sample. Note
*     that the range lo to hi is INCLUSIVE. If both lo and hi are zero
*     then the entire range is used. On error a mean and std deviation
*     of VAL__BADD are returned.

*  Notes: 

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-17 (AGG):
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
#define FUNC_NAME "smf_calc_stats"

void smf_calc_stats ( const smfData *data, const char *mode, const int index,
                      int lo, int hi, double mean, double sigma, 
		      int *status) {

  /* Local variables */
  double *indata = NULL;      /* Pointer to input data array */
  size_t k;                   /* Loop counter */
  size_t npts;                /* Number of data points in range */
  int nbol;                   /* Number of bolometers */
  int nmax;                   /* Max value for index */
  int nsamp;                  /* Number of samples */
  double *statsdata = NULL;   /* Pointer to array holding data for computing stats */
  int temp;                   /* Temporary variable */

  /* Check status */
  if (*status != SAI__OK) return;

  /* Initialize mean and std deviation to bad values */
  mean = VAL__BADD;
  sigma = VAL__BADD;

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
    msgOutif(MSG__VERB, FUNC_NAME, "Calculating stats for a fixed bolometer", status);
    nmax = (data->dims)[0] * (data->dims)[1];
    nsamp = (data->dims)[2];
    nbol = nmax;
  } else if ( strncmp( mode, "t", 1 ) == 0 ) {
    msgOutif(MSG__VERB, FUNC_NAME, "Calculating stats for a fixed timeslice", status);
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
  if ( index > nmax || index < 0 ) {
    if ( *status == SAI__OK) {
      msgSeti("I", index);
      msgSeti("N", nmax);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested index, ^I, is out of range (max is ^N).", status);
      return;
    }
  }

  /* Check requested range is valid */
  if ( lo > nsamp || lo < 0 ) {
    if ( *status == SAI__OK) {
      msgSeti("J", lo);
      msgSeti("N", nsamp);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested sample, ^J, is out of range (0 < lo < ^N).", status);
      return;
    }
  }
  if ( hi > nsamp || hi < 0 ) {
    if ( *status == SAI__OK) {
      msgSeti("J", hi);
      msgSeti("N", nsamp);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested sample, ^J, is out of range (0 < hi < ^N).", status);
      return;
    }
  }

  /* Check hi is larger than lo; swap if not */
  if ( lo > hi ) {
    temp = lo;
    lo = hi;
    hi = temp;
    msgOutif(MSG__VERB, FUNC_NAME, "Oops - lo > hi. Swapping them round.", status);
  }

  /* If lo and hi are both zero then the whole range is assumed */
  if ( lo == 0 && hi == 0 ) {
    hi = nsamp - 1;
  }

  /* Allocate memory for data */
  npts = hi - lo + 1;
  indata = smf_malloc( npts, sizeof(double), 1, status );
  if ( indata == NULL ) {
    errRep( FUNC_NAME, "Unable to allocate memory for statistics array", status );
    return;
  }

  /* Set range of data. Use <= because the range is inclusive. */
  indata = (data->pntr)[0];
  if ( strncmp( mode, "b", 1 ) == 0 ) {
    /* Pick out a bolometer time series */
    for ( k=lo; k<=hi; k++) {
      statsdata[k] = indata[index + k*nbol];
    }
  } else {
    /* Pick out a range of bolomters from a timeslice */
    for ( k=lo; k<=hi; k++) {
      statsdata[k] = indata[nbol*index + k];
    }
  }

  /* Calculate stats */
  mean = gsl_stats_mean( statsdata, 1, npts );

  sigma = gsl_stats_sd( statsdata, 1, npts );

}
