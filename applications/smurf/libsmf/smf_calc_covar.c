/*
*+
*  Name:
*     smf_calc_covar

*  Purpose:
*     Low-level routine to compute covariance using GSL

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     covar = smf_calc_covar ( const smfData *data, dim_t i, dim_t j,
*                              dim_t lo, dim_t hi, int *status )

*  Arguments:
*     data = const smfData* (Given)
*        Pointer to input data struct
*     i = dim_t (Given)
*        Index of bolometer 1
*     j = dim_t (Given)
*        Index of bolometer 2
*     lo = dim_t (Given)
*        Lower index bound into array
*     hi = dim_t (Given)
*        Upper index bound into array
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns the covariance of two time streams, using
*     the GSL routine gsl_stats_covariance. The bolometer indices are
*     given along with the range of values to include in the
*     samples. Note that the range lo to hi is INCLUSIVE. If both lo
*     and hi are zero then the entire range is used. On error a value
*     of VAL__BADD is returned.

*  Notes:

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-17 (AGG):
*        Initial test version
*     2006-05-26 (AGG):
*        Free allocated resources
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <stdio.h>

/* GSL includes */
#include "gsl/gsl_statistics_double.h"

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
#define FUNC_NAME "smf_calc_covar"

double smf_calc_covar ( const smfData *data, dim_t i, dim_t j,
			dim_t lo, dim_t hi, int *status) {

  /* Local variables */
  double *indata = NULL;      /* Pointer to input data array */
  dim_t k;                    /* Loop counter */
  dim_t nframes;              /* Max number of data points*/
  dim_t npts;                 /* Number of points in timeseries */
  dim_t nbol;                 /* Number of bolometers */
  double *idata = NULL;       /* Pointer to array for bolometer 1 data */
  double *jdata = NULL;       /* Pointer to array for bolometer 2 data */
  dim_t temp;                 /* Temporary variable */
  double covar = VAL__BADD;   /* Covariance, initialuzed to bad */

  /* Check status */
  if (*status != SAI__OK) return covar;

  /* Should check data type for double */
  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status)) return covar;

  /* Do we have 2-D image or 3-D timeseries data? */
  smf_get_dims( data,  NULL, NULL, &nbol, &nframes, NULL, NULL, NULL, status);
  if (*status != SAI__OK) return covar;

  /* Check bolometer indices are in range */
  if ( i > nframes ) {
    if ( *status == SAI__OK) {
      msgSetk("I", i);
      msgSetk("N", nframes);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested bolometer index, ^I, is out of range (0 < i < ^N).", status);
      return covar;
    }
  }
  if ( j > nframes ) {
    if ( *status == SAI__OK) {
      msgSetk("I", j);
      msgSetk("N", nframes);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested bolometer index, ^I, is out of range (0 < i < ^N).", status);
      return covar;
    }
  }

  /* Check requested range is valid */
  if ( lo > nframes ) {
    if ( *status == SAI__OK) {
      msgSetk("J", lo);
      msgSetk("N", nframes);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested sample, ^J, is out of range (0 < lo < ^N).", status);
      return covar;
    }
  }
  if ( hi > nframes ) {
    if ( *status == SAI__OK) {
      msgSetk("J", hi);
      msgSetk("N", nframes);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Requested sample, ^J, is out of range (0 < hi < ^N).", status);
      return covar;
    }
  }

  /* Check hi is larger than lo; swap if not */
  if ( lo > hi ) {
    temp = lo;
    lo = hi;
    hi = temp;
    msgOutif(MSG__VERB," ", "Oops - lo > hi. Swapping them round.", status);
  }

  /* If lo and hi are both zero then the whole range is assumed */
  if ( lo == 0 && hi == 0 ) {
    hi = nframes - 1;
  }

  /* Allocate memory for data */
  npts = hi - lo + 1;
  idata = astCalloc( npts, sizeof(*idata) );
  if ( idata == NULL ) {
    msgSetk("N",i);
    errRep( FUNC_NAME, "Unable to allocate memory for bolometer ^N timeseries", status );
    return covar;
  }
  jdata = astCalloc( npts, sizeof(*jdata) );
  if ( jdata == NULL ) {
    msgSetk("N",j);
    errRep( FUNC_NAME, "Unable to allocate memory for bolometer ^N timeseries", status );
    return covar;
  }

  /* Set range of data. Use <= because the range is inclusive. */
  indata = (data->pntr)[0];
  /* Pick out a bolometer time series */
  for ( k=lo; k<=hi; k++) {
    idata[k] = indata[i + k*nbol];
    jdata[k] = indata[j + k*nbol];
  }

  /* Calculate stats */
  covar = gsl_stats_covariance( idata, 1, jdata, 1, npts );

  /* Free resources */
  if ( idata != NULL)
    idata = astFree( idata );
  if ( jdata != NULL)
    jdata = astFree( jdata );

  return covar;

}
