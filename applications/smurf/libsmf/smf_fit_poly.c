/*
*+
*  Name:
*     smf_fit_poly

*  Purpose:
*     Low-level polynomial sky fitting routine

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_fit_poly( const smfData *data, const int order, int *status ) 

*  Arguments:
*     data = smfData* (Given and Returned)
*        Pointer to input data struct
*     order = int (Given)
*        Order of polynomial fit
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine fits a polynomial of arbitrary order to each
*     bolometer time stream of N timeslices. Execution is halted with
*     an error if the polynomial order is greater than N-1. The GSL
*     library is used to carry out the fit. The fitted polynomial
*     coefficients are stored in the poly pointer in the supplied
*     smfData.

*  Notes: 
*     No sigma-clipping is carried out to refine the fit. This
*     accounts for any differences between this method and
*     sc2math_fitsky (for order = 1).

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-15 (AGG):
*        Initial test version
*     2006-05-15 (AGG):
*        Add check for non-NULL poly pointer
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
#include <gsl/gsl_multifit.h>

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

/* SC2DA includes */
#include "sc2da/sc2math.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_fit_poly"

void smf_fit_poly( const smfData *data, const int order, double *poly, int *status) {

  /* Local variables */
  double chisq;            /* Chi-squared from the linear regression fit */
  dim_t i;                 /* Loop counter */
  dim_t j;                 /* Loop counter */
  double *indata;          /* Pointer to data array */
  dim_t k;                 /* Loop counter */
  size_t nframes = 0;      /* Number of frames */
  size_t nbol;             /* Number of bolometers */
  double xik;              /* */
  gsl_matrix *X;           /* Matrix of input positions */
  gsl_vector *psky;        /* Vector containing sky brightness */
  gsl_vector *weight;      /* Weights for sky brightness vector */
  gsl_vector *coeffs;      /* Solution vector */
  gsl_matrix *mcov;        /* Covariance matrix */
  gsl_multifit_linear_workspace *work; /* Workspace */
  size_t ncoeff = 2;       /* Number of coefficients to fit for; default straight line */

  /* Check status */
  if (*status != SAI__OK) return;

  if ( smf_history_check( data, FUNC_NAME, status) ) {
    msgSetc("F", FUNC_NAME);
    msgOutif( MSG__VERB, FUNC_NAME, 
	      "^F has already been run on these data, returning to caller", status);
    return;
  }

  /* Do we have 2-D image or 3-D timeseries data? */
  if (data->ndims == 3 ) {
    nframes = (data->dims)[2];
  } else {
    /* Abort with an error if the number of dimensions is not  3 */
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti("ND", data->ndims);
      errRep(FUNC_NAME,
	     "Number of dimensions of input file is ^ND: should be either 3",
	     status);
    }
  }

  /* Should check data type for double */
  smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status);
  if ( *status != SAI__OK) return;

  /* Check that poly is not a NULL pointer */
  if ( poly == NULL ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Input pointer for storing coefficients is NULL. Possible programming error.", status);
      return;
    }
  }

  /* Assign pointer to input data array */
  /* of course, check status on return... */
  indata = (data->pntr)[0]; 
  nbol = (data->dims)[0] * (data->dims)[1]; 

  /* Return with error if order is greater than the number of data
     points */
  if ( order >= nframes ) {
    if ( *status == SAI__OK) {
      msgSeti("O",order);
      msgSeti("NF",nframes);
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Requested polynomial order, ^O, greater than or equal to the number of points, ^NF. Unable to fit polynomial.", status );
      return;
    }
  }
  /* If order is -ve, something's wrong! */
  if ( order < 0 ) {
    if ( *status == SAI__OK) {
      msgSeti("O",order);
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Polynomial order, ^O, is negative. Unable to fit polynomial", status );
      return;
    }
  }

  ncoeff = order + 1;

  /* Allocate workspace */
  work = gsl_multifit_linear_alloc( nframes, ncoeff );
  X = gsl_matrix_alloc( nframes, ncoeff );
  psky = gsl_vector_alloc( nframes );
  weight = gsl_vector_alloc( nframes );
  coeffs = gsl_vector_alloc( ncoeff );
  mcov = gsl_matrix_alloc( ncoeff, ncoeff );

  /* Loop over bolometers */
  for ( j=0; j<nbol; j++) {

    /* Fill the matrix, vectors and weights arrays */
    for ( i=0; i<nframes; i++) {
      /* Matrix elements */
      for ( k=0; k<ncoeff; k++) {
	xik = (double)pow(i,k);
	gsl_matrix_set( X, i, k, xik );
      }
      /* Vector of observations */
      gsl_vector_set( psky, i, indata[j + nbol*i] );
      /* Set weights accordingly */
      if (indata[nbol*i + j] != VAL__BADD) {
	gsl_vector_set( weight, i, 1.0);
      } else {
	gsl_vector_set( weight, i, 0.0);
      }
    }
    /* Carry out fit */
    gsl_multifit_wlinear( X, weight, psky, coeffs, mcov, &chisq, work );

    /* Store coefficients in the polynomial array */
    for ( k=0; k<ncoeff; k++) {
      poly[j + k*nbol] = gsl_vector_get ( coeffs, k );
    }

  }
  /* Free up workspace */
  gsl_multifit_linear_free( work );
  gsl_matrix_free( X );
  gsl_vector_free( psky );
  gsl_vector_free( weight );
  gsl_vector_free( coeffs );
  gsl_matrix_free( mcov );  

}
