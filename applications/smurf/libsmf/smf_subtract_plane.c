/*
*+
*  Name:
*     smf_subtract_plane

*  Purpose:
*     Low-level sky fitting and removal routine

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_subtract_plane( smfData *data, const char *fittype, int *status ) 

*  Arguments:
*     data = smfData* (Given and Returned)
*        Pointer to input data struct
*     fittype = char* (Given)
*        Fit-type for PLANE sky-removal method
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine performs the bulk sky removal task for the case
*     when the PLANE method is specified in smurf_remsky. Three
*     methods of removing the sky are offered: mean, slope and plane
*     (although only the first two are supported as yet). In the first
*     method, the mean sky power is calculated and subtracted from
*     each data point. In the second method, the (assumed linear)
*     gradient in the sky emission is calculated and subtracted. The
*     third method offers a full 2-D plane-fitting procedure to allow
*     for azimuthal variations as well.
*
*     The 1-D fit requires a transformation to the AzEl coordinate
*     system. Initially this was done using astTran2 to transform all
*     the pixels in the subarray. The current method only transforms
*     two points in the subarray and then calculates the angle between
*     the long axis of the subarray and the zenith. A simple geometric
*     transformation is made to the Y pixel values to yield a set of
*     effective elevations which are used in the 1-D fit. The gradient
*     is calculated using the GSL multifit method and subtracted from
*     the data values. For subsequent frames (timeslices), the change
*     in the header variable tcs_az_angle (the angle between focal
*     plane UP and zenith NORTH) is used to calculate the new long
*     axis-zenith angle.

*  Notes: 
*     There is a lot of duplicated code between this routine and
*     smf_correct_extinction as they both work in the AzEl coordinate
*     system

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-02-24 (AGG):
*        Initial test version
*     2006-03-07 (AGG):
*        Use GSL for linear regression
*     2006-03-09 (AGG):
*        Use GSL multifit for both 1-D and 2-D calculations
*     2006-04-07 (AGG):
*        Refactor code to omit AST calls where possible
*     2006-04-12 (AGG):
*        Speedup 1-D fit by calculating the angle between the long
*        axis of the subarray and the zenith, and incrementing it for
*        subsequent timesteps
*     2006-04-21 (AGG):
*        Add history check, and update history if routine successful
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
#define FUNC_NAME "smf_subtract_plane"

void smf_subtract_plane(smfData *data, const char *fittype, int *status) {

  /* Local variables */
  smfHead *hdr;            /* Pointer to full header struct */
  dim_t i;                 /* Loop counter */
  dim_t j;                 /* Loop counter */
  const char *origsystem = '\0';  /* Character string to store the coordinate
			      system on entry */
  AstFrameSet *wcs = NULL; /* Pointer to AST WCS frameset */
  double *indata;          /* Pointer to data array */
  double *vardata;         /* Pointer to variance array */
  dim_t index;             /* index into vectorized data array */
  dim_t k;                 /* Loop counter */
  double *xin = NULL;      /* X coordinates of input mapping */
  double *xout = NULL;     /* X coordinates of output */
  double *yin = NULL;      /* Y coordinates of input */
  double *yout = NULL;     /* Y coordinates of output */
  size_t *indices;

  size_t nframes = 0;      /* Number of frames */
  size_t npts;             /* Number of data points */
  size_t base;             /* Starting point for index into arrays */
  int z;                   /* Counter */
  double sky;              /* Sky power to be subtracted */
  double sky0;             /* Sky power fit - intercept */
  double dskyel;           /* Sky power fit - elev gradient */
  double dskyaz;           /* Sky power fit - azimuth gradient */
  double chisq;            /* Chi-squared from the linear regression fit */

  gsl_matrix *azel;        /* Matrix of input positions */
  gsl_vector *psky;        /* Vector containing sky brightness */
  gsl_vector *weight;      /* Weights for sky brightness vector */
  gsl_vector *skyfit;      /* Solution vector */
  gsl_matrix *mcov;        /* Covariance matrix */
  gsl_multifit_linear_workspace *work; /* Workspace */
  size_t ncoeff = 2;       /* Number of coefficients to fit for; default straight line */

  size_t needast = 0;      /* Flag to specify if astrometry is needed for fit */
  size_t fitmean = 0;      /* Flag to specify if the fit type is mean */
  size_t fitslope = 0;     /* Flag to specify if the fit is a 1-D elev slope */
  size_t fitplane = 0;     /* Flag to specify if the fit is a 2-D plane */

  double *x0 = NULL;       /* Pixel coordinates of x points in subarray */
  double *y0 = NULL;       /* Pixel coordinates of y points in subarray */
  double *ynew = NULL;     /* Transformed y coordinates */
  double a[2];             /* Coordinates for point A */
  double b[2];             /* Coordinates for point B */
  double c[2];             /* Coordinates for point C (zenith) */
  double alpha = 0;        /* Angle ABC (radians) */
  double dalpha;           /* Change in focal plane angle (radians) */

  /* Check status */
  if (*status != SAI__OK) return;

  if ( smf_history_check( data, FUNC_NAME, status) ) {
    msgSetc("F", FUNC_NAME);
    msgOutif( MSG__VERB, FUNC_NAME, 
	      "^F has already been run on these data, returning to caller", status);
    return;
  }

  /* Set some flags depending on desired FIT type */
  if ( strncmp( fittype, "MEAN", 4 ) == 0 ) {
    needast = 0;
    fitmean = 1;
    ncoeff = 1; /* Not needed :-) */
  } else if ( strncmp( fittype, "SLOP", 4 ) == 0 )  {
    needast = 1;
    fitslope = 1;
    ncoeff = 2;
  } else  if ( strncmp( fittype, "PLAN", 4 ) == 0 ) {
    needast = 0;
    fitplane = 1;
    ncoeff = 3;
  } else {
    *status = SAI__ERROR;
    msgSetc("F", fittype);
    errRep(FUNC_NAME, "Unknown FIT type, ^F: programming error?", status);
   }

  /* Do we have 2-D image or 3-D timeseries data? */
  if (data->ndims == 2) {
    nframes = 1;
  } else if (data->ndims == 3 ) {
    nframes = (data->dims)[2];
  } else {
    /* Abort with an error if the number of dimensions is not 2 or 3 */
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti("ND", data->ndims);
      errRep(FUNC_NAME,
	     "Number of dimensions of input file is ^ND: should be either 2 or 3",
	     status);
    }
  }

  /* Should check data type for double */
  smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status);
  if ( *status != SAI__OK) return;

  /* Assign pointer to input data array */
  /* of course, check status on return... */
  indata = (data->pntr)[0]; 
  vardata = (data->pntr)[1];
  npts = (data->dims)[0] * (data->dims)[1];

  /* It is more efficient to call astTran2 with all the points
     rather than one point at a time */
  xin = smf_malloc( npts, sizeof(double), 0, status );
  yin = smf_malloc( npts, sizeof(double), 0, status );
  if ( needast ) {
    /*    xout = smf_malloc( npts, sizeof(double), 0, status );
	  yout = smf_malloc( npts, sizeof(double), 0, status );*/
    xout = smf_malloc( 2, sizeof(double), 0, status );
    yout = smf_malloc( 2, sizeof(double), 0, status );
    x0 = smf_malloc( 2, sizeof(double), 0, status );
    y0 = smf_malloc( 2, sizeof(double), 0, status );
    ynew = smf_malloc( npts, sizeof(double), 0, status );
  }
  indices = smf_malloc( npts, sizeof(size_t), 0, status );

  /* Jump to the cleanup section if status is bad by this point
     since we need to free memory */
  if (*status != SAI__OK) goto CLEANUP;

  /* Prefill with coordinates */
  z = 0;
  for (j = 0; j < (data->dims)[1]; j++) {
    base = j *(data->dims)[0];
    for (i = 0; i < (data->dims)[0]; i++) {
      xin[z] = (double)i + 1.0;
      yin[z] = (double)j + 1.0;
      indices[z] = base + i; /* index into data array */
      z++;
    }
  }

  /* Loop over timeslice index */
  for ( k=0; k<nframes; k++) {
    /* Call tslice_ast to update the header for the particular
       timeslice */
    smf_tslice_ast( data, k, 1, status ); /* We're never in quick mode here */

    /* If we need the astrometry... */
    if ( needast ) {
      /* Retrieve header info */
      hdr = data->hdr;
      /* Set coordinate frame to AzEl */
      wcs = hdr->wcs;
      /* Check current frame and store it */
      origsystem = astGetC( wcs, "SYSTEM");
      /* Select the AZEL system : what if wcs == NULL? */
      if (wcs != NULL) 
	astSetC( wcs, "SYSTEM", "AZEL" );
      if (!astOK) {
	if (*status == SAI__OK) {
	  *status = SAI__ERROR;
	  errRep( FUNC_NAME, "Error from AST", status);
	}
      }
      /* Transfrom from pixels to AZEL */
      /*      astTran2(wcs, npts, xin, yin, 1, xout, yout );*/
      if (k == 0) {
	x0[0] = xin[0];
	x0[1] = xin[1];
	y0[0] = yin[0];
	y0[1] = yin[1];

	astTran2(wcs, 2, x0, y0, 1, xout, yout );
	a[0] = xout[0];
	a[1] = yout[0];
	b[0] = xout[1];
	b[1] = yout[1];
	c[0] = xout[1];
	c[1] = M_PI_2;
	alpha = astAngle( wcs, a, b, c);

	/*	printf("alpha = %g\n",alpha);*/
      }
      dalpha = hdr->sc2head->tcs_az_ang - alpha;
      alpha += dalpha;
      for (i=0; i< npts; i++) {
	ynew[i] = yin[i] * cos( alpha );
      }
    }

    /* Offset into 3d data array */
    base = npts * k; 
    /* Check fit type */
    if ( fitmean ) {
      /* Calculate average of all pixels in current timeslice */
      sky0 = 0;
      for (i=0; i < npts; i++ ) {
	index = indices[i] + base;
	if (indata[index] != VAL__BADD) {
	  sky0 += indata[index];
	}
      }
      sky0 /= npts;
      dskyaz = 0.0;
      dskyel = 0.0;
    } else {
      /* Allocate workspace */
      work = gsl_multifit_linear_alloc( npts, ncoeff );
      azel = gsl_matrix_alloc( npts, ncoeff );
      psky = gsl_vector_alloc( npts );
      weight = gsl_vector_alloc( npts );
      skyfit = gsl_vector_alloc( ncoeff );
      mcov = gsl_matrix_alloc( ncoeff, ncoeff );

      /* Fill the matrix, vectors and weights arrays */
      for ( i=0; i<npts; i++) {
	index = indices[i] + base;
	gsl_matrix_set( azel, i, 0, 1.0 );
	if ( fitslope ) {
	  /*	  gsl_matrix_set( azel, i, 1, yout[indices[i]] );*/
	  gsl_matrix_set( azel, i, 1, ynew[indices[i]] );
	} else  if ( fitplane ) {
	  gsl_matrix_set( azel, i, 1, yin[indices[i]] );
	  gsl_matrix_set( azel, i, 2, xin[indices[i]] );
	}
	gsl_vector_set( psky, i, indata[index] );
	/* Set weights accordingly */
	if (indata[index] != VAL__BADD) {
	  gsl_vector_set( weight, i, 1.0);
	} else {
	  gsl_vector_set( weight, i, 0.0);
	}
      }
      /* Carry out fit */
      gsl_multifit_wlinear( azel, weight, psky, skyfit, mcov, &chisq, work);

      /* Retrieve solution */
      sky0 = gsl_vector_get(skyfit, 0);
      dskyel = gsl_vector_get(skyfit, 1);
      if ( ncoeff == 3 ) {
	dskyaz = gsl_vector_get(skyfit, 2);
      } else {
	dskyaz = 0.0;
      }

      /* Free up workspace */
      gsl_multifit_linear_free( work );
      gsl_matrix_free( azel );
      gsl_vector_free( psky );
      gsl_vector_free( weight );
      gsl_vector_free( skyfit );
      gsl_matrix_free( mcov );

    }

    /* Subtract fit from timeslice */
    for (i=0; i < npts; i++ ) {
      index = indices[i] + base;
      if (indata[index] != VAL__BADD) {

	/* Calculate sky value as a function of position */
	if (fitslope) {
	  /*	  sky = sky0 + dskyel * yout[indices[i]] + dskyaz * xout[indices[i]];*/
	  sky = sky0 + dskyel * ynew[indices[i]];
	} else {
	  sky = sky0 + dskyel * yin[indices[i]] + dskyaz * xin[indices[i]];
	}
	/* Subtract sky value; no need to update variance */
	indata[index] -= sky;
      }
    }

    /* Reset coordinate frame to that on entry if necessary */
    if (needast) {
      if ( *status == SAI__OK) {
	astSetC( wcs, "SYSTEM", origsystem );
	/* Check AST status */
    	if (!astOK) {
	  if (*status == SAI__OK) {
	    *status = SAI__ERROR;
	    errRep( FUNC_NAME, "Error from AST", status);
	  }
	}
      }
    }

  } /* End of loop over timeslice frame */

  /* Write history entry */
  if ( *status == SAI__OK ) {
    smf_history_add( data, FUNC_NAME, 
		       "Plane sky subtraction successful", status);
  } else {
    errRep(FUNC_NAME, "Error: status set bad. Possible programming error.", 
	   status);
  }

  CLEANUP:
  smf_free(xin,status);
  smf_free(yin,status);
  if ( needast ) {
    smf_free(xout,status);
    smf_free(yout,status);
    smf_free(x0,status);
    smf_free(y0,status);
    smf_free(ynew,status);
  }
  smf_free(indices,status);

}
