/*
*+
*  Name:
*     smf_fit_poly

*  Purpose:
*     Low-level polynomial fitting routine for bolometer data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_fit_poly( ThrWorkForce *wf, smfData *data, const int order,
*                   int remove, double *poly, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     data = smfData* (Given and Returned)
*        Pointer to input data struct
*     order = int (Given)
*        Order of polynomial fit
*     remove = int (Given)
*        If set, remove the fitted polynomial for the data
*     poly = double * (Returned)
*        Buffer to store polynomial fit coefficients for all bolos. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine fits a polynomial of arbitrary order to each
*     bolometer time stream of N timeslices. Execution is halted with
*     an error if the polynomial order is greater than N-1. Fitting
*     itself is done with smf_fit_poly1d. If remove is set, the polynomial
*     will be evaluated and removed from the input data. In this case
*     poly may also be set to NULL in which case the coefficients are
*     only stored in a temporary buffer long enough to remove the fit.

*  Notes:
*     This routine will fail if there is no associated QUALITY component.
*     No sigma-clipping is carried out to refine the fit. This
*     accounts for any differences between this method and
*     sc2math_fitsky (for order = 1).

*  Authors:
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-05-15 (AGG):
*        Initial test version
*     2006-05-15 (AGG):
*        Add check for non-NULL poly pointer
*     2008-03-17 (EC):
*        - Use QUALITY in addition to VAL__BADD to ignore bad data
*        - handle both time- and bolo-ordered data
*     2008-04-03 (EC):
*        - Added quality to interface
*     2008-12-03 (TIMJ):
*        Use smf_get_dims
*     2010-09-15 (EC):
*        Switch to using smf_fit_poly1d for fitting each bolo
*     2010-09-16 (EC):
*        -Add remove flag to remove fitted polynomials
*        -Parallelize over blocks of bolometers
*     2010-10-19 (DSB):
*        Free job_data before returning.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     Copyright (C) 2006-2008,2010 University of British Columbia.
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

/* Standard includes */
#include <stdio.h>
#include <stdint.h>
#include <string.h>

/* GSL includes */
#include "gsl/gsl_multifit.h"

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

/* ------------------------------------------------------------------------ */
/* Local variables and functions */

/* Structure containing information about blocks of bolos that each
   thread will process */
typedef struct {
  dim_t b1;               /* Index of first bolometer of block */
  dim_t b2;               /* Index of last bolometer of block */
  dim_t bstride;          /* bolometer stride for res/qua */
  double *indata;         /* pointer to the bolometer data */
  int ijob;               /* Job identifier */
  int isTordered;         /* how are the raw data ordered */
  dim_t nbolo;            /* number of bolometers */
  dim_t ntslice;          /* number of time slices */
  dim_t order;            /* order of polynomial */
  double *poly;           /* pointer to buffer to store poly coeffs */
  smf_qual_t *qual;       /* pointer to the quality array */
  int remove;             /* set if removing poly fit from data */
  dim_t tstride;          /* time stride for res/qua */
} smfFitPolyData;

/* Function to be executed in thread: fit polynomials to blocks of bolos */

static void smfFitPolyPar( void *job_data_ptr, int *status );

static void smfFitPolyPar( void *job_data_ptr, int *status ) {
  dim_t bstride;             /* bolo strides */
  double *curbolo=NULL;       /* pointer to current bolo data */
  double *curpoly=NULL;       /* pointer to poly coeffs fit to curbolo */
  double *curpolydata=NULL;   /* evaluated poly for curbolo */
  smf_qual_t *curqual=NULL;   /* pointer to current bolo quality */
  dim_t i;                   /* Loop counter */
  double *indata=NULL;        /* Pointer to data array */
  dim_t j;                   /* Loop counter */
  dim_t k;                   /* Loop counter */
  dim_t nbolo;                /* Number of bolometers */
  dim_t ncoeff;              /* Number of poly coeffs */
  dim_t ntslice;              /* Number of time slices */
  int64_t nused;              /* Number of samples used in fit */
  smfFitPolyData *pdata=NULL; /* Pointer to job data */
  double *poly=NULL;          /* Pointer external poly coeff storage buffer */
  smf_qual_t *qual=NULL;      /* Pointer to QUALITY component */
  dim_t tstride;             /* time strides */

  /* Retrieve job data */
  pdata = job_data_ptr;
  bstride = pdata->bstride;
  indata = pdata->indata;
  nbolo = pdata->nbolo;
  ntslice = pdata->ntslice;
  poly = pdata->poly;
  qual = pdata->qual;
  tstride = pdata->tstride;

  ncoeff = pdata->order + 1;

  /* Debugging message indicating thread started work */
  msgOutiff( SMF__TIMER_MSG, "",
             "smfFitPolyPar: thread starting on bolos %zu -- %zu",
             status, pdata->b1, pdata->b2 );

  /* If time-ordered need to copy bolometer into contiguous arrays.
     Otherwise we can use the bolometer data in-place. Polynomial
     data per-bolo also needs to be re-ordered so allocate a temp array. */

  if( pdata->isTordered ) {
    curbolo = astMalloc( ntslice*sizeof(*curbolo) );
    curqual = astMalloc( ntslice*sizeof(*curqual) );
  }
  curpoly = astMalloc( ncoeff*sizeof(*curpoly) );

  /* If removing the fit from the data, allocate space for evaluated
     polynomial here */

  if( pdata->remove ) {
    curpolydata = astMalloc( ntslice*sizeof(*curpolydata) );
  }

  /* Loop over bolometers. Only fit this bolometer if it is not
     flagged SMF__Q_BADB */
  for ( j=pdata->b1; (j<=pdata->b2) && (*status==SAI__OK); j++) {
    if( !(qual[j*bstride] & SMF__Q_BADB) ) {

      /* Pointer to current bolometer data */
      if( pdata->isTordered ) {
        for( i=0; i<ntslice; i++ ) {
          curbolo[i] = indata[i*tstride + j*bstride];
          curqual[i] = qual[i*tstride + j*bstride];
        }
      } else {
        curbolo = indata + j*bstride;
        curqual = (smf_qual_t *) qual + j*bstride;
      }

      smf_fit_poly1d( pdata->order, ntslice, 0, 0, NULL, curbolo, NULL, curqual,
                      curpoly,NULL, curpolydata, &nused, status );

      if( *status == SAI__OK ) {
        /* Remove fit from data */
        if( pdata->remove ) {
          for( i=0; i<ntslice; i++ ) {
            if( (indata[i*tstride + j*bstride] != VAL__BADD) &&
                !(qual[j*bstride + i*tstride]&SMF__Q_MOD) ) {
              indata[i*tstride + j*bstride] -= curpolydata[i];
            }
          }
        }

        /* Copy the poly coefficients for this bolometer into poly */
        if( poly ) {
          for ( k=0; k<ncoeff; k++) {
            poly[j + k*nbolo] = curpoly[k];
          }
        }
      }
    }
  }

  /* Free up temp space */
  if( pdata->isTordered ) {
    curbolo = astFree( curbolo );
    curqual = astFree( curqual );
  }
  curpoly = astFree( curpoly );
  curpolydata = astFree( curpolydata );

  /* Debugging message indicating thread finished work */
  msgOutiff( SMF__TIMER_MSG, "",
             "smfFitPolyPar: thread finishing bolos %zu -- %zu",
             status, pdata->b1, pdata->b2 );
}

/* ------------------------------------------------------------------------ */

/* Simple default string for errRep */
#define FUNC_NAME "smf_fit_poly"

void smf_fit_poly( ThrWorkForce *wf, smfData *data, const dim_t order,
                   int remove, double *poly, int *status) {

  /* Local variables */
  dim_t bstride;              /* bolo strides */
  int i;                      /* Loop counter */
  smfFitPolyData *job_data=NULL;/* Array of job data for each thread */
  dim_t nbolo=0;              /* Number of bolometers */
  int njobs=0;                /* Number of jobs to be processed */
  dim_t ntslice = 0;          /* Number of time slices */
  int nw;                     /* Number of worker threads */
  smfFitPolyData *pdata=NULL; /* Pointer to job data */
  smf_qual_t *qual;           /* pointer to the quality array */
  dim_t step;                 /* step size for dividing up work */
  dim_t tstride;              /* time strides */

  /* Check status */
  if (*status != SAI__OK) return;

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Should check data type for double */
  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status)) return;

  if ( smf_history_check( data, FUNC_NAME, status) ) {
    msgSetc("F", FUNC_NAME);
    msgOutif(MSG__VERB," ",
             "^F has already been run on these data, returning to caller",
             status);
    return;
  }

  /* Get the dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                &tstride, status);

  /* Return with error if there is no QUALITY component */
  qual = smf_select_qualpntr( data, NULL, status );

  if( !qual && (*status == SAI__OK) ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Data doesn't have a QUALITY component.", status );
    return;
  }

  /* Return with error if order is greater than the number of data
     points */
  if ( order >= ntslice ) {
    if ( *status == SAI__OK) {
      msgSetk("O",order);
      msgSetk("NF",ntslice);
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Requested polynomial order, ^O, greater than or "
              "equal to the number of points, ^NF. Unable to fit polynomial.",
              status );
    }
    return;
  }

  /* Set up the job data */

  if( nw > (int) nbolo ) {
    step = 1;
  } else {
    step = nbolo/nw;
    if( !step ) {
      step = 1;
    }
  }

  job_data = astCalloc( nw, sizeof(*job_data) );

  for( i=0; (*status==SAI__OK)&&i<nw; i++ ) {
    pdata = job_data + i;

     pdata->b1 = i*step;
     pdata->b2 = (i+1)*step-1;

     /* if b1 is greater than the number of bolometers, we've run out of jobs */
     if( pdata->b1 >= nbolo ) {
       break;
     }

     /* increase the jobs counter */
     njobs++;

     /* Ensure that the last thread picks up any left-over bolometers */
     if( (i==(nw-1)) && (pdata->b1<(nbolo-1)) ) {
       pdata->b2=nbolo-1;
     }

     pdata->ijob = -1;   /* Flag job as ready to start */
     pdata->bstride = bstride;
     pdata->indata = data->pntr[0];
     pdata->isTordered = data->isTordered;
     pdata->nbolo = nbolo;
     pdata->ntslice = ntslice;
     pdata->order = order;
     pdata->poly = poly;
     pdata->qual = qual;
     pdata->remove = remove;
     pdata->tstride = tstride;
   }

  /* Submit jobs to fit polynomial baselines to block of bolos */
  thrBeginJobContext( wf, status );
  for( i=0; (*status==SAI__OK)&&i<njobs; i++ ) {
    pdata = job_data + i;
    pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata,
                               smfFitPolyPar, 0, NULL, status );
  }

  /* Wait until all of the submitted jobs have completed */
  thrWait( wf, status );
  thrEndJobContext( wf, status );

  /* Free local resources. */
  job_data = astFree( job_data );

}
