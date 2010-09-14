/*
*+
*  Name:
*     smf_flag_spikes

*  Purpose:
*     Flag n-sigma excursions (spikes) from the mean of a data stream

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_flag_spikes( smfWorkForce *wf, smfData *data, double *bolovar,
*                      smf_qual_t mask, double thresh, size_t niter,
*                      size_t maxiter, size_t *aiter, size_t *nflagged,
*                      int *status )

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads
*     data = smfData * (Given and Returned)
*        The data that will be flagged. Locations of spikes
*        will have bit SMF__Q_SPIKE set.
*     bolvar = double * (Given)
*        If supplied, an array of externally supplied estimates of bolo
*        variance. Can be NULL in which case the variance is measured.
*     mask = smf_qual_t (Given)
*        Define which bits in quality are relevant to ignore data in
*        the calculation.
*     thresh = double (Given)
*        N-sigma threshold for spike detection
*     niter = size_t (Given)
*        Number of iterations. If set to 0 iterate until the list of
*        flagged sources doesn't change (converges). If a value greather
*        than 10000 is supplied, then smf_flag_spikes2 will be invoked to
*        flag the spikes, with the box size being set to "niter - 10000".
*        See smf_flag_spikes2 for further details.
*     maxiter = size_t (Given)
*        If niter=0 maxiter is a maximum number of iterations.
*     aiter = size_t * (Returned)
*        The actual number of iterations executed. May be NULL.
*     nflagged = size_t * (Returned)
*        The number of new samples that were flagged. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Flags spikes using one of teo different algorithms, as specified by
*     the "niter" argument.
*
*     If niter is not equal to 10000, measures the rms and mean of each
*     bolometer, excluding data that has been flagged with bits specified
*     in mask. Samples that lie a factor of thresh * rms away from the mean
*     are flagged SMF__Q_SPIKE. This process can be run a fixed number of
*     times, or until the list of flagged samples converges (niter=0).
*
*     If niter is greater than 10000, smf_flag_spikes2 is called to flag
*     the spikes, using the value of "niter-10000" as the box size. See
*     smf_flag_spikes2 for details of the algorithm.

*  Notes:
*     This routine asserts bolo-ordered data.

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-04-02 (EC):
*        Initial Version
*     2008-04-14 (EC):
*        -Added mask to interface
*        -fixed array index bug
*     2008-04-18 (EC):
*        - Return nflagged
*        - use size_t in interface
*     2009-11-25 (TIMJ):
*        Tidy up status handling a bit.
*     2010-01-14 (DSB):
*        Call smf_flag_spikes2 if "niter" is 10000.
*     2010-01-19 (DSB):
*        Do not use "maxiter" when invoking smf_flag_spikes2 as it cannot
*        be specified in the config file.

*  Copyright:
*     Copyright (C) 2008 Univeristy of British Columbia.
*     Copyright (C) 2010 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */
#include <math.h>

#define FUNC_NAME "smf_flag_spikes"

void smf_flag_spikes( smfWorkForce *wf, smfData *data, double *bolovar,
                      smf_qual_t mask, double thresh, size_t niter,
                      size_t maxiter, size_t *aiter, size_t *nflagged,
                      int *status ) {

  /* Local Variables */
  dim_t base;                   /* Base index for bolometer */
  double *dat=NULL;             /* Pointer to bolo data */
  double dist;                  /* Size of significant excursion */
  int done=0;                   /* Loop exit flag */
  dim_t i;                      /* Loop Counter */
  size_t iter=0;                /* Actual number of iterations */
  dim_t j;                      /* Loop Counter */
  smf_qual_t *qua=NULL;      /* Pointer to quality flags */
  double mean;                  /* Bolometer signal mean */
  dim_t nbolo=0;                /* Number of bolometers */
  size_t nflag;                 /* Number of samples flagged */
  size_t ngood;                 /* How many good samples in measurement */
  dim_t ntslice=0;              /* Number of time slices */
  double sig;                   /* Bolometer signal rms */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* If requested, use the alternative spike flagger. */
  if( niter > 10000 ) {
     smf_flag_spikes2( wf, data, mask, thresh, niter-10000, nflagged,
                       status );
     /* Set aiter to something since smf_flag_spikes2 doesn't */
     if( aiter ) {
       *aiter = 0;
     }
     return;
  }

  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status )) return;

  /* Assert bolo-ordered data to make life easier */
  smf_dataOrder( data, 0, status );
  if (*status != SAI__OK) return;

  /* Pointers to data and quality */
  dat = data->pntr[0];
  qua = smf_select_qualpntr( data, NULL, status );

  if( !qua ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": No valid QUALITY array was provided", status );
    return;
  }

  if( !dat ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component",status);
    return;
  }

  if( *status == SAI__OK ) {
    /* obtain data dimensions */
    smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, NULL, NULL,
                  status );

    /* Valid threshold check */
    if( thresh <= 0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetd("THRESH",thresh);
      errRep( "", FUNC_NAME ": Can't find spikes: thresh=^THRESH, must be > 0",
	     status);
      return;
    }
  }

  /* Iteratively find spikes */
  nflag = 0;
  while( !done ) {

    done = 1;
    iter++;

    /* Loop over bolometer */
    for( i=0; i<nbolo; i++ ) if( !(qua[i*ntslice] & SMF__Q_BADB) ) {

      base = i*ntslice;

      if( bolovar ) {
        /* User-supplied variance */
        if (bolovar[i] == VAL__BADD) {
          if (*status == SAI__OK) {
            *status = SAI__ERROR;
            errRepf( "", FUNC_NAME ": error, supplied variance for bolometer %d is the bad value", status, (int)i );
          }
        } else if( bolovar[i] > 0 ) {
          sig = sqrt(bolovar[i]);
        } else {
          if (*status == SAI__OK) {
            *status = SAI__ERROR;
            errRepf( "", FUNC_NAME ": error, supplied variance of %g for bolometer %d is not positive",
                     status, bolovar[i], (int)i );
          }
        }
      } else {
        /* Calculate mean and rms of the bolometer */
        if (*status == SAI__OK) {
          smf_stats1D( dat+base, 1, ntslice, qua+base, 0, mask, &mean,
                       &sig, &ngood, status );

          if( *status == SMF__INSMP ) {
            /* Insufficient samples for this bolometer. Annul the error. */
            errAnnul( status );
            sig = 0;
          }
        }
      }

      if( (*status == SAI__OK) && (sig > 0) ) {
	/* Size of excursion for significant event */
	dist = thresh*sig;

	/* Loop over all samples and flag outliers */
	for( j=0; j<ntslice; j++ ) if( !(qua[base+j] & mask) ) {

	  /* Significant excursion */
	  if( fabs(dat[base+j]-mean) > dist ) {

	    /* If it wasn't flagged before set not done */
	    if( !(qua[base+j] & SMF__Q_SPIKE) ) {
	      done = 0;
	      nflag++;
	    }

	    /* Flag the data point */
	    qua[base+j] |= SMF__Q_SPIKE;

	  }
	}
      }
    }

    /* Check that the exit condition has been met */
    if( (niter && (iter >= niter)) || (maxiter && (iter > maxiter)) ||
                                       (*status != SAI__OK) ) {
      done = 1;
    }
  }

  /* Return stuff if requested */
  if( aiter ) {
    *aiter = iter;
  }

  if( nflagged ) {
    *nflagged = nflag;
  }

}
