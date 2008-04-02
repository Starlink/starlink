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
*     smf_flag_spikes( smfData *data, unsigned char *quality, 
*                      double thresh, unsigned int niter, 
*                      unsigned int maxiter, unsigned int *aiter, 
*                      int *status )

*  Arguments:
*     data = smfData * (Given and Returned)
*        The data that will be flagged
*     quality = unsigned char * (Given and Returned)
*        If set, use this buffer instead of QUALITY associated with data.
*        If NULL, use the QUALITY associated with data. Locations of spikes
*        will have bit SMF__Q_SPIKE set. 
*     thresh = double (Given)
*        N-sigma threshold for spike detection
*     niter = int (Given)
*        Number of iterations. If set to 0 iterate until the list of
*        flagged sources doesn't change (converges).
*     maxiter = int (Given)
*        If niter=0 maxiter is a maximum number of iterations
*     aiter = int (Returned)
*        The actual number of iterations executed. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Measures the rms and mean of each bolometer, excluding data that
*     has been flagged SMF__Q_BADS | SMF__Q_BADB |
*     SMF__Q_SPIKE. Samples that lie a factor of thresh * rms away
*     from the mean are flagged SMF__Q_SPIKE. This process can be run
*     a fixed number of times, or until the list of flagged samples
*     converges (niter=0).

*  Notes:
*     This routine asserts bolo-ordered data.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-04-02 (EC):
*        Initial Version

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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

void smf_flag_spikes( smfData *data, unsigned char *quality, double thresh, 
		      unsigned int niter, unsigned int maxiter, 
		      unsigned int *aiter, int *status ) {

  /* Local Variables */
  dim_t base;                   /* Base index for bolometer */
  double *dat=NULL;             /* Pointer to bolo data */
  double dist;                  /* Size of significant excursion */
  int done=0;                   /* Loop exit flag */
  dim_t i;                      /* Loop Counter */
  int iter=0;                   /* Actual number of iterations */
  dim_t j;                      /* Loop Counter */
  unsigned char *qua=NULL;      /* Pointer to quality flags */
  unsigned char mask;           /* bitmask for quality */
  double mean;                  /* Bolometer signal mean */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ngood;                  /* How many good samples in measurement */
  dim_t ntslice;                /* Number of time slices */
  double sigma;                 /* Bolometer signal mean */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Assert bolo-ordered data to make life easier */
  smf_dataOrder( data, 0, status );

  /* Pointers to data and quality */
  dat = data->pntr[0];
  if( quality ) {
    qua = quality;
  } else {
    qua = data->pntr[2];
  }

  if( !qua ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "No valid QUALITY array was provided", status );
    return;
  }

  if( !dat ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData does not contain a DATA component", status );
    return;
  }

  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, 
	   "Data is not double-precision", status);
    return;
  }

  if( *status == SAI__OK ) {
    /* obtain data dimensions */
    ntslice = (data->dims)[0];
    nbolo = (data->dims)[1]*(data->dims)[2];

    /* Valid threshold check */
    if( thresh <= 0 ) {
      *status = SAI__ERROR;
      msgSetd("THRESH",thresh);
      errRep(FUNC_NAME, "Can't find spikes: thresh=^THRESH, must be > 0",
	     status);
    }

    /* Valid maxiter */
    if( iter == 0 ) if( maxiter < 0 ) {
      *status = SAI__ERROR;
      msgSetd("MAXITER",maxiter);
      errRep(FUNC_NAME, "Can't find spikes: maxiter=^MAXITER must be >= 0",
	     status);
    }
  }

  /* Define the QUALITY bit mask for samples to ignore */
  mask = SMF__Q_BADS | SMF__Q_BADB | SMF__Q_SPIKE;

  /* Iteratively find spikes */
  while( !done ) {

    done = 1;
    iter++;

    /* Loop over bolometer */

    for( i=0; i<nbolo; i++ ) if( !(qua[i*ntslice] & SMF__Q_BADB) ) {
      
      base = i*ntslice;

      /* Calculate mean and rms of the bolometer */
      smf_simple_stats( &dat[base], 0, ntslice, qua, mask, &mean,
			&sigma, &ngood, status );

      if( *status == SMF__INSMP ) {
	/* Insufficient samples for this bolometer. Annul the error. */
	errAnnul( status );
      }

      /* Only continue with good status and positive sigma */
      if( (*status == SAI__OK) && (sigma > 0) ) {
	/* Size of excursion for significant event */
	dist = thresh*sigma;
	
	/* Loop over all samples and flag outliers */
	for( j=0; j<ntslice; j++ ) if( !(qua[base+j] & mask) ) {

	  /* Significant excursion */
	  if( fabs(dat[base+j]-mean) > dist ) {
	    
	    /* If it wasn't flagged before flag it now and set not done */
	    if( !(qua[base+j] & SMF__Q_SPIKE) ) {
	      qua[base+j] |= SMF__Q_SPIKE;
	      done = 0;
	    }
	  }
	}
      }
    }

    /* Check that the exit condition has been met */
    if( (niter && (iter >= niter)) || (*status != SAI__OK) ) {
      done = 1;
    }
  }

  /* Return actual iterations if requested */
  if( aiter ) {
    *aiter = iter;
  }
}
