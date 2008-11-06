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
*                      unsigned char mask, double thresh, size_t niter, 
*                      size_t maxiter, size_t *aiter, size_t *nflagged,
*                      int *status )

*  Arguments:
*     data = smfData * (Given and Returned)
*        The data that will be flagged
*     quality = unsigned char * (Given and Returned)
*        If set, use this buffer instead of QUALITY associated with data.
*        If NULL, use the QUALITY associated with data. Locations of spikes
*        will have bit SMF__Q_SPIKE set. 
*     mask = unsigned char (Given)
*        Define which bits in quality are relevant to ignore data in
*        the calculation.
*     thresh = double (Given)
*        N-sigma threshold for spike detection
*     niter = size_t (Given)
*        Number of iterations. If set to 0 iterate until the list of
*        flagged sources doesn't change (converges).
*     maxiter = size_t (Given)
*        If niter=0 maxiter is a maximum number of iterations
*     aiter = size_t * (Returned)
*        The actual number of iterations executed. May be NULL.
*     nflagged = size_t * (Returned)
*        The number of new samples that were flagged. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Measures the rms and mean of each bolometer, excluding data that
*     has been flagged with bits specified in mask. Samples that lie a
*     factor of thresh * rms away from the mean are flagged
*     SMF__Q_SPIKE. This process can be run a fixed number of times,
*     or until the list of flagged samples converges (niter=0).

*  Notes:
*     This routine asserts bolo-ordered data.

*  Authors:
*     Edward Chapin (UBC)
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

void smf_flag_spikes( smfData *data, unsigned char *quality, 
                      unsigned char mask, double thresh, size_t niter, 
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
  unsigned char *qua=NULL;      /* Pointer to quality flags */
  double mean;                  /* Bolometer signal mean */
  dim_t nbolo=0;                /* Number of bolometers */
  size_t nflag;                 /* Number of samples flagged */
  dim_t ngood;                  /* How many good samples in measurement */
  dim_t ntslice=0;              /* Number of time slices */
  double sig;                   /* Bolometer signal rms */

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
    errRep( "", FUNC_NAME ": No valid QUALITY array was provided", status );
    return;
  }

  if( !dat ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component",status);
    return;
  }

  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep("", FUNC_NAME ": smfData is not double-precision", status);
    return;
  }

  if( *status == SAI__OK ) {
    /* obtain data dimensions */
    smf_get_dims( data, &nbolo, &ntslice, NULL, status );

    /* Valid threshold check */
    if( thresh <= 0 ) {
      *status = SAI__ERROR;
      msgSetd("THRESH",thresh);
      errRep( "", FUNC_NAME ": Can't find spikes: thresh=^THRESH, must be > 0",
	     status);
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

      /* Calculate mean and rms of the bolometer */
      smf_stats1( &dat[base], 0, ntslice, &qua[base], mask, &mean,
                  &sig, &ngood, status );

      if( *status == SMF__INSMP ) {
	/* Insufficient samples for this bolometer. Annul the error. */
	errAnnul( status );
      } else if( (*status == SAI__OK) && (sig > 0) ) {

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
