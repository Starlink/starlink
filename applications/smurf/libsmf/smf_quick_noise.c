/*
*+
*  Name:
*     smf_quick_noise

*  Purpose:
*     Low-level routine for estimating the approximate white-noise level
*     in a given bolometer of a smfData.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_quick_noise( const smfData *data, dim_t bolo, dim_t nsamp, dim_t nchunk,
*                      smf_qual_t mask, int *status )

*  Arguments:
*     data = const smfData* (Given)
*        Pointer to bolo-ordered smfData. If no quality are available we only
*        ignore data with value VAL__BADD.
*     bolo = dim_t (Given)
*        The offset of the bolometer in question (in range [0,nbolo-1])
*     nsamp = dim_t (Given)
*        Length of the interval in which to calculate sample standard deviation
*     nchunk = dim_t (Given)
*        Number of evenly-spaced chunks of time-stream over which to estimate
*        the sample standard deviation
*     mask = smf_qual_t (Given)
*        Use with qual to define which bits in quality are relevant to
*        ignore data in the calculation.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns the approximate white-noise level of a given
*     bolometer by measuring the r.m.s. in a small window at a number of
*     different locations in the time stream. The routine returns the
*     smallest r.m.s. of all the windows that it checks. As long as the
*     window is small enough the r.m.s. is dominated by high-frequency
*     noise rather than slower baseline drift and should give a reasonable
*     relative noise level for the requested detector.

*  Notes:
*     The routine expects bolo-ordered data. If ICD-compliant data is
*     supplied instead, SMF__TORDB status is set and the function returns.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-03-28 (EC):
*        Initial version
*     2008-04-17 (EC):
*        Fixed indexing problem
*     2008-04-18 (EC):
*        Improved range checking on inputs
*     2009-11-17 (EC):
*        Add ability to skip padding/apodization at time stream ends
*     2011-12-02 (DSB):
*        Can now work with bolometer or time ordered data.
*     2013-11-22 (DSB):
*        Avoid divide by zero if nchunk is 1.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 University of British Columbia.
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
#include <math.h>

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
#include "libsmf/smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_quick_noise"


double smf_quick_noise( const smfData *data, dim_t bolo, dim_t nsamp, dim_t nchunk,
			smf_qual_t mask, int *status ) {

  /* Local variables */
  dim_t bstride;               /* Stride between bolometers */
  double *dat=NULL;             /* Pointer to bolo data */
  double goodfrac;              /* Fraction of good samples */
  dim_t i;                      /* Loop counter */
  dim_t istart;                /* Start of the good range */
  dim_t iend;                  /* End of the good range */
  dim_t len;                    /* Length of interval including starts */
  double minsig;                /* Minimum measured r.m.s. */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ngood;                 /* Number of good samples in r.m.s. check */
  dim_t ntslice;                /* Number of time slices */
  const smf_qual_t *qua=NULL;   /* Pointer to quality flags */
  double retval=0;              /* Return value */
  double sig;                   /* r.m.s. of this chunk */
  dim_t tstride;               /* Stride between time slices */

  /* Check status */
  if (*status != SAI__OK) return retval;

  /* Check for double-precision data */
  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status )) return retval;

  /* Check for QUALITY arrays */
  qua = smf_select_cqualpntr( data, NULL, status );

  /* Obtain data dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride, &tstride,
                status );

  /* Check for reasonable bolo/nsamp/nchunk */
  if( bolo >= nbolo ) {
    *status = SAI__ERROR;
    msgSetk("BOLO",bolo);
    msgSetk("NBOLO",nbolo-1);
    errRep(FUNC_NAME, "Invalid bolo: ^BOLO, must be in range [0,^NBOLO]",
	   status);
    return retval;
  }

  if( nsamp < SMF__MINSTATSAMP ) {
    *status = SAI__ERROR;
    msgSeti("MIN",SMF__MINSTATSAMP);
    msgSetk("NSAMP",nsamp);
    errRep(FUNC_NAME,
	   "Invalid nsamp: ^NSAMP, must be > ^MIN",
	   status);
    return retval;
  }

  if( nsamp > ntslice ) {
    msgSetk("NSAMP",nsamp);
    msgSetk("NTSLICE",ntslice);
    msgOutif( MSG__VERB, " ",
	   "SMF_QUICK_NOISE: Shortening nsamp ^NSAMP to file length ^NTSLICE",
	   status);
    nsamp = ntslice;
  }

  if( nchunk < 1 ) {
    *status = SAI__ERROR;
    msgSetk("NCHUNK",nchunk);
    errRep(FUNC_NAME, "Invalid nchunk: ^NCHUNK, must be >= 1", status);
    return retval;
  }

  if( nchunk > ntslice) {
    msgSetk("NCHUNK",nchunk);
    msgOutif( MSG__VERB, " ",
	      "SMF_QUICK_NOISE: Shortening nchunk ^NCHUNK to 1", status);
    nchunk = 1;
  }

  /* Now calculate the rms in nchunk uniformly distributed chunks starting
     with length nsamp, and skipping over padding/apodization */

  if( qua ) {
    smf_get_goodrange( qua, ntslice, tstride, SMF__Q_BOUND, &istart, &iend,
                       status );
  } else {
    istart = 0;
    iend = ntslice-1;
  }

  dat = data->pntr[0];
  len = ( nchunk > 1 ) ? ( (iend-istart+1)-nsamp )/(nchunk-1) : 0;
  minsig = 0;

  for( i=0; i<nchunk; i++ ) {
    /* Calculate the r.m.s. of this chunk */
    smf_stats1D( dat+bolo*bstride+(istart+i*len)*tstride,
                 tstride, nsamp, qua?qua+bolo*bstride+(istart+i*len)*tstride:NULL,
                 0, mask, NULL, &sig, NULL, &ngood, status );

    if( *status == SMF__INSMP ) {
      /* Annul the bad status; there simply weren't enough samples for
	 statistics */
      errAnnul( status );

      /* Set fraction to 0 */
      goodfrac = 0;
    } else {
      /* fraction of good samples used in measurement */
      goodfrac = (double) ngood / (double) nsamp;
    }

    /* Update estimate if there were enough samples. Arbitrarily choose
       90% of the samples as the threshold */
    if( goodfrac >= 0.9 ) {
      if( !minsig ) {
	minsig = sig;
      } else if( sig < minsig ) {
	minsig = sig;
      }
    }
  }

  /* If minsig is still zero, couldn't calculate an estimate */
  if( !minsig ) {
    *status = SMF__INSMP;
    errRepf( FUNC_NAME, "Insufficient good samples to calculate noise in "
             "bolometer %d.", status, (int) bolo );
  }

  return minsig;

}
