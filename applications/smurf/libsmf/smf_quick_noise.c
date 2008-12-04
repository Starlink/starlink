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
*     smf_quick_noise( smfData *data, dim_t bolo, dim_t nsamp, dim_t nchunk, 
*                      unsigned char *quality, unsigned char mask, 
*                      int *status )

*  Arguments:
*     data = smfData* (Given)
*        Pointer to bolo-ordered smfData
*     bolo = dim_t (Given)
*        The offset of the bolometer in question (in range [0,nbolo-1])
*     nsamp = dim_t (Given)
*        Length of the interval in which to calculate sample standard deviation
*     nchunk = dim_t (Given)
*        Number of evenly-spaced chunks of time-stream over which to estimate
*        the sample standard deviation
*     quality = usigned char* (Given)
*        If specified, use this QUALITY array instead of that in data
*        to decide which samples to use (provided mask). Otherwise
*        data are only ignored if set to VAL__BADD.
*     mask = unsigned char (Given)
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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


double smf_quick_noise( smfData *data, dim_t bolo, dim_t nsamp, dim_t nchunk, 
			unsigned char *quality, unsigned char mask, 
			int *status ) {

  /* Local variables */
  double *dat=NULL;             /* Pointer to bolo data */
  double goodfrac;              /* Fraction of good samples */
  dim_t i;                      /* Loop counter */
  dim_t len;                    /* Length of interval including starts */
  double minsig;                /* Minimum measured r.m.s. */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ngood;                  /* Number of good samples in r.m.s. check */
  dim_t ntslice;                /* Number of time slices */
  unsigned char *qua=NULL;      /* Pointer to quality flags */
  double retval=0;              /* Return value */
  double sig;                   /* r.m.s. of this chunk */

  /* Check status */
  if (*status != SAI__OK) return retval;

  /* Check for bolo-ordered data */
  if( data->isTordered ) {
    *status = SMF__TORDB;
    errRep( FUNC_NAME, "Data is time-ordered, must be bolo-ordered", status );
    return retval;
  }

  /* Check for double-precision data */
  smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status );

  /* Check for QUALITY arrays */
  if( quality ) {
    qua = quality;
  } else {
    qua = data->pntr[2];
  }

  /* Obtain data dimensions */
  smf_get_dims( data, &nbolo, &ntslice, NULL, NULL, NULL, status );
  
  /* Check for reasonable bolo/nsamp/nchunk */
  if( bolo >= nbolo ) {
    *status = SAI__ERROR;
    msgSeti("BOLO",bolo);
    msgSeti("NBOLO",nbolo-1);
    errRep(FUNC_NAME, "Invalid bolo: ^BOLO, must be in range [0,^NBOLO]",
	   status);
    return retval;
  }

  if( nsamp < SMF__MINSTATSAMP ) { 
    *status = SAI__ERROR;
    msgSeti("MIN",SMF__MINSTATSAMP);
    msgSeti("NSAMP",nsamp);
    errRep(FUNC_NAME, 
	   "Invalid nsamp: ^NSAMP, must be > ^MIN",
	   status);
    return retval;
  }

  if( nsamp > ntslice ) {
    msgSeti("NSAMP",nsamp);
    msgSeti("NTSLICE",ntslice);
    msgOutif( MSG__VERB, " ",
	   "SMF_QUICK_NOISE: Shortening nsamp ^NSAMP to file length ^NTSLICE",
	   status);
    nsamp = ntslice;
  }

  if( nchunk < 1 ) {
    *status = SAI__ERROR;
    msgSeti("NCHUNK",nchunk);
    errRep(FUNC_NAME, "Invalid nchunk: ^NCHUNK, must be >= 1", status);
    return retval;
  }

  if( nchunk > ntslice) {
    msgSeti("NCHUNK",nchunk);
    msgOutif( MSG__VERB, " ", 
	      "SMF_QUICK_NOISE: Shortening nchunk ^NCHUNK to 1", status);
    nchunk = 1;
  }

  /* Now calculate the rms in nchunk uniformly distributed chunks starting
     with length nsamp */

  dat = data->pntr[0];
  len = ntslice-nsamp;
  minsig = 0;

  for( i=0; i<nchunk; i++ ) {
    /* Calculate the r.m.s. of this chunk */
    smf_stats1( &dat[bolo*ntslice], i*len/(nchunk-1), nsamp, 
                &qua[bolo*ntslice], mask, NULL, &sig, &ngood, status );
	
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
    errRep( FUNC_NAME, "Insufficient good samples for statistics", status );
  }

  return minsig;
  
}
