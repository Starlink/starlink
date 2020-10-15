/*
*+
*  Name:
*     smf_fft_avpspec

*  Purpose:
*     Calculate the average power spectrum of all working bolometers

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_fft_avpspec( const smfData *pspec,
*                             smf_qual_t *quality, dim_t qstride,
*                             smf_qual_t mask, double *weights, int *status ) {

*  Arguments:
*     pspec = smfData * (Given)
*        Pointer to a 4-d smfData containing individual bolo power spectra
*     quality = smf_qual_t* (Given)
*        If specified, use this QUALITY array to decide which bolometers
*        to use (provided mask). Otherwise data are only ignored if set
*        to VAL__BADD.
*     qstride = dim_t (Given)
*        Bolo stride for quality. If 0 assumed to be same stride as pspec.
*     mask = smf_qual_t (Given)
*        Use with qual to define which bits in quality are relevant to
*        ignore data in the calculation.
*     weights = double * (Given)
*        Optional array of weights for each bolometer (otherwise a straight
*        average is performed).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     Pointer to newly created smfData containing a 2-d average
*     power spectrum.

*  Description:
*     At each time slice calculate the average power spectrum from
*     each detector. Also generates a variance array from the scatter. It
*     is assumed that the input power spectrum is already in the correct
*     polar/power form (i.e. not cartesian -- see smf_fft_cart2pol). If
*     a noise array is supplied, 1/noise^2 weighting is used in the average.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2010-02-04 (EC):
*        Initial version
*     2010-02-25 (TIMJ):
*        Fix 32-bit incompatibility.
*     2010-09-21 (COBA):
*        Add SMF__NOCREATE_FTS
*     2011-06-13 (EC):
*        Add weights.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010,2011 University of British Columbia.
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

/* System includes */
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"
#include "fftw3.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_fft_avpspec"

smfData *smf_fft_avpspec( const smfData *pspec, smf_qual_t *quality,
                          dim_t qstride, smf_qual_t mask, double *weights,
                          int *status ) {

  dim_t fdims[2];               /* Lengths of frequency-space axes */
  dim_t i;                     /* Loop counter */
  double *idptr=NULL;           /* Pointer to input data */
  double mean;                  /* Mean value at time slice */
  dim_t nbolo=0;                /* Number of detectors  */
  dim_t ngood;                 /* Number of good samples */
  dim_t ndata;                 /* Total number of data points */
  dim_t nf=0;                   /* Number of frequencies in FFT */
  double *odptr=NULL;           /* Pointer to output data */
  double *ovptr=NULL;           /* Pointer to output variance */
  dim_t rdims[2];               /* Lengths of real-space axes */
  smfData *retdata=NULL;        /* Returned power spectrum */
  double sigma;                 /* RMS value at time slice */

  if (*status != SAI__OK) return NULL;

   /* Check for NULL pointer */
  if( pspec == NULL ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData pointer is NULL", status );
    return NULL;
  }

  /* Check that we have frequency-domain input */
  if( !smf_isfft( pspec, rdims, &nbolo, fdims, NULL, NULL, status ) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Input data are not FFT!", status );
    return NULL;
  }
  nf = fdims[0];

  /* Check that we don't have a 1-d input power spectrum */
  if( pspec->ndims != 4 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Input data are not 4-dimensional!", status );
    return NULL;
  }

  idptr = pspec->pntr[0];
  if( !idptr ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Null input data", status );
    return NULL;
  }

  /* Assume qstride = bolo stride of main array if not specified */
  if( !qstride ) {
    qstride = nf;
  }

  /* Create a new smfData, copying over everything except for the bolo
     data itself */

  retdata = smf_deepcopy_smfData( NULL, pspec, 0, SMF__NOCREATE_DATA |
                                  SMF__NOCREATE_VARIANCE |
                                  SMF__NOCREATE_QUALITY |
                                  SMF__NOCREATE_FILE |
                                  SMF__NOCREATE_DA |
                                  SMF__NOCREATE_FTS, 0, 0, status );
  if( *status == SAI__OK ) {
    /* Allocate space for the averaged power spectrum */
    retdata->ndims = 4;
    retdata->dims[0] = nf;
    retdata->dims[1] = 1;
    retdata->dims[2] = 1;
    retdata->dims[3] = 2;
    retdata->dtype = SMF__DOUBLE;

    ndata=1;
    for( i=0; i<retdata->ndims; i++ ) {
      ndata *= retdata->dims[i];
    }

    /* Allocate space for DATA and VARIANCE components */
    retdata->pntr[0] = astCalloc( ndata, smf_dtype_sz(retdata->dtype,status) );
    retdata->pntr[1] = astCalloc( ndata, smf_dtype_sz(retdata->dtype,status) );

    /* Pointers to output data */
    odptr = retdata->pntr[0];
    ovptr = retdata->pntr[1];

     /* Call the returned data tordered even though it is irrelevant */
    retdata->isTordered=0;

    /* Since we assumed we're in power/polar form just need to calculate
       average and scatter of the amplitude coefficients. The phase
       coefficients are left at their initialized values of 0. */

    for( i=0; i<nf; i++ ) {
      /* The bolometer stride is nf */
      smf_weightstats1D( idptr+i, nf, nbolo, quality, qstride, mask,
                         weights, 1, &mean, &sigma, &ngood, status );

      if( *status == SMF__INSMP ) {
        /* If not enough samples just annul and set bad value */
        errAnnul( status );
        odptr[i] = VAL__BADD;
        ovptr[i] = VAL__BADD;
      } else {
        odptr[i] = mean;
        ovptr[i] = sigma*sigma;
      }
    }
  }

  return retdata;
}
