/*
*+
*  Name:
*     smf_isfft

*  Purpose:
*     Decide whether the supplied smfData is FFT'd data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     int smf_isfft( const smfData *infile, dim_t *ntslice, dim_t *nbolo,
*                    dim_t *nf, int * status );

*  Arguments:
*     infile = const smfData * (Given)
*        smfData to test.
*     ntslice = dim_t* (Returned)
*        Optionally return the number of time slices in the corresponding
*        time-series data.
*     nbolo = dim_t* (Returned)
*        Optionally return the number of bolometers
*     nf = dim_t* (Returned)
*        Optionally return the number of frequencies
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Boolean int. True if a FFT, false if it is time-series. Status is
*     set to SAI__ERROR if the data doesn't appear to be either.

*  Description:
*     Tests a smfData to see if it is FFT'd data. Also optionally returns
*     the number of time slices that correspond to the input data. This is
*     not the completely trivial case that ntslice = nfreq * 2 since
*     the FFT of real data in FFTW has ntslice/2 + 1 samples. To determine
*     whether the input had an even or odd number of samples, this routine
*     checks the last element of the FFT to see if it is real-valued (if it
*     is, it is the Nyquist frequency of an even-number of input samples).
*     If data is time-domain, simply returns the known ntslice of the input.

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  Notes:
*     - Currently just checks for reasonable dimensionality, should check
*       header as well.
*     - The check for even/odd input length will give the wrong
*       answer if the FFT for all of the detectors is identically 0, and
*       the input had an even number of samples.

*  History:
*     2008-07-23 (EC):
*        Initial version copied from smf_isfft.
*     2008-07-28 (EC):
*        Calculate ntslice.
*     2010-10-29 (EC):
*        Better job of figuring out ntslice

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2008 University of British Columbia.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_isfft"

int smf_isfft( const smfData *indata, dim_t *ntslice, dim_t *nbolo,
               dim_t *nf, int *status ) {
  dim_t i;                      /* Loop counter */
  int isreal=1;                 /* Flag for real-valued Nyquist frequency */
  dim_t nbolo0=0;               /* Number of detectors  */
  dim_t nf0=0;                  /* Number of frequencies */
  int retval=0;                 /* The return value */
  double *val=NULL;             /* Pointer to element of data array */

  if (*status != SAI__OK) return 0;

  if (indata == NULL) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfData pointer provided"
            " (possible programming error)", status);
    return 0;
  }

  if( (indata->dtype == SMF__DOUBLE) &&
      ( ( (indata->ndims==2) && (indata->dims[1]==2) ) ||
        ( (indata->ndims==4) && (indata->dims[3]==2) ) ) ) {
    /* Looks like frequency-domain data */
    retval = 1;

    /* Check dimensions of data if requested */
    if( ntslice || nbolo || nf ) {

      /* Get numbers of detectors */
      if( indata->ndims==2 ) {
        nbolo0 = 1;
      } else {
        nbolo0 = indata->dims[1]*indata->dims[2];
      }

      /* Frequency is always the first dimension */
      nf0 = indata->dims[0];

      /* Try to figure out ntslices */
      if( ntslice ) {
        if( indata->hdr && indata->hdr->nframes) {
        /* If we have a header we can try to figure out the length of the
           time axis there */

          *ntslice = indata->hdr->nframes;
        } else if( indata->pntr[0] ) {
          /* Otherwise check for a real value at the Nyquist frequency to
             decide if the number of time slices is even or odd. */

          val = indata->pntr[0];   /* Init pointer to last imag. of 1st bolo */
          val += nf0*nbolo0 + (nf0-1);

          for( i=0; isreal && (i<nbolo0); i++ ) {
            if( *val ) {
              /* If complex-valued the input array had an odd length */
              isreal = 0;
            }

            /* Increment pointer to end of next bolo */
            val += nf0;
          }

          *ntslice = nf0*2 - 1 - isreal;
        } else {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME ": couldn't establish ntslices", status );
        }
      }

      /* Then do nbolo and nf */
      if( nbolo ) *nbolo = nbolo0;
      if( nf ) *nf = nf0;

    }

  } else if( (indata->ndims==1) || (indata->ndims==3) ) {
    /* Looks like time-domain data */
    retval = 0;

    /* Check dimensions of data if requested */

    if( nf ) *nf = 0;

    if( ntslice || nbolo ) {
      if( indata->ndims == 1 ) {
        if( ntslice ) *ntslice = indata->dims[0];
        if( nbolo ) *nbolo = 1;
      } else {
        if( indata->isTordered ) {
          if( ntslice ) *ntslice = indata->dims[2];
          if( nbolo ) *nbolo = indata->dims[0]*indata->dims[1];
        } else {
          if( ntslice ) *ntslice = indata->dims[0];
          if( nbolo ) *nbolo = indata->dims[1]*indata->dims[2];
        }
      }
    }
  } else {
    /* Don't know... set SMF__WDIM so that caller can trap */
    *status = SMF__WDIM;
    errRep( "", FUNC_NAME
            ": Can't determine whether data are time- or frequency-domain",
            status);
  }

  return retval;
}
