/*
*+
*  Name:
*     smf_isfft

*  Purpose:
*     Decide whether the supplied smfData is FFT'd data and get dimensions

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     int smf_isfft( const smfData *indata, dim_t *rdims, dim_t *nbolo,
*                    dim_t fdims[2], double df[2], int *ndims,
*                    int *status );

*  Arguments:
*     indata = const smfData * (Given)
*        smfData to test.
*     rdims = dim_t* (Returned)
*        Optionally return the lengths of the real-space axes (e.g. length
*        of the time series, or lengths of the orthogonal map axes).
*        Unused dimensions are set to a length of 0. Can be NULL.
*     nbolo = dim_t* (Returned)
*        If the data are a time-series cube, return the number
*        of bolometers (>= 1). If we are dealing with map data nbolo is set
*        to 0. Can be NULL.
*     fdims = dim_t[2] (Returned)
*        Optionally return the lengths of the transformed frequency
*        axes (see ndims). Unused dimensions are set to a length of 0. Can
*        be NULL.
*     df = double[2] (Returned)
*        If requested, calculate frequency step sizes in the FFT (Hz if 1-d,
*        1/arcsec is 2-d). Can be NULL.
*     ndims = int * (Returned)
*        Number of real-space dimensions. Set to 1 if transformed
*        time-series data, or 2 if transformed map data. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Boolean int. True if an FFT, false if it is real-space. Status
*     is set to SAI__ERROR if it is ambiguous whether we have FFT or
*     real-space data, and return value is 0. If the data are real-space
*     but have dimensions that are not 2- or 3-dimensional, bad status
*     will also be set.

*  Description:
*     Checks the smfData.isFFT flag to decide if we have real-space
*     (-1) or FFT data (>1). If it is set to 0 it is ambiguous, and
*     status is set to SAI__ERROR. Also optionally returns the lengths
*     of the frequency space and corresponding real space axes (either
*     length of the time series, or lengths of map axes). Regardless of
*     whether the data are real space or FFT, all of rdims, nbolo, fdims
*     and ndims will be populated with the correct values. Finally, this
*     function does a number of sanity checks.

*  Authors:
*     Ed Chapin (UBC)
*     AGM: Gaelen Marsden (UBC)
*     {enter_new_authors_here}

*  Notes:

*  History:
*     2008-07-23 (EC):
*        Initial version copied from smf_isfft.
*     2008-07-28 (EC):
*        Calculate ntslice.
*     2010-10-29 (EC):
*        Better job of figuring out ntslice
*     2011-09-21 (EC):
*        Simplify behaviour now that we have smfData.isFFT, make it
*        support FFTs of 2D maps, and return real and fourier-space dimensions
*     2011-10-03 (EC):
*        Add df.
*     2014-07-31 (AGM):
*        more reliable test for real-space map or time-stream (old one failed
*           for ntslice*ndet*1 time-stream data sets)
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2008,2010-2011,2014 University of British Columbia.
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

int smf_isfft( const smfData *indata, dim_t rdims[2], dim_t *nbolo,
               dim_t fdims[2], double df[2], int *ndims, int *status ) {

  dim_t fdims0[2]={0,0};        /* Lengths of frequency axes */
  dim_t i;                      /* Loop counter */
  dim_t nbolo0=0;               /* Number of detectors if time-series cube */
  int   ndims0=0;               /* Number of dimensions */
  dim_t rdims0[2]={0,0};        /* Real space dimensions */
  int retval=0;                 /* The return value */

  if( *status != SAI__OK ) return retval;

  if( indata == NULL ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfData pointer provided"
            " (possible programming error)", status);
    return retval;
  }

  if( !indata->isFFT ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": ambiguous whether data are real-space or FFT",
            status);
    return retval;
  }

  /* The possible dimensions for transformed data are:

     3: the FFT of a map (2 spatial axes, and the complex components)
     4: the FFT of a time-series cube (ntslice/2+1, nx, ny, complex components)

     In all cases transformed smfData must also be of type DOUBLE.

     The possible dimensions for real-space data are:
     2: a map (2 spatial axes)
     3: a time-series cube (nx, ny, ntslice) or (ntslice, nx, ny)

     Also note that the last axis for the transform of real-spaced data
     using the FFTW library has a length n/2 + 1 (where n is the real-space
     length) since the other half of the data is redundant (simply complex
     conjugate of the first half). */

  if( indata->isFFT > 0 ) {
    /* FFT data */

    if( indata->dtype != SMF__DOUBLE ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME
              ": data are flagged as FFT but not double precision", status);
      return retval;
    }

    if( (indata->ndims < 3) || (indata->ndims > 4) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME
              ": data are flagged as FFT but not 3- or 4-dimensional", status);
      return retval;
    }

    if( indata->ndims == 3 ) {
      /* Transform of a map */
      ndims0 = 2;
      fdims0[0] = indata->dims[0];
      fdims0[1] = indata->dims[1];
      rdims0[0] = indata->dims[0];
      rdims0[1] = (dim_t) indata->isFFT;
    } else {
      /* Transform of a data cube */
      ndims0 = 1;
      fdims0[0] = indata->dims[0];
      rdims0[0] = (dim_t) indata->isFFT;
      nbolo0 = indata->dims[1]*indata->dims[2];
    }

    if( fdims0[ndims0-1] != (rdims0[ndims0-1]/2 + 1) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME
              ": data are flagged as FFT but real/frequency axes don't match",
              status);
      return retval;
    }

    retval = 1;

  } else {
    /* Real space data */

    if( (indata->ndims < 2) || (indata->ndims > 3) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME
              ": data appear to be real-space but are neither 2- nor "
              "3-dimensional", status);
      return retval;
    }

    if( !indata->hdr || !indata->hdr->nframes || indata->hdr->nframes <= 1 ) {
      /* A 2D map. Note that maps produced by SMURF have a 3rd
       frequency axis of length 1 */
      ndims0 = 2;
      rdims0[0] = indata->dims[0];
      rdims0[1] = indata->dims[1];
      fdims0[0] = indata->dims[0];
      fdims0[1] = indata->dims[1]/2+1;
    } else {
      /* A time-series cube */
      ndims0 = 1;
      smf_get_dims( indata, NULL, NULL, &nbolo0, rdims0, NULL, NULL, NULL,
                    status );
      fdims0[0] = rdims0[0]/2+1;
    }

    retval = 0;
  }

  /* Debugging messages */
  msgOutiff( MSG__DEBUG, "", FUNC_NAME
             ": isfft=%i ndims=%d nbolo=%zu", status, retval, ndims0, nbolo0 );
  for( i=0; i<ndims0; i++ ) {
    msgOutiff( MSG__DEBUG, "", FUNC_NAME
               ": dim(%zu) real_len=%zu freq_len=%zu", status,
               i, rdims0[i], fdims0[i] );
  }

  /* Return values */
  if( rdims ) {
    rdims[0] = rdims0[0];
    rdims[1] = rdims0[1];
  }

  if( nbolo ) *nbolo = nbolo0;

  if( fdims ) {
    fdims[0] = fdims0[0];
    fdims[1] = fdims0[1];
  }

  if( df ) {

    if( indata->hdr ) {
      df[0] = VAL__BADD;
      df[1] = VAL__BADD;

      if( ndims0 == 1 ) {
        double steptime = indata->hdr->steptime;

        if( steptime >= 0 ) {
          df[0] = 1. / (steptime * (double) rdims0[0]);
        } else if( *status == SAI__OK ) {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME
                  ": Can't calculate df because steptime is <= 0!",
                  status );
        }
      } else if( ndims0 == 2 ) {
        double pixsize;

        pixsize = smf_map_getpixsize( indata, status );

        if( *status == SAI__OK ) {
          for( i=0; i<ndims0; i++ ) {
            df[i] = 1. / (pixsize * (double) rdims0[i]);
          }
        }
      }
    } else {
      if( *status == SAI__OK ) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME
                ": df requested, but supplied smfData has no header", status );
      }
    }
  }

  if( ndims ) *ndims = ndims0;

  return retval;
}
