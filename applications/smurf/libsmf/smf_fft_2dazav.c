/*
*+
*  Name:
*     smf_fft_2dazav

*  Purpose:
*     Calculate the azimuthal average of a 2-d FFT

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_fft_2dazav( const smfData *data, double *df, int *status ) {

*  Arguments:
*     data = smfData * (Given)
*        Pointer to a smfData containing the FFT of a 2-d map
*     df = double * (Returned)
*        Return the frequency spacing of the power spectrum. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     Pointer to newly created 1-d smfData containing the azimuthal average

*  Description:
*     At each scalar spatial frequency calculate the average over all
*     position angles. Only the first component of the FFT is
*     considered, so for an azimuthally-averaged power spectrum first
*     convert the cartesian representation to polar power using
*     smf_fft_cart2pol (the first component will contain the
*     amplitudes^2 of the Fourier coefficients). If this is not done,
*     the first component will contain the real part of the transform
*     as output by smf_fft_data, and the average will likely come out
*     to something close to zero...

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-09-23 (EC):
*        Initial version
*     2011-09-30 (EC):
*        Should only be calculated up to the Nyquist frequency
*     2011-10-05 (EC):
*        Optionally return df.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 University of British Columbia.
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
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"
#include "fftw3.h"
#include "star/kaplibs.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_fft_2dazav"

smfData *smf_fft_2dazav( const smfData *data, double *df, int *status ) {

  dim_t *count=NULL;            /* Count of data at this radius */
  double df_o=1;                /* Spatial freq. step of output */
  double df0[2]={1,1};          /* Spatial freq. step x/y-axes of map */
  dim_t fdims[2];               /* Frequency space dimensions */
  dim_t i;                      /* Loop counter */
  double *idata=NULL;           /* Input 2d data pointer */
  dim_t j;                      /* Loop counter */
  dim_t nf_o=0;                 /* Length output frequency axis (to Nyquist) */
  int ndims;                    /* Number of real dimensions */
  double *odata=NULL;           /* Output 1d data pointer */
  double pixsize;               /* Map pixel size */
  smfData *retdata=NULL;        /* Returned 1-d smfData */
  dim_t rdims[2];               /* Real space dimensions */
  int whichaxis=0;              /* Which frequency axis for output */

  if (*status != SAI__OK) return NULL;

   /* Check for NULL pointer */
  if( data == NULL ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData pointer is NULL", status );
    return NULL;
  }

  /* Check that we have frequency-domain input */
  if( !smf_isfft( data, rdims, NULL, fdims, df0, &ndims, status ) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Input data are not FFT!", status );
    return NULL;
  }

  if( ndims != 2 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Input data are not FFT of a 2-d map!", status );
    return NULL;
  }

  /* Get pixel size to help determine step size for the 1-d output */
  pixsize = smf_map_getpixsize( data, status );

  if( *status == SAI__OK ) {
    /* To ensure that we hit the full range of frequencies sampled up
       to Nyquist, and at the highest natural resolution, we span the
       frequency axes in steps the size of the higher-resolution
       (longer spatial) axis */

    if( rdims[1] > rdims[0] ) {
      df_o = df0[1];
      whichaxis = 2;
    } else {
      df_o = df0[0];
      whichaxis = 1;
    }

    nf_o = round((0.5/pixsize)/df_o);
  }

  /* Allocate space for the answer */
  retdata = smf_deepcopy_smfData( NULL, data, 0, SMF__NOCREATE_DATA |
                                  SMF__NOCREATE_VARIANCE |
                                  SMF__NOCREATE_QUALITY |
                                  SMF__NOCREATE_FILE |
                                  SMF__NOCREATE_DA |
                                  SMF__NOCREATE_FTS, 0, 0, status );

  if( *status == SAI__OK ) {
    /* Allocate space for the averaged FFT and work arrays*/
    retdata->ndims=1;
    retdata->dims[0] = nf_o;
    retdata->lbnd[0] = 1;
    retdata->dtype = SMF__DOUBLE;

    retdata->pntr[0] = astCalloc( nf_o, smf_dtype_sz(retdata->dtype,status) );
    count = astCalloc( nf_o, sizeof(*count) );
  }

  /* Loop over elements of the input data and decide which radial bin
     they belong in */
  if( *status == SAI__OK ) {
    double x;
    double y;
    dim_t d;

    /* Pointers to input and output data*/
    idata = data->pntr[0];
    odata = retdata->pntr[0];

    for( i=0; (*status==SAI__OK)&&(i<fdims[0]); i++ ) {

      x = FFT_INDEX_TO_FREQ(i,rdims[0]) * df0[0];

      for( j=0; j<fdims[1]; j++ ) {
        /* Work out cartesian distance from origin (which is at 0,0
           and wraps-around the edges. x and y give the frequency
           indices multiplied by the frequency step size along each
           axis. */

        y = FFT_INDEX_TO_FREQ(j,rdims[1]) * df0[1];

        /* The final distance is just truncated to an integer number of
           steps in the return array */
        d = (dim_t) (sqrt(x*x + y*y) / df_o);

        /* Only consider frequencies up to Nyquist */
        if( d < nf_o ) {
          /* Accumulate values at this radius */
          odata[d] += idata[i+j*fdims[0]];
          count[d]++;
        }
      }
    }

    /* Re-normalize */
    for( i=0; i<nf_o; i++ ) {
      if( count[i] ) odata[i] /= (double) count[i];
      else odata[i] = VAL__BADD;
    }
  }

  /* Create WCS for the data based on the input WCS if it exists */
  if( data->hdr && data->hdr->tswcs ) {
    AstFrame *curframe=NULL;
    AstMapping *fftmapping=NULL;
    AstFrameSet *fftwcs=NULL;
    AstMapping *map_in=NULL;
    int nout;

    /* Start and AST context */
    astBegin;

    /* Create a new frameset containing a 1d base GRID frame */
    fftwcs = astFrameSet( astFrame( 1, "Domain=GRID" ), " " );

    /* The current frame will be spatial frequency */
    curframe = astFrame( 1,"label=Spatial Frequency,Unit=1/arcsec");

    /* Obtain the correct mapping from the input */
    map_in = astGetMapping( data->hdr->tswcs, AST__BASE, AST__CURRENT );
    astMapSplit( map_in, 1, &whichaxis, &nout, &fftmapping );

    /* Add curframe with the fftmapping to the frameset */
    astAddFrame( fftwcs, AST__BASE, fftmapping, curframe );

    /* Export and store in the tswcs slot of the output smfHead */
    astExport( fftwcs );
    astEnd;

    if( retdata->hdr->tswcs ) {
      retdata->hdr->tswcs = astAnnul(retdata->hdr->tswcs);
    }
    retdata->hdr->tswcs = fftwcs;
  }

  /* Return df? */
  if( df ) {
    *df = df_o;
  }

  /* Clean up */
  if( count ) count = astFree( count );

  return retdata;
}
