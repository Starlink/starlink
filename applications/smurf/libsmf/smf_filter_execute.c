/*
*+
*  Name:
*     smf_fft_filter

*  Purpose:
*     Filter data in the frequency domain using the FFTW3 library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_filter_execute( smfData *data, double srate, int *status )

*  Arguments:
*     data = smfData * (Given and Returned)
*        The data to be filtered (performed in-place)
*     srate = double (Given)
*        If nonzero specifies sample rate of data in Hz (otherwise taken
*        from JCMTState associated with data).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Filter the data.

*  Notes:
*     Currently buggy / not fully implemented.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-09-27 (EC):
*        Initial Version
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-06-06 (EC):
*        Renamed from smf_fft_filter to smf_filter_execute

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
#include "fftw3.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_filter_execute"

void smf_filter_execute( smfData *data, double srate, int *status ) {

  /* Local Variables */
  double *base;                 /* Pointer to start of current bolo in array */
  double df;                    /* Width of frequency bin in FFT */
  double *filter;               /* Filter */
  fftw_complex *fft;            /* The FFT of the data */
  dim_t i;                      /* Loop counter */
  dim_t icut;                   /* cutoff index for the filter */
  dim_t j;                      /* Loop counter */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Total number of data points */
  dim_t ntslice;                /* Number of time slices */
  fftw_plan plan;               /* stores method used by FFTW3 */


  /* Main routine */
  if (*status != SAI__OK) return;

  /* If srate == 0 should try to figure it out from the header */

  /* Check for a valid sample rate */
  if( srate <= 0 ) {
    *status = SAI__ERROR;
    msgSetd("SRATE",srate);
    errRep(FUNC_NAME, "Invalid srate: ^SRATE [Hz]", status);
    return;
  }

  /* Ensure that the smfData is ordered correctly (bolo ordered) */
  smf_dataOrder( data, 0, status );

  /* Obtain the dimensions of the array being filtered */

  if( data->ndims == 1 ) {
    /* If 1-dimensional assume the dimension is time */
    ntslice = data->dims[0];
    ndata = ntslice;
    nbolo = 1;
  } else if( data->ndims == 3 ) {
    /* If 3-dimensional this should be bolo-ordered at this point */
    ntslice = data->dims[0];
    nbolo = data->dims[1]*data->dims[2];
    ndata = nbolo*ntslice;
  } else {
    /* Can't handle other dimensions */
    *status = SAI__ERROR;
    msgSeti("NDIMS",data->ndims);
    errRep(FUNC_NAME, "Can't handle ^NDIMS dimensions.", status);
    return;
  }

  /* Allocate array for the FFT of the time-stream data. It's length is
     ntslice/2+1 because the input array is real; FFTW optimizes memory
     usage in this case since the negative frequencies in the transform
     contain the same information as the positive frequencies */

  fft = (fftw_complex *) fftw_malloc(sizeof(fftw_complex)*(ntslice/2+1));
  filter = smf_malloc( ntslice/2+1, sizeof(*filter), 0, status );

  /* Build the filter array (test with a low-pass filter, 10 Hz) */

  df = srate/(double) ntslice; 
  icut = (dim_t) 1./df;         /* index of cutoff frequency in filter */

  for( i=0; i<ntslice/2+1; i++ ) {
    if( i < icut ) {
      filter[i] = 0;
    } else {
      filter[i] = 1;
    }
  }

  /* Filter the data one bolo at a time */
  for( i=0; i<nbolo; i++ ) {
    /* Obtain pointer to the correct chunk of data */
    base = &((double *)data->pntr[0])[i*ntslice];
    
    /* Setup FFTW */
    plan = fftw_plan_dft_r2c_1d( ntslice, base, fft, FFTW_ESTIMATE );

    /* Execute the FFT */
    fftw_execute( plan );

    /* Destroy the plan */ 
    fftw_destroy_plan( plan );

    /* Apply the frequency-domain filter */
    for( j=0; j<ntslice/2+1; j++ ) {
      fft[j][0] *= filter[j];
      fft[j][1] *= filter[j];
    }

    /* Setup FFTW for the inverse transform */
    plan = fftw_plan_dft_c2r_1d(ntslice, fft, base, FFTW_ESTIMATE);

    /* Perform inverse transform and normalize the result */
    fftw_execute( plan );

    for( j=0; j<ntslice; j++ ) {
      base[j] /= (double) ntslice;
    }

    /* Destroy the plan */ 
    fftw_destroy_plan( plan );
  }

  /* Clean up */

  fftw_free( fft );
  filter = smf_free( filter, status );
}
