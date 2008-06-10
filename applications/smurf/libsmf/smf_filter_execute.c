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

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-09-27 (EC):
*        Initial Version
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-06-06 (EC):
*        -Renamed from smf_fft_filter to smf_filter_execute
*        -Modified interface to take external smfFilter
*        -Handle real and complex filters
*     2008-06-10 (EC):
*        Move normalization to smf_filter_ident

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

void smf_filter_execute( smfData *data, smfFilter *filt, int *status ) {

  /* Local Variables */
  double ac, bd, aPb, cPd;      /* Components for complex multiplication */
  double *base;                 /* Pointer to start of current bolo in array */
  double df;                    /* Width of frequency bin in FFT */
  fftw_complex *data_fft;       /* The FFT of a single bolometer */
  dim_t i;                      /* Loop counter */
  dim_t icut;                   /* cutoff index for the filter */
  dim_t j;                      /* Loop counter */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Total number of data points */
  dim_t ntslice;                /* Number of time slices */
  fftw_plan plan_forward;       /* plan for forward transformation */
  fftw_plan plan_inverse;       /* plan for inverse transformation */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check for NULL pointers */
  if( !data ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "NULL smfData pointer", status );
  }

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "NULL smfFilter pointer", status );
  }

  /* Ensure that the smfData is ordered correctly (bolo ordered) */
  smf_dataOrder( data, 0, status );

  /* Obtain the dimensions of the array being filtered */
  if( *status == SAI__OK ) {
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
    }

    /* Check that the filter dimensions are appropriate for the data */
    if( ntslice != filt->ntslice ) {
      msgSeti("DATALEN",ntslice);
      msgSeti("FILTLEN",filt->ntslice);
      errRep(FUNC_NAME, 
             "Filter for length ^FILTLEN doesn't match data length ^FILTLEN",
             status);
    }
  }

  if( *status != SAI__OK ) return;

  /* Allocate array for the FFT of the time-stream data. */

  data_fft = smf_malloc( data_fft, sizeof(fftw_complex), filt->dim, status );

  /* Filter the data one bolo at a time */
  for( i=0; i<nbolo; i++ ) {
    /* Obtain pointer to the correct chunk of data */
    base = &((double *)data->pntr[0])[i*ntslice];
    
    /* Setup forward FFT */
    plan_forward = fftw_plan_dft_r2c_1d( ntslice, base, data_fft, 
                                         FFTW_ESTIMATE );

    /* Execute the FFT */
    fftw_execute( plan_forward );

    /* Destroy the plan */ 
    fftw_destroy_plan( plan_forward );

    /* Apply the frequency-domain filter */
    if( filt->isComplex ) {
      for( j=0; j<ntslice/2+1; j++ ) {
        /* Complex times complex */
        ac = data_fft[j][0] * ((fftw_complex *)filt->buf)[j][0];
        bd = data_fft[j][1] * ((fftw_complex *)filt->buf)[j][1];
        aPb = data_fft[j][0] + data_fft[j][1];
        cPd = ((fftw_complex *)filt->buf)[j][0] + 
          ((fftw_complex *)filt->buf)[j][1];

        /* This method only needs 3 multiplies */
        data_fft[j][0] = ac - bd;
        data_fft[j][1] = aPb*cPd - ac - bd;
      }
    } else { 
      for( j=0; j<ntslice/2+1; j++ ) {
        /* Complex times real */
        data_fft[j][0] *= ((double *)filt->buf)[j];
        data_fft[j][1] *= ((double *)filt->buf)[j];
      }
    }

    /* Setup inverse FFT */
    plan_inverse = fftw_plan_dft_c2r_1d(ntslice, data_fft, base, FFTW_ESTIMATE);

    /* Perform inverse transform and normalize the result */
    fftw_execute( plan_inverse );

    /* Destroy the plan */ 
    fftw_destroy_plan( plan_inverse );
  }

  /* Clean up */
  data_fft = smf_free( data_fft, status );
}
