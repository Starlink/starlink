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
*     2008-06-11 (EC):
*        Switched to "guru" FFTW interface to facilitate future multi-threading
*     2008-06-12 (EC):
*        Switch to split real/imaginary arrays for smfFilter
*     2008-06-13 (EC):
*        Only create plans once since we're using guru interface

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
  double *data_fft_r;           /* Real part of the data FFT */
  double *data_fft_i;           /* Imaginary part of the data FFT */
  dim_t i;                      /* Loop counter */
  fftw_iodim iodim;             /* I/O dimensions for transformations */
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
      *status = SAI__ERROR;
      msgSeti("DATALEN",ntslice);
      msgSeti("FILTLEN",filt->ntslice);
      errRep(FUNC_NAME, 
             "Filter for length ^FILTLEN doesn't match data length ^FILTLEN",
             status);
    }
  }

  if( *status != SAI__OK ) return;

  /* Allocate arrays for the FFT of the time-stream data. */
  data_fft_r = smf_malloc( filt->dim, sizeof(*data_fft_r), 0, status );
  data_fft_i = smf_malloc( filt->dim, sizeof(*data_fft_i), 0, status );

  /* Describe the input and output array dimensions for FFTW guru interface  */
  iodim.n = filt->ntslice;
  iodim.is = 1;
  iodim.os = 1;

  /* Setup forward FFT plan using guru interface */
  plan_forward = fftw_plan_guru_split_dft_r2c( 1, &iodim, 0, NULL,
                                               data->pntr[0], data_fft_r, 
                                               data_fft_i, 
                                               FFTW_ESTIMATE | 
                                               FFTW_UNALIGNED);
  
  if( !plan_forward ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, 
           "FFTW3 could not create plan for forward transformation",
           status);
  }

  /* Setup inverse FFT plan using guru interface */
  plan_inverse = fftw_plan_guru_split_dft_c2r( 1, &iodim, 0, NULL,
                                               data_fft_r, data_fft_i, 
                                               data->pntr[0], FFTW_ESTIMATE | 
                                               FFTW_UNALIGNED);
  
  if( !plan_inverse ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, 
           "FFTW3 could not create plan for inverse transformation",
           status);
  }
   
  /* Filter the data one bolo at a time */
  for( i=0; (*status==SAI__OK) && (i<nbolo); i++ ) {
    /* Obtain pointer to the correct chunk of data */
    base = &((double *)data->pntr[0])[i*ntslice];
    
    /* Execute forward transformation using the guru interface */
    fftw_execute_split_dft_r2c( plan_forward, base, data_fft_r, data_fft_i );

    /* Apply the frequency-domain filter */
    if( filt->isComplex ) {
      for( j=0; j<ntslice/2+1; j++ ) {
        /* Complex times complex, using only 3 multiplies */
        ac = data_fft_r[j] * filt->real[j];
        bd = data_fft_i[j] * filt->imag[j];
        
        aPb = data_fft_r[j] + data_fft_i[j];
        cPd = filt->real[j] + filt->imag[j];
        
        data_fft_r[j] = ac - bd;
        data_fft_i[j] = aPb*cPd - ac - bd;
      }
    } else { 
      for( j=0; j<ntslice/2+1; j++ ) {
        /* Complex times real */
        data_fft_r[j] *= filt->real[j];
        data_fft_i[j] *= filt->real[j];
      }
    }    
    
    /* Perform inverse transformation using guru interface */
    fftw_execute_split_dft_c2r( plan_inverse, data_fft_r, data_fft_i, base );
  }

  /* Destroy the plans */ 
  fftw_destroy_plan( plan_forward );
  fftw_destroy_plan( plan_inverse );

  /* Clean up */
  data_fft_r = smf_free( data_fft_r, status );
  data_fft_i = smf_free( data_fft_i, status );
}
