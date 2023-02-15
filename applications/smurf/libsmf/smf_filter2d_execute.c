/*
*+
*  Name:
*     smf_filter2d_execute

*  Purpose:
*     Filter 2-d smfData in the frequency domain using the FFTW3 library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_filter2d_execute( ThrWorkForce *wf, smfData *data, smfFilter *filt,
*                           int complement, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given and Returned)
*        The data to be filtered (performed in-place)
*     filter = smfFilter* (Given and Returned)
*        A smfFilter to apply to the supplied data
*     complement = int (Given)
*        If 1 set the filter to its complement before applying to the data.
*        If -1 set the filter back to its original state, and then apply it.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Filter a 2-d smfData. This specialized routine is called if needed from
*     smf_filter_execute.c

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-10-03 (EC):
*        Initial version based on smf_filter_execute
*     2012-01-04 (EC):
*        -Use WCS from original image since smf_fft_data doesn't calculate
*         it for the inverse transform.
*        -Apply filter to the VARIANCE component if it is supplied
*     2012-01-16 (DSB):
*        Fix a memory leak (fdata was not closed).
*
*  Copyright:
*     Copyright (C) 2011-2012 University of British Columbia.
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
#include <pthread.h>

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"
#include "fftw3.h"

/* SMURF includes */
#include "libsmf/smf_typ.h"
#include "libsmf/smf.h"

#define FUNC_NAME "smf_filter2d_execute"

void smf_filter2d_execute( ThrWorkForce *wf, smfData *data, smfFilter *filt,
                           int complement, int *status ) {

  double *data_i=NULL;     /* Imaginary part of the transformed data */
  double *data_r=NULL;     /* Real part of the transformed data */
  smfData *fdata=NULL;     /* Transform of data */
  dim_t fdims[2]={0,0};    /* Frequency dimensions */
  dim_t i;                 /* loop counter */
  int ndims=0;             /* Number of real-space dimensions */
  dim_t nfdata;            /* Total number of frequency data points */
  smfData *varfilt=NULL;   /* real-space square of supplied filter for var */
  AstFrameSet *wcs=NULL;   /* Copy of real-space WCS */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check for NULL pointers */
  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfData pointer", status );
    return;
  }

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfFilter pointer", status );
    return;
  }

  if( filt->ndims != 2 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Filter must be 2-dimensional", status );
    return;
  }

  if( smf_isfft( data, NULL, NULL, fdims, NULL, &ndims, status ) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": FFT'd data supplied!", status );
    return;
  }

  if( ndims != 2 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": supplied data are not a 2-d map", status );
    return;
  }

  /* Check that the filter dimensions are appropriate for the data */
  for( i=0; i<ndims; i++ ) {
    if( fdims[i] != filt->fdims[i] ) {
      *status = SAI__ERROR;
      errRepf( "", FUNC_NAME
               ": Filter axis %zu has length %zu, doesn't match data %zu",
               status, i, filt->fdims[i], fdims[i] );
      return;
    }
  }

  /* Dimensions of the transformed data */
  nfdata = 1;
  for( i=0; i<ndims; i++ ) {
    if( filt->fdims[i] != fdims[i] ) {
      *status = SAI__ERROR;
      errRepf( "", FUNC_NAME
               ": Filter axis %zu has dim %zu doesn't match data dim %zu",
               status, i, filt->fdims[i], fdims[i]);
      return;
    }

    nfdata *= fdims[i];
  }

  /* Using complement of the filter? */
  if( complement ) smf_filter_complement( filt, status );

  /* Get a copy of the wcs of the input map */
  if( data->hdr && data->hdr->wcs ) {
    wcs = astCopy( data->hdr->wcs );
  }

  /* Transform the data */
  fdata = smf_fft_data( wf, data, NULL, 0, 0, status );

  /* Copy the FFT of the data if we will also be filtering the VARIANCE
     since this will get us a useful container of the correct dimensions
     for the squared filter */
  if( data->pntr[1] ) {
    varfilt = smf_deepcopy_smfData( wf, fdata, 0, SMF__NOCREATE_VARIANCE |
                                    SMF__NOCREATE_QUALITY |
                                    SMF__NOCREATE_FILE |
                                    SMF__NOCREATE_DA, 0, 0, status );
  }

  /* Apply the frequency-domain filter. */
  if( *status == SAI__OK ) {
    data_r = fdata->pntr[0];
    data_i = data_r + nfdata;

    if( filt->isComplex ) {
       errRep( " ", "smf_filter2d_execute: complex filers are not "
               "supported.", status );
    } else {
      for( i=0; i<nfdata; i++ ) {
        data_r[i] *= filt->real[i];
        data_i[i] *= filt->real[i];
      }
    }
  }

  /* Transform back */
  smf_fft_data( wf, fdata, data, 1, 0, status );

  /* Insert the copy of original real-space WCS into the smfHead since
     smf_fft_data does not currently calculate it for inverse
     transforms. */

  if( data->hdr ) {
    /* Annul current wcs if it exists */
    if( data->hdr->wcs ) {
      data->hdr->wcs = astAnnul( data->hdr->wcs );
    }
    data->hdr->wcs = wcs;
  }

  /* If we have a VARIANCE component we also need to smooth it. This
     is slightly complicated because we have to do the equivalent of a
     real-space convolution between the variance map and the
     element-wise square of the real-space filter. So we first stuff
     the supplied filter into a smfData (frequency space), take its
     inverse to real space and square it. We then transform back to
     frequency space, and run it through smf_filter2d_execute to apply
     it to the VARIANCE map (which is also stuffed into its own
     smfData and then copied into the correct location of the supplied
     smfData when finished. */

  if( (data->pntr[1]) && (*status==SAI__OK) ) {
    dim_t ndata;
    double *ptr=NULL;
    smfData *realfilter=NULL; /* Real space smfData container for var filter */
    smfData *vardata=NULL;    /* smfData container for variance only */
    smfFilter *vfilt=NULL;    /* The var filter */

    /* Copy the filter into the smfData container and transform into the
       time domain. */
    ptr = varfilt->pntr[0];
    memcpy(ptr, filt->real, nfdata*sizeof(*ptr));
    if( filt->imag) {
      memcpy(ptr+nfdata, filt->imag, nfdata*sizeof(*ptr));
    } else {
      memset(ptr+nfdata, 0, nfdata*sizeof(*ptr));
    }

    realfilter = smf_fft_data( wf, varfilt, NULL, 1, 0, status );
    smf_close_file( wf, &varfilt, status );

    /* Square each element of the real-space filter and then transform
       back to the frequency domain and stuff into a smfFilter
       (vfilt). We just point the real and imaginary parts of the
       smfFilter to the respective regions of the smfData to save
       memory/time, but we need to be careful when freeing at the
       end. */

    if( *status == SAI__OK ) {
      ptr = realfilter->pntr[0];

      smf_get_dims( realfilter, NULL, NULL, NULL, NULL, &ndata, NULL, NULL,
                    status );

      if( *status == SAI__OK ) {
        double norm = 1. / (double) ndata;
        for(i=0; i<ndata; i++) {
          /* Note that we need an additional normalization of N samples */
          ptr[i] *= ptr[i] * norm;
        }
      }
    }

    varfilt =  smf_fft_data( wf, realfilter, NULL, 0, 0, status );

    if( *status == SAI__OK ) {
      ptr = varfilt->pntr[0];
      vfilt = smf_create_smfFilter( data, status );
      vfilt->real = ptr;
      if( filt->isComplex ) {
        /* Only worry about imaginary part if the original filter was
           complex. */
        vfilt->isComplex = 1;
        vfilt->imag = ptr + nfdata;
      }
    }

    /* Now stuff the variance array into a smfData and filter it. */
    vardata = smf_deepcopy_smfData( wf, data, 0, SMF__NOCREATE_VARIANCE |
                                    SMF__NOCREATE_QUALITY |
                                    SMF__NOCREATE_FILE |
                                    SMF__NOCREATE_DA, 0, 0, status );

    if( *status == SAI__OK ) {
      ptr = vardata->pntr[0];
      memcpy( ptr, data->pntr[1], ndata*sizeof(*ptr) );
      smf_filter2d_execute( wf, vardata, vfilt, 0, status );
    }

    /* Finally, copy the filtered variance into our output filtered smfData */
    if( *status == SAI__OK ) {
      ptr = data->pntr[1];
      memcpy( ptr, vardata->pntr[0], ndata*sizeof(*ptr) );
    }

    /* Clean up */
    if( realfilter ) smf_close_file( wf, &realfilter, status );
    if( vardata ) smf_close_file( wf, &vardata, status );
    if( vfilt ) {
      vfilt->real = NULL;
      vfilt->imag = NULL;
      vfilt = smf_free_smfFilter( vfilt, status );
    }

  }


  /* Return the filter to its original state if required */
  if( complement == -1 ) smf_filter_complement( filt, status );

  /* Clean up */
  if( varfilt ) smf_close_file( wf, &varfilt, status );
  if( fdata ) smf_close_file( wf, &fdata, status );

}
