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

  double *data_i=NULL;          /* Imaginary part of the transformed data */
  double *data_r=NULL;          /* Real part of the transformed data */
  smfData *fdata=NULL;          /* Transform of data */
  dim_t fdims[2]={0,0};         /* Frequency dimensions */
  size_t i;                     /* loop counter */
  size_t ndims=0;               /* Number of real-space dimensions */
  size_t nfdata;                /* Total number of frequency data points */

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

  /* Transform the data */
  fdata = smf_fft_data( wf, data, NULL, 0, 0, status );

  /* Apply the frequency-domain filter. */
  if( *status == SAI__OK ) {
    data_r = fdata->pntr[0];
    data_i = data_r + nfdata;

    if( filt->isComplex ) {
      double ac, bd, aPb, cPd;
      for( i=0; i<nfdata; i++ ) {
        ac = data_r[i] * filt->real[i];
        bd = data_i[i] * filt->imag[i];

        aPb = data_r[i] + data_i[i];
        cPd = filt->real[i] + filt->imag[i];
      }
    } else {
      for( i=0; i<nfdata; i++ ) {
        data_r[i] *= filt->real[i];
        data_i[i] *= filt->real[i];
      }
    }
  }

  /* Transform back */
  smf_fft_data( wf, fdata, data, 1, 0, status );

  /* Return the filter to its original state if required */
  if( complement == -1 ) smf_filter_complement( filt, status );

}
