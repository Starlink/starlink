/*
*+
*  Name:
*     smf_fft_data

*  Purpose:
*     Calculate the forward or inverse FFT of a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_fft_data( const smfData *indata, int inverse, int *status );

*  Arguments:
*     indata = smfData * (Given)
*        Pointer to the input smfData
*     inverse = int (Given)
*        If set perform inverse transformation. Otherwise forward.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     Pointer to newly created smfData containing the forward or inverse
*     transformed data.

*  Description: 
*     Perform the forward or inverse FFT of a smfData. In the time
*     domain the data may be 1-d (e.g. a single bolometer), or 3-d
*     (either x,y,time or time,x,y depending on isTordered flag). The
*     frequency domain representation of the data is 2-d
*     (frequency,component) if the input was 1-d, and 4-d if the input was
*     3-d (always frequency,x,y,component -- i.e. bolo-ordered). Component
*     is an axis of length 2 containing the real and imaginary parts. 
*     Inverse transforms always leave the data in bolo-ordered format. If the
*     data are already transformed, this routine returns a NULL pointer.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-07-09 (EC):
*        Initial version
*     2008-07-23 (EC):
*        Forward transformations now seem to work.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "fftw3.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_create_smfFilter"

smfData *smf_fft_data( const smfData *indata, int inverse, int *status ) {
  double *baseR=NULL;           /* base pointer to real part of transform */
  double *baseI=NULL;           /* base pointer to imag part of transform */
  double *baseB=NULL;           /* base pointer to bolo in time domain */
  const smfData *data=NULL;     /* pointer to bolo-ordered data */
  dim_t i;                      /* Loop counter */
  fftw_iodim iodim;             /* I/O dimensions for transformations */
  int isFFT=0;                  /* Are the input data freq. domain? */
  dim_t nbolo=0;                /* Number of detectors  */
  dim_t ndata=0;                /* Number of elements in new array */
  dim_t ntslice=0;              /* Number of time slices */
  dim_t nf=0;                   /* Number of frequencies in FFT */
  fftw_plan plan;               /* plan for FFT */
  smfData *retdata=NULL;        /* Pointer to new transformed smfData */
  smfData *tempdata=NULL;       /* Temporary data pointer */

  if (*status != SAI__OK) return NULL;

  /* Check for NULL pointer */
  if( indata == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData pointer is NULL", status );
    return NULL;
  }

  /* Check for double-precision data */
  if( indata->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData is not double precision", status );
    return NULL;
  }

  /* Create a copy of the input data since FFT operations often do
     calculations in-place */
  tempdata = smf_deepcopy_smfData( indata, 0, SMF__NOCREATE_VARIANCE | 
                                   SMF__NOCREATE_QUALITY | 
                                   SMF__NOCREATE_FILE |
                                   SMF__NOCREATE_DA, status );

  /* Re-order a time-domain cube if needed */
  if( indata->isTordered && (indata->ndims == 3) ) {
    smf_dataOrder( tempdata, 0, status );
  } 

  data = tempdata;

    
  /* Data dimensions. Time dimensions are either 1-d or 3-d. Frequency
     dimensions are either 2- or 4-d to store the real and imaginary
     parts along the last index. */

  if( data->ndims == 3 ) {
    nbolo = data->dims[1]*data->dims[2];
    ntslice = data->dims[0];
    nf = ntslice/2+1;
    isFFT = 0;
  } else if( data->ndims == 1 ) {
    /* If 1-d data, only one axis to choose from */
    ntslice = data->dims[0];
    nf = ntslice/2+1;
    nbolo=1;
    isFFT = 0;
  } else if( (data->ndims==2) && (data->dims[1]==2) ) {
    /* 1-d FFT of a single bolo */
    nf = data->dims[0];
    ntslice = (nf-1)*2;
    nbolo=1;
    isFFT = 1;
  } else if( (data->ndims==4) && (data->dims[3]==2) ) {
    /* 3-d FFT of entire subarray */
    nf = data->dims[0];
    ntslice = (nf-1)*2;
    nbolo=data->dims[1]*data->dims[2];
    isFFT = 1;
  } else {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData has strange dimensions", status );
  }

  /* If the data are already transformed to the requested domain return
     a NULL pointer */
     
  if( (*status==SAI__OK) && (isFFT != inverse) ) {
    retdata = NULL;
    goto CLEANUP;
  }

  /* Create a new smfData, copying over everything except for the bolo
     data itself */

  retdata = smf_deepcopy_smfData( data, 0, SMF__NOCREATE_DATA |
                                  SMF__NOCREATE_VARIANCE | 
                                  SMF__NOCREATE_QUALITY | 
                                  SMF__NOCREATE_FILE |
                                  SMF__NOCREATE_DA, status );

  if( *status == SAI__OK ) {
      
    /* Allocate space for the transformed data */

    if( inverse ) {
      /* Doing an inverse FFT to the time domain */
      if( nbolo == 1 ) {
        retdata->ndims = 1;
        retdata->dims[0] = ntslice;
      } else {
        retdata->ndims = 3;
        retdata->dims[0] = ntslice;
        retdata->dims[1] = data->dims[1];
        retdata->dims[2] = data->dims[2];
      }
    } else {
      /* Doing a forward FFT to the frequency domain */
      if( nbolo == 1 ) {
        retdata->ndims = 2;
        retdata->dims[0] = nf;
        retdata->dims[1] = 2;
      } else {
        retdata->ndims = 4;
        retdata->dims[0] = nf;
        retdata->dims[1] = data->dims[1];
        retdata->dims[2] = data->dims[2];
        retdata->dims[3] = 2;
      }
    }

    ndata=1;
    for( i=0; i<retdata->ndims; i++ ) {
      ndata *= retdata->dims[i];
    }    

    retdata->pntr[0] = smf_malloc( ndata, smf_dtype_sz(retdata->dtype,status), 
                                   1, status );

    /* Describe the array dimensions for FFTW guru interface  */
    iodim.n = ntslice;
    iodim.is = 1;
    iodim.os = 1;

    if( inverse ) {        /* Perform inverse fft */
      /* Setup inverse FFT plan using guru interface */
      baseR = data->pntr[0];
      baseI = baseR + nf*nbolo;
      baseB = retdata->pntr[0];

      plan = fftw_plan_guru_split_dft_c2r( 1, &iodim, 0, NULL,
                                           baseR, baseI, 
                                           baseB, 
                                           FFTW_ESTIMATE | FFTW_UNALIGNED);

      if( !plan ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, 
               "FFTW3 could not create plan for inverse transformation",
               status);
      }

      for( i=0; (*status==SAI__OK)&&(i<nbolo); i++ ) {
        /* Transform bolometers one at a time */
        baseR = data->pntr[0];
        baseR += i*nf;

        baseI = baseR + nf*nbolo;

        baseB = retdata->pntr[0];
        baseB += i*ntslice;        

        fftw_execute_split_dft_c2r( plan, baseR, baseI, baseB );
      }

    } else {               /* Perform forward fft */

      /* Setup forward FFT plan using guru interface */
      baseB = data->pntr[0];
      baseR = retdata->pntr[0];
      baseI = baseR + nf*nbolo;

      plan = fftw_plan_guru_split_dft_r2c( 1, &iodim, 0, NULL,
                                           baseB, 
                                           baseR, baseI, 
                                           FFTW_ESTIMATE | FFTW_UNALIGNED);

      if( !plan ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, 
               "FFTW3 could not create plan for inverse transformation",
               status);
      }

      for( i=0; (*status==SAI__OK)&&(i<nbolo); i++ ) {
        /* Transform bolometers one at a time */
        baseB = data->pntr[0];
        baseB += i*ntslice;        

        baseR = retdata->pntr[0];
        baseR += i*nf;

        baseI = baseR + nf*nbolo;
        fftw_execute_split_dft_r2c( plan, baseB, baseR, baseI );
      }

    }
    
    /* Fix up the time/frequency axis in the tswcs */

    

  }
  
 CLEANUP:
  if( tempdata ) tempdata = smf_free( tempdata, status );

  return retdata;

}
