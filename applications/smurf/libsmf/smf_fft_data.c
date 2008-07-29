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
*     2008-07-28 (EC):
*        -Calculate correct ntslice for inverse.
*        -Code stub for generation of 4-d WCS of forward transformation.
*     2008-07-29 (EC):
*        Calculate WCS for 4-d transformed data.
*     2008-07-29 (TIMJ):
*        Steptime is now in smfHead.
*     {enter_further_changes_here}

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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "fftw3.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_fft_data"

smfData *smf_fft_data( const smfData *indata, int inverse, int *status ) {
  double *baseR=NULL;           /* base pointer to real part of transform */
  double *baseI=NULL;           /* base pointer to imag part of transform */
  double *baseB=NULL;           /* base pointer to bolo in time domain */
  AstUnitMap *cmapping=NULL;    /* Mapping from grid to curframe3d */
  AstFrame *curframe1d=NULL;    /* Current 1d frame (real/imag coeff) */
  AstFrame *curframe2d=NULL;    /* Current 2d frame (bolo x,y) */
  AstCmpFrame *curframe3d=NULL; /* Current 3d frame (x,y,coeff) */
  AstCmpFrame *curframe4d=NULL; /* Current Frame for 4-d FFT */
  smfData *data=NULL;           /* pointer to bolo-ordered data */
  double df=0;                  /* Frequency step size in Hz */
  AstCmpMap *fftmapping=NULL;   /* Mapping from GRID to curframe2d */
  dim_t i;                      /* Loop counter */
  fftw_iodim iodim;             /* I/O dimensions for transformations */
  int isFFT=0;                  /* Are the input data freq. domain? */
  AstCmpMap *mapping3d=NULL;    /* Mapping from 3d GRID to FREQ, X, Y */
  dim_t nbolo=0;                /* Number of detectors  */
  dim_t ndata=0;                /* Number of elements in new array */
  dim_t nf=0;                   /* Number of frequencies in FFT */
  double norm=1.;               /* Normalization factor for the FFT */
  dim_t ntslice=0;              /* Number of time slices */
  fftw_plan plan;               /* plan for FFT */
  smfData *retdata=NULL;        /* Pointer to new transformed smfData */
  AstZoomMap *scalemapping=NULL;/* Scale grid coordinates by df */
  AstSpecFrame *specframe=NULL; /* Current Frame of 1-D spectrum */
  AstCmpMap *specmapping=NULL;  /* Mapping from GRID to FREQ */
  double steptime;              /* Length of a sample in seconds */
  double *val=NULL;             /* Element of data to be normalized */
  AstFrameSet *tswcs=NULL;      /* WCS for 4d FFT data */
  double zshift2[3];            /* Amount by which to shift bolo origin */
  double zshift;                /* Amount by which to shift freq. origin */
  AstShiftMap *zshiftmapping=NULL;  /* Map to shift origin of freq. GRID */
  AstShiftMap *zshiftmapping2=NULL; /* Map to shift origin of bolo GRID */

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
    errRep( FUNC_NAME, 
            "Data is not double precision, must be flat-fielded first", 
            status );
    return NULL;
  }

  /* Create a copy of the input data since FFT operations often do
     calculations in-place */
  data = smf_deepcopy_smfData( indata, 0, SMF__NOCREATE_VARIANCE | 
                               SMF__NOCREATE_QUALITY | 
                               SMF__NOCREATE_FILE |
                               SMF__NOCREATE_DA, status );

  /* Re-order a time-domain cube if needed */
  if( indata->isTordered && (indata->ndims == 3) ) {
    smf_dataOrder( data, 0, status );
  } 

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
    nbolo=1;
    isFFT = smf_isfft( data, &ntslice, status );
  } else if( (data->ndims==4) && (data->dims[3]==2) ) {
    /* 3-d FFT of entire subarray */
    nf = data->dims[0];
    nbolo=data->dims[1]*data->dims[2];
    isFFT = smf_isfft( data, &ntslice, status );
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

    /* Returned data is always bolo-ordered */
    retdata->isTordered=0;

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

      if( *status == SAI__OK ) {
        for( i=0; i<nbolo; i++ ) {
          /* Transform bolometers one at a time */
          baseB = data->pntr[0];
          baseB += i*ntslice;        
          
          baseR = retdata->pntr[0];
          baseR += i*nf;
          
          baseI = baseR + nf*nbolo;
          fftw_execute_split_dft_r2c( plan, baseB, baseR, baseI );
        }

        /* Each sample needs to have a normalization applied */
        norm = 1. / (double) ntslice; 
        
        val = retdata->pntr[0];
        for( i=0; i<nf*nbolo*2; i++ ) {
          *val *= norm;
          val++;
        }

        /* Setup the WCS for the FFT of data cube */

        if( indata->hdr && retdata->hdr && (retdata->ndims==4) ) {
          steptime = retdata->hdr->steptime;
          if( steptime < 0 ) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, 
                   "FITS header error: STEPTIME must be > 0",
                   status);
          }

          if( *status == SAI__OK ) {
            /* Frequency steps in the FFT */
            df = 1. / (steptime * (double) ntslice );

             /* Start an AST context */
            astBegin;
            
            /* Create a new astFrameSet containing a 4d base GRID frame */
            tswcs = astFrameSet( astFrame( 4, "Domain=GRID" ), "" );
            
            /* The current frame will have frequency along the first axis,
               x, y bolo coordinates along the second and third axes,
               and the component along a fourth axis of length two (real,
               imaginary coefficients). */
            
            specframe = astSpecFrame( "System=freq,Unit=Hz,"
                                      "StdOfRest=Topocentric" );

            curframe2d = astFrame( 2, "Domain=BOLO" ); /* x, y, component */
            curframe3d = astCmpFrame( specframe, curframe2d, "" );            
            curframe1d = astFrame( 1, "Domain=COEFF"); /* real/imag component*/
            curframe4d = astCmpFrame( curframe3d, curframe1d, "" );

            /* The mapping from 4d grid coordinates to (frequency, x,
               y, coeff) is accomplished with a shift and a zoommap
               for the 1st dimension, and a shift for the others */

            zshift = -1;
            zshiftmapping = astShiftMap( 1, &zshift, "" ); 
            scalemapping = astZoomMap( 1, df, "" );
            specmapping = astCmpMap( zshiftmapping, scalemapping, 1, "" );
            
            zshift2[0] = -1; /* Set BOLO origin to 0, 0 */
            zshift2[1] = -1;
            zshiftmapping2 = astShiftMap( 2, zshift2, "" );

            mapping3d = astCmpMap( specmapping, zshiftmapping2, 0, "" );

            cmapping = astUnitMap( 1, "" );
            fftmapping = astCmpMap( mapping3d, cmapping, 0, "" );

            /* Add the curframe4d with the fftmapping to the frameset */
            astAddFrame( tswcs, AST__BASE, fftmapping, curframe4d );

            /* Export the frameset before ending the AST context */
            astExport( tswcs );
            astEnd;

            /* Free the old TSWCS if it exists, and insert the new TSWCS */
            if( retdata->hdr->tswcs ) {
              retdata->hdr->tswcs = astFree(retdata->hdr->tswcs);
            }
            retdata->hdr->tswcs = tswcs;
          }
        }
      }
    }
  }
  
 CLEANUP:
  if( data ) data = smf_free( data, status );

  return retdata;

}
