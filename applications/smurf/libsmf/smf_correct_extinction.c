/*
*+
*  Name:
*     smf_correct_extinction

*  Purpose:
*     Low-level EXTINCTION correction function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_correct_extinction(smfData *data, const char *method, 
*                            const int quick, double tau, double *allextcorr, 
*                            int *status) {

*  Arguments:
*     data = smfData* (Given)
*        smfData struct
*     method = const char* (Given)
*        Extinction correction method
*     quick = const int (Given)
*        Flag to denote whether to use a single airmass for all measurements
*     tau = double (Given)
*        Optical depth
*     allextcorr = double* (Given and Returned)
*        If given, store calculated corrections for each bolo/time slice. Must
*        have same dimensions as bolos in *data
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main low-level routine implementing the EXTINCTION
*     task. If allextcorr is specified, no correction is applied to
*     the data (the correction factors are just stored). 

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (TIMJ)
*     Ed Chapin (UBC))
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (AGG):
*        Initial test version
*     2005-11-14 (AGG):
*        Update API to accept a smfData struct
*     2005-12-20 (AGG):
*        Store the current coordinate system on entry and reset on return.
*     2005-12-21 (AGG):
*        Use the curslice as a frame offset into the timeseries data
*     2005-12-22 (AGG):
*        Deprecate use of curslice, make use of virtual flag instead
*     2006-01-11 (AGG):
*        API updated: tau no longer needed as it is retrieved from the
*        header. For now, image data uses the MEANWVM keyword from the
*        FITS header
*     2006-01-12 (AGG):
*        API update again: tau will be needed in the case the user
*        supplies it at the command line
*     2006-01-24 (AGG):
*        Floats to doubles
*     2006-01-25 (AGG):
*        Code factorization to simplify logic. Also corrects variance
*        if present.
*     2006-01-25 (TIMJ):
*        1-at-a-time astTran2 is not fast. Rewrite to do the astTran2
*        a frame at a time. Speed up from 85 seconds to 2 seconds.
*     2006-02-03 (AGG):
*        Add the quick flag. Not pretty but it gives a factor of 2 speed up
*     2006-02-07 (AGG):
*        Can now use the WVMRAW mode for getting the optical
*        depth. Also calculate the filter tau from the CSO tau in the
*        CSOTAU method
*     2006-02-17 (AGG):
*        Store and monitor all three WVM temperatures
*     2006-04-21 (AGG):
*        Check and update history if routine successful
*     2006-07-04 (AGG):
*        Update calls to slaAirmas to reflect new C interface
*     2006-07-26 (TIMJ):
*        sc2head no longer used. Use JCMTState instead.
*     2007-03-05 (EC):
*        Return allextcorr.
*        Check for existence of header.
*     2007-03-22 (AGG):
*        Check for incompatible combinations of data and parameters
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-04-03 (EC):
*        Assert time-ordered (ICD compliant) data
*     2008-04-29 (AGG):
*        Code reorg to ensure correct status handling
*     2008-04-30 (EC):
*        - Extra valid pointer checks
*        - Removed time-ordered data assertion. Handle bolo-ordered case.
*     2008-05-03 (EC):
*        - Only write history if applying extinction (!allextcorr case)
*     2008-06-12 (TIMJ):
*        - fix compiler warnings. origsystem must be copied in case
*        another call to astGetC is inserted in the code without thought.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2005-2008 University of British
*     Columbia. All Rights Reserved.

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

/* Standard includes */
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "star/slalib.h"
#include "star/one.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_correct_extinction"

void smf_correct_extinction(smfData *data, const char *method, const int quick,
                            double tau, double *allextcorr, int *status) {

  /* Local variables */
  double airmass;          /* Airmass */
  size_t base;             /* Offset into 3d data array */
  dim_t indexb;            /* bolo-ordered array offset */
  double extcorr = 1.0;    /* Extinction correction factor */
  char filter[81];         /* Name of filter */
  smfHead *hdr = NULL;     /* Pointer to full header struct */
  dim_t i;                 /* Loop counter */
  double *indata = NULL;   /* Pointer to data array */
  dim_t index;             /* index into vectorized data array */
  size_t *indices = NULL;  /* Index into data array */
  int isTordered;          /* data order of input data */
  dim_t j;                 /* Loop counter */
  dim_t k;                 /* Loop counter */
  size_t ndims;            /* Number of dimensions in input data */
  size_t newtau = 0;       /* Flag to denote whether to calculate a
                              new tau from the WVM data */
  double newtwvm[3];       /* Array of WVM temperatures */
  size_t nframes = 0;      /* Number of frames */
  size_t npts = 0;         /* Number of data points */
  dim_t nx = 0;            /* # pixels in x-direction */
  dim_t ny = 0;            /* # pixels in y-direction */
  double oldtwvm[3] = {0.0, 0.0, 0.0}; /* Cached value of WVM temperatures */
  char origsystem[10];  /* Character string to store the coordinate system */
  double *vardata = NULL;  /* Pointer to variance array */
  AstFrameSet *wcs = NULL; /* Pointer to AST WCS frameset */
  int wvmr = 0;            /* Flag to denote whether the WVMRAW method
                              is to be used */
  double *xin = NULL;      /* X coordinates of input mapping */
  double *xout = NULL;     /* X coordinates of output */
  double *yin = NULL;      /* Y coordinates of input */
  double *yout = NULL;     /* Y coordinates of output */
  int z;                   /* Counter for number of data points */
  double zd;               /* Zenith distance */

  /* Check status */
  if (*status != SAI__OK) return;

  if( smf_history_check( data, FUNC_NAME, status) ) {
    /* If caller not requesting allextcorr fail here */
    if( !allextcorr ) {
      msgSetc("F", FUNC_NAME);
      msgOutif(MSG__VERB," ", 
               "^F has already been run on these data, returning to caller", 
               status);
      return;
    }
  }

  /* Acquire the data order */
  isTordered = data->isTordered;

  /* Do we have 2-D image data? */
  ndims = data->ndims;
  if (ndims == 2) {
    nframes = 1;
  } else if (ndims == 3 ) {
    if( isTordered ) {
      nframes = (data->dims)[2];
    } else {
      nframes = (data->dims)[0];
    }
  } else {
    /* Abort with an error if the number of dimensions is not 2 or 3 */
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti("ND", data->ndims);
      errRep(FUNC_NAME,
             "Number of dimensions of input file is ^ND: should be either 2 or 3",
             status);
    }
  }

  /* Tell user we're correcting for extinction */
  msgSetc("M", method);
  msgOutif(MSG__VERB," ", 
           "Correcting for extinction with method ^M", status);

  /* Should check data type for double if not allextcorr case */
  if( !allextcorr ) {
    smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status);
    if ( *status != SAI__OK) return;
  }

  /* Check desired optical depth method */
  if ( strncmp( method, "WVMR", 4) == 0 ) {
    wvmr = 1;
  }

  /* Check that we're not trying to use the WVM for 2-D data */
  if ( ndims == 2 && wvmr ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Method WVMRaw can not be used on 2-D image data", status );
    }
  }

  /* If we have a CSO Tau then convert it to the current filter */
  if ( strncmp( method, "CSOT", 4) == 0 ) {
    hdr = data->hdr;
    if( hdr == NULL ) {
      if ( *status == SAI__OK ) {
        *status = SAI__ERROR;
        errRep( FUNC_NAME, "Input data has no header", status);
      }
    }
    smf_fits_getS( hdr, "FILTER", filter, 81, status);
    tau = smf_scale_tau( tau, filter, status);
  }

  /* Assign pointer to input data array if status is good */
  if ( *status == SAI__OK ) {
    indata = (data->pntr)[0]; 
    vardata = (data->pntr)[1];

    if( isTordered ) {
      nx = (data->dims)[0];
      ny = (data->dims)[1];
    } else {
      nx = (data->dims)[1];
      ny = (data->dims)[2];
    }
    npts = nx*ny;
  }
  /* It is more efficient to call astTran2 with all the points
     rather than one point at a time */
  if (!quick) {
    xin = smf_malloc( npts, sizeof(double), 0, status );
    yin = smf_malloc( npts, sizeof(double), 0, status );
    xout = smf_malloc( npts, sizeof(double), 0, status );
    yout = smf_malloc( npts, sizeof(double), 0, status );
  }
  indices = smf_malloc( npts, sizeof(size_t), 0, status );

  /* Jump to the cleanup section if status is bad by this point
     since we need to free memory */
  if (*status != SAI__OK) goto CLEANUP;

  /* Prefill with coordinates */
  z = 0;
  for (j = 0; j < ny; j++) {
    base = j * nx;
    for (i = 0; i < nx; i++) {
      if (!quick) {
        xin[z] = (double)i + 1.0;
        yin[z] = (double)j + 1.0;
      }
      indices[z] = base + i; /* index into data array */
      z++;
    }
  }

  /* Loop over number of time slices/frames */

  indexb = 0;
  for ( k=0; k<nframes && (*status == SAI__OK) ; k++) {
    /* Call tslice_ast to update the header for the particular
       timeslice. If we're in QUICK mode then we don't need the WCS */
    if (quick) {
      smf_tslice_ast( data, k, 0, status );
    } else {
      smf_tslice_ast( data, k, 1, status );
    }
    /* Retrieve header info */
    hdr = data->hdr;
    if( hdr == NULL ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Data has no header after attempt to update", status);
    } else {
      /* See if we have a new WVM value */
      if (wvmr) {
        newtwvm[0] = hdr->state->wvm_t12;
        newtwvm[1] = hdr->state->wvm_t42;
        newtwvm[2] = hdr->state->wvm_t78;
        /* Have any of the temperatures changed? */
        if ( (newtwvm[0] != oldtwvm[0]) || 
             (newtwvm[1] != oldtwvm[1]) || 
             (newtwvm[2] != oldtwvm[2]) ) {
          newtau = 1;
          oldtwvm[0] = newtwvm[0];
          oldtwvm[1] = newtwvm[1];
          oldtwvm[2] = newtwvm[2];
        } else {
          newtau = 0;
        }
        if (newtau) {
          tau = smf_calc_wvm( hdr, status );
          newtau = 0;
          /* Check status and/or value of tau */
          if ( tau == VAL__BADD ) {
            if ( *status == SAI__OK ) {
              *status = SAI__ERROR;
              errRep("", "Error calculating tau from WVM temperatures", 
                     status);
            }
          }
        }
      }

      /* If we're using the QUICK application method, we assume a single
         airmass and tau for the whole array */
      if (quick) {
        /* For 2-D data, get airmass from the FITS header rather than
           the state structure */
        if ( ndims == 2 ) {
          /* This may change depending on exact FITS keyword */
          smf_fits_getD( hdr, "AMSTART", &airmass, status );
        } else {
          /* Else use airmass value in state structure */
          airmass = hdr->state->tcs_airmass;
        }
        extcorr = exp(airmass*tau);
      } else {
        /* Not using quick so retrieve WCS to obtain elevation info */
        wcs = hdr->wcs;
        /* Check current frame, store it and then select the AZEL
           coordinate system */
        if (wcs != NULL) {
          one_strlcpy( origsystem, astGetC(wcs,"SYSTEM"),
                       sizeof(origsystem), status);
          astSetC( wcs, "SYSTEM", "AZEL" );
          /* Transfrom from pixels to AZEL */
          astTran2(wcs, npts, xin, yin, 1, xout, yout );
          if (!astOK) {
            if (*status == SAI__OK) {
              *status = SAI__ERROR;
              errRep( FUNC_NAME, "Error from AST when attempting to set SYSTEM to AZEL: WCS is NULL", status);
            }
          }
        } else {
          /* Throw an error if no WCS */
          if ( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep("", "Error: input file has no WCS", status);
          }
        }
      } 
      /* Loop over data in time slice. Start counting at 1 since this is
         the GRID coordinate frame */
      base = npts * k;  /* Offset into 3d data array (time-ordered) */
      indexb = k;       /* Offset into 3d data array (bolo-ordered) */

      for (i=0; i < npts && ( *status == SAI__OK ); i++ ) {
        /* update array indices */
        index = indices[i] + base;

        if (!quick) {
          zd = M_PI_2 - yout[indices[i]];
          airmass = slaAirmas( zd );
          extcorr = exp(airmass*tau);
        }
	
        if( allextcorr ) {
          /* Store extinction correction factor */
          if( isTordered ) {
            allextcorr[index] = extcorr;
          } else {
            allextcorr[indexb] = extcorr;
          }
        } else {
          /* Otherwise Correct the data */
          if( indata && (indata[index] != VAL__BADD) ) {
            if( isTordered ) {
              indata[index] *= extcorr;
            } else {
              indata[indexb] *= extcorr;
            }
          }

          /* Correct the variance */
          if( vardata && (vardata[index] != VAL__BADD) ) {
            if( isTordered ) {
              vardata[index] *= extcorr*extcorr;
            } else {
              vardata[indexb] *= extcorr*extcorr;
            }
          }
        }

        /* Incremeber bolo-ordered data index */
        indexb += nframes;
      }
      
      if (!quick) {
        /* Restore coordinate frame to original */
        if ( *status == SAI__OK) {
          astSetC( wcs, "SYSTEM", origsystem );
          /* Check AST status */
          if (!astOK) {
            if (*status == SAI__OK) {
              *status = SAI__ERROR;
              errRep( FUNC_NAME, "Error from AST when attempting to return to input coordinate system", status);
            }
          }
        }
      }
    }    
  } /* End loop over timeslice */

  /* Add history entry if !allextcorr */
  if( (*status == SAI__OK) && !allextcorr ) {
    smf_history_add( data, FUNC_NAME, 
                     "Extinction correction successful", status);
  }

 CLEANUP:
  if (!quick) {
    xin = smf_free(xin,status);
    yin = smf_free(yin,status);
    xout = smf_free(xout,status);
    yout = smf_free(yout,status);
  }
  indices = smf_free(indices,status);
}
