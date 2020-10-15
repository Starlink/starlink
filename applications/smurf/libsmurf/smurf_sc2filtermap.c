/*
*+
*  Name:
*     SC2FILTERMAP

*  Purpose:
*     Filter a 2-d map

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2mapfft( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine takes the FFT of a 2D map, applies a Fourier-space filter,
*     and then transforms back to real-space before writing out. Currently
*     the only available filter is a whitening filter which is measured
*     using a supplied reference image.

*  Notes:
*     Transforming data loses the VARIANCE and QUALITY components.

*  ADAM Parameters:
*     FILT_EDGEHIGH = _REAL (Write)
*          High-pass filter frequency (1/arcsec)
*     FILT_EDGELOW = _REAL (Write)
*          Low-pass filter frequency (1/arcsec)
*     IN = NDF (Read)
*          Input files to be transformed.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output transformed files.
*     OUTFILTER = NDF (Write)
*          Optional NDF for the filter.
*     WHITEN = _LOGICAL (Read)
*          If selected, measure azimuthally-averaged angular power
*          spectrum in WHITEREFMAP, fit a model A/F^B + W, and apply
*          its complement to the FFT of the data (normalized to
*          W). AZAVSPEC is set implicitly. [FALSE]
*     WHITEREFMAP = NDF (Read)
*          Reference map in which to measure whitening filter. [FALSE]
*     ZEROBAD = _LOGICAL (Read)
*          Zero any bad values in the data before taking FFT. [TRUE]

*  Related Applications:
*     SMURF: SC2MAPFFT

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-10-24 (EC):
*        Initial version - based on sc2mapfft task

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "fftw3.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smurf_sc2filtermap"
#define TASK_NAME "SC2FILTERMAP"

void smurf_sc2filtermap( int *status ) {

  Grp *fgrp = NULL;         /* Output filter group */
  smfFilter *filt=NULL;     /* Filter */
  double filt_edgehigh=0;   /* High-pass filter */
  double filt_edgelow=0;    /* Low-pass filter */
  size_t fsize;             /* Number of files in fgrp */
  size_t i;                 /* Loop (grp) counter */
  smfData *idata;           /* Pointer to input smfData */
  Grp *igrp = NULL;         /* Input group of files */
  int isfft=0;              /* Are data fft or real space? */
  int *mask=NULL;           /* Mask indicating where bad data are */
  dim_t ndata=0;            /* Number of pixels in the map */
  int ndims;                /* Number of real space dimensions */
  smfData *odata=NULL;      /* Pointer to output smfData to be exported */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Number of files in output group */
  size_t size;              /* Number of files in input group */
  ThrWorkForce *wf = NULL;  /* Pointer to a pool of worker threads */
  smfData *wrefmap=NULL;    /* Whitening reference map */
  int whiten;               /* Applying whitening filter? */
  Grp *wgrp = NULL;         /* Whitening reference map group */
  size_t wsize;             /* Size of wgrp */
  int zerobad;              /* Zero VAL__BADD before taking FFT? */

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );
  size = grpGrpsz( igrp, status );

  if (size > 0) {
    int parstate=0;           /* ADAM parameter state */

    /* Get output file(s) */
    kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
               &ogrp, &outsize, status );

    /* Write out the filter? */
    parState( "OUTFILTER", &parstate, status );
    if( parstate != PAR__GROUND ) {
      kpg1Wgndf( "OUTFILTER", igrp, size, size,
                 "More output filter files required...",
                 &fgrp, &fsize, status );
    }

  }

  /* Are we going to zero bad values first? */
  parGet0l( "ZEROBAD", &zerobad, status );

  /* High/low-pass filters? */
  parGet0d( "FILT_EDGEHIGH", &filt_edgehigh, status );
  parGet0d( "FILT_EDGELOW", &filt_edgelow, status );

  /* Are we applying a spatial whitening filter? */
  parGet0l( "WHITEN", &whiten, status );

  if( whiten ) {
    /* We also need the reference map to measure the whitening filter. We
       make a deep copy of it so that we can set bad values to 0 etc. */

    smfData *tempdata=NULL;

    kpg1Rgndf( "whiterefmap", 0, 1, "", &wgrp, &wsize, status );
    if( (*status == SAI__OK) && (wsize != 1) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": WHITEREFMAP must be a single reference map",
              status );
    }

    smf_open_file( wf, wgrp, 1, "READ", SMF__NOTTSERIES, &tempdata, status );
    wrefmap = smf_deepcopy_smfData( wf, tempdata, 0, 0, 0, 0, status );
    smf_close_file( wf, &tempdata, status );

    /* Set VAL__BADD to zero if requested */
    if( (*status==SAI__OK) && zerobad ) {
      double *d=NULL;
      dim_t j;

      ndata=1;
      for( j=0; j<wrefmap->ndims; j++ ) ndata *= wrefmap->dims[j];

      d = wrefmap->pntr[0];

      if( d ) {
        for( j=0; j<ndata; j++ ) {
          if( d[j] == VAL__BADD ) {
            d[j] = 0;
          }
        }
      }
    }

  }

  for( i=1;(*status==SAI__OK)&&i<=size; i++ ) {
    smf_open_file( wf, igrp, i, "READ", SMF__NOTTSERIES, &idata, status );
    isfft = smf_isfft( idata, NULL, NULL, NULL, NULL, &ndims, status);

    if( (*status==SAI__OK) && isfft ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Input data are FFT, not real-space!\n",
              status );
      break;
    }

    if( (*status==SAI__OK) && (ndims != 2) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Input data not a 2D map!\n",
              status );
      break;
    }

    /* smf_filter_execute operates in-place, so first create the output
       data as a copy of the input */

    odata = smf_deepcopy_smfData( wf, idata, 0, 0, 0, 0, status );

    /* Set VAL__BADD to zero if requested */
    if( (*status==SAI__OK) && zerobad ) {
      double *d=NULL;
      dim_t j, k;

      ndata=1;
      for( j=0; j<odata->ndims; j++ ) ndata *= odata->dims[j];

      mask = astCalloc( ndata, sizeof(*mask) );

      /* Do both DATA and VARIANCE */
      if( *status == SAI__OK ) {
        for( k=0; k<2; k++ ) {
          d = odata->pntr[k];

          if( d ) {
            for( j=0; j<ndata; j++ ) {
              if( d[j] == VAL__BADD ) {
                d[j] = 0;
                mask[j] = 1;
              }
            }
          }
        }
      }

    }

    /* Measure and apply the whitening filter. We need to do this
       every time because the dimensions of filt need to match idata
       (not the wrefmap) and they might be different each time. We
       could try to be more clever in the future if this is too slow. */

    filt = smf_create_smfFilter( idata, status );
    /* Set to the identity in case no whitening is applied */
    msgOut( "", TASK_NAME ": initializing filter", status );
    smf_filter_ident( filt, 0, status );

    if( whiten ) {
      msgOut( "", TASK_NAME ": whitening the filter", status );
      smf_filter2d_whiten( wf, filt, wrefmap, 0, 0, 3, status );
    }

    if( filt_edgelow ) {
      msgOutf( "", TASK_NAME ": applying low-pass at < %lg 1/arcsec", status,
               filt_edgelow );
      smf_filter2d_edge( filt, filt_edgelow, 1, status );
    }

    if( filt_edgehigh ) {
      msgOutf( "", TASK_NAME ": applying high-pass at >= %lg 1/arcsec", status,
               filt_edgehigh );
      smf_filter2d_edge( filt, filt_edgehigh, 0, status );
    }

    smf_filter_execute( wf, odata, filt, 0, 0, status );

    /* Set bad values from the mask */
    if( mask ) {
      double *d=NULL;
      dim_t j, k;

      /* Do both DATA and VARIANCE */
      for( k=0; k<2; k++ ) {
        d = odata->pntr[k];

        if( d ) {
          for( j=0; j<ndata; j++ ) {
            if( mask[j] ) {
              d[j] = VAL__BADD;
            }
          }
        }
      }
    }

    /* Export the data to a new file */
    smf_write_smfData( wf, odata, NULL, NULL, ogrp, i, 0, MSG__NORM, 0,
                       NULL, NULL, status );

    /* Write out filters? */
    if( fgrp ) smf_write_smfFilter( wf, filt, NULL, fgrp, i, status );
    if( filt ) smf_free_smfFilter( filt, status );

  }

  /* Tidy up after ourselves */

  if( fgrp ) grpDelet( &fgrp, status);
  if( igrp ) grpDelet( &igrp, status);
  if( ogrp ) grpDelet( &ogrp, status);
  if( wgrp ) grpDelet( &wgrp, status );

  if( odata ) smf_close_file( wf, &odata, status );
  if( wrefmap ) smf_close_file( wf, &wrefmap, status );

  if( mask ) mask = astFree( mask );

  ndfEnd( status );

  /* Ensure that FFTW doesn't have any used memory kicking around */
  fftw_cleanup();
}
