/*
*+
*  Name:
*     SC2MAPFFT

*  Purpose:
*     Fourier Transform 2D maps

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
*     This routine performs the forward or inverse FFT of a 2D map.
*     The FFT of the data are stored in a 3-dimensional array with
*     dimensions xfrequency, yfrequency, component (where component is
*     a dimension of length 2 holding the real and imaginary parts, or
*     ampliude and phase if in polar form). The inverse flag is used
*     to transform back to the spatial domain from the frequency
*     domain.  If the data are already in the requested domain, the
*     ouput file is simply a copy of the input file.

*  Notes:
*     Transforming data loses the VARIANCE and QUALITY components.

*  ADAM Parameters:
*     AZAVPSPEC = _LOGICAL (Read)
*          If true, calculate the azimuthally-averaged angular power
*          power spectrum. POLAR and POWER are set implicitly. [FALSE]
*     IN = NDF (Read)
*          Input files to be transformed.
*     INVERSE = _LOGICAL (Read)
*          Perform inverse transform. [FALSE]
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output transformed files.
*     POLAR = _LOGICAL (Update)
*          Use polar representation (amplitude, argument) of
*          FFT. [FALSE]
*     POWER = _LOGICAL (Update)
*          Use polar representation of FFT with squared
*          amplitudes divided by the frequency bin spacing (gives a
*          power spectral density, PSD). [FALSE]
*     ZEROBAD = _LOGICAL (Read)
*          Zero any bad values in the data before taking FFT. [TRUE]

*  Related Applications:
*     SMURF: SC2FFT

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-09-22 (EC):
*        Initial version - based on sc2fft task
*     2011-09-26 (EC):
*        Add AZAVPSPEC option

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

#define FUNC_NAME "smurf_sc2mapfft"
#define TASK_NAME "SC2MAPFFT"

void smurf_sc2mapfft( int *status ) {

  int azavpspec=0;          /* Flag for doing az-averaged power spectrum */
  size_t i;                 /* Loop (grp) counter */
  smfData *idata;           /* Pointer to input smfData */
  Grp *igrp = NULL;         /* Input group of files */
  int inverse=0;            /* If set perform inverse transform */
  int isfft=0;              /* Are data fft or real space? */
  size_t ndims;             /* Number of real space dimensions */
  smfData *odata=NULL;      /* Pointer to output smfData to be exported */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Number of files in output group */
  int polar=0;              /* Flag for FFT in polar coordinates */
  int power=0;              /* Flag for squaring amplitude coeffs */
  size_t size;              /* Number of files in input group */
  ThrWorkForce *wf = NULL;  /* Pointer to a pool of worker threads */
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
    /* Get output file(s) */
    kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
               &ogrp, &outsize, status );
  }

  /* Are we doing an inverse transform? */
  parGet0l( "INVERSE", &inverse, status );

  /* Are we calculating the azimuthally-average power spectrum? */
  parGet0l( "AZAVPSPEC", &azavpspec, status );
  if( azavpspec ) {
    /* If azavpec set we also set power and polar */
    msgOutif( MSG__NORM, "", TASK_NAME ": AZAVPSPEC implies POWER", status );
    parPut0l( "POWER", 1, status );
    power = 1;
  } else {
    /* Are we calculating the power spectrum? */
    parGet0l( "POWER", &power, status );
  }

  if( power ) {
    /* If power set we also set polar */
    msgOutif( MSG__NORM, "", TASK_NAME ": POWER implies POLAR", status );
    parPut0l( "POLAR", 1, status );
     polar = 1;
  } else {
    /* Are we calculating FFT in polar form? */
    parGet0l( "POLAR", &polar, status );
   }

  /* Are we going to zero bad values first? */
  parGet0l( "ZEROBAD", &zerobad, status );




  for( i=1;(*status==SAI__OK)&&i<=size; i++ ) {
    smf_open_file( wf, igrp, i, "READ", SMF__NOTTSERIES, &idata, status );
    isfft = smf_isfft( idata, NULL, NULL, NULL, NULL, &ndims, status);

    if( (*status==SAI__OK) && (ndims != 2) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Input data are not a 2D map, nor its FFT!\n",
              status );
      break;
    }

    /* If zeroing VAL__BADD, make a local copy that we can modify */
    if( (*status==SAI__OK) && zerobad ) {
      double *d=NULL;
      smfData *tempdata=NULL;
      size_t j;
      size_t ndata;

      tempdata = smf_deepcopy_smfData( wf, idata, 0, 0, 0, 0, status );
      smf_close_file( wf, &idata, status );
      idata = tempdata;

      ndata=1;
      for( j=0; j<idata->ndims; j++ ) ndata *= idata->dims[j];

      d = idata->pntr[0];

      if( d ) {
        for( j=0; j<ndata; j++ ) {
          if( d[j] == VAL__BADD ) {
            d[j] = 0;
          }
        }
      }
    }

    /* Only do a transform if needed */
    if( (*status == SAI__OK) && (isfft == inverse) ) {

      /* If inverse transform convert to cartesian representation first */
      if( inverse && polar ) {
        smf_fft_cart2pol( wf, idata, 1, power, status );
      }

      /* Transform the data */
      odata = smf_fft_data( wf, idata, NULL, inverse, 0, status );
      smf_convert_bad( wf, odata, status );

      if( !inverse && polar ) {
        /* Store FFT of data in polar form */
        smf_fft_cart2pol( wf, odata, 0, power, status );
      }

      /* Calculate azimuthally-averaged angular power spectrum if requested */
      if( azavpspec ) {
        smfData *tempdata=NULL;

        tempdata = smf_fft_2dazav( odata, NULL, status );
        smf_close_file( wf, &odata, status );
        odata = tempdata;
      }

      /* Export the data to a new file */
      smf_write_smfData( wf, odata, NULL, NULL, ogrp, i, 0, MSG__VERB, 0,
                         NULL, NULL, status );
    }  else {
      msgOutif( MSG__NORM, " ",
                "Data are already transformed. No output will be produced",
                status );
    }

  }

  /* Tidy up after ourselves: release the resources used by the grp routines */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  smf_close_file( wf, &odata, status );

  ndfEnd( status );

  /* Ensure that FFTW doesn't have any used memory kicking around */
  fftw_cleanup();
}
