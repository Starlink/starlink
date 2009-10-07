/*
*+
*  Name:
*     CALCNOISE

*  Purpose:
*     Calculate noise image

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_calcnoise( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculates the white noise on the array by performing
*     an FFT to generate a power spectrum and then extracting the
*     data between two frequency ranges.

*  Notes:
*     Transforming data loses the VARIANCE and QUALITY components.

*  ADAM Parameters:
*     FREQ = _REAL (Given)
*          Frequency range (Hz) to use to calculate the white noise [2,10]
*     IN = NDF (Read)
*          Input files to be transformed. Files from the same sequence
*          will be combined.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output files. Number of output files may differ from the
*          number of input files.
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line). If a null (!) value is supplied no file is created. [!]

*  Related Applications:
*     SMURF: SC2CONCAT, SC2CLEAN, SC2FFT

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-10-01 (TIMJ):
*        Initial version - based on sc2fft task
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
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

#define FUNC_NAME "smurf_calcnoise"
#define TASK_NAME "CALCNOISE"

void smurf_calcnoise( int *status ) {

  Grp * basegrp = NULL;     /* Basis group for output filenames */
  smfArray *concat=NULL;     /* Pointer to a smfArray */
  size_t contchunk;          /* Continuous chunk counter */
  smfArray *darks = NULL;   /* dark frames */
  Grp *dkgrp = NULL;        /* Group of dark frames */
  size_t dksize = 0;        /* Number of darks found */
  Grp *fgrp = NULL;         /* Filtered group, no darks */
  size_t gcount=0;           /* Grp index counter */
  size_t i=0;               /* Counter, index */
  smfData *idata=NULL;      /* Pointer to input smfData */
  Grp *igrp = NULL;         /* Input group of files */
  smfGroup *igroup=NULL;     /* smfGroup corresponding to igrp */
  int inverse=0;            /* If set perform inverse transform */
  char fname[GRP__SZNAM+1]; /* Name of container file without suffix */
  dim_t maxconcat=0;         /* Longest continuous chunk length in samples */
  size_t ncontchunks=0;      /* Number continuous chunks outside iter loop */
  smfData *odata=NULL;      /* Pointer to output smfData to be exported */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Total number of NDF names in the output group */
  char *pname=NULL;         /* Poiner to fname */
  int polar=0;              /* Flag for FFT in polar coordinates */
  int power=0;              /* Flag for squaring amplitude coeffs */
  size_t size;              /* Number of files in input group */
  smfWorkForce *wf = NULL;  /* Pointer to a pool of worker threads */

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = smf_create_workforce( smf_get_nthread( status ), status );

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_darks( igrp, &fgrp, &dkgrp, 1, SMF__NULL, &darks, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  dksize = grpGrpsz( dkgrp, status );
  grpDelet( &igrp, status );

  /* If we have all darks then we assume it's the dark files
     that we are actually wanting to use for the noise. Otherwise
     assume that the darks are to be ignored. */
  if (size > 0) {
    igrp = fgrp;
    fgrp = NULL;
    grpDelet( &dkgrp, status ); /* no longer needed */
  } else {
    msgOutif( MSG__NORM, " ", TASK_NAME ": Calculating noise properties of darks",
              status );
    size = dksize;
    igrp = dkgrp;
    dkgrp = NULL;
    grpDelet( &fgrp, status );
    smf_close_related( &darks, status );
  }

  /* We now need to combine files from the same subarray and same sequence
     to form a continuous time series */
  smf_grp_related( igrp, size, 1, 0, &maxconcat, &igroup,
                   &basegrp, status );

  /* Get output file(s) */
  size = grpGrpsz( basegrp, status );
  kpg1Wgndf( "OUT", basegrp, size, size, "More output files required...",
             &ogrp, &outsize, status );

  /* Obtain the number of continuous chunks and subarrays */
  if( *status == SAI__OK ) {
    ncontchunks = igroup->chunk[igroup->ngroups-1]+1;
  }
  msgOutiff( MSG__NORM, "", "Found %d continuous chunk%s", status, ncontchunks,
             (ncontchunks > 1 ? "s" : "") );

  /* Loop over input data as contiguous chunks */
  gcount = 1;
  for( contchunk=0;(*status==SAI__OK)&&contchunk<ncontchunks; contchunk++ ) {
    size_t idx;

    /* Concatenate this continuous chunk but forcing a raw data read.
       We will need quality. */
    smf_concat_smfGroup( wf, igroup, darks, NULL, contchunk, 0, 1, NULL, 0, NULL,
                         NULL, 0, 0, 0, &concat, status );

    /* Now loop over each subarray */
    /* Export concatenated data for each subarray to NDF file */
    for( idx=0; (*status==SAI__OK)&&idx<concat->ndat; idx++ ) {
      if( concat->sdata[idx] ) {
        smfData *thedata = concat->sdata[idx];
        smfData *outdata = NULL;
        dim_t nelem = 0;

        /* Remove baselines */
        smf_scanfit( thedata, NULL, 0, status );
        smf_subtract_poly( thedata, NULL, 0, status );

        /* Convert the data to amps */
        smf_scalar_multiply( thedata, RAW2CURRENT, status );

        /* Apodize */
        smf_apodize(thedata, NULL, (thedata->dims)[2] / 2, status );

        /* Create the output file */
        smf_create_bolfile( ogrp, gcount, thedata, "Noise", SIPREFIX "A Hz**-0.5",
                            &outdata, status );

        smf_bolonoise( NULL, thedata, NULL, 0, 0.5,
                       SMF__F_WHITELO, SMF__F_WHITEHI, 0,
                       (outdata->pntr)[0], NULL, 1, status );

        /* Bolonoise gives us a variance - we want square root */
        for (i = 0; i < (outdata->dims)[0]*(outdata->dims)[1]; i++) {
          double * od = (outdata->pntr)[0];
          if ( od[i] != VAL__BADD ) od[i] = sqrt( od[i] );
        }

        smf_close_file( &outdata, status );

      } else {
        *status = SAI__ERROR;
        errRepf( FUNC_NAME,
                "Internal error obtaining concatenated data set for chunk %d",
                 status, contchunk );
      }

      /* Increment the group index counter */
      gcount++;
    }

    /* Close the smfArray */
    smf_close_related( &concat, status );

  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines */
 CLEANUP:
  if (igrp) grpDelet( &igrp, status);
  if (ogrp) grpDelet( &ogrp, status);
  if (basegrp) grpDelet( &basegrp, status );
  if( igroup ) smf_close_smfGroup( &igroup, status );
  if (darks) smf_close_related( &darks, status );
  if( wf ) wf = smf_destroy_workforce( wf );

  ndfEnd( status );

  /* Ensure that FFTW doesn't have any used memory kicking around */
  fftw_cleanup();
}
