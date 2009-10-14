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
*     data between two frequency ranges. It additionally calculates an
*     NEP image and an image of the ratio of the power at a specified
*     frequency to the whitenoise.

*  ADAM Parameters:
*     FLOW = _DOUBLE (Given)
*          Frequency to use when determining noise ratio image. The noise
*          ratio image is determined by dividing the power at this frequency
*          by the white noise [0.5]
*     FREQ = _DOUBLE (Given)
*          Frequency range (Hz) to use to calculate the white noise [2,10]
*     IN = NDF (Read)
*          Input files to be transformed. Files from the same sequence
*          will be combined.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output files (either noise or NEP images depending on the NEP
*          parameter). Number of output files may differ from the
*          number of input files. These will be 2 dimensional.
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line) from the OUT parameter. If a null (!) value is supplied
*          no file is created. [!]
*     POWER = NDF (Write)
*          Output files to contain the power spectra for each processed
*          chunk. There will be the same number of output files as
*          created for the OUT parameter. If a null (!) value
*          is supplied no files will be created. [!]

*  Notes:
*     - NEP and NOISERATIO images are stored in the .MORE.SMURF extension

*  Related Applications:
*     SMURF: SC2CONCAT, SC2CLEAN, SC2FFT

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-10-01 (TIMJ):
*        Initial version - based on sc2fft task
*     2009-10-07 (TIMJ):
*        Add NEP
*     2009-10-08 (TIMJ):
*        Remove NEP parameter. Write NEP and noise ratio image to extension.
*     2009-10-13 (TIMJ):
*        Add POWER and FLOW parameters.
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
#include "star/one.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smurf_calcnoise"
#define TASK_NAME "CALCNOISE"

static smfData *
smf__create_bolfile_extension( const Grp * ogrp, size_t gcount,
                               const smfData *refdata, const char hdspath[], 
                               const char datalabel[], const char units[],
                               int * status );


void smurf_calcnoise( int *status ) {

  Grp * basegrp = NULL;     /* Basis group for output filenames */
  smfArray *concat=NULL;     /* Pointer to a smfArray */
  size_t contchunk;          /* Continuous chunk counter */
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
  Grp *powgrp = NULL;       /* Group for output power spectra */
  int power=0;              /* Flag for squaring amplitude coeffs */
  size_t size;              /* Number of files in input group */
  smfWorkForce *wf = NULL;  /* Pointer to a pool of worker threads */
  double f_low = 0.5;       /* Frequency to use for noise ratio image */
  double freqdef[] = { SMF__F_WHITELO,
                       SMF__F_WHITEHI }; /* Default values for frequency range */
  double freqs[2];          /* Frequencies to use for white noise */

  if (*status != SAI__OK) return;

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = smf_create_workforce( smf_get_nthread( status ), status );

  /* Get frequency range of interest for white noise measurement */
  parGdr1d( "FREQ", 2, freqdef, 0.0, 50.0, 1, freqs, status );
  /* Get the low frequency to use for the noise ratio */
  parGdr0d( "FLOW", f_low, 0.0, 50.0, 1, &f_low, status );

  msgOutf( "", "Calculating noise between %g and %g Hz and noise ratio for %g Hz", status,
           freqs[0], freqs[1], f_low);


  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_darks( igrp, &fgrp, &dkgrp, 1, SMF__NULL, NULL, status );

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
  }

  /* We now need to combine files from the same subarray and same sequence
     to form a continuous time series */
  smf_grp_related( igrp, size, 1, 0, &maxconcat, &igroup,
                   &basegrp, status );

  /* Get output file(s) */
  size = grpGrpsz( basegrp, status );
  kpg1Wgndf( "OUT", basegrp, size, size, "More output files required...",
             &ogrp, &outsize, status );

  /* and see if we want power spectra */
  if (*status == SAI__OK) {
    kpg1Wgndf( "POWER", basegrp, size, size, "More output files required...",
               &powgrp, &outsize, status );
    if (*status == PAR__NULL) {
      errAnnul( status );
    }
  }

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
    smf_concat_smfGroup( wf, igroup, NULL, NULL, contchunk, 0, 1, NULL, 0, NULL,
                         NULL, 0, 0, 0, &concat, status );

    /* Now loop over each subarray */
    /* Export concatenated data for each subarray to NDF file */
    for( idx=0; (*status==SAI__OK)&&idx<concat->ndat; idx++ ) {
      if( concat->sdata[idx] ) {
        smfData *thedata = concat->sdata[idx];
        smfData *outdata = NULL;
        smfData *ratdata = NULL;
        smfData *nepdata = NULL;
        smfData *powdata = NULL;

        /* Convert the data to amps */
        smf_scalar_multiply( thedata, RAW2CURRENT, status );

        /* Apodize */
        smf_apodize(thedata, NULL, (thedata->dims)[2] / 2, status );

        /* Create the output file if required, else a malloced smfData */
        smf_create_bolfile( ogrp, gcount, thedata, "Noise",
                            SIPREFIX "A Hz**-0.5", &outdata, status );

        /* Create groups to handle the NEP and ratio images */
        ratdata = smf__create_bolfile_extension( ogrp, gcount, thedata,
                                                 ".MORE.SMURF.NOISERATIO",
                                                 "Noise Ratio", NULL, status );

        if (*status == SAI__OK) {
          smf_bolonoise( wf, thedata, NULL, 0, f_low, freqs[0], freqs[1], 10.0, 1,
                         (outdata->pntr)[0], (ratdata->pntr)[0],
                         (powgrp ? &powdata : NULL), status );

          /* Bolonoise gives us a variance - we want square root */
          for (i = 0; i < (outdata->dims)[0]*(outdata->dims)[1]; i++) {
            double * od = (outdata->pntr)[0];
            if ( od[i] != VAL__BADD ) od[i] = sqrt( od[i] );
          }

          if (powdata) {
            char fname[GRP__SZNAM+1]; /* Name of container file without suffix */
            char *pname = NULL;
            pname = fname;
            grpGet( powgrp, gcount, 1, &pname, sizeof(fname), status );
            smf_write_smfData( powdata, NULL, NULL, fname, NDF__NOID, status );
            smf_close_file( &powdata, status );
          }

        }

        /* now to create the NEP image using the flatfield information */
        if (*status == SAI__OK) {
          smfDA *da = thedata->da;
          smfData * nepdata = NULL;
          size_t ngood;
          smfData * respmap = NULL;

          if (!da) {
            *status = SAI__ERROR;
            errRep( " ", "Attempting to calculate NEP image but no"
                    " flatfield information available", status);
          }

          smf_create_bolfile( NULL, 1, thedata, "Responsivity", "A/W",
                              &respmap, status );
          if (*status == SAI__OK) {
            /* use a snr of 5 since we don't mind if we get a lot of
               bolometers that are a bit dodgy since the point is the NEP */
            ngood = smf_flat_responsivity( respmap, 5.0, da->nflat, da->flatpar,
                                           da->flatcal, status );
          }
          if (*status == SAI__OK && ngood == 0) {
            *status = SAI__ERROR;
            errRep( "", "No good responsivities found in flatfield."
                    " Unable to calculate NEP", status );
          }

          /* now create the output image for NEP data */
          nepdata = smf__create_bolfile_extension( ogrp, gcount, thedata,
                                                   ".MORE.SMURF.NEP", "NEP",
                                                   "W Hz**-0.5", status );

          /* and divide the noise data by the responsivity
             correcting for SIMULT */
          if (*status == SAI__OK) {
            for (i = 0; i < (nepdata->dims)[0]*(nepdata->dims)[1]; i++) {
              /* ignore variance since noise will not have any */
              double * noise = (outdata->pntr)[0];
              double * resp = (respmap->pntr)[0];
              double * nep  = (nepdata->pntr)[0];
              if (noise[i] == VAL__BADD || resp[i] == VAL__BADD) {
                nep[i] = VAL__BADD;
              } else {
                nep[i] = (noise[i] / SIMULT) / resp[i];
              }
            }
          }
          if (nepdata) smf_close_file( &nepdata, status );
        }

        if (outdata) smf_close_file( &outdata, status );
        if (ratdata) smf_close_file( &ratdata, status );

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
  if (powgrp) grpDelet( &powgrp, status );
  if (basegrp) grpDelet( &basegrp, status );
  if( igroup ) smf_close_smfGroup( &igroup, status );
  if( wf ) wf = smf_destroy_workforce( wf );

  ndfEnd( status );

  /* Ensure that FFTW doesn't have any used memory kicking around */
  fftw_cleanup();
}

static smfData *
smf__create_bolfile_extension( const Grp * ogrp, size_t gcount,
                               const smfData *refdata, const char hdspath[],
                               const char datalabel[], const char units[],
                               int * status ) {
  char tempfile[SMF_PATH_MAX];
  char * pname;
  Grp * tempgrp = NULL;
  smfData *newdata = NULL;

  if (*status != SAI__OK) return newdata;

  pname = tempfile;
  grpGet( ogrp, 1, 1, &pname, SMF_PATH_MAX, status );
  one_strlcat( tempfile, hdspath, sizeof(tempfile), status);
  tempgrp = grpNew( "Ratio", status );
  grpPut1( tempgrp, tempfile, 0, status );
  smf_create_bolfile( tempgrp, 1, refdata, datalabel, units,
                      &newdata, status );
  if (tempgrp) grpDelet( &tempgrp, status );
  return newdata;

}
