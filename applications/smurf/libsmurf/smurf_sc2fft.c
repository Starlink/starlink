/*
*+
*  Name:
*     SC2FFT

*  Purpose:
*     Fourier Transform SCUBA-2 time-series data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2fft( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine performs the forward or inverse FFT of SCUBA-2
*     time-series data.  The FFT of the data are stored in a
*     4-dimensional array with dimensions frequency, xbolo, ybolo,
*     component (where component is a dimension of length 2 holding
*     the real and imaginary parts). The inverse flag is used to
*     transform back to the time domain from the frequency domain.  If
*     the data are already in the requested domain, the ouput file is
*     simply a copy of the input file.

*  Notes:
*     Transforming data loses the VARIANCE and QUALITY components.

*  ADAM Parameters:
*     AVPSPEC = _LOGICAL (Read)
*          Calculate average power spectral density over "good" bolometers.
*          By default a 1/noise^2 weight is applued (see WEIGHTAVPSPEC). [FALSE]
*     AVPSPECTHRESH = _DOUBLE (Read)
*          N-sigma noise threshold to define "good" bolometers for AVPSPEC
*          [5]
*     BBM = NDF (Read)
*          Group of files to be used as bad bolometer masks. Each data file
*          specified with the IN parameter will be masked. The corresponding
*          previous mask for a subarray will be used. If there is no previous
*          mask the closest following will be used. It is not an error for
*          no mask to match. A NULL parameter indicates no mask files to be
*          supplied. [!]
*     FLAT = _LOGICAL (Read)
*          If set ensure data are flatfielded. If not set do not scale the
*          data in any way (but convert to DOUBLE). [TRUE]
*     FLATMETH = _CHAR (Read)
*          Method to use to calculate the flatfield solution. Options
*          are POLYNOMIAL and TABLE. Polynomial fits a polynomial to
*          the measured signal. Table uses an interpolation scheme
*          between the measurements to determine the power. [POLYNOMIAL]
*     FLATORDER = _INTEGER (Read)
*          The order of polynomial to use when choosing POLYNOMIAL method.
*          [1]
*     FLATSNR = _DOUBLE (Read)
*          Signal-to-noise ratio threshold to use when filtering the
*          responsivity data to determine valid bolometers for the
*          flatfield. [3.0]
*     FLATUSENEXT = _LOGICAL (Read)
*          If true the previous and following flatfield will be used to
*          determine the overall flatfield to apply to a sequence. If false
*          only the previous flatfield will be used. A null default will
*          use both flatfields for data when we did not heater track
*          at the end, and will use a single flatfield when we did heater
*          track. The parameter value is not sticky and will revert to
*          the default unless explicitly over-ridden. [!]
*     IN = NDF (Read)
*          Input files to be transformed.
*     INVERSE = _LOGICAL (Read)
*          Perform inverse transform. [FALSE]
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     NGOOD = _INTEGER (Write)
*          Number of good bolometers which contribute to average power
*          spectrum.
*     OUT = NDF (Write)
*          Output files. The number of output files can differ from
*          the number of input files due to darks being filtered out
*          and also to files from the same sequence being concatenated
*          before aplying the FFT.
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line). If a null (!) value is supplied no file is created. [!]
*     POLAR = _LOGICAL (Read)
*          Use polar representation (amplitude, argument) of
*          FFT. [FALSE]
*     POWER = _LOGICAL (Read)
*          Use polar representation of FFT with squared
*          amplitudes divided by the frequency bin spacing (gives a
*          power spectral density, PSD). [FALSE]
*     RESIST = GROUP (Read)
*          A group expression containing the resistor settings for
*          each bolometer.  Usually specified as a text file using "^"
*          syntax. An example can be found in
*          $STARLINK_DIR/share/smurf/resist.cfg
*          [$STARLINK_DIR/share/smurf/resist.cfg]
*     RESPMASK = _LOGICAL (Read)
*          If true, responsivity data will be used to mask bolometer data
*          when calculating the flatfield. [TRUE]
*     WEIGHTAVPSPEC = _LOGICAL (Read)
*          If set, weight power spectrum of each bolo by 1/noise^2 when
*          calculating the average (estimated from 2--20 Hz spectrum). [TRUE]
*     ZEROBAD = _LOGICAL (Read)
*          Zero any bad values in the data before taking FFT. [TRUE]

*  Related Applications:
*     SMURF: SC2CONCAT, SC2CLEAN, CALCNOISE

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-22 (EC):
*        Initial version - based on sc2clean task
*     2008-07-25 (TIMJ):
*        Use kaplibs for group in/out.
*     2008-07-30 (EC):
*        Handle raw data (filter out / apply darks, flatfield)
*     2009-03-30 (TIMJ):
*        Add OUTFILES parameter.
*     2009-04-30 (EC):
*        Use threads
*     2009-10-13 (TIMJ):
*        Concatenate files from the same sequence.
*     2009-12-07 (EC):
*        Add FLAT parameter.
*     2010-02-09 (AGG):
*        - Store number of good bolometers used to create average power
*          spectrum in NGOOD parameter.
*        - Calculate lowest available frequency based on length of input
*          data stream.
*     2010-03-11 (TIMJ):
*        Support flatfield ramps.
*     2010-05-12 (EC):
*        Add zerobad option.
*     2010-06-25 (TIMJ):
*        Add BBM parameter
*     2010-10-29 (EC):
*        Go back to single files, no concatenation
*     2010-11-01 (EC):
*        Revert to concatenation method, but now smf_grp_related and
*        smf_concat_smfGroup properly handle 4d FFT data so that no
*        concatenation happens for inverse tranforms.
*     2011-02-17 (TIMJ):
*        Use AVPSPECTHRESH rather than simply ignoring it.
*     2011-06-13 (EC):
*        Add WEIGHTAVPSPEC option.
*     2013-08-21 (AGG):
*        Do not call grpList if no output files are generated. This
*        avoids a GRP__INVID error in such cases.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2011 Science and Technology Facilities Council.
*     Copyright (C) 2008-2011,2013 University of British Columbia.
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

#define FUNC_NAME "smurf_sc2fft"
#define TASK_NAME "SC2FFT"

void smurf_sc2fft( int *status ) {

  int avpspec=0;            /* Flag for doing average power spectrum */
  double avpspecthresh=0;   /* Threshold noise for detectors in avpspec */
  Grp * basegrp = NULL;     /* Basis group for output filenames */
  smfArray *bbms = NULL;    /* Bad bolometer masks */
  smfArray *concat=NULL;    /* Pointer to a smfArray */
  dim_t contchunk;          /* Continuous chunk counter */
  smfArray *darks = NULL;   /* dark frames */
  int ensureflat;           /* Flag for flatfielding data */
  Grp *fgrp = NULL;         /* Filtered group, no darks */
  smfArray *flatramps = NULL;/* Flatfield ramps */
  AstKeyMap *heateffmap = NULL;    /* Heater efficiency data */
  dim_t gcount=0;           /* Grp index counter */
  dim_t i;                  /* Loop counter */
  smfGroup *igroup=NULL;    /* smfGroup corresponding to igrp */
  Grp *igrp = NULL;         /* Input group of files */
  int inverse=0;            /* If set perform inverse transform */
  int isfft=0;              /* Are data fft or real space? */
  dim_t maxconcat=0;        /* Longest continuous chunk length in samples */
  dim_t ncontchunks=0;      /* Number continuous chunks outside iter loop */
  smfData *odata=NULL;      /* Pointer to output smfData to be exported */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Total number of NDF names in the output group */
  int polar=0;              /* Flag for FFT in polar coordinates */
  int power=0;              /* Flag for squaring amplitude coeffs */
  size_t size;              /* Number of files in input group */
  smfData *tempdata=NULL;   /* Temporary smfData pointer */
  int weightavpspec=0;      /* Flag for 1/noise^2 weighting */
  ThrWorkForce *wf = NULL;  /* Pointer to a pool of worker threads */
  int zerobad;              /* Zero VAL__BADD before taking FFT? */

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_science( wf, igrp, &fgrp, 1, NULL, NULL, 1, 1, SMF__NULL, &darks,
                    &flatramps, &heateffmap, NULL, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  /* We now need to combine files from the same subarray and same sequence
     to form a continuous time series */
  smf_grp_related( igrp, size, 1, 0, 0, NULL, NULL, &maxconcat, NULL, &igroup,
                   &basegrp, NULL, status );

  /* Get output file(s) */
  size = grpGrpsz( basegrp, status );
  if( size > 0 ) {
    kpg1Wgndf( "OUT", basegrp, size, size, "More output files required...",
               &ogrp, &outsize, status );
  } else {
    msgOutif(MSG__NORM, " ", TASK_NAME ": All supplied input frames were DARK,"
             " nothing to do", status );
  }

  /* Get group of bolometer masks and read them into a smfArray */
  smf_request_mask( wf, "BBM", &bbms, status );

  /* Obtain the number of continuous chunks and subarrays */
  if( *status == SAI__OK ) {
    ncontchunks = igroup->chunk[igroup->ngroups-1]+1;
  }
  msgOutiff( MSG__NORM, "", "Found %zu continuous chunk%s", status, ncontchunks,
             (ncontchunks > 1 ? "s" : "") );

  /* Are we flatfielding? */
  parGet0l( "FLAT", &ensureflat, status );

  /* Are we doing an inverse transform? */
  parGet0l( "INVERSE", &inverse, status );

  /* Are we using polar coordinates instead of cartesian for the FFT? */
  parGet0l( "POLAR", &polar, status );

  /* Are we going to assume amplitudes are squared? */
  parGet0l( "POWER", &power, status );

  /* Are we going to zero bad values first? */
  parGet0l( "ZEROBAD", &zerobad, status );

  /* Are we calculating the average power spectrum? */
  parGet0l( "AVPSPEC", &avpspec, status );

  if( avpspec ) {
    power = 1;
    parGet0d( "AVPSPECTHRESH", &avpspecthresh, status );

    parGet0l( "WEIGHTAVPSPEC", &weightavpspec, status );
  }

  /* If power is true, we must be in polar form */
  if( power && !polar) {
    msgOutif( MSG__NORM, " ", TASK_NAME
              ": power spectrum requested so setting POLAR=TRUE", status );
    polar = 1;
  }

  gcount = 1;
  for( contchunk=0;(*status==SAI__OK)&&contchunk<ncontchunks; contchunk++ ) {
    dim_t idx;

    /* Concatenate this continuous chunk but forcing a raw data read.
       We will need quality. */
    smf_concat_smfGroup( wf, NULL, igroup, darks, NULL, flatramps, heateffmap,
                         contchunk, ensureflat, 1, NULL, 0, NULL, NULL,
                         NO_FTS, 0, 0, 0,
                         &concat, NULL, status );

    /* Now loop over each subarray */
    /* Export concatenated data for each subarray to NDF file */
    for( idx=0; (*status==SAI__OK)&&idx<concat->ndat; idx++ ) {
      if( concat->sdata[idx] ) {
        smfData * idata = concat->sdata[idx];
        int provid = NDF__NOID;
        dim_t nbolo;                /* Number of detectors  */
        dim_t ndata;                /* Number of data points */

        /* Apply a mask to the quality array and data array */
        smf_apply_mask( wf, idata, bbms, SMF__BBM_QUAL|SMF__BBM_DATA, 0, status );

        smf_get_dims( idata,  NULL, NULL, &nbolo, NULL, &ndata, NULL, NULL,
                      status );


        /* Check for double precision data */
        if( idata->dtype != SMF__DOUBLE ) {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME ": data are not double precision.", status );
        }

        /* Are we zeroing VAL__BADD? */
        if( (*status==SAI__OK) && zerobad ) {
          double *data= (double *) idata->pntr[0];

          for( i=0; i<ndata; i++ ) {
            if( data[i] == VAL__BADD ) {
              data[i] = 0;
            }
          }
        }

        /* Check whether we need to transform the data at all */
        isfft = smf_isfft(idata,NULL,NULL,NULL,NULL,NULL,status);

        if( isfft && avpspec && (*status == SAI__OK) ) {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME
                  ": to calculate average power spectrum input data cannot "
                  "be FFT", status );
        }

        if( (*status == SAI__OK) && (isfft == inverse) ) {

          if( avpspec ) {
            /* If calculating average power spectrum do the transforms with
               smf_bolonoise so that we can also measure the noise of
               each detector */

            double *whitenoise=NULL;
            smf_qual_t *bolomask=NULL;
            double mean, sig, freqlo;
            dim_t ngood, newgood;

            whitenoise = astCalloc( nbolo, sizeof(*whitenoise) );
            bolomask = astCalloc( nbolo, sizeof(*bolomask) );

	    freqlo = 1. / (idata->hdr->steptime * idata->hdr->nframes);

            smf_bolonoise( wf, idata, -1.0, 1, freqlo, SMF__F_WHITELO,
                           SMF__F_WHITEHI, 1, 0, whitenoise, NULL, &odata,
                           status );

            /* Initialize quality */
            for( i=0; i<nbolo; i++ ) {
              if( whitenoise[i] == VAL__BADD ) {
                bolomask[i] = SMF__Q_BADB;
              } else {
                /* smf_bolonoise returns a variance, so take sqrt */
                whitenoise[i] = sqrt(whitenoise[i]);
              }
            }

            ngood=-1;
            newgood=0;

            /* Iteratively cut n-sigma noisy outlier detectors */
            while( ngood != newgood ) {
              ngood = newgood;
              smf_stats1D( whitenoise, 1, nbolo, bolomask, 1, SMF__Q_BADB,
                           &mean, &sig, NULL, NULL, status );
              msgOutiff( MSG__DEBUG, "", TASK_NAME
                         ": mean=%lf sig=%lf ngood=%li\n", status,
                         mean, sig, ngood);

              newgood=0;
              for( i=0; i<nbolo; i++ ) {
                if( whitenoise[i] != VAL__BADD ){
                  if( (whitenoise[i] - mean) > avpspecthresh *sig ) {
                    whitenoise[i] = VAL__BADD;
                    bolomask[i] = SMF__Q_BADB;
                  } else {
                    newgood++;
                  }
                }
              }
            }

            msgOutf( "", TASK_NAME
                     ": Calculating average power spectrum of best %li "
                     " bolometers.", status, newgood);

            /* If using 1/noise^2 weights, calculate 1/whitenoise^2 in-place
               to avoid allocating another array */
            if( weightavpspec ) {
              msgOutif( MSG__VERB, "", TASK_NAME ": using 1/noise^2 weights",
                        status );

              for( i=0; i<nbolo; i++ ) {
                if( whitenoise[i] && (whitenoise[i] != VAL__BADD) ) {
                  whitenoise[i] = 1/(whitenoise[i]*whitenoise[i]);
                }
              }
            }

            /* Calculate the average power spectrum of good detectors */
            tempdata = smf_fft_avpspec( odata, bolomask, 1, SMF__Q_BADB,
                                        weightavpspec ? whitenoise : NULL,
                                        status );
            smf_close_file( wf, &odata, status );
            whitenoise = astFree( whitenoise );
            bolomask = astFree( bolomask );
            odata = tempdata;
            tempdata = NULL;
	    /* Store the number of good bolometers */
	    parPut0k( "NGOOD", newgood, status );
          } else {
            /* Otherwise do forward/inverse transforms here as needed */

            /* If inverse transform convert to cartesian representation first */
            if( inverse && polar ) {
              smf_fft_cart2pol( wf, idata, 1, power, status );
            }

            /* Tranform the data */
            odata = smf_fft_data( wf, idata, NULL, inverse, 0, status );
            smf_convert_bad( wf, odata, status );

            if( inverse ) {
              /* If output is time-domain, ensure that it is ICD bolo-ordered */
              smf_dataOrder( wf, odata, 1, status );
            } else if( polar ) {
              /* Store FFT of data in polar form */
              smf_fft_cart2pol( wf, odata, 0, power, status );
            }
          }

          /* open a reference input file for provenance propagation */
          ndgNdfas( basegrp, gcount, "READ", &provid, status );

          /* Export the data to a new file */
          smf_write_smfData( wf, odata, NULL, NULL, ogrp, gcount, provid,
                             MSG__VERB, 0, NULL, NULL, status );

          /* Free resources */
          ndfAnnul( &provid, status );
          smf_close_file( wf, &odata, status );
        } else {
          msgOutif( MSG__NORM, " ",
                    "Data are already transformed. No output will be produced",
                    status );
        }
      }

      /* Update index into group */
      gcount++;
    }

    /* Close the smfArray */
    smf_close_related( wf, &concat, status );
  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK && ogrp ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);
  if (basegrp) grpDelet( &basegrp, status );
  if( igroup ) smf_close_smfGroup( &igroup, status );
  if( flatramps ) smf_close_related( wf, &flatramps, status );
  if (heateffmap) heateffmap = smf_free_effmap( heateffmap, status );
  if (bbms) smf_close_related( wf, &bbms, status );

  ndfEnd( status );

  /* Ensure that FFTW doesn't have any used memory kicking around */
  fftw_cleanup();
}
