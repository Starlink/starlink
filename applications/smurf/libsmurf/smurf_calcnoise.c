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
*     This routine cleans the supplied data and then calculates the white
*     noise on the array by performing an FFT to generate a power spectrum
*     and then extracting the data between two frequency ranges. It
*     additionally calculates an NEP image and an image of the ratio of
*     the power at a specified frequency to the whitenoise.

*  ADAM Parameters:
*     CONFIG = GROUP (Read)
*          Specifies values for the cleaning parameters. If the string
*          "def" (case-insensitive) is supplied, a
*          set of default configuration parameter values will be used.
*          CONFIG=! disables all cleaning and simply applies apodisation.
*          This is generally not a recommended use of calcnoise.
*
*          The supplied value should be either a comma-separated list of
*          strings or the name of a text file preceded by an up-arrow
*          character "^", containing one or more comma-separated lists of
*          strings. Each string is either a "keyword=value" setting, or
*          the name of a text file preceded by an up-arrow character
*          "^". Such text files should contain further comma-separated
*          lists which will be read and interpreted in the same manner
*          (any blank lines or lines beginning with "#" are
*          ignored). Within a text file, newlines can be used as
*          delimiters, as well as commas. Settings are applied in the
*          order in which they occur within the list, with later
*          settings over-riding any earlier settings given for the same
*          keyword.
*
*          Each individual setting should be of the form:
*
*             <keyword>=<value>
*
*          The available parameters are identical to the cleaning
*          parameters used by the iterative map-maker (method=ITER)
*          and are are described in the "Configuration Parameters"
*          appendix of SUN/258. Default values will be used for any
*          unspecified parameters. Assigning
*          the value "<def>" (case insensitive) to a keyword has the
*          effect of resetting it to its default value. Options
*          available to the map-maker but not understood by CALCNOISE
*          will be ignored. Parameters not understood will trigger an
*          error. Use the "cleandk." namespace for configuring
*          cleaning parameters for the dark squids.
*
*          If a null value (!) is given all cleaning will be disabled and
*          the full time series will be apodized with no padding.
*          This differs to the behaviour of SC2CLEAN where the defaults
*          will be read and used. [current value]
*     EFFNEP = _DOUBLE (Write)
*          The effective noise of the .MORE.SMURF.NEP image. See the EFFNOISE
*          parameter for details of how it is calculated.
*     EFFNOISE = _DOUBLE (Write)
*          The effective noise of the primary output image. If this command
*          was run on raw data it will be the current noise and if run on
*          flatfielded data it will be the effective NEP. Calculated as
*          the sqrt of 1/sum(1/sigma^2). See also the EFFNEP parameter.
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
*     NEPCLIPHIGH = _DOUBLE (Given)
*          Flag NEP values this number of standard deviations above the median.
*          If a null (!) value is supplied now high-outlier clipping. [!]
*     NEPCLIPLOW = _DOUBLE (Given)
*          Flag NEP values this number of standard deviations below the median.
*          If a null (!) value is supplied now low-outlier clipping. [3]
*     NEPCLIPLOG = _LOGICAL (Given)
*          Clip based on the log of the NEP. [TRUE]
*     NEPGOODBOL = _INTEGER (Write)
*          The number of bolometers with good NEP measurements (see EFFNEP)
*     NOICLIPHIGH = _DOUBLE (Given)
*          Flag NOISE values this number of standard deviations above the
*          median. If a null (!) value is supplied now high-outlier
*          clipping. [!]
*     NOICLIPLOW = _DOUBLE (Given)
*          Flag NOISE values this number of standard deviations below the
*          median. If a null (!) value is supplied now low-outlier clipping. [3]
*     NOICLIPLOG = _LOGICAL (Given)
*          Clip based on the log of the NOISE. [TRUE]
*     NOISEGOODBOL = _INTEGER (Write)
*          The number of bolometers with good NOISE measurements (see EFFNOISE)
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
*     RESIST = GROUP (Read)
*          A group expression containing the resistor settings for
*          each bolometer.  Usually specified as a text file using "^"
*          syntax. An example can be found in
*          $STARLINK_DIR/share/smurf/resist.cfg
*          [$STARLINK_DIR/share/smurf/resist.cfg]
*     RESPMASK = _LOGICAL (Read)
*          If true, responsivity data will be used to mask bolometer data
*          when calculating the flatfield. [TRUE]
*     TSERIES = NDF (Write)
*          Output files to contain the cleaned time-series for each processed
*          chunk. There will be the same number of output files as
*          created for the OUT parameter. If a null (!) value
*          is supplied no files will be created. [!]

*  Notes:
*     - NEP and NOISERATIO images are stored in the .MORE.SMURF extension
*     - NEP image is only created for raw, unflatfielded data.
*     - If the data have flatfield information available the noise and
*       NOISERATIO images will be masked by the flatfield bad
*       bolometer mask.  The mask can be removed using SETQUAL or
*       SETBB (clear the bad bits mask).
*     - NOICLIP[LOW/HIGH] and NEPCLIP[LOW/HIGH] are done independently
*       for the NOISE and NEP images (so a bolometer may be clipped in one,
*       but not the other).

*  Related Applications:
*     SMURF: SC2CONCAT, SC2CLEAN, SC2FFT

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     David S Berry (JAC, Hawaii)
*     Ed Chapin (UBC)
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
*     2009-10-21 (TIMJ):
*        Propagate units properly if we do not have raw data.
*     2009-11-30 (TIMJ):
*        Add quality mask so that bolometers known to be masked by
*        bad responsivity will be masked in the noise data.
*     2010-01-28 (TIMJ):
*        Flatfield routines now use smfData
*     2010-02-03 (TIMJ):
*        Update smf_flat_responsivity API
*     2010-02-04 (TIMJ):
*        New smf_flat_responsibity and smf_flat_smfData API to support
*        flatfield method.
*     2010-03-09 (TIMJ):
*        Change type of flatfield method in smfDA
*     2010-03-11 (TIMJ):
*        Support flatfield ramps.
*     2010-06-03 (TIMJ):
*        Rely on smf_find_science to work out whether we are calculating
*        noise of darks or skys.
*     2010-07-01 (TIMJ):
*        smf_bolonoise can now handle apodization.
*     2010-12-06 (TIMJ):
*        Assign flatramp flatfields to noise data correctly.
*     2010-12-07 (TIMJ):
*        Let the NEP output image be blank.
*     2011-04-19 (TIMJ):
*        Presence of DA struct is not enough to determine if flatfield is
*        available.
*     2011-04-20 (DSB):
*        - Clean the arrays before calculating the noise.
*        - Added CONFIG parameter.
*        - When calling smf_bolonoise, only apodize if ZEROPAD is set (as done
*          in the makemap NOI model).
*     2011-04-20 (TIMJ):
*        CONFIG=! disables all cleaning and uses full apodization.
*        Use rt(s) instead of /rt(Hz) for noise units.
*     2011-04-26 (TIMJ):
*        Calculate effective NEP
*     2011-04-29 (TIMJ):
*        Calculate effective NEP over all input chunks/subarrays.
*     2011-06-03 (EC):
*        Add NEPGOODBOL and NOISEGOODBOL output ADAM parameters
*     2011-06-17 (EC):
*        Add ability to asymmetrically clip outlier NEP/NOISE values.
*     2011-06-23 (EC):
*        Moved private function smf__clipnoise to public smf_clipnoise
*     2011-08-23 (TIMJ):
*        Write effective noise to output image. This allows the pipeline
*        to easily look at the results when multiple files have been
*        created.
*     2011-08-23 (DSB):
*        Do not call grpList if no output files are generated. This
*        avoids a GRP__INVID error in such cases.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009-2011 Science and Technology Facilities Council.
*     Copyright (C) 2011 University of British Columbia
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
#include "star/atl.h"
#include "star/kaplibs.h"
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
#define CREATOR PACKAGE_UPCASE ":" TASK_NAME

static smfData *
smf__create_bolfile_extension( ThrWorkForce *wf, const Grp * ogrp, size_t gcount,
                               const smfData *refdata, const char hdspath[],
                               const char datalabel[], const char units[],
                               int * status );

static void
smf__write_effnoise( const smfData * data, const char * noisekey,
                     const char * units, size_t ngood, double noisesum,
                     int *status );

void smurf_calcnoise( int *status ) {

  smfArray *array = NULL;   /* Data to be cleaned */
  Grp * basegrp = NULL;     /* Basis group for output filenames */
  int calc_effnoise = 1;    /* Can we calculate the effective noise? */
  smfArray *concat=NULL;    /* Pointer to a smfArray */
  size_t contchunk;         /* Continuous chunk counter */
  int dkclean;              /* Clean dark squids? */
  int doclean = 0;          /* Do we clean the time series? */
  double downsampscale;     /* Downsample factor to preserve this scale */
  Grp *fgrp = NULL;         /* Filtered group, no darks */
  smfData *firstdata=NULL;  /* First smfData in the current chunk */
  smfArray * flatramps = NULL; /* Flatfield ramps */
  size_t gcount=0;          /* Grp index counter */
  size_t i=0;               /* Counter, index */
  Grp *igrp = NULL;         /* Input group of files */
  smfGroup *igroup=NULL;    /* smfGroup corresponding to igrp */
  AstKeyMap *keymap=NULL;   /* KeyMap holding configuration parameters */
  dim_t maxconcat=0;        /* Longest continuous chunk length in samples */
  size_t ncontchunks=0;     /* Number continuous chunks outside iter loop */
  size_t nepbolo;           /* Number of bolometers in NEP map */
  double nepcliphigh=VAL__BADD; /* Clip high NEP values */
  double nepcliplow=VAL__BADD;  /* Clip low NEP values */
  int nepcliplog=1;             /* Clip based on log(NEP) */
  size_t nepgoodbol = 0;    /* Number of bolometers used in eff NEP calc */
  double nepsum = 0.0;      /* Sum of weights for effective NEP calculation */
  size_t noisebolo;         /* Number of bolometers in noise map*/
  double noicliphigh=VAL__BADD; /* Clip high noise values */
  double noicliplow=VAL__BADD;  /* Clip low noise values */
  int noicliplog=1;             /* Clip based on log(NOISE) */
  size_t noisegoodbol = 0;  /* Number of bolometers used in eff noise calc */
  double noisesum = 0.0;    /* Sum of weights for effective noise calculation */
  smfData *odata = NULL;    /* Pointer to output data struct */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Total number of NDF names in the output group */
  dim_t pad;                /* No. of samples of padding at start and end */
  int parstate = 0;         /* CONFIG parameter state */
  Grp *powgrp = NULL;       /* Group for output power spectra */
  char refunits[SMF__CHARLABEL];/* Reference units for effective noise calc */
  char refnepunits[SMF__CHARLABEL]; /* Reference units for effective NEP calc */
  size_t size;              /* Number of files in input group */
  AstKeyMap *sub_instruments=NULL; /* KeyMap holding subinstrument names */
  Grp *tsgrp = NULL;        /* Group for output cleaned time-series */
  ThrWorkForce *wf = NULL;  /* Pointer to a pool of worker threads */
  int zeropad;              /* Pad with zeros before FFTing? */
  double f_low = 0.5;       /* Frequency to use for noise ratio image */
  double freqdef[] = { SMF__F_WHITELO,
                       SMF__F_WHITEHI };/* Default values for frequency range */
  double freqs[2];          /* Frequencies to use for white noise */

  if (*status != SAI__OK) return;

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

  /* Get frequency range of interest for white noise measurement */
  parGdr1d( "FREQ", 2, freqdef, 0.0, 50.0, 1, freqs, status );
  /* Get the low frequency to use for the noise ratio */
  parGdr0d( "FLOW", f_low, 0.0, 50.0, 1, &f_low, status );

  msgOutf( "",
           "Calculating noise between %g and %g Hz and noise ratio for %g Hz",
           status, freqs[0], freqs[1], f_low);

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_science( wf, igrp, &fgrp, 1, NULL, NULL, 1, 1, SMF__NULL, NULL,
                    &flatramps, NULL, NULL, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status );

  if (size > 0) {
    igrp = fgrp;
    fgrp = NULL;
  } else {
    msgOutif( MSG__NORM, " ", "No valid frames supplied. Nothing to do.",
              status );
    if (fgrp) grpDelet( &fgrp, status );
    goto CLEANUP;
  }

  /* We now need to combine files from the same subarray and same sequence
     to form a continuous time series */
  smf_grp_related( igrp, size, 1, 0, 0, NULL, NULL, &maxconcat, NULL, &igroup,
                   &basegrp, NULL, status );

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

  /* and time-sereis */
  if (*status == SAI__OK) {
    kpg1Wgndf( "TSERIES", basegrp, size, size, "More output files required...",
               &tsgrp, &outsize, status );
    if (*status == PAR__NULL) {
      errAnnul( status );
    }
  }

  /* Get clipping parameters */
  parGet0d( "NEPCLIPHIGH", &nepcliphigh, status );
  if( *status == PAR__NULL ) {
    nepcliphigh = VAL__BADD;
    errAnnul( status );
  }
  if( (*status==SAI__OK) && (nepcliphigh!=VAL__BADD) && (nepcliphigh < 0) ) {
    *status = SAI__ERROR;
    errRep( "", TASK_NAME ": NEPCLIPHIGH must be >= 0", status );
  }

  parGet0d( "NEPCLIPLOW", &nepcliplow, status );
  if( *status == PAR__NULL ) {
    nepcliplow = VAL__BADD;
    errAnnul( status );
  }
  if( (*status==SAI__OK) && (nepcliplow!=VAL__BADD) && (nepcliplow < 0) ) {
    *status = SAI__ERROR;
    errRep( "", TASK_NAME ": NEPCLIPLOW must be >= 0", status );
  }

  parGet0l( "NEPCLIPLOG", &nepcliplog, status );

  parGet0d( "NOICLIPHIGH", &noicliphigh, status );
  if( *status == PAR__NULL ) {
    noicliphigh = VAL__BADD;
    errAnnul( status );
  }
  if( (*status==SAI__OK) && (noicliphigh!=VAL__BADD) && (noicliphigh < 0) ){
    *status = SAI__ERROR;
    errRep( "", TASK_NAME ": NOICLIPHIGH must be >= 0", status );
  }

  parGet0d( "NOICLIPLOW", &noicliplow, status );
  if( *status == PAR__NULL ) {
    noicliphigh = VAL__BADD;
    errAnnul( status );
  }
  if( (*status==SAI__OK) && (noicliplow!=VAL__BADD) && (noicliplow < 0) ) {
    *status = SAI__ERROR;
    errRep( "", TASK_NAME ": NOICLIPLOW must be >= 0", status );
  }

  parGet0l( "NOICLIPLOG", &noicliplog, status );

  /* Obtain the number of continuous chunks and subarrays */
  if( *status == SAI__OK ) {
    ncontchunks = igroup->chunk[igroup->ngroups-1]+1;
  }
  msgOutiff( MSG__NORM, "", "Found %zu continuous chunk%s", status, ncontchunks,
             (ncontchunks > 1 ? "s" : "") );

  /* See if the CONFIG parameter has been set to NULL */
  parState( "CONFIG", &parstate, status );
  if (parstate == PAR__NULLST) {
    msgOutif(MSG__NORM, "", "Cleaning of input data disabled",
             status );
  } else {
    doclean = 1;
  }

  /* We will calculate an effective noise for all input data */
  noisegoodbol = 0;
  noisesum = 0.0;
  nepsum = 0.0;
  nepgoodbol = 0;
  refnepunits[0] = '\0';
  refunits[0] = '\0';

  /* Loop over input data as contiguous chunks */
  pad = 0;
  gcount = 1;
  for( contchunk=0;(*status==SAI__OK)&&contchunk<ncontchunks; contchunk++ ) {
    size_t idx;

    if (doclean) {
      /* Get the first smfData that will contribute to this continuous chunk,
         but do not concatenate the data just yet. */
      smf_concat_smfGroup( wf, NULL, igroup, NULL, NULL, NULL, NULL, contchunk,
                           0, 1, NULL, 0, NULL, NULL, NO_FTS, 0, 0, 0, NULL,
                           &firstdata, status );

      /* Get the configuration parameters to use, selecting the values
         approproate to the subinstrument that generated this continuous
         chunk. Do this inside the loop in case we are processing chunks
         from differing sub-instruments. Note that we use the map-maker
         defaults file here so that we populate the locked keymap with all
         the parameters that people may come across to allow them to load
         their map-maker config directly into calcnoise. */
      sub_instruments = smf_subinst_keymap( SMF__SUBINST_NONE, firstdata, NULL,
                                            0, status );
      keymap = kpg1Config( "CONFIG", "$SMURF_DIR/smurf_makemap.def",
                           sub_instruments, 1, status );
      if( sub_instruments ) sub_instruments = astAnnul( sub_instruments );

      /* Are we downsampling the data? */
      astMapGet0D( keymap, "DOWNSAMPSCALE", &downsampscale );

      /* Get the padding to use. */
      if( firstdata ) pad = smf_get_padding( keymap, 0, firstdata->hdr, VAL__BADD, status );

      /* Free the first smfData. */
      smf_close_file( wf, &firstdata, status );

    } else {
      zeropad = 1; /* full apodisation */
    }

    /* Now that we have the padding, concatenate this continuous chunk but
       forcing a raw data read. We will need quality. */
    smf_concat_smfGroup( wf, keymap, igroup, NULL, NULL, NULL, NULL, contchunk,
                         0, 1, NULL, 0, NULL, NULL, NO_FTS, pad, pad, 0,
                         &concat, NULL, status );

    if (doclean) {
      /* Clean the dark squids now since we might need to use them
         to clean the bolometer data */
      smf_get_cleanpar( keymap, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        &dkclean, NULL, &zeropad, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, status);

      for( idx=0; dkclean&&(*status==SAI__OK)&&idx<concat->ndat; idx++ ) {
        odata = concat->sdata[idx];

        if( odata && odata->da && odata->da->dksquid ) {
          smfData *dksquid = odata->da->dksquid;
          AstKeyMap *kmap=NULL;

          msgOut("", TASK_NAME ": cleaning dark squids", status);

          /* fudge the header so that we can get at JCMTState */
          dksquid->hdr = odata->hdr;

          /* clean darks using cleandk.* parameters */
          astMapGet0A( keymap, "CLEANDK", &kmap );
          array = smf_create_smfArray( status );
          smf_addto_smfArray( array, dksquid, status );
          smf_clean_smfArray( wf, array, NULL, NULL, NULL, kmap, status );
          if( array ) {
            array->owndata = 0;
            smf_close_related( wf, &array, status );
          }
          if( kmap ) kmap = astAnnul( kmap );

          /* Unset hdr pointer so that we don't accidentally close it */
          dksquid->hdr = NULL;
        }
      }

      /* Then clean the main data arrays */
      msgOut("", TASK_NAME ": cleaning bolometer data", status );
      smf_clean_smfArray( wf, concat, NULL, NULL, NULL, keymap, status );

      /* Report statistics (currently need a smfArray for that) */
      if (*status == SAI__OK) {
        size_t last_qcount[SMF__NQBITS];
        size_t last_nmap = 0;
        smf_qualstats_report( wf, MSG__VERB, SMF__QFAM_TSERIES, 1, concat,
                              last_qcount, &last_nmap, 1, NULL, NULL, status );
      }
    }

    /* Now loop over each subarray */
    /* Export concatenated data for each subarray to NDF file */
    for( idx=0; (*status==SAI__OK)&&idx<concat->ndat; idx++ ) {
      if( concat->sdata[idx] ) {
        smfData *thedata = concat->sdata[idx];
        smfData *outdata = NULL;
        smfData *ratdata = NULL;
        smfData *powdata = NULL;
        size_t thisnoisegoodbol = 0;
        double thisnoisesum = 0.0;
        int do_nep = 1;
        char noiseunits[SMF__CHARLABEL];

        if ( ! thedata || !thedata->hdr ) {
          *status = SAI__ERROR;
          errRepf( "", "Concatenated data set %zu is missing a header. "
                   "Should not be possible", status, idx);
          break;
        }

        /* Convert the data to amps if we have DAC units. Else leave them
           alone. */
        if ( strncmp(thedata->hdr->units, "adu", 3) == 0) {
          msgOutiff( MSG__VERB, "", "Scaling data from '%s' to amps",
                     status, thedata->hdr->units );
          smf_scalar_multiply( thedata, smf_raw2current( thedata->hdr, status ),
                               status );
          smf_set_clabels( NULL, NULL, SIPREFIX "A", thedata->hdr, status );
        } else {
          do_nep = 0;
          msgOutiff( MSG__VERB, "",
                     "Data in units of '%s' and not raw, so not generating NEP "
                     "image", status, strlen(thedata->hdr->units) ?
                     thedata->hdr->units : "<none>" );
        }

        one_strlcpy( noiseunits, thedata->hdr->units, sizeof(noiseunits),
                     status );
        if (strlen(noiseunits)) one_strlcat( noiseunits, " ",
                                             sizeof(noiseunits), status );
        one_strlcat( noiseunits, "s**0.5", sizeof(noiseunits), status );

        /* Sanity check units for effective noise calculations - this
           will also disable NEP */
        if (strlen(refunits)) {
          if (strcmp( refunits, noiseunits ) != 0) {
            msgOutiff(MSG__QUIET, "", "Units for input data (%s) do not match "
                      "previous files (%s). Will not calculate effective noise",
                      status, noiseunits, refunits );
            calc_effnoise = 0;
          }
        } else {
          one_strlcpy( refunits, noiseunits, sizeof(refunits), status );
        }

        /* Create the output file if required, else a malloced smfData */
        smf_create_bolfile( wf, ogrp, gcount, thedata, "Noise",
                            noiseunits, SMF__MAP_QUAL, &outdata, status );

        /* Create groups to handle the NEP and ratio images */
        ratdata = smf__create_bolfile_extension( wf, ogrp, gcount, thedata,
                                                 ".MORE.SMURF.NOISERATIO",
                                                 "Noise Ratio", NULL, status );

        if (*status == SAI__OK) {
          double * od = (outdata->pntr)[0];
          smf_bolonoise( wf, thedata, -1.0, 0, f_low, freqs[0], freqs[1],
                         1, zeropad ? SMF__MAXAPLEN : SMF__BADSZT,
                         od, (ratdata->pntr)[0],
                         (powgrp ? &powdata : NULL), status );

          noisebolo = (outdata->dims)[0]*(outdata->dims)[1];

          /* Bolonoise gives us a variance - we want square root */
          for (i = 0; i < noisebolo; i++) {
            if ( od[i] != VAL__BADD ) {
              od[i] = sqrt( od[i] );
            }
          }


          /* Write out power spectra and cleaned tseries if requested */

          if (powdata) {
            int provid = NDF__NOID;
            /* open a reference input file for provenance propagation */
            ndgNdfas( basegrp, gcount, "READ", &provid, status );
            smf_write_smfData( wf, powdata, NULL, NULL, powgrp, gcount, provid,
                               MSG__VERB, 0, NULL, NULL, status );
            smf_close_file( wf, &powdata, status );
            ndfAnnul( &provid, status );
          }

          if (tsgrp ) {
            int provid = NDF__NOID;
            /* open a reference input file for provenance propagation */
            ndgNdfas( basegrp, gcount, "READ", &provid, status );

            /* Ensure ICD data order */
            smf_dataOrder( wf, thedata, 1, status );

            /* Write it out */
            smf_write_smfData( wf, thedata, NULL, NULL, tsgrp, gcount, provid,
                               MSG__VERB, 0, NULL, NULL, status );
            ndfAnnul( &provid, status );
          }

        }

        /* we want to use a quality mask derived from the flatfield
           and apply it to the NOISE and NOISERATIO data so that we can
           give people the option of looking at all the data or all
           the data with working bolometers */
        if (*status == SAI__OK) {
          smfDA *da = thedata->da;
          size_t ngood = 0;
          smfData * respmap = NULL;

          if (da && da->nflat) {
            smf_create_bolfile( wf, NULL, 1, thedata, "Responsivity", "A/W",
                                SMF__MAP_VAR, &respmap, status );
            if (*status == SAI__OK) {
              /* use a snr of 5 since we don't mind if we get a lot of
                 bolometers that are a bit dodgy since the point is the NEP */
              smfData * powval;
              smfData * bolval;
              smf_flatmeth flatmethod;
              double refres;
              smf_flat_override( flatramps, thedata, status );
              smf_flat_smfData( thedata, &flatmethod, &refres, &powval, &bolval,
                                status );
              ngood = smf_flat_responsivity( flatmethod, respmap, 5.0, 1,
                                             powval, bolval, refres,
                                             NULL, status);
              if (powval) smf_close_file( wf, &powval, status );
              if (bolval) smf_close_file( wf, &bolval, status );
            }
          } else {
            if (do_nep) {
              *status = SAI__ERROR;
              errRep( " ", "Attempting to calculate NEP image but no"
                      " flatfield information available", status);
            } else {
              /* we might simply be running calcnoise on data
                 that already have been flatfielded */
              msgOutif( MSG__VERB, " ", "Unable to add bad bolometer mask since"
                        " no flatfield information avaialable", status);
            }
          }

          if (do_nep) {
            smfData * nepdata = NULL;

            if (ngood == 0) {
              msgOutif( MSG__QUIET, "",
                        "WARNING: No good responsivities in flatfield. Blank "
                        "NEP image.", status );
            }

            /* now create the output image for NEP data */
            nepdata = smf__create_bolfile_extension( wf, ogrp, gcount, thedata,
                                                     ".MORE.SMURF.NEP", "NEP",
                                                     "W s**0.5", status );

            /* and divide the noise data by the responsivity
               correcting for SIMULT */
            if (*status == SAI__OK) {
              double * noise = (outdata->pntr)[0];
              double * resp = (respmap->pntr)[0];
              double * nep  = (nepdata->pntr)[0];
              size_t thisnepgoodbol = 0;
              double thisnepsum = 0.0;

              nepbolo = (nepdata->dims)[0]*(nepdata->dims)[1];

              if (strlen(refnepunits) == 0) one_strlcpy( refnepunits,
                                                         nepdata->hdr->units,
                                                         sizeof(refnepunits),
                                                         status );

              for (i = 0; i < nepbolo; i++) {
                /* ignore variance since noise will not have any */
                if (noise[i] == VAL__BADD || resp[i] == VAL__BADD) {
                  nep[i] = VAL__BADD;
                } else {
                  nep[i] = (noise[i] / SIMULT) / resp[i];
                }
              }

              /* Clip outlier NEPs */
              msgOutif( MSG__VERB, "", TASK_NAME ": Clipping NEP outliers",
                        status );
              smf_clipnoise( nep, nepbolo, nepcliplog, nepcliplow,
                             nepcliphigh, NULL, status );

              /* Finally, count good NEP values and calculate nepsum */
              for( i=0; i<nepbolo; i++ ) {
                if( nep[i] != VAL__BADD ) {
                  thisnepsum += ( 1.0 / (nep[i] * nep[i]) );
                  thisnepgoodbol++;
                }
              }
              nepsum += thisnepsum;
              nepgoodbol += thisnepgoodbol;
              smf__write_effnoise( nepdata, "EFFNEP", refnepunits, thisnepgoodbol, thisnepsum, status );

            }
            if (*status == SAI__OK && nepdata->file) {
              smf_accumulate_prov( NULL, basegrp, 1, nepdata->file->ndfid,
                                   CREATOR, NULL, status );
            }
            if (nepdata) smf_close_file( wf, &nepdata, status );
          }

          /* Clip outlier noise values now that we've finished with the
             NEP */
          msgOutif( MSG__VERB, "", TASK_NAME ": Clipping NOISE outliers",
                    status );
          smf_clipnoise( (outdata->pntr)[0], noisebolo, noicliplog,
                         noicliplow, noicliphigh, NULL, status );

          /* count good noise values and calculate noisesum */
          for (i = 0; i < noisebolo; i++) {
            double *od = (outdata->pntr)[0];
            if ( od[i] != VAL__BADD ) {
              thisnoisegoodbol++;
              thisnoisesum += 1.0 / od[i];
            }
          }
          noisegoodbol += thisnoisegoodbol;
          noisesum += thisnoisesum;
          smf__write_effnoise( outdata, "EFFNOISE", refunits, thisnoisegoodbol, thisnoisesum, status );

          /* now mask the noise and ratio data using responsivity*/
          if (respmap) {
            double * rdata = (respmap->pntr)[0];
            smf_qual_t * outq = outdata->qual;
            smf_qual_t * ratq = NULL;

            if (ratdata) ratq = ratdata->qual;

            /* set the quality mask */
            for (i = 0; i < noisebolo; i++) {
              if ( rdata[i] == VAL__BADD) {
                outq[i] |= SMF__Q_BADB;
                if (ratq) ratq[i] |= SMF__Q_BADB;
              }
            }

            /* by default we enable the mask */
            if (outdata && outdata->file) {
              ndfSbb( SMF__Q_BADB, outdata->file->ndfid, status );
            }
            if (ratdata && ratdata->file) {
              ndfSbb( SMF__Q_BADB, ratdata->file->ndfid, status );
            }

          }

          if (respmap) smf_close_file( wf, &respmap, status );
        }

        if (*status == SAI__OK && outdata->file) {
          smf_accumulate_prov( NULL, basegrp, 1, outdata->file->ndfid,
                               CREATOR, NULL, status );
        }
        if (outdata) smf_close_file( wf, &outdata, status );
        if (*status == SAI__OK && ratdata && ratdata->file) {
          smf_accumulate_prov( NULL, basegrp, 1, ratdata->file->ndfid,
                               CREATOR, NULL, status );
        }
        if (ratdata) smf_close_file( wf, &ratdata, status );

      } else {
        *status = SAI__ERROR;
        errRepf( FUNC_NAME,
                "Internal error obtaining concatenated data set for chunk %zu",
                 status, contchunk );
      }

      /* Increment the group index counter */
      gcount++;
    }

    /* Close the smfArray */
    smf_close_related( wf, &concat, status );

    /* Annul the configuration keymap. */
    if (keymap) keymap = astAnnul( keymap );
  }


  if (calc_effnoise && noisegoodbol) {
    double noiseeff;
    noiseeff = sqrt( 1.0 / noisesum );
    msgOutf( "", "Effective noise = %g %s from %zu bolometers",
             status, noiseeff, refunits, noisegoodbol );
    parPut0d( "EFFNOISE", noiseeff, status );
    parPut0i( "NOISEGOODBOL", noisegoodbol, status );
  } else {
    parPut0d( "EFFNOISE", VAL__BADD, status );
    parPut0i( "NOISEGOODBOL", VAL__BADI, status );
  }
  if (calc_effnoise && nepgoodbol) {
    double nepeff;
    nepeff = sqrt( 1.0 / nepsum );
    msgOutf( "", "Effective NEP = %g %s from %zu bolometers",
             status, nepeff, refnepunits, nepgoodbol );
    parPut0d( "EFFNEP", nepeff, status );
    parPut0i( "NEPGOODBOL", nepgoodbol, status );
  } else {
    parPut0d( "EFFNEP", VAL__BADD, status );
    parPut0i( "NEPGOODBOL", VAL__BADI, status );
  }


 CLEANUP:
  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. Do not attempt do this if no output files
     were created. */
  if( *status == SAI__OK && ogrp ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines */
  if (igrp) grpDelet( &igrp, status);
  if (ogrp) grpDelet( &ogrp, status);
  if (powgrp) grpDelet( &powgrp, status );
  if (tsgrp) grpDelet( &tsgrp, status );
  if (basegrp) grpDelet( &basegrp, status );
  if( igroup ) smf_close_smfGroup( &igroup, status );
  if( flatramps ) smf_close_related( wf, &flatramps, status );

  ndfEnd( status );

  /* Ensure that FFTW doesn't have any used memory kicking around */
  fftw_cleanup();
}

static smfData *
smf__create_bolfile_extension( ThrWorkForce *wf, const Grp * ogrp, size_t gcount,
                               const smfData *refdata, const char hdspath[],
                               const char datalabel[], const char units[],
                               int * status ) {
  char tempfile[SMF_PATH_MAX];
  char * pname;
  Grp * tempgrp = NULL;
  smfData *newdata = NULL;

  if (*status != SAI__OK) return newdata;

  pname = tempfile;
  grpGet( ogrp, gcount, 1, &pname, SMF_PATH_MAX, status );
  one_strlcat( tempfile, hdspath, sizeof(tempfile), status);
  tempgrp = grpNew( "Ratio", status );
  grpPut1( tempgrp, tempfile, 0, status );
  smf_create_bolfile( wf, tempgrp, 1, refdata, datalabel, units,
                      SMF__MAP_QUAL, &newdata, status );
  if (tempgrp) grpDelet( &tempgrp, status );
  return newdata;

}


static void
smf__write_effnoise( const smfData * data, const char * noisekey,
                     const char * units, size_t ngood, double noisesum,
                     int *status ) {

  double noiseeff = 0.0;
  AstFitsChan * fchan = NULL;
  char noisecom[SZFITSTR];

  if (*status != SAI__OK) return;
  if (!smf_validate_smfData( data, 1, 1, status )) return;

  if (!smf_validate_smfHead( data->hdr, 1, 0, status )) return;

  noiseeff = sqrt( 1.0 / noisesum );

  /* Get the FitsChan */
  fchan = data->hdr->fitshdr;

  /* Add the relevant headers */
  atlPtfti( fchan, "NBOLO", ngood, "Number of bolometers used in eff noise stats",
            status );

  sprintf( noisecom, "[%s] Effective noise in image", units );
  atlPtftd( fchan, noisekey, noiseeff, noisecom, status );

  /* Update the file */
  kpgPtfts( data->file->ndfid, fchan, status );

  return;
}
