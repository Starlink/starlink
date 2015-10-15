/*
*+
*  Name:
*     SC2CLEAN

*  Purpose:
*     Clean SCUBA-2 time-series data.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2clean( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This command is a  stand-alone task for cleaning SCUBA-2
*     time-series data. Cleaning operations include:
*     - flag entire bolometer data streams as bad based on a threshold
*     fraction of bad samples;
*     - removing large-scale detector drifts by fitting and removing
*     low-order polynomial baselines;
*     - identifying and repairing DC steps;
*     - flagging spikes;
*     - replacing spikes and other gaps in the data with a constrained
*     realization of noise; and
*     - applying other frequency-domain filters, such
*     as a high-pass or correction of the DA system response.
*
*     All the above operations can be performed on the dark squid data. These
*     take the same parameters used for cleaning the primary bolometer data
*     but use the "cleandk" namespace. For example, "dcthresh" would become
*     "cleandk.dcthresh".

*  ADAM Parameters:
*     BBM = NDF (Read)
*          Group of files to be used as bad bolometer masks. Each data file
*          specified with the IN parameter will be masked. The corresponding
*          previous mask for a subarray will be used. If there is no previous
*          mask the closest following will be used. It is not an error for
*          no mask to match. A NULL parameter indicates no mask files to be
*          supplied. [!]
*     COM = NDF (Write)
*          If COMPREPROCESS is set in the configuration file, the common mode
*          is calculated and removed from the bolometer data. The COM adam
*          parameter can then be used to specify an NDF to store the
*          common mode. See also GAI. [!]
*     CONFIG = GROUP (Read)
*          Specifies values for the cleaning parameters. If the string
*          "def" (case-insensitive) or a null (!) value is supplied, a
*          set of default configuration parameter values will be used.
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
*          and the available parameters are listed in the "Configuration
*          Parameters" appendix of SUN/258. Default values will be used
*          for any unspecified parameters. Assigning
*          the value "<def>" (case insensitive) to a keyword has the
*          effect of resetting it to its default value. Options
*          available to the map-maker but not understood by SC2CLEAN
*          will be ignored. Parameters not understood will trigger an
*          error. Use the "cleandk." namespace for configuring
*          cleaning parameters for the dark squids. [current value]
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
*     GAI  = NDF (Write)
*          If COMPREPROCESS is set in the configuration file, the common mode
*          is calculated and removed from the bolometer data. The GAI adam
*          parameter can then be used to specify an NDF to store the
*          gain/offset/correlation coefficients of the common-mode template
*          for each bolometer. See also COM. [!]
*     IN = NDF (Read)
*          Input files to be cleaned
*     MAXLEN = _DOUBLE (Read)
*          Maximum length (in seconds) for concatenated file. The
*          default is to use all data if possible (subject to
*          available memory). [!]
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output file(s).
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line). If a null (!) value is supplied no file is created. [!]
*     PADEND = _INTEGER (Read)
*          Number of samples to pad at end. Default is no padding. [!]
*     PADSTART = _INTEGER (Read)
*          Number of samples to pad at start. Default is no padding. [!]
*     RESIST = GROUP (Read)
*          A group expression containing the resistor settings for
*          each bolometer.  Usually specified as a text file using "^"
*          syntax. An example can be found in
*          $STARLINK_DIR/share/smurf/resist.cfg
*          [$STARLINK_DIR/share/smurf/resist.cfg]
*     RESPMASK = _LOGICAL (Read)
*          If true, responsivity data will be used to mask bolometer data
*          when calculating the flatfield. [TRUE]
*     USEDARKS = _LOGICAL (Read)
*          Use darks to mask data. [TRUE]

*  Notes:
*     - The default values and allowed parameters can be found in
*     $SMURF_DIR/smurf_sc2clean.def
*     - An iterative map-maker config file can be used.

*  Related Applications:
*     SMURF: MAKEMAP, SC2CONCAT, SC2FFT

*  Authors:
*     EC: Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-03-27 (EC):
*        Initial version - based on flatfield task
*     2008-04-02 (EC):
*        Added spike flagging
*     2008-07-22 (TIMJ):
*        Use kaplibs for IN OUT params. Filter darks.
*     2009-01-06 (EC):
*        Added FLAGSTAT, use parGdr0X in more places.
*     2009-03-30 (TIMJ):
*        Add OUTFILES parameter.
*     2009-04-17 (EC):
*        Factor out parsing parameters to smf_get_cleanpar
*        Factor filter generation out to smf_filter_fromkeymap
*     2009-04-30 (EC):
*        Use threads
*     2010-01-11 (EC):
*        Add FILLGAPS parameter
*     2010-03-11 (TIMJ):
*        Support flatfield ramps.
*     2010-05-10 (TIMJ):
*        Remove DCBAD and DCFLAGALL. Rename DCBOX to DCFITBOX
*     2010-05-13 (EC):
*        Complete the conversion to smf_fix_steps from smf_correct_steps;
*        add DCMAXSTEPS and DCMEDIANWIDTH, remove DCTHRESH2.
*     2010-05-13 (DSB):
*        Added dclimcorr to smf_fix_steps.
*     2010-05-31 (EC):
*        Factor heavy lifting out to smf_clean_smfData
*     2010-06-03 (EC):
*        -switch to using a config file from many ADAM parameters
*        -use cleandk.* parameters to clean dark squids
*     2010-06-09 (TIMJ):
*        Add sub-instrument support to config reading.
*        Read the config file inside the loop so that the command will
*        run properly with files that are from different sub-instruments.
*     2010-06-10 (EC):
*        Dark squids now have their own quality.
*     2010-06-15 (TIMJ):
*        Add BBM support
*     2010-08-19 (DSB):
*        Complete output NDF history by calling smf_puthistory before exiting.
*     2011-03-30 (EC):
*        Add concatenation functionality from SMURF:SC2CONCAT
*     2011-06-09 (EC):
*        Make it possible to downsample with some re-ordering of the code
*     2011-08-08 (EC):
*        Add ability to export COM and GAI of COMPREPROCESS specified
*     2013-08-21 (AGG):
*        Do not call grpList if no output files are generated. This
*        avoids a GRP__INVID error in such cases.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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
#include "star/atl.h"
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

#define FUNC_NAME "smurf_sc2clean"
#define TASK_NAME "SC2CLEAN"

void smurf_sc2clean( int *status ) {
  smfArray *array = NULL;    /* Data to be cleaned */
  Grp *basegrp=NULL;         /* Grp containing first file each chunk */
  size_t basesize;           /* Number of files in base group */
  smfArray *bbms = NULL;     /* Bad bolometer masks */
  smfArray *concat=NULL;     /* Pointer to a smfArray */
  size_t contchunk;          /* Continuous chunk counter */
  smfArray *darks = NULL;    /* Dark data */
  int ensureflat;            /* Flag for flatfielding data */
  smfArray *flatramps = NULL;/* Flatfield ramps */
  AstKeyMap *heateffmap = NULL;    /* Heater efficiency data */
  smfData *odata = NULL;     /* Pointer to output data struct */
  Grp *fgrp = NULL;          /* Filtered group, no darks */
  size_t gcount=0;           /* Grp index counter */
  size_t idx;                /* Subarray counter */
  Grp *igrp = NULL;          /* Input group of files */
  smfGroup *igroup=NULL;     /* smfGroup corresponding to igrp */
  dim_t maxconcat=0;         /* Longest continuous chunk length in samples */
  double maxlen=0;           /* Constrain maxconcat to this many seconds */
  size_t ncontchunks=0;      /* Number continuous chunks outside iter loop */
  Grp *ogrp = NULL;          /* Output group of files */
  size_t osize;              /* Total number of NDF names in the output group */
  dim_t padStart=0;          /* How many samples padding at start */
  dim_t padEnd=0;            /* How many samples padding at end */
  size_t size;               /* Number of files in input group */
  int temp;                  /* Temporary signed integer */
  int usedarks;              /* flag for using darks */
  ThrWorkForce *wf = NULL;   /* Pointer to a pool of worker threads */
  int writecom;              /* Write COMmon mode to NDF if calculated? */
  int writegai;              /* Write GAIns to NDF if calculated? */

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

  /* Read the input file */
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

  if (size == 0) {
    msgOutif(MSG__NORM, " ","All supplied input frames were filtered,"
       " nothing to do", status );
    goto CLEANUP;
  }

  /* --- Parse ADAM parameters ---------------------------------------------- */

  /* Maximum length of a continuous chunk */
  parGdr0d( "MAXLEN", 0, 0, VAL__MAXD, 1, &maxlen, status );

  /* Padding */
  parGdr0i( "PADSTART", 0, 0, VAL__MAXI, 1, &temp, status );
  padStart = (dim_t) temp;

  parGdr0i( "PADEND", 0, 0, VAL__MAXI, 1, &temp, status );
  padEnd = (dim_t) temp;

  /* Are we using darks? */
  parGet0l( "USEDARKS", &usedarks, status );

  /* Are we flatfielding? */
  parGet0l( "FLAT", &ensureflat, status );

  /* Write COM/GAI to NDFs if calculated? */
  parGet0l( "COM", &writecom, status );
  parGet0l( "GAI", &writegai, status );

  /* Get group of bolometer masks and read them into a smfArray */
  smf_request_mask( wf, "BBM", &bbms, status );

  /* Group the input files by subarray and continuity ----------------------- */
  smf_grp_related( igrp, size, 1, 0, maxlen-padStart-padEnd, NULL, NULL,
                   &maxconcat, NULL, &igroup, &basegrp, NULL, status );

  /* Obtain the number of continuous chunks and subarrays */
  if( *status == SAI__OK ) {
    ncontchunks = igroup->chunk[igroup->ngroups-1]+1;
  }

  basesize = grpGrpsz( basegrp, status );

  /* Get output file(s) */
  kpg1Wgndf( "OUT", basegrp, basesize, basesize,
             "More output files required...",
             &ogrp, &osize, status );

  /* Loop over continuous chunks and clean -----------------------------------*/
  gcount = 1;
  for( contchunk=0;(*status==SAI__OK)&&contchunk<ncontchunks; contchunk++ ) {
    AstKeyMap *keymap=NULL;
    int dkclean;
    AstKeyMap *sub_instruments=NULL;

    /* Place cleaning parameters into a keymap and set defaults. Do
       this inside the loop in case we are cleaning files with
       differing sub-instruments.  Note that we use the map-maker
       defaults file here (which loads the sc2clean defaults) so that
       we populate the locked keymap with all the parameters that
       people may come across to allow them to load their map-maker
       config directly into sc2clean.
    */

    sub_instruments = smf_subinst_keymap( SMF__SUBINST_NONE,
                                          NULL, igrp,
                                          igroup->subgroups[contchunk][0],
                                          status );

    keymap = kpg1Config( "CONFIG", "$SMURF_DIR/smurf_makemap.def",
                         sub_instruments, 1, status );
    if( sub_instruments ) sub_instruments = astAnnul( sub_instruments );

    /* Now rerun smf_grp_related to figure out how long each downsampled
       chunk of data will be. */

    if( basegrp ) grpDelet( &basegrp, status );
    if( igroup ) smf_close_smfGroup( &igroup, status );

    smf_grp_related( igrp, size, 1, 0, maxlen-padStart-padEnd, NULL, keymap,
                     &maxconcat, NULL, &igroup, &basegrp, NULL, status );

    /* Concatenate this continuous chunk */
    smf_concat_smfGroup( wf, NULL, igroup, usedarks ? darks:NULL, bbms, flatramps,
                         heateffmap, contchunk, ensureflat, 1, NULL, 0, NULL,
                         NULL, NO_FTS, padStart, padEnd, 0, &concat, NULL, status );

    if( *status == SAI__OK) {
      /* clean the dark squids now since we might need to use them
         to clean the bolometer data */

      smf_get_cleanpar( keymap, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        &dkclean, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, status );

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

      /* Then the main data arrays */
      if( *status == SAI__OK ) {
        smfArray *com = NULL;
        smfArray *gai = NULL;
        char filename[GRP__SZNAM+1];

        msgOut("", TASK_NAME ": cleaning bolometer data", status );
        smf_clean_smfArray( wf, concat, NULL, &com, &gai, keymap, status );

        /* If ADAM parameters for COM or GAI were specified, and the
           common-mode was calculated, export to files here */

        if( writecom && com ) {
          for( idx=0; (*status==SAI__OK)&&(idx<com->ndat); idx++ ) {
            smf_model_createHdr( com->sdata[idx], SMF__COM, concat->sdata[idx],
                                 status );
            smf_stripsuffix( com->sdata[idx]->file->name,
                             SMF__DIMM_SUFFIX, filename, status );

            smf_dataOrder( wf, com->sdata[idx], 1, status );

            smf_write_smfData( wf, com->sdata[idx], NULL, filename, NULL, 0,
                               NDF__NOID, MSG__NORM, 0, NULL, NULL, status );
          }
        }

        if( writegai && gai ) {
          for( idx=0; (*status==SAI__OK)&&(idx<gai->ndat); idx++ ) {
            smf_model_createHdr( gai->sdata[idx], SMF__GAI, concat->sdata[idx],
                                 status );
            smf_stripsuffix( gai->sdata[idx]->file->name,
                             SMF__DIMM_SUFFIX, filename, status );

            smf_dataOrder( wf, gai->sdata[idx], 1, status );
            smf_write_smfData( wf, gai->sdata[idx], NULL, filename, NULL, 0,
                               NDF__NOID, MSG__NORM, 0, NULL, NULL, status );
          }
        }

        /* Close com and gai */
        if( com ) smf_close_related( wf, &com, status );
        if( gai ) smf_close_related( wf, &gai, status );

      }

      /* Report statistics (currently need a smfArray for that) */
      if (*status == SAI__OK) {
        size_t last_qcount[SMF__NQBITS];
        size_t last_nmap = 0;
        smf_qualstats_report( wf, MSG__VERB, SMF__QFAM_TSERIES, 1, concat,
                              last_qcount, &last_nmap, 1, NULL, NULL, status );
      }

      /* Clean up for contchunk loop */
      if( keymap ) keymap = astAnnul( keymap );
    }

    /* Export concatenated/cleaned data for each subarray to NDF file */
    for( idx=0; (*status==SAI__OK)&&idx<concat->ndat; idx++ ) {
      odata = concat->sdata[idx];

      /* Complete the history information in the output NDF so that it
         includes group parameters accessed since the default history
         information was written to the NDF (in smf_open_and_flatfield). */
      smf_puthistory( odata, "SMURF:SC2CLEAN", status );

      /* Ensure ICD data order */
      smf_dataOrder( wf, odata, 1, status );

      if( odata->file && odata->file->name ) {
        smf_write_smfData( wf, odata, NULL, NULL, ogrp, gcount, NDF__NOID,
                           MSG__VERB, 0, NULL, NULL, status );
      } else {
        *status = SAI__ERROR;
        errRep( FUNC_NAME,
                "Unable to determine file name for concatenated data.",
                status );
      }

      /* Increment the group index counter */
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

 CLEANUP:

  /* Tidy up after ourselves: release the resources used by the grp routines */
  if( darks ) smf_close_related( wf, &darks, status );
  if( flatramps ) smf_close_related( wf, &flatramps, status );
  if (heateffmap) heateffmap = smf_free_effmap( heateffmap, status );
  if( bbms ) smf_close_related( wf, &bbms, status );
  if( igrp ) grpDelet( &igrp, status);
  if( ogrp ) grpDelet( &ogrp, status);
  if( basegrp ) grpDelet( &basegrp, status );
  if( igroup ) smf_close_smfGroup( &igroup, status );
  fftw_cleanup();
  ndfEnd( status );
}
