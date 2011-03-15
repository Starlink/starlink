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
*          and the list of parameters is explained in the
*          "Configuration Parameters" section and can be found, with
*          defaults, in $SMURF_DIR/smurf_sc2clean.def.  Default values
*          will be used for any unspecified parameters. Assigning the
*          value "<def>" (case insensitive) to a keyword has the
*          effect of resetting it to its default value. Options
*          available to the map-maker but not understood by SC2CLEAN
*          will be ignored. Parameters not understood will trigger an
*          error. Use the "cleandk." namespace for configuring
*          cleaning parameters for the dark squids. [current value]
*     IN = NDF (Read)
*          Input files to be cleaned
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

*  Configuration Parameters:
*     APOD = INTEGER Apodize signals (smoothly roll-off) using
*       sine/cosine functions at start and end of the signal across
*       this many samples. The supplied APOD value is ignored and a
*       value of zero is used if ZEROPAD is set to 0.
*     BADFRAC = REAL
*       Flag entire bolometer as dead if at least this fraction of the samples
*       in a detector time series were flagged as bad by the DA system.
*     DCFITBOX = REAL
*       Number of samples (box size) used on either side of a DC step to
*       estimate the height of the step.
*     DCMAXSTEPS = INTEGER
*       The maximum number of steps that can be corrected in each minute of
*       good data (i.e. per 12000 samples) from a bolometer before the entire
*       bolometer is flagged as bad. A value of zero will cause a bolometer to
*       be rejected if any steps are found in the bolometer data stream.
*     DCSMOOTH = INTEGER
*       The width of the median filter used to smooth a bolometer data stream
*       prior to finding DC steps.
*     DCTHRESH = REAL
*       Threshold S/N to detect and flag DC (baseline) steps.
*     DKCLEAN = LOGICAL
*       Clean the bolometers using the dark squids. Defaults to false.
*     FILLGAPS = LOGICAL
*       Fill vicinity of spikes / DC steps with constrained realization of
*       noise. Also (unless ZEROPAD is 1), fill the padded region at the start
*       and end of each time stream with artificial data. You almost always
*       want to do this.
*     FILT_EDGEHIGH = REAL
*       Hard-edge high-pass frequency-domain filter (Hz).
*     FILT_EDGELOW = REAL
*       Hard-edge low-pass frequency-domain filter (Hz).
*     FILT_NOTCHHIGH( ) = REAL
*       Hard-edge band-cut frequency-domain notch filters. FILT_NOTCHHIGH is
*       an array of upper-edge frequencies (Hz).
*     FILT_NOTCHLOW( ) = REAL
*       Array of lower-edge frequencies corresponding to FILT_NOTCHHIGH.
*     FLAGSTAT = REAL
*       Flag data taken while the telescope was stationary so that it
*       they are ignored in the final map. The value given is a threshold
*       slew velocity (arcsec/sec) measured in tracking coordinates
*       below which the telescope is considered to be stationary.
*     ORDER = INTEGER
*       Subtract a fitted baseline polynomial of this order (0 to remove mean).
*     SPIKEBOX = INTEGER
*       The size of the filter box for the sigma-clipper.
*     SPIKETHRESH = REAL
*       Threshold S/N to flag spikes using sigma-clipper.
*     ZEROPAD = LOGICAL
*       Determines the nature of the padding added to the start and end of
*       each bolometer time stream. Padding is needed to avoid
*       interaction between the data values at the start and end of each
*       bolometer time stream, due to the cyclic wrap-around nature of the
*       FFTs used to filter each time stream. If ZEROPAD is set to 1, the
*       padded sections will be filled with zeros. This requires that the
*       data also be apodised (see APOD) to avoid ringing due the sudden
*       steps down to zero at the start and end of each time stream). If
*       ZEROPAD is set to 0, then the padded regions will be filled with
*       artificial data that links the data values at the start and end of
*       the time stream smoothly. In this case, no apodisation is
*       performed, and the value of the APOD parameter is ignored. The
*       default for ZEROPAD is 0 (i.e. pad with artificial data rather
*       than zeros) unless FILLGAPS is zero, in which case the supplied
*       ZEROPAD value is ignored and a value of 1 is always used.

*  Notes:
*     - Replacing spikes with noise is not yet implemented.
*     - Assumes that padding has been applied externally (e.g. in SC2CONCAT)
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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2008-2010 University of British Columbia.
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
  smfArray *array = NULL;   /* Data to be cleaned */
  smfArray *bbms = NULL;    /* Bad bolometer masks */
  smfArray *darks = NULL;   /* Dark data */
  smfArray *flatramps = NULL;/* Flatfield ramps */
  smfData *ffdata = NULL;   /* Pointer to output data struct */
  Grp *fgrp = NULL;         /* Filtered group, no darks */
  size_t i = 0;             /* Counter, index */
  Grp *igrp = NULL;         /* Input group of files */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Total number of NDF names in the output group */
  size_t size;              /* Number of files in input group */
  smfWorkForce *wf = NULL;  /* Pointer to a pool of worker threads */

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = smf_create_workforce( smf_get_nthread( status ), status );

  /* Read the input file */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_science( igrp, &fgrp, 1, NULL, NULL, 1, 1, SMF__NULL, &darks,
                    &flatramps, NULL, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  if (size > 0) {
    /* Get output file(s) */
    kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
               &ogrp, &outsize, status );
  } else {
    msgOutif(MSG__NORM, " ","All supplied input frames were filtered,"
       " nothing to do", status );
  }

  /* Get group of bolometer masks and read them into a smfArray */
  smf_request_mask( "BBM", &bbms, status );

  /* Loop over input files */
  if( *status == SAI__OK ) for( i=1; i<=size; i++ ) {
    AstKeyMap * sub_instruments = NULL;
    AstKeyMap * keymap = NULL;

    /* Open and flatfield in case we're using raw data */
    smf_open_and_flatfield(igrp, ogrp, i, darks, flatramps, &ffdata, status);

    /* Apply a mask to the quality array and data array */
    smf_apply_mask( ffdata, bbms, SMF__BBM_QUAL|SMF__BBM_DATA, 0, status );

    /* Place cleaning parameters into a keymap and set defaults. Do
       this inside the loop in case we are cleaning files with
       differing sub-instruments.  Note that we use the map-maker
       defaults file here (which loads the sc2clean defaults) so that
       we populate the locked keymap with all the parameters that
       people may come across to allow them to load their map-maker
       config directly into sc2clean.
    */

    sub_instruments = smf_subinst_keymap( ffdata, NULL, 0, status );
    keymap = kpg1Config( "CONFIG", "$SMURF_DIR/smurf_makemap.def",
                         sub_instruments, status );
    sub_instruments = astAnnul( sub_instruments );

    if (*status != SAI__OK) {
      /* Tell the user which file went bad... */
      /* Might be user-friendly to trap 1st etc and then continue on... */
      msgSeti("I",i);
      msgSeti("N",size);
      errRep(FUNC_NAME,	"Error opening file ^I of ^N", status);
      if (size > 1 && i != size) errFlush(status);
    } else {
      int dkclean;

      /* clean the dark squids now since we might need to use them
         to clean the bolometer data */

      smf_get_cleanpar( keymap, NULL, NULL, NULL, NULL, NULL, NULL, &dkclean,
                        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, status );

      if( dkclean && ffdata && ffdata->da && ffdata->da->dksquid ) {
        smfData *dksquid = ffdata->da->dksquid;
        AstKeyMap *kmap=NULL;

        msgOut("", TASK_NAME ": cleaning dark squids", status);

        /* fudge the header so that we can get at JCMTState */
        dksquid->hdr = ffdata->hdr;

        /* clean darks using cleandk.* parameters */
        astMapGet0A( keymap, "CLEANDK", &kmap );
        array = smf_create_smfArray( status );
        smf_addto_smfArray( array, dksquid, status );
        smf_clean_smfArray( wf, array, NULL, kmap, status );
        if( array ) {
          array->owndata = 0;
          smf_close_related( &array, status );
        }
        if( kmap ) kmap = astAnnul( kmap );

        /* Unset hdr pointer so that we don't accidentally close it */
        dksquid->hdr = NULL;
      }

      msgOut("", TASK_NAME ": cleaning bolometer data", status );

      /* Clean the data */
      array = smf_create_smfArray( status );
      smf_addto_smfArray( array, ffdata, status );
      smf_clean_smfArray( wf, array, NULL, keymap, status );
      if( array ) {
        array->owndata = 0;
        smf_close_related( &array, status );
      }

      /* Ensure that the data is ICD ordered before closing */
      smf_dataOrder( ffdata, 1, status );

      /* Report statistics (currently need a smfArray for that) */
      if (*status == SAI__OK) {
        smfArray *tmparr = smf_create_smfArray( status );
        size_t last_qcount[SMF__NQBITS];
        size_t last_nmap = 0;
        if (tmparr) tmparr->owndata = 0;  /* someone else owns smfData */
        smf_addto_smfArray( tmparr, ffdata, status );
        smf_qualstats_report( MSG__VERB, SMF__QFAM_TSERIES, 1, tmparr,
                              last_qcount, &last_nmap, 1, NULL, NULL, status );
        smf_close_related( &tmparr, status );
      }
    }

    /* Complete the history iunformation in the output NDF so that it
       includes group parameters accessed since the default history
       information was written to the NDF (in smf_open_and_flatfield). */
    smf_puthistory( ffdata, "SMURF:SC2CLEAN", status );

    /* Free resources for output data */
    smf_close_file( &ffdata, status );
    if( keymap ) keymap = astAnnul( keymap );

  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines */
  if (darks) smf_close_related( &darks, status );
  if (flatramps) smf_close_related( &flatramps, status );
  if (bbms) smf_close_related( &bbms, status );
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);
  if( wf ) wf = smf_destroy_workforce( wf );
  fftw_cleanup();
  ndfEnd( status );
}
