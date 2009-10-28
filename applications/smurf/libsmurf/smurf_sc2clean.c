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
*     - (i) flag entire bolometer
*     data streams as bad based on a threshold fraction of bad
*     samples;
*     - (ii) removing large-scale detector drifts by fitting
*     and removing low-order polynomial baselines;
*     - (iii) identifying and repairing DC steps;
*     - (iv) flagging spikes;
*     - (v) replacing spikes and other gaps in the data with a constrained
*     realization of noise; and
*     - (vi) applying other frequency-domain filters, such
*     as a high-pass or correction of the DA system response.

*  Notes:
*     - Point (v) is not yet implemented.

*  ADAM Parameters:
*     APOD = _INTEGER (Read)
*          Apodize time series start and end with a Hanning window that rolls
*          off in APOD samples.
*     BADFRAC = _DOUBLE (Read)
*          Fraction of bad samples in order for entire bolometer to be
*          flagged as bad
*     DCBAD = _LOGICAL (Read)
*          If true, instead of repairing DC steps, flag bolo as bad
*     DCBOX = _INTEGER (Read)
*          Width of the box (samples) over which to estimate the mean
*          signal level for DC step detection
*     DCTHRESH = _DOUBLE (Read)
*          N-sigma threshold at which to detect DC steps
*     DKCLEAN = _LOGICAL (Read)
*          If true fit and remove dark squid signals
*     FILT_EDGEHIGH = _DOUBLE (Read)
*          Apply a hard-edged high-pass filter at this frequency (Hz)
*     FILT_EDGELOW = _DOUBLE (Read)
*          Apply a hard-edged low-pass filter at this frequency (Hz)
*     FILT_NOTCHHIGH = _DOUBLE (Read)
*          Array of upper-frequency edges for hard notch filters (Hz)
*     FILT_NOTCHLOW = _DOUBLE (Read)
*          Array of lower-frequency edges for hard notch filters (Hz)
*     FLAGSTAT = _DOUBLE (Read)
*          Flag data during slew speeds less than FLAGSTAT (arcsec/sec)
*     IN = NDF (Read)
*          Input files to be uncompressed and flatfielded
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     ORDER = INTEGER (Read)
*          Fit and remove polynomial baselines of this order
*     OUT = NDF (Write)
*          Output file(s)
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line). If a null (!) value is supplied no file is created. [!]
*     SPIKEITER = _INTEGER (Read)
*          If 0 iteratively find spikes until convergence. Otherwise
*          execute precisely this many iterations.
*     SPIKETHRESH = _DOUBLE (Read)
*          Flag spikes SPIKETHRESH-sigma away from mean

*  Related Applications:
*     SMURF: SC2FFT

*  Authors:
*     Edward Chapin (UBC)
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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2008-2009 University of British Columbia.
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

  size_t aiter;             /* Number of iterations in sigma-clipper */
  size_t apod=0;            /* Length of apodization window */
  double badfrac=0;         /* Fraction of bad samples to flag bad bolo */
  smfArray *darks = NULL;   /* Dark data */
  int dcbad;                /* Flag bolometers with steps as bad */
  dim_t dcbox=0;            /* width of box for measuring DC steps */
  double dcthresh=0;        /* n-sigma threshold for DC steps */
  int dkclean;              /* Flag for dark squid cleaning */
  double flagstat;          /* Threshold for flagging stationary regions */
  smfData *ffdata = NULL;   /* Pointer to output data struct */
  Grp *fgrp = NULL;         /* Filtered group, no darks */
  size_t i = 0;             /* Counter, index */
  Grp *igrp = NULL;         /* Input group of files */
  AstKeyMap *keymap;        /* Keymap for storing cleaning parameters */
  size_t nflag;             /* Number of flagged samples */
  Grp *ogrp = NULL;         /* Output group of files */
  int order;                /* Order of polynomial for baseline fitting */
  size_t outsize;           /* Total number of NDF names in the output group */
  size_t size;              /* Number of files in input group */
  double spikethresh;       /* Threshold for finding spikes */
  size_t spikeiter=0;       /* Number of iterations for spike finder */
  int dofft=0;              /* Set if freq. domain filtering the data */
  smfFilter *filt=NULL;     /* Pointer to filter struct */
  smfWorkForce *wf = NULL;  /* Pointer to a pool of worker threads */

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = smf_create_workforce( smf_get_nthread( status ), status );

  /* Read the input file */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_darks( igrp, &fgrp, NULL, 1, SMF__NULL, &darks, status );

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
    msgOutif(MSG__NORM, " ","All supplied input frames were DARK,"
       " nothing to do", status );
  }

  /* Place cleaning parameters into a keymap and extract values */
  keymap = astKeyMap( " " );
  if( !astOK ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "ast error detected creating new astKeyMap", status );
  } else {
    atlGetParam( "APOD", keymap, status );
    atlGetParam( "BADFRAC", keymap, status );
    atlGetParam( "DCTHRESH", keymap, status );
    parGet0l( "DCBAD", &dcbad, status );
    astMapPut0I( keymap, "DCBAD", dcbad, NULL );
    atlGetParam( "DCBOX", keymap, status );
    parGet0l( "DKCLEAN", &dkclean, status );
    astMapPut0I( keymap, "DKCLEAN", dkclean, NULL );
    atlGetParam( "FILT_EDGEHIGH", keymap, status );
    atlGetParam( "FILT_EDGELOW", keymap, status );
    atlGetParam( "FILT_NOTCHHIGH", keymap, status );
    atlGetParam( "FILT_NOTCHLOW", keymap, status );
    atlGetParam( "FLAGSTAT", keymap, status );
    atlGetParam( "ORDER", keymap, status );
    atlGetParam( "SPIKEITER", keymap, status );
    atlGetParam( "SPIKETHRESH", keymap, status );

    smf_get_cleanpar( keymap, &apod, &badfrac, &dcbox, &dcbad, &dcthresh,
                      &dkclean, NULL, NULL, NULL, NULL, NULL, NULL,
                      &flagstat, &order, &spikethresh, &spikeiter, status );
  }

  /* Loop over input files */
  if( *status == SAI__OK ) for( i=1; i<=size; i++ ) {

    /* Open and flatfield in case we're using raw data */
    smf_open_and_flatfield(igrp, ogrp, i, darks, &ffdata, status);

    /* Check status to see if data are already flatfielded */
    if (*status == SMF__FLATN) {
      errAnnul( status );
      msgOutif(MSG__VERB," ",
	     "Data are already flatfielded", status);
    } else if ( *status == SAI__OK) {
      msgOutif(MSG__VERB," ", "Flat field applied", status);
    } else {
      /* Tell the user which file it was... */
      /* Would be user-friendly to trap 1st etc... */
      /*errFlush( status );*/
      msgSeti("I",i);
      msgSeti("N",size);
      errRep(FUNC_NAME,	"Unable to flatfield data from file ^I of ^N", status);
    }

    /* Update quality flags to match bad samples, and to apply badfrac */
    smf_update_quality( ffdata, NULL, 1, NULL, badfrac, status );

    /* Remove baselines */
    if( order >= 0 ) {
      msgSeti("ORDER",order);
      msgOutif(MSG__VERB," ",
               "Fitting and removing ^ORDER-order polynomial baselines",
               status);
      smf_scanfit( ffdata, NULL, order, status );
      smf_subtract_poly( ffdata, NULL, 0, status );
    }

    /* Fix large DC steps */
    if( dcthresh && dcbox ) {
      msgSetd("DCTHRESH",dcthresh);
      msgSeti("DCBOX",dcbox);
      if( dcbad ) {
        msgOutif(MSG__VERB," ",
                 "Flagging bolos with ^DCTHRESH-sigma DC steps in ^DCBOX "
                 "samples", status);
      } else {
        msgOutif(MSG__VERB," ",
	       "Fixing DC steps of size ^DCTHRESH-sigma in ^DCBOX samples",
                 status);
      }
      smf_correct_steps( ffdata, NULL, dcthresh, dcbox, dcbad, status );
    }

    /* Flag spikes */
    if( spikethresh ) {
      msgSetd("SPIKETHRESH",spikethresh);
      msgSeti("SPIKEITER",spikeiter);

      if( !spikeiter ) {
	msgOutif(MSG__VERB," ",
		 "Flagging ^spikethresh-sigma spikes iteratively to "
                 "convergence.", status);

      } else {
	msgOutif(MSG__VERB," ",
		 "Flagging ^spikethresh-sigma spikes with ^spikeiter "
                 "iterations", status);
      }

      smf_flag_spikes( ffdata, NULL, NULL, SMF__Q_MOD, spikethresh, spikeiter,
                       100, &aiter, &nflag, status );

      if( *status == SAI__OK ) {
	msgSeti("AITER",aiter);
	msgOutif(MSG__VERB," ", "Finished in ^AITER iterations",
		 status);
      }
    }

    /* Flag periods of stationary pointing */
    if( flagstat ) {
      msgSetd("THRESH",flagstat);
      msgOutif(MSG__VERB, "",
               "Flagging regions with speeds < ^THRESH arcsec/sec", status );
      smf_flag_stationary( ffdata, NULL, flagstat, &nflag, status );
      if( *status == SAI__OK ) {
	msgSeti("N",nflag);
	msgOutif(MSG__VERB," ", "^N new time slices flagged",
		 status);
      }
    }

    /* Clean out the dark squid signal */
    if( dkclean ) {
	msgOutif(MSG__VERB," ",
		 "Cleaning dark squid signals from data.",
		 status);
      smf_clean_dksquid( ffdata, NULL, 0, 100, NULL, 0, 0, status );
    }

    /* Apodization */
    if( apod ) {
      msgOutif(MSG__VERB," ",
               "Apodizing data.",
               status);
      smf_apodize( ffdata, NULL, apod, status );
    }

    /* frequency-domain filtering */
    filt = smf_create_smfFilter( ffdata, status );
    smf_filter_fromkeymap( filt, keymap, &dofft, status );

    if( dofft ) {
      msgOutif( MSG__VERB," ", "Apply frequency domain filter", status );
      smf_filter_execute( wf, ffdata, filt, status );
      smf_convert_bad( ffdata, status );
    }

    filt = smf_free_smfFilter( filt, status );


    /* Ensure that the data is ICD ordered before closing */
    smf_dataOrder( ffdata, 1, status );

    /* Free resources for output data */
    smf_close_file( &ffdata, status );
  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines */
  if (darks) smf_close_related( &darks, status );
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);
  if( keymap ) keymap = astAnnul( keymap );
  if( wf ) wf = smf_destroy_workforce( wf );
  fftw_cleanup();
  ndfEnd( status );
}
