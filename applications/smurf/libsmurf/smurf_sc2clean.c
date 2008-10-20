/*
*+
*  Name:
*     SC2CLEAN

*  Purpose:
*     Top-level SCUBA-2 data cleaning routine

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
*     This is the main routine implementing the stand-alone SC2CLEAN
*     task.  Cleaning operations include: (i) flag entire bolometer
*     data streams as bad based on a threshold fraction of bad
*     samples; (ii) removing large-scale detector drifts by fitting
*     and removing low-order polynomial baselines; (iii) identifying
*     and repairing DC steps; (iv) flagging spikes; (v) replacing
*     spikes and other gaps in the data with a constrained realization
*     of noise; and (vi) applying other frequency-domain filters, such
*     as a high-pass or correction of the DA system response.

*  Notes:
*     Point (v) is not yet implemented.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be uncompressed and flatfielded
*     OUT = NDF (Write)
*          Output file
*     BADFRAC = _DOUBLE (Read)
*          Fraction of bad samples in order for entire bolometer to be
*          flagged as bad
*     DCTHRESH = _DOUBLE (Read)
*          N-sigma threshold at which to detect DC steps
*     DCBOX = INTEGER (Read)
*          Width of the box (samples) over which to estimate the mean
*          signal level for DC step detection
*     DCBAD - _LOGICAL (Read)
*          If true, instead of repaiting DC steps, flag bolo as bad
*     DKSQUID = _LOGICAL (Read)
*          If true fit and remove dark squid signals
*     FILT_EDGEHIGH = _DOUBLE (Read)
*          Apply a hard-edged high-pass filter at this frequency (Hz)
*     FILT_EDGELOW = _DOUBLE (Read)
*          Apply a hard-edged low-pass filter at this frequency (Hz)
*     FILT_NOTCHHIGH = _DOUBLE (Read)
*          Array of upper-frequency edges for hard notch filters (Hz)
*     FILT_NOTCHLOW = _DOUBLE (Read)
*          Array of lower-frequency edges for hard notch filters (Hz)
*     ORDER = INTEGER (Read)
*          Fit and remove polynomial baselines of this order
*     SPIKETHRESH = _DOUBLE (Read)
*          Flag spikes SPIKETHRESH-sigma away from mean
*     SPIKEITER = INTEGER (Read)
*          If 0 iteratively find spikes until convergence. Otherwise 
*          execute precisely this many iterations.

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

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2008 University of British Columbia.
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

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smurf_sc2clean"
#define TASK_NAME "SC2CLEAN"

void smurf_sc2clean( int *status ) {

  size_t aiter;             /* Number of iterations in sigma-clipper */
  double badfrac=0;         /* Fraction of bad samples to flag bad bolo */
  smfArray *darks = NULL;          /* Dark data */
  int dcbad;                /* Flag bolometers with steps as bad */
  dim_t dcbox=0;            /* width of box for measuring DC steps */
  int dcbox_s=0;            /* signed int version of dcbox */
  double dcthresh=0;        /* n-sigma threshold for DC steps */
  int dkclean;              /* Flag for dark squid cleaning */
  smfData *ffdata = NULL;   /* Pointer to output data struct */
  Grp *fgrp = NULL;         /* Filtered group, no darks */
  size_t i = 0;             /* Counter, index */
  int j;                    /* Loop counter */
  Grp *igrp = NULL;         /* Input group of files */
  unsigned char mask;       /* Bitmask for quality */
  size_t nflag;             /* Number of flagged samples */
  Grp *ogrp = NULL;         /* Output group of files */
  int order;                /* Order of polynomial for baseline fitting */
  size_t outsize;           /* Total number of NDF names in the output group */
  size_t size;              /* Number of files in input group */
  double spikethresh;       /* Threshold for finding spikes */
  size_t spikeiter=0;       /* Number of iterations for spike finder */
  int spikeiter_s;          /* Signed int version of spikeiter_s */

  int dofft=0;              /* Set if freq. domain filtering the data */
  double f_edgelow;         /* Freq. cutoff for low-pass edge filter */
  double f_edgehigh;        /* Freq. cutoff for high-pass edge filter */
  int f_nnotch=0;           /* Number of notch filters in array */
  int f_nnotch2=0;          /* Number of notch filters in array */
  double f_notchlow[SMF__MXNOTCH]; /* Array low-freq. edges of notch filters */
  double f_notchhigh[SMF__MXNOTCH];/* Array high-freq. edges of notch filters */
  smfFilter *filt=NULL;     /* Pointer to filter struct */

  /* Main routine */
  ndfBegin();

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

  /* Check for badfrac threshold for flagging bad bolos */
  parGet0d( "BADFRAC", &badfrac, status );

  /* Check for DC step correction paramaters */
  parGet0d( "DCTHRESH", &dcthresh, status );
  parGdr0i( "DCBOX", 1000, 0, NUM__MAXI, 1, &dcbox_s, status );
  if( *status == SAI__OK ) {
    dcbox = (dim_t) dcbox_s;
  }
  parGet0l( "DKCLEAN", &dkclean, status );

  /* Order of polynomial for baseline fits */
  parGet0i( "ORDER", &order, status );

  /* Spike flagging */
  parGet0d( "SPIKETHRESH", &spikethresh, status );
  parGdr0i( "SPIKEITER", 0, 0, NUM__MAXI, 1, &spikeiter_s, status );
  if( *status == SAI__OK ) {
    spikeiter = (size_t) spikeiter_s;
  }
  parGet0l( "DKCLEAN", &dcbad, status );

  /* Clean dark squids */
  parGet0l( "DKCLEAN", &dkclean, status );

  /* Frequency-domain filtering */
  parGet0d( "FILT_EDGELOW", &f_edgelow, status );
  parGet0d( "FILT_EDGEHIGH", &f_edgehigh, status );
  if( f_edgelow || f_edgehigh ) {
    dofft = 1;
  }

  parGet1d( "FILT_NOTCHLOW", SMF__MXNOTCH, f_notchlow, &f_nnotch, status ); 
  parGet1d( "FILT_NOTCHHIGH", SMF__MXNOTCH, f_notchhigh, &f_nnotch2, status ); 
  if( f_nnotch ) {
    /* Number of upper and lower edges must match */
    if( f_nnotch != f_nnotch2 ) {
      msgOut( "", FUNC_NAME "Number of upper and lower notch edges don't match,"
              "no notch filters will be applied.", status );
      f_nnotch = 0;
    } else {
      for( j=0; j<f_nnotch; j++ ) {
        if( f_notchlow[j] != f_notchhigh[j] ) {
          dofft = 1;
        }
      }
    }
  }

  /* Loop over input files */
  if( *status == SAI__OK ) for( i=1; i<=size; i++ ) {

    /* Open and flatfield in case we're using raw data */
    smf_open_and_flatfield(igrp, ogrp, i, darks, &ffdata, status);

    /* Which QUALITY bits should be considered for ignoring data */
    mask = ~SMF__Q_JUMP;

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

      smf_flag_spikes( ffdata, NULL, mask, spikethresh, spikeiter, 100, &aiter, 
                       &nflag, status );

      if( *status == SAI__OK ) {
	msgSeti("AITER",aiter);
	msgOutif(MSG__VERB," ", "Finished in ^AITER iterations",
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

    /* frequency-domain filtering */
    if( dofft ) {
      msgOutif( MSG__VERB," ", "Apply frequency domain filter", status );
      
      filt = smf_create_smfFilter( ffdata, status );
      
      if( f_edgelow ) {
        smf_filter_edge( filt, f_edgelow, 1, status );
      }
      
      if( f_edgehigh ) {
        smf_filter_edge( filt, f_edgehigh, 0, status );
      }
      
      if( f_nnotch ) {
        smf_filter_notch( filt, f_notchlow, f_notchhigh, f_nnotch,
                          status );
      }
      
      smf_filter_execute( ffdata, filt, status );
      
      filt = smf_free_smfFilter( filt, status );
    }


    /* Ensure that the data is ICD ordered before closing */
    smf_dataOrder( ffdata, 1, status );

    /* Free resources for output data */
    smf_close_file( &ffdata, status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines */
  if (darks) smf_close_related( &darks, status );
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
