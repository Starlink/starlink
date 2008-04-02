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
*     Points (iv-vi) are not yet implemented.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be uncompressed and flatfielded
*     OUT = NDF (Write)
*          Output file
*     BADFRAC = _DOUBLE (READ)
*          Fraction of bad samples in order for entire bolometer to be
*          flagged as bad
*     DCTHRESH = _DOUBLE (Read)
*          N-sigma threshold at which to detect DC steps
*     DCBOX = INTEGER (Read)
*          Width of the box (samples) over which to estimate the mean
*          signal level for DC step detection
*     ORDER = INTEGER (Read)
*          Fit and remove polynomial baselines of this order

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-03-27 (EC):
*        Initial version - based on flatfield task
*     2008-04-02 (EC):
*        Added spike flagging

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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

  int aiter;                /* Number of iterations in sigma-clipper */
  double badfrac=0;         /* Fraction of bad samples to flag bad bolo */
  dim_t dcbox=0;            /* width of box for measuring DC steps */
  double dcthresh=0;        /* n-sigma threshold for DC steps */
  smfData *ffdata = NULL;   /* Pointer to output data struct */
  int flag;                 /* Flag for how group is terminated */
  int i = 0;                /* Counter, index */
  Grp *igrp = NULL;         /* Input group of files */
  Grp *ogrp = NULL;         /* Output group of files */
  int order;                /* Order of polynomial for baseline fitting */
  int outsize;              /* Total number of NDF names in the output group */
  int size;                 /* Number of files in input group */
  double spikethresh;       /* Threshold for finding spikes */
  unsigned int spikeiter;   /* Number of iterations for spike finder */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get output file(s) */
  ndgCreat( "OUT", igrp, &ogrp, &outsize, &flag, status );

  /* Check for badfrac threshold for flagging bad bolos */
  parGet0d( "BADFRAC", &badfrac, status );

  /* Check for DC step correction paramaters */
  parGet0d( "DCTHRESH", &dcthresh, status );
  parGet0i( "DCBOX", &dcbox, status );

  /* Order of polynomial for baseline fits */
  parGet0i( "ORDER", &order, status );

  /* Spike flagging */
  parGet0d( "SPIKETHRESH", &spikethresh, status );
  parGet0i( "SPIKEITER", &spikeiter, status );

  /* Loop over input files */
  for( i=1; i<=size; i++ ) {

    /* Open and flatfield in case we're using raw data */
    smf_open_and_flatfield(igrp, ogrp, i, &ffdata, status);

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
    msgSeti("ORDER",order);
    msgOutif(MSG__VERB," ",
	     "Fitting and removing ^ORDER-order polynomial baselines", 
	     status);  
    smf_scanfit( ffdata, order, status );
    smf_subtract_poly( ffdata, 0, status );

    /* Fix large DC steps */
    if( dcthresh && dcbox ) {
      msgSetd("DCTHRESH",dcthresh);
      msgSeti("DCBOX",dcbox);
      msgOutif(MSG__VERB," ",
	       "Fixing DC steps of size ^DCTHRESH-sigma in ^DCBOX samples", 
	       status); 
      smf_correct_steps( ffdata, NULL, dcthresh, dcbox, status );
    }
    
    /* Flag spikes */
    if( spikethresh && (spikeiter>=0) ) {
      msgSetd("SPIKETHRESH",spikethresh);
      msgSeti("SPIKEITER",spikeiter);

      if( !spikeiter ) {
	msgOutif(MSG__VERB," ",
		 "Flagging ^spikethresh-sigma spikes iteratively to convergence.",
		 status);
	
      } else {
	msgOutif(MSG__VERB," ",
		 "Flagging ^spikethresh-sigma spikes with ^spikeiter iterations",
		 status);
      }

      smf_flag_spikes( ffdata, NULL, spikethresh, spikeiter, 100, &aiter,
		       status );
      if( *status == SAI__OK ) {
	msgSeti("AITER",aiter);
	msgOutif(MSG__VERB," ", "Finished in ^AITER iterations",
		 status); 
      }
    }

    /* Ensure that the data is ICD ordered before closing */
    smf_dataOrder( ffdata, 1, status );

    /* Free resources for output data */
    smf_close_file( &ffdata, status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
