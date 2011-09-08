/*
*+
*  Name:
*     SC2PCA

*  Purpose:
*     Use principal component analysis to identify correlated SCUBA-2 signals

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2pca( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculates a new set of N statistically independent
*     basis vectors (i.e. with a diagonal covariance matrix) for the N
*     bolometer time series, and calculates the projection of the
*     bolometers along this new basis. This "Principal Component
*     Analysis" is useful for identifying time-correlated noise
*     signals. The ouput array of components contains the new basis
*     vectors, normalized by their RMS, though ordered by decreasing
*     significance. The output amplitudes data cube gives the
*     amplitude of each component for each bolometer across the focal
*     plane. Generally speaking the component time series illustrate
*     the time-varying shape of the correlated signals, and the
*     amplitudes show how strong they are, and which bolometers are
*     affected.

*  Notes:

*  ADAM Parameters:
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
*     IN = NDF (Read)
*          Input files to be uncompressed and flatfielded. Any darks provided
*          will be subtracted prior to flatfielding.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUTAMP = NDF (Write)
*          Amplitude data cube. The first two coordinates are bolometer
*          location, and the third enumerates component.
*     OUTAMPFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names
*          of all the output amplitude NDFs created by this
*          application (one per line). If a NULL (!) value is supplied
*          no file is created. [!]
*     OUTCOMP = NDF (Write)
*          Component vector data cubes (N components * 1 * M time slices). A
*          cube is created so that it may be run through SC2FFT if desired.
*     OUTCOMPFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names
*          of all the output component NDFs created by this
*          application (one per line). If a NULL (!) value is supplied
*          no file is created. [!]

*  Related Applications:
*     SMURF: SC2CLEAN, SC2FFT

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-03-18 (EC):
*        Initial version based on smurf_extinction
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2011 University of British Columbia.
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
#include "par.h"
#include "par_err.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smurf_sc2pca"
#define TASK_NAME "SC2PCA"

void smurf_sc2pca( int *status ) {

  smfData *amplitudes=NULL;  /* Amplitudes of each component */
  smfArray *bbms=NULL;       /* Bad bolometer masks */
  smfData *components=NULL;  /* Components */
  smfArray *darks=NULL ;     /* Dark data */
  int ensureflat;            /* Flag for flatfielding data */
  smfData *data=NULL;        /* Pointer to input smfData */
  Grp *fgrp=NULL;            /* Filtered group, no darks */
  smfArray *flatramps=NULL;  /* Flatfield ramps */
  AstKeyMap *heateffmap = NULL;    /* Heater efficiency data */
  size_t i=0;                /* Counter, index */
  Grp *igrp=NULL;            /* Input group of files */
  Grp *outampgrp=NULL;       /* Output amplitude group of files */
  Grp *outcompgrp=NULL;      /* Output component group of files */
  size_t outampsize;         /* Total number of NDF names in ocompgrp */
  size_t outcompsize;        /* Total number of NDF names in ocompgrp */
  size_t size;               /* Number of files in input group */
  smfWorkForce *wf=NULL;     /* Pointer to a pool of worker threads */

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = smf_get_workforce( smf_get_nthread( status ), status );

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Are we flatfielding? */
  parGet0l( "FLAT", &ensureflat, status );

  /* Filter out useful data (revert to darks if no science data) */
  smf_find_science( igrp, &fgrp, 1, NULL, NULL, 1, 1, SMF__NULL, &darks,
                    &flatramps, &heateffmap, NULL, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  if( size > 0 ) {
    /* Get output file(s) */
    kpg1Wgndf( "OUTAMP", igrp, size, size, "More output files required...",
               &outampgrp, &outampsize, status );

    kpg1Wgndf( "OUTCOMP", igrp, size, size, "More output files required...",
               &outcompgrp, &outcompsize, status );
  } else {
    msgOutif(MSG__NORM, " ","All supplied input frames were DARK,"
       " nothing to flatfield", status );
  }

  /* Get group of bolometer masks and read them into a smfArray */
  smf_request_mask( "BBM", &bbms, status );

  for( i=1; i<=size; i++ ) {

    if( *status != SAI__OK ) break;

    /* Load data, flatfielding and/or opening raw as double as necessary */
    smf_open_asdouble( igrp, i, darks, flatramps, heateffmap, ensureflat, &data, status );

    /* Mask out bad bolometers - mask data array not quality array */
    smf_apply_mask( data, bbms, SMF__BBM_DATA, 0, status );

    /* Calculate the PCA */
    smf_clean_pca( wf, data, 0, &components, &amplitudes, 0, NULL, status );

    /* Write out to the new files */
    smf_write_smfData( amplitudes, NULL, NULL, outampgrp, i, 0, MSG__VERB,
                       status );
    smf_write_smfData( components, NULL, NULL, outcompgrp, i, 0, MSG__VERB,
                       status );

    /* Free resources for output data */
    smf_close_file( &data, status );
    smf_close_file( &amplitudes, status );
    smf_close_file( &components, status );
  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK ) {
    grpList( "OUTAMPFILES", 0, 0, NULL, outampgrp, status );
    if( *status == PAR__NULL ) errAnnul( status );

    grpList( "OUTCOMPFILES", 0, 0, NULL, outcompgrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
  if( igrp ) grpDelet( &igrp, status);
  if( outampgrp ) grpDelet( &outampgrp, status);
  if( outcompgrp ) grpDelet( &outcompgrp, status);
  if( darks ) smf_close_related( &darks, status );
  if( bbms ) smf_close_related( &bbms, status );
  if( flatramps ) smf_close_related( &flatramps, status );
  if (heateffmap) heateffmap = smf_free_effmap( heateffmap, status );
  ndfEnd( status );
}

