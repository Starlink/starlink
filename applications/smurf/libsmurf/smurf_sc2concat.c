/*
*+
*  Name:
*     SC2CONCAT

*  Purpose:
*     Concatenate files into a larger file.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2concat( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a list of input files this task concatenates them into larger
*     files. The rules it follows are:
*     - data files are grouped by subarray;
*     - files are only concatenated if they are continuous in time;
*     - the longest a concatenated file may be is given by MAXLEN (in sec.);
*     - for each continuous chunk of data, shorter than MAXLEN, a file
*     is generated on disk for each subarray. The file name is determined
*     as the name of the first input file for the chunk, with a suffix
*     "_con". The can be modified using the parameter OUT.

*  ADAM Parameters:
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
*          Input file(s).
*     MAXLEN = _DOUBLE (Read)
*          Maximum length (in seconds) for concatenated file. The
*          default is to use all data if possible (subject to
*          available memory). [!]
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output concatenated files. Only used if OUTBASE is null (!). Note,
*          the correct number of output files must be specified for OUT. If
*          this number is not known, use parameter OUTBASE instead.
*     OUTBASE = LITERAL (Write)
*          The base name for the output NDFs. Each output NDF has a name
*          equal to "base_<n>" where <n> is an integer greater than or
*          equal to 1. If a null (!) value is supplied, the output NDFs
*          are instead specified by parameter OUT. [!]
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

*  Related Applications:
*     SMURF: SC2FFT, SC2CLEAN

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-07-03 (EC):
*        Initial version.
*     2008-07-22 (TIMJ):
*        Handle darks. USe kaplibs for groups. Free memory leak.
*     2008-07-29 (EC):
*        -Added parameter OUT
*        -Switch to parGdr routines, and use steptime from header
*     2009-03-30 (TIMJ):
*        Add OUTFILES parameter.
*     2009-10-20 (EC):
*        Add USEDARKS parameter.
*     2009-12-07 (EC):
*        Add FLAT parameter.
*     2010-03-11 (TIMJ):
*        Support flatfield ramps.
*     2013-08-21 (AGG):
*        Do not call grpList if no output files are generated. This
*        avoids a GRP__INVID error in such cases.
*     2014-02-26 (DSB):
*        Added parameter OUTBASE.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2005-2009,2013 University of British Columbia.
*     Copyright (C) 2008-2014 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
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

/* Starlink includes */
#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "star/one.h"
#include "par.h"
#include "par_err.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smurf_sc2concat"
#define TASK_NAME "SC2CONCAT"

void smurf_sc2concat( int *status ) {

  /* Local Variables */
  Grp *basegrp=NULL;         /* Grp containing first file each chunk */
  size_t basesize;           /* Number of files in base group */
  smfArray *concat=NULL;     /* Pointer to a smfArray */
  size_t contchunk;          /* Continuous chunk counter */
  smfArray *darks = NULL;    /* dark frames */
  int ensureflat;            /* Flag for flatfielding data */
  Grp *fgrp = NULL;          /* Filtered group, no darks */
  smfArray * flatramps = NULL; /* Flatfield ramps */
  AstKeyMap *heateffmap = NULL;    /* Heater efficiency data */
  size_t gcount=0;           /* Grp index counter */
  size_t idx;                /* Subarray counter */
  int usedarks;              /* flag for using darks */
  Grp *igrp = NULL;          /* Group of input files */
  smfGroup *igroup=NULL;     /* smfGroup corresponding to igrp */
  size_t isize;              /* Number of files in input group */
  dim_t maxconcat=0;         /* Longest continuous chunk length in samples */
  double maxlen;             /* Constrain maxconcat to this many seconds */
  size_t ncontchunks=0;      /* Number continuous chunks outside iter loop */
  Grp *ogrp = NULL;          /* Output files  */
  size_t osize;              /* Number of files in input group */
  char outbase[ 200 ];       /* Base name for output NDFs */
  dim_t padStart=0;          /* How many samples padding at start */
  dim_t padEnd=0;            /* How many samples padding at end */
  int temp;                  /* Temporary signed integer */
  ThrWorkForce *wf = NULL;   /* Pointer to a pool of worker threads */

  if (*status != SAI__OK) return;

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

  /* Read the input file */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &isize, status );

  /* Filter out darks */
  smf_find_science( wf, igrp, &fgrp, 1, NULL, NULL, 1, 1, SMF__NULL, &darks,
                    &flatramps, &heateffmap, NULL, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  isize = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  if (isize == 0) {
    msgOutif(MSG__NORM, " ","All supplied input frames were filtered,"
       " nothing to do", status );
    goto CLEANUP;
  }

  /* --- Parse ADAM parameters ------------------------ */

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

  /* Group the input files by subarray and continuity */
  smf_grp_related( igrp, isize, 1, 0, maxlen-padStart-padEnd, NULL, NULL,
                   &maxconcat, NULL, &igroup, &basegrp, NULL, status );

  /* Obtain the number of continuous chunks and subarrays */
  if( *status == SAI__OK ) {
    ncontchunks = igroup->chunk[igroup->ngroups-1]+1;
  }

  basesize = grpGrpsz( basegrp, status );

  /* First see the output NDF names are to be generated automatically
     from a supplied base name. */
  if( *status == SAI__OK ) {
    parGet0c( "OUTBASE", outbase, sizeof( outbase ), status );

    /* If yes, generate the names and put them in "ogrp". */
    if( *status == SAI__OK ) {
       char *fname = astMalloc( sizeof( outbase ) + 10 );
       if( fname ) {
          ogrp = grpNew( "Output NDFs", status );
          for( gcount = 0; gcount < basesize; gcount++ ) {
             sprintf( fname, "%s_%zu", outbase, gcount + 1 );
             grpPut1( ogrp, fname, 0, status );
          }
          osize = basesize;
          fname = astFree( fname );
       }

    /* If not, anull the error and get an explicit list of output NDFs. */
    } else if( *status == PAR__NULL ) {
       errAnnul( status );
       kpg1Wgndf( "OUT", basegrp, basesize, basesize,
                  "More output files required...",
                  &ogrp, &osize, status );
    }
  }

  /* Loop over continuous chunks */
  gcount = 1;
  for( contchunk=0;(*status==SAI__OK)&&contchunk<ncontchunks; contchunk++ ) {

    /* Concatenate this continuous chunk */
    smf_concat_smfGroup( wf, NULL, igroup, usedarks ? darks:NULL, NULL, flatramps,
                         heateffmap, contchunk, ensureflat, 1, NULL, 0, NULL, NULL,
                         NO_FTS, padStart, padEnd, 0, &concat, NULL, status );

    /* Export concatenated data for each subarray to NDF file */
    for( idx=0; (*status==SAI__OK)&&idx<concat->ndat; idx++ ) {
      if( concat->sdata[idx]->file && concat->sdata[idx]->file->name ) {
        smf_write_smfData( wf, concat->sdata[idx], NULL, NULL, ogrp, gcount,
                           NDF__NOID, MSG__VERB, 0, NULL, NULL, status );
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
  if( darks ) smf_close_related( wf, &darks, status );
  if( flatramps ) smf_close_related( wf, &flatramps, status );
  if (heateffmap) heateffmap = smf_free_effmap( heateffmap, status );
  if( igrp ) grpDelet( &igrp, status);
  if( basegrp ) grpDelet( &basegrp, status );
  if( ogrp ) grpDelet( &ogrp, status );
  if( igroup ) smf_close_smfGroup( &igroup, status );

  ndfEnd( status );

  if( *status == SAI__OK ) {
    msgOutif(MSG__VERB," ","SC2CONCAT succeeded.", status);
  } else {
    msgOutif(MSG__VERB," ","SC2CONCAT failed.", status);
  }

}
