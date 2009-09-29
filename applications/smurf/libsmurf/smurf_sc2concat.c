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
 *     - data files are grouped by subarray
 *     - files are only concatenated if they are continuous in time
 *     - the longest a concatenated file may be is given by MAXLEN (in sec.)
 *     - for each continuous chunk of data, shorter than MAXLEN, a file
 *     is generated on disk for each subarray. The file name is determined
 *     as the name of the first input file for the chunk, with a suffix
 *     "_con". The can be modified using the parameter OUT.

 *  ADAM Parameters:
 *     IN = NDF (Read)
 *          Input file(s)
 *     MSG_FILTER = _CHAR (Read)
 *          Control the verbosity of the application. Values can be
 *          NONE (no messages), QUIET (minimal messages), NORMAL,
 *          VERBOSE, DEBUG or ALL. [NORMAL]
 *     OUT = NDF (Write)
 *          Output concatenated files
 *     OUTFILES = LITERAL (Write)
 *          The name of text file to create, in which to put the names of
 *          all the output NDFs created by this application (one per
 *          line). If a null (!) value is supplied no file is created. [!]
 *     PADEND = _INTEGER (Read)
 *          Number of samples to pad at end.
 *     PADSTART = _INTEGER (Read)
 *          Number of samples to pad at start.
 *     MAXLEN = _DOUBLE (Read)
 *          Maximum length (in seconds) concatenated file).

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
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
 *     Council. Copyright (C) 2005-2008 University of British Columbia.
 *     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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
 *     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
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
  smfData *data=NULL;        /* Pointer to a smfData */
  Grp *fgrp = NULL;          /* Filtered group, no darks */
  char fname[GRP__SZNAM+1];  /* String for holding filename */
  size_t gcount=0;           /* Grp index counter */
  size_t i;                  /* Loop counter */
  size_t idx;                /* Subarray counter */
  Grp *igrp = NULL;          /* Group of input files */
  smfGroup *igroup=NULL;     /* smfGroup corresponding to igrp */
  size_t isize;              /* Number of files in input group */
  dim_t maxconcat=0;         /* Longest continuous chunk length in samples */
  dim_t maxlen=0;            /* Constrain maxconcat to this many samples */
  double maxlen_s;           /* Constrain maxconcat to this many seconds */
  size_t ncontchunks=0;      /* Number continuous chunks outside iter loop */
  Grp *ogrp = NULL;          /* Output files  */
  size_t osize;              /* Number of files in input group */
  dim_t padStart=0;          /* How many samples padding at start */
  dim_t padEnd=0;            /* How many samples padding at end */
  char *pname=NULL;          /* Poiner to fname */
  int temp;                  /* Temporary signed integer */
  smfWorkForce *wf = NULL;   /* Pointer to a pool of worker threads */

  if (*status != SAI__OK) return;

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = smf_create_workforce( smf_get_nthread( status ), status );

  /* Read the input file */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &isize, status );

  /* Filter out darks */
  smf_find_darks( igrp, &fgrp, NULL, 1, SMF__NULL, &darks, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  isize = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  if (isize == 0) {
    msgOutif(MSG__NORM, " ","All supplied input frames were DARK,"
             " nothing to concatenate", status );
    goto CLEANUP;
  }

  /* --- Parse ADAM parameters ------------------------ */

  /* Maximum length of a continuous chunk */
  parGdr0d( "MAXLEN", 0, 0, VAL__MAXI, 1, &maxlen_s, status );
  if( maxlen_s > 0 ) {
    /* Obtain sample length from header of first file in igrp */
    smf_open_file( igrp, 1, "READ", SMF__NOCREATE_DATA, &data, status );
    if( (*status == SAI__OK) && data && (data->hdr) ) {
      maxlen = (dim_t) (maxlen_s / data->hdr->steptime);
    }
    if( data ) smf_close_file( &data, status );
  } else {
    maxlen = 0;
  }

  /* Padding */
  parGdr0i( "PADSTART", 0, 0, VAL__MAXI, 1, &temp, status );
  padStart = (dim_t) temp;

  parGdr0i( "PADEND", 0, 0, VAL__MAXI, 1, &temp, status );
  padEnd = (dim_t) temp;

  /* Group the input files by subarray and continuity */
  smf_grp_related( igrp, isize, 1, maxlen-padStart-padEnd, &maxconcat, &igroup,
                   status );

  /* Obtain the number of continuous chunks and subarrays */
  if( *status == SAI__OK ) {
    ncontchunks = igroup->chunk[igroup->ngroups-1]+1;
  }

  /* Create a base group of filenames */
  basegrp = grpNew( "Base Group", status );

  /* Loop over time chunks */
  for( i=0; (*status==SAI__OK)&&(i<igroup->ngroups); i++ ) {
    /* Loop over subarray */
    for( idx=0; idx<igroup->nrelated; idx++ ) {
      /* Check for new continuous chunk */
      if( i==0 || (igroup->chunk[i] != igroup->chunk[i-1]) ) {
        ndgCpsup( igroup->grp, igroup->subgroups[i][idx], basegrp, status );
      }
    }
  }

  basesize = grpGrpsz( basegrp, status );

  /* Get output file(s) */
  kpg1Wgndf( "OUT", basegrp, basesize, basesize,
             "More output files required...",
             &ogrp, &osize, status );

  /* Loop over continuous chunks */
  gcount = 1;
  for( contchunk=0;(*status==SAI__OK)&&contchunk<ncontchunks; contchunk++ ) {

    /* Concatenate this continuous chunk */
    smf_concat_smfGroup( wf, igroup, darks, NULL, contchunk, 1, NULL, 0, NULL,
                         NULL, padStart, padEnd, 0, &concat, status );

    /* Export concatenated data for each subarray to NDF file */
    for( idx=0; (*status==SAI__OK)&&idx<concat->ndat; idx++ ) {
      if( concat->sdata[idx]->file && concat->sdata[idx]->file->name ) {

        /* Get the file name: note that we have to be careful to read
           them out of this group in the same order that we put them
           in above! */
        pname = fname;
        grpGet( ogrp, gcount, 1, &pname, GRP__SZNAM, status);
        smf_write_smfData( concat->sdata[idx], NULL, NULL, fname, NDF__NOID,
                           status );
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
    smf_close_related( &concat, status );

  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

 CLEANUP:
  if( wf ) wf = smf_destroy_workforce( wf );
  if( darks ) smf_close_related( &darks, status );
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
