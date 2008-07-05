/*
*+
*  Name:
*     SC2CONCAT

*  Purpose:
*     Top-level SC2CONCAT implementation

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
*       o data files are grouped by subarray
*       o files are only concatenated if they are continuous in time 
*       o the longest a concatenated file may be is given by MAXLEN (in sec.)
*       o for each continuous chunk of data, shorter than MAXLEN, a file
*         is generated on disk for each subarray. The file name is determined
*         as the name of the first input file for the chunk, with a suffix
*         "_con".

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input file(s)
*     PADEND = _INTEGER (Read)
*          Number of samples to pad at end.
*     PADSTART = _INTEGER (Read)
*          Number of samples to pad at start.
*     MAXLEN = _DOUBLE (Read)
*          Maximum length (in seconds) concatenated file).

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-07-03 (EC):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2005-2008 University of British Columbia.
*     Copyright (C) 2007-2008 Science and Technology Facilities Council.
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
  smfArray *concat=NULL;     /* Pointer to a smfArray */
  size_t contchunk;          /* Continuous chunk counter */
  smfData *data=NULL;        /* Pointer to a smfData */
  int flag;                  /* Flag for how group is terminated */
  size_t idx;                /* Subarray counter */
  Grp *igrp = NULL;          /* Group of input files */
  smfGroup *igroup=NULL;     /* smfGroup corresponding to igrp */
  int isize;                 /* Number of files in input group */
  dim_t maxconcat=0;         /* Longest continuous chunk length in samples */
  dim_t maxlen=0;            /* Constrain maxconcat to this many samples */
  double maxlen_s;           /* Constrain maxconcat to this many seconds */
  size_t ncontchunks=0;      /* Number continuous chunks outside iter loop */
  dim_t padStart=0;          /* How many samples padding at start */
  dim_t padEnd=0;            /* How many samples padding at end */
  double steptime;           /* Length of an individual sample */
  int temp;                  /* Temporary signed integer */

  if (*status != SAI__OK) return;

  /* Main routine */
  ndfBegin();

  /* Get group of input files */
  ndgAssoc( "IN", 1, &igrp, &isize, &flag, status );

  /* Parse ADAM parameters */

  /* Maximum length of a continuous chunk */
  parGet0d( "MAXLEN", &maxlen_s, status );
  if( *status == PAR__NULL ) {
    errAnnul( status );
    maxlen = 0;
  } else if( maxlen_s < 0 ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "MAXLEN cannot be < 0.", status);
  } else if( maxlen_s > 0 ) {    
    /* Obtain sample length from header of first file in igrp */
    smf_open_file( igrp, 1, "READ", SMF__NOCREATE_DATA, &data, status );
    if( (*status == SAI__OK) && data && (data->hdr) ) {
      smf_fits_getD(data->hdr, "STEPTIME", &steptime, status);
      
      if( steptime > 0 ) {
        maxlen = (dim_t) (maxlen_s / (double) steptime);
      } else {
        /* Trap invalud sample length in header */
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Invalid STEPTIME in FITS header.", status);
      }
    }
    if( data ) smf_close_file( &data, status );
  } else {
    maxlen = 0;
  }
  
  /* Padding */

  parGet0i( "PADSTART", &temp, status );
  if( *status == PAR__NULL ) {
    errAnnul( status );
    padStart = 0;
  } else if( temp < 0 ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "PADSTART cannot be < 0.", status);
  } else {
    padStart = (dim_t) temp;
  }

  parGet0i( "PADEND", &temp, status );
  if( *status == PAR__NULL ) {
    errAnnul( status );
    padEnd = 0;
  } else if( temp < 0 ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "PADEND cannot be < 0.", status);
  } else {
    padEnd = (dim_t) temp;
  }

  /* Group the input files by subarray and continuity */
  smf_grp_related( igrp, isize, 1, maxlen, &maxconcat, &igroup, status );

  /* Obtain the number of continuous chunks and subarrays */
  if( *status == SAI__OK ) {
    ncontchunks = igroup->chunk[igroup->ngroups-1]+1;
  }

  /* Loop over continuous chunks */
  for( contchunk=0;(*status==SAI__OK)&&contchunk<ncontchunks; contchunk++ ) {

    /* Concatenate. Note that smf_concat_smfGroup stores filenames for
       each piece in concat->sdata[*]->file->name */
    smf_concat_smfGroup( igroup, contchunk, 1, NULL, 0, NULL, 
                         NULL, padStart, padEnd, 0, &concat, status );
    
    /* Export concatenated data for each subarray to NDF file */
    for( idx=0; (*status==SAI__OK)&&idx<concat->ndat; idx++ ) {
      if( concat->sdata[idx]->file && concat->sdata[idx]->file->name ) {
        smf_NDFexport( concat->sdata[idx], NULL, NULL, NULL, 
                       concat->sdata[idx]->file->name, status );
      } else {
        *status = SAI__ERROR;
        errRep( FUNC_NAME, 
                "Unable to determine file name for concatenated data.",
                status );
      }
    }

    /* Close the smfArray */
    smf_close_related( &concat, status );
  
  }

  if( igrp != NULL ) grpDelet( &igrp, status);

  ndfEnd( status );
  
  if( *status == SAI__OK ) {
    msgOutif(MSG__VERB," ","SC2CONCAT succeeded.", status);
  } else {
    msgOutif(MSG__VERB," ","SC2CONCAT failed.", status);
  }

}
