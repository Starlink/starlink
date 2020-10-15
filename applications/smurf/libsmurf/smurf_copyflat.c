/*
*+
*  Name:
*     COPYFLAT

*  Purpose:
*     Copy the flatfield information from a reference file

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_copyflat( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine copies the flatfield parameters from a reference
*     data file (usually a raw SCUBA-2 observation or the output from
*     the CALCFLAT command) to a group of files. The flatfield
*     is updated in place.

*  ADAM Parameters:
*     REF = NDF (Read)
*          File from which to read the flatfield parameters. Can be
*          a raw data file that contains the required flatfield or
*          output from the CALCFLAT command. Only a single file
*          for a single subarray can be provided.
*     IN = NDF (Read)
*          Input files to be updated. The subarray must match that
*          used for the reference file.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]

*  Related Applications:
*     SMURF: CALCFLAT

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-10-07 (TIMJ):
*        Original version
*     2010-03-09 (TIMJ):
*        Change type of flatfield method in smfDA
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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

#define FUNC_NAME "smurf_copyflat"
#define TASK_NAME "COPYFLAT"

/* Maximum length for subarray name string */
#define SUB__MAXNAM 8

void smurf_copyflat( int *status ) {

  Grp * flatgrp = NULL;     /* Group of files to update */
  smfData * refdata = NULL; /* reference file */
  Grp * refgrp = NULL;      /* Group for reference file */
  size_t size = 0;          /* size of a group */
  char filename[GRP__SZNAM+1]; /* Input filename, derived from GRP */
  smfDA * refda = NULL;     /* DA struct for reference  */
  char refsub[SUB__MAXNAM+1]; /* Name of reference subarray */
  char flatfile[72];        /* Name of flatfield file */
  size_t i;

  if (*status != SAI__OK) return;

  ndfBegin();

  /* Get reference NDF and open it in the normal way (to get
     the DA extensions). */
  kpg1Rgndf( "REF", 1, 1, "Reference file", &refgrp, &size, status );
  smf_open_file( NULL, refgrp, 1, "READ", SMF__NOCREATE_DATA, &refdata, status );
  grpDelet( &refgrp, status );

  /* make sure that we do have a flatfield solution */
  if ( *status == SAI__OK) {
    if ( refdata && refdata->da && refdata->da->nflat > 0 ) {
      /* looks okay */
      refda = refdata->da;
    } else {
      *status = SAI__ERROR;
      errRep( " ", "Could not read any flatfield information from file",
              status );
      goto CLEANUP;
    }
  }

  /* reference subarray */
  if (*status == SAI__OK) smf_find_subarray( refdata->hdr, refsub, sizeof(refsub),
                                             NULL, status );
  if (*status == SAI__OK) {
    smf_fits_getS( refdata->hdr, "FLAT", flatfile, sizeof(flatfile),
                   status );
  }

  /* Now read the input files */
  kpg1Rgndf( "IN", 0, 1, "Files to update", &flatgrp, &size, status );

  for (i = 1; i <= size; i++ ) {
    char *pname = filename;
    dim_t colsize = (refdata->dims)[SC2STORE__ROW_INDEX];
    dim_t rowsize = (refdata->dims)[SC2STORE__COL_INDEX];
    smfData * moddata = NULL;
    char subarray[SUB__MAXNAM+1];
    AstFitsChan * fitshdr = NULL;
    int place;
    int indf = NDF__NOID;

    /* need to open the file ourselves to work out the
       subarray and get the FITS header */
    smf_open_file( NULL, flatgrp, (int) i, "READ", SMF__NOCREATE_DATA,
                   &moddata, status );

    if (*status != SAI__OK) break;

    smf_find_subarray( moddata->hdr, subarray, sizeof(subarray),
                       NULL, status );

    if (strcmp( subarray, refsub ) != 0 ) {
      if (*status == SAI__OK) {
        *status = SAI__ERROR;
        errRepf( " ", "Subarrays do not match for file %d (%s ne %s)",
                 status, (int)i, subarray, refsub);
      }
      break;
    }

    /* Update the fits header accordingly (since this information
       is not part of the flatfield itself. Get a local copy. */
    if (*status != SAI__OK) break;
    smf_fits_updateS( moddata->hdr, "FLAT", flatfile, "Name of flat-field file",
                      status );
    fitshdr = astClone( moddata->hdr->fitshdr );

    /* No longer need the smfData */
    smf_close_file( NULL, &moddata, status );

    /* Get the filename and update the flatfield */
    grpGet( flatgrp, i, 1, &pname, SMF_PATH_MAX, status );
    sc2store_updflatcal( pname, colsize, rowsize,
                         refda->nflat, refda->refres, smf_flat_methstring(refda->flatmeth,status),
                         refda->flatcal, refda->flatpar, status );

    /* Now we need to update the FITS header - can not use the smfData
       since sc2store reads do not leave the file open */
    ndfOpen( NULL, pname, "UPDATE", "OLD", &indf, &place, status );
    kpgPtfts( indf, fitshdr, status );
    ndfAnnul( &indf, status );
  }

 CLEANUP:
  if (refdata) smf_close_file( NULL, &refdata, status );
  if (flatgrp) grpDelet( &flatgrp, status );
  ndfEnd( status );

}
