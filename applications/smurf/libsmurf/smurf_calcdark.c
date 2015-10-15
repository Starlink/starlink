/*
*+
*  Name:
*     CALCDARK

*  Purpose:
*     Calculate the 2d dark frame from dark observation.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_calcdark( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a set of dark observations, calculate a mean dark frame
*     from each. A bad bolometer mask can be supplied to remove known
*     bad bolometers. Does not flatfield.

*  Notes:
*     Dark files will be subtracted from raw data during the
*     flatfielding step. Commands that flatfield data can use either
*     raw dark files or the output from CALCDARK.

*  ADAM Parameters:
*     BBM = NDF (Read)
*          Group of files to be used as bad bolometer masks. Each data file
*          specified with the IN parameter will be masked. The corresponding
*          previous mask for a subarray will be used. If there is no previous
*          mask the closest following one will be used. It is not an error for
*          no mask to match. A NULL parameter indicates no mask files to be
*          supplied. [!]
*     IN = NDF (Read)
*          Input files to be processed. Non-darks will be filtered out.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output dark files. These can be used as bad bolometer masks
*          in subsequent processing steps via the BBM parameter in
*          other SCUBA-2 SMURF commands.

*  Related Applications:
*     SMURF: FLATFIELD, MAKEMAP

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-08-22 (TIMJ):
*        Initial version.
*     2008-11-12 (AGG):
*        Check status before beginning loop
*     2008-12-12 (TIMJ):
*        Allow BPM masking.
*     2010-01-08 (AGG):
*        Change BPM to BBM.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "star/kaplibs.h"
#include "star/grp.h"
#include "star/ndg.h"
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

#define FUNC_NAME "smurf_calcdark"

void smurf_calcdark( int *status ) {

  smfArray *bbms = NULL;    /* Bad bolometer masks */
  smfArray *darks = NULL;   /* set of processed darks */
  Grp *dgrp = NULL;         /* Group of darks */
  size_t i;                 /* Loop index */
  int indf;                 /* NDF identifier for input file */
  Grp *igrp = NULL;         /* Input group of files */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Total number of NDF names in the output group */
  size_t size;              /* Number of files in input group */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out non-darks and reduce the darks themselves */
  smf_find_science( NULL, igrp, NULL, 0, &dgrp, NULL, 1, 0, SMF__DOUBLE, &darks, NULL,
                    NULL, NULL, status );

  /* no longer need the input group */
  grpDelet( &igrp, status );

  /* Get output file(s) */
  size = grpGrpsz( dgrp, status );
  kpg1Wgndf( "OUT", dgrp, size, size, "More output files required...",
             &ogrp, &outsize, status );

  /* Get group of bolometer masks and read them into a smfArray */
  smf_request_mask( NULL, "BBM", &bbms, status );

  for (i=1; i<=size && *status == SAI__OK; i++ ) {
    smfData * dark = (darks->sdata)[i-1]; /* This dark */

    /* Open input file and create output file. Do not propagate
       since we do not want to get a large file the wrong size */
    ndgNdfas( dgrp, i, "READ", &indf, status );

    smf_apply_mask( NULL, dark, bbms, SMF__BBM_DATA, 0, status );
    smf_write_smfData( NULL, dark, NULL, NULL, ogrp, i, indf, MSG__VERB,
                       0, NULL, NULL, status );
    ndfAnnul( &indf, status);
  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &dgrp, status);
  grpDelet( &ogrp, status);
  smf_close_related( NULL, &darks, status );
  smf_close_related( NULL, &bbms, status );

  ndfEnd( status );
}

