/*
*+
*  Name:
*     SC2EXPANDMODEL

*  Purpose:
*     Expand a DIMM model component into a full time-series data cube

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2expandmodel( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This command is a stand-alone task for converting a DIMM model component
*     (which may be stored as a series of model parameters) into a full
*     time-series representation (a data cube with time along the third axis).

*  Notes:

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be uncompressed and flatfielded.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output file(s).

*  Related Applications:
*     SMURF: makemap

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-05-19 (EC):
*        Initial version
*     2010-06-9 (DSB):
*        Guard against null idata pointers.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
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
#include "star/atl.h"
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

#define FUNC_NAME "smurf_sc2expandmodel"
#define TASK_NAME "SC2EXPANDMODEL"

void smurf_sc2expandmodel( int *status ) {
  smf_expmodelptr expptr=NULL;/* Pointer to current model calc function */
  size_t i=0;               /* Counter, index */
  smfData *idata=NULL;      /* Input data */
  Grp *igrp = NULL;         /* Input group of files */
  smfData *odata=NULL;      /* Output data */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Number of files in output group */
  size_t size;              /* Number of files in input group */

  /* Main routine */
  ndfBegin();

  /* Read the input file */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Get output file(s) */
  kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
             &ogrp, &outsize, status );

  /* Loop over input files */
  for( i=1; (*status==SAI__OK) && (i<=size); i++ ) {

    /* Open file */
    smf_open_file(NULL, igrp, i, "READ", 0, &idata, status);

    /* Check to see if this is a DIMM model component before expanding */
    if( idata && idata->hdr && (idata->hdr->mtype != SMF__NUL) ) {
      expptr = smf_model_getexpptr( idata->hdr->mtype, status );

      if( *status == SAI__OK ) {
        (*expptr)( idata, &odata, status );
        smf_write_smfData( NULL, odata, NULL, NULL, ogrp, i, 0,
                           MSG__VERB, 0, NULL, NULL, status );
      }
    }
  }

  /* Cleanup */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
